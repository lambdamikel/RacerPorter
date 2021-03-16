;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(defun is-number-p (string)
  (let* ((string (coerce string 'list))
         (first (first string))
         (found-p nil))
    (when (and first (char= first #\-))
      (pop string))
    (every #'(lambda (x)
               (or (<= 48 (char-code x) 57) ; 0 - 9
                   (when (and (not found-p)
                              (char= x #\.))
                     (setf found-p t))))
           string)))

(defun special-char-p (char) 
  (and (not (alphanumericp char))
       (not (member char '(#\? #\* #\+)))))

(defun needs-bars-p (symbol)
  ;;; nur f. printed representation! 
  ;;; read benoetigt dennoch welche! 
  ;;; NUR "visuelle Vereinfachung" f. die pretty-printer!
  (let ((name (symbol-name symbol)))
    (find-if #'(lambda (x)
                 (whitespace-char-p x))
             name)))

(defun reader-macro-symbol-p (symbol)
  (let* ((name (symbol-name symbol))
        (n (length name)))
    (and (char= (elt name 0) #\#)
         (let ((char (elt name 1)))
           (or (and (> n 2)
                    (member char 
                            '(#\! #\&)))
               (and (= n 2)
                    (member char '(#\T #\F #\t #\f))))))))

(defun io-needs-bars-p (symbol)
  (and (not (reader-macro-symbol-p symbol))
       (let ((name (symbol-name symbol)))
         (find-if #'(lambda (x)
                      (or (whitespace-char-p x)
                          (not (alpha-char-p x))))
                  name))))

(defun racer-symbol-p (symbol)
  (gethash (string-upcase (symbol-name symbol)) +racer-symbols+))

;;;
;;;
;;;

(defmacro with-standard-pretty-printing (&rest body)
  `(let ((*print-pretty* t)
         (*print-pprint-dispatch* *old-pprint-dispatch*))
     ,@body))

(defmacro with-pretty-printing ((&key (print-doublequotes-p t) (one-line-p nil)
                                      (escape-newlines-etc-p one-line-p)
                                      ;;(interpret-newlines-p t)
                                      (keywords-p t) (no-bars-p nil))
                                &rest body)
  (progn 
    `(let ((*print-pretty* t)
           (*print-pprint-dispatch* *pprint-dispatch1*)
           ,@(when one-line-p `((*print-lines* 1)
                                (*print-right-margin* 
                                 ,(if (numberp  one-line-p)
                                      one-line-p 
                                    10000)))))

       #|
       (set-pprint-dispatch 'character
                            #'(lambda (stream &rest args)
                                 (let* ((char (first args)))
                                   (if (and ,interpret-newlines-p 
                                            (char= char #\newline))
                                       (terpri stream)
                                     (write-char char stream)))))
       |# 

       (set-pprint-dispatch 'symbol
                            #'(lambda (stream &rest args)
                                (let* ((symbol (first args))
                                       (bars-needed-p 
                                        (and ,(not no-bars-p)
                                             (needs-bars-p symbol))))

                                  (when bars-needed-p
                                    (write-string "|" stream))
                                  
                                  (when (and ,keywords-p (keywordp symbol))
                                    (write-string ":" stream))

                                  (write-string
                                   (if (profile-remove-urls (active-profile)) 
                                       (remove-url-prefixes (symbol-name symbol))
                                     (symbol-name symbol))
                                   stream)

                                  (when bars-needed-p
                                    (write-string "|" stream)))))

       (set-pprint-dispatch 'string
                            #'(lambda (stream &rest args)
                                (let ((string 
                                       (first args)))

                                  (when ,print-doublequotes-p 
                                    (write-string "\"" stream))

                                  (write-string 
                                   ,(if escape-newlines-etc-p
                                        `(escape-newlines-etc string)
                                      'string)
                                   stream)

                                  (when ,print-doublequotes-p 
                                    (write-string "\"" stream)))))

       ,@body)))

(defmacro with-io-printing ((&key (double-backslashes-if-needed-p t)) &rest body)
  (progn 
    `(let ((*print-pretty* t)
           (*print-lines* 1)
           (*print-right-margin* 10000)
           (*print-pprint-dispatch* *pprint-dispatch2*))
       
       (set-pprint-dispatch 'symbol
                            #'(lambda (stream &rest args)
                                (let ((symbol (first args)))

                                  (cond ((eq (slot-value (active-profile) 'alisp-racer) t)
                                         
                                         (when (keywordp symbol)
                                           (write-string ":" stream))                

                                         (let* ((name (symbol-name symbol))
                                                (racer-symbol-p (racer-symbol-p symbol))
                                                (bars-p (and 
                                                         (or 
                                                          #+:racer-with-sirius
                                                          t
                                                          #-:racer-with-sirius
                                                          (not racer-symbol-p)
                                                          (is-racer-mixed-case-command-p name))
                                                         (not
                                                          (reader-macro-symbol-p symbol))
                                                         (or (io-needs-bars-p symbol)
                                                             (find-if #'(lambda (char) 
                                                                          (lower-case-p char))
                                                                      name))))
                                                (name (if bars-p
                                                          name
                                                        (string-upcase name)))
                                                (name (if ,double-backslashes-if-needed-p 
                                                          (double-backslashes-if-needed name)
                                                        name)))

                                           (declare (ignorable racer-symbol-p))

                                           (when bars-p (write-string "|" stream))

                                           (write-string name stream)
                                           
                                           (when bars-p (write-string "|" stream))))


                                        (t

                                         (let* ((bars-p (io-needs-bars-p symbol))
                                                (name (symbol-name symbol))
                                                (name (if ,double-backslashes-if-needed-p
                                                          (double-backslashes-if-needed name)
                                                        name)))

                                           (when (keywordp symbol)
                                             (write-string ":" stream))
                                           
                                           (when bars-p  (write-string "|" stream))

                                           (write-string name stream)

                                           (when bars-p  (write-string "|" stream))))))))

       (set-pprint-dispatch 'string
                            #'(lambda (stream &rest args)
                                (write-string "\"" stream)
                                (write-string 
                                 (escape-newlines-etc ; sonst Zeilenumbrueche, und "..." im Ergebnis! 
                                  (first args))
                                 stream)
                                (write-string "\"" stream)))
       ,@body)))

;;;
;;;
;;;

(defun block-item-printer (x &optional stream highlight-p)
  (with-pretty-printing ()
                        (with-profile-access
                          (let ((*package* (find-package :racer-user)))
                            (format stream 
                                    (if highlight-p ">>> ~S <<<" "~S")
                                    (if (blank-line-p x)
                                        ""
                                      x))))))

(defun line-item-printer (x &optional stream highlight-p)
  (with-pretty-printing (:one-line-p t)
                        (with-profile-access
                          (let ((*package* (find-package :racer-user)))
                            (format stream 
                                    (if highlight-p ">>> ~S <<<" "~S") 
                                    (if (blank-line-p x)
                                        ""
                                      x))))))

(defun line-item-printer-also-print-nil (x &optional stream highlight-p)
  (with-pretty-printing (:one-line-p t)
                        (with-profile-access
                          (let ((*package* (find-package :racer-user)))
                            (format stream 
                                    (if highlight-p ">>> ~S <<<" "~S") 
                                    x)))))

(defun line-item-printer-permit-empty-content (x &optional stream highlight-p)
  (if x
      (with-pretty-printing (:one-line-p t)
                            (with-profile-access
                              (let ((*package* (find-package :racer-user)))
                                (format stream 
                                        (if highlight-p ">>> ~S <<<" "~S")
                                        x))))
    (format stream "")))


(defun line-item-printer-max-width-permit-empty-content (x &optional stream highlight-p)
  (if x 
      (with-pretty-printing (:one-line-p 40)
                            (with-profile-access
                              (let ((*package* (find-package :racer-user)))
                                (format stream 
                                        (if highlight-p ">>> ~S <<<" "~S") 
                                        (if (blank-line-p x)
                                            ""
                                          x)))))
    (format stream "")))


(defun line-item-printer-no-doublequotes (x &optional stream highlight-p)
  (with-pretty-printing (:one-line-p t :print-doublequotes-p nil)
                        (with-profile-access
                          (let ((*package* (find-package :racer-user)))
                            (format stream 
                                    (if highlight-p ">>> ~S <<<" "~S") 
                                    (if (blank-line-p x)
                                        ""
                                      x))))))

(defun line-item-printer-no-doublequotes-no-keywords (x &optional stream highlight-p)
  (with-pretty-printing (:one-line-p t :print-doublequotes-p nil :keywords-p nil)
                        (with-profile-access
                          (let ((*package* (find-package :racer-user)))
                            (format stream 
                                    (if highlight-p ">>> ~S <<<" "~S") 
                                    (if (blank-line-p x)
                                        ""
                                      x))))))


(defun line-item-printer-no-doublequotes-no-keywords-no-bars (x &optional stream highlight-p)
  (with-pretty-printing (:one-line-p t :print-doublequotes-p nil :keywords-p nil :no-bars-p t)
                        (with-profile-access
                          (let ((*package* (find-package :racer-user)))
                            (format stream 
                                    (if highlight-p ">>> ~S <<<" "~S") 
                                    (if (blank-line-p x)
                                        ""
                                      x))))))

(defun line-item-printer-no-doublequotes-permit-empty-content (x &optional stream highlight-p)
  (if x
      (with-pretty-printing (:one-line-p t :print-doublequotes-p nil)
                            (with-profile-access
                              (let ((*package* (find-package :racer-user)))
                                (format stream 
                                        (if highlight-p ">>> ~S <<<" "~S")
                                        x))))
    (format stream "")))

(defun raw-item-printer (x &optional stream highlight-p)
  (let ((*package* (find-package :racer-user)))
    (format stream 
            (if highlight-p ">>> ~A <<<" "~A") 
            (if (blank-line-p x)
                ""
              x))))
                 
;;;
;;;
;;;

      
(defun print-pretty-block (strings stream &optional (indent :center) n)
  (if (not stream)
      (with-output-to-string (stream)
        (print-pretty-block strings stream indent n))
    (let ((n 
           (or n
               (max (or n 0) (loop as i in strings maximize (length i))))))

      (dolist (string strings)
      
        (when (or (not (eq indent :block))
                  (string= string ""))
          (format stream "~%"))

        (let* ((m (length string))
               (r (ecase indent
                    ((:left :block) 0)
                    (:center (- (/ n 2) (/ m 2)))
                    (:right (- n m)))))
        
          (loop as i from 1 to r do (format stream " "))
          (format stream "~A" string)))

      (format stream "~%"))))

