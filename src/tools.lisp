;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(defvar *debug-reader* nil)

(defun whitespace-char-p (char)
  (or (lispworks:whitespace-char-p char)
      (member char '(#\No-Break-Space))))

(defun is-racer-command-p (string)
  (let ((string (ensure-string string)))
    (gethash string 
             +commands-hash+)))

(defun is-racer-mixed-case-command-p (string)
  (let ((string (ensure-string string)))
    (gethash string
             +mixed-case-commands-hash+)))


(defun copy-hash-table (hash)
  (let ((newhash
         (make-hash-table 
          :size (hash-table-size hash)
          :rehash-size (hash-table-rehash-size hash)
          :rehash-threshold (hash-table-rehash-threshold hash)
          :test (hash-table-test hash))))

    (maphash #'(lambda (key val)
                 (setf (gethash key newhash) val))
             hash)

    newhash))

(defun make-hash-table-from (items)
  (let ((hash (make-hash-table :size 1000 :rehash-size 2.0)))
    (dolist (item items)
      (setf (gethash item hash) t))
    hash))

(defun msort (x &rest args)
  (apply #'sort (copy-list x) args))

(defun short-string (s-expr)
  (cond ((stringp s-expr)
         (with-output-to-string (stream)
           (handler-case 
               (progn 
                 (write-string (subseq s-expr 0 4) stream)
                 (write-string "..." stream))
             (error ()
               (write-string s-expr stream)))))
        (t 
         (let ((*print-length* 4)
               (*print-level* 4))
           (format nil "~S" s-expr)))))

(defun tree-find (tree x &rest args)
  (or (apply #'member x tree args)
      (some #'(lambda (sub)
		(and (consp sub)
		     (apply #'tree-find sub x args)))
	    tree)))

(defun tree-count (tree)
  (let ((count 0))

    (labels ((do-it (tree)
               (when tree
                 (incf count)
                 (dolist (child (rest tree))
                   (do-it child)))))
      (do-it tree)
      count)))


(defun tree-member (x tree &rest args)
  (apply #'tree-find tree x args))

(defun tflatten (tree)
  (if (consp tree)
      (append (tflatten (car tree))
	      (tflatten (cdr tree)))
    (when tree (list tree))))


(defun blank-line-p (string)
  (or (not string)
      (and (stringp string)
           (every #'whitespace-char-p string))))

(defun get-separator (string &optional (char #\=))
  (make-string (length string) :initial-element char))

(defun get-longest-string (strings)
  (sort (copy-list strings) #'> :key #'length))

(defun escape-newlines-etc (string)
  (let ((n (1+ (length string))))
    (labels ((do-it (string)
               (let* ((pos1 (or (position #\newline string) n))
                      (pos2 (or (position #\" string) n))
                      (pos (min pos1 pos2 )))

                 (if (not (= pos n))
                     (cond ((= pos pos1)
                            (concatenate 'string 
                                         (subseq string 0 pos)
                                         "\\N"
                                         (do-it (subseq string (+ 1 pos)))))
                           ((= pos pos2)
                            (concatenate 'string 
                                         (subseq string 0 pos)
                                         "S"
                                         (do-it (subseq string (+ 1 pos))))))
                   string))))

      (do-it string))))


(defun expand-n-s-etc (string)
  (let ((n (1+ (length string))))
    (labels ((do-it (string)
               (let* ((pos1 (or (search "\\N" string) n))
                      (pos2 (or (search "\\S" string) n))
                      (pos (min pos1 pos2 )))

                 (if (not (= pos n))
                     (cond ((= pos pos1)
                            (concatenate 'string 
                                         (subseq string 0 pos)
                                         +newline-string+
                                         (do-it (subseq string (+ 2 pos)))))
                           ((= pos pos2)
                            (concatenate 'string 
                                         (subseq string 0 pos)
                                         "\""
                                         (do-it (subseq string (+ 2 pos))))))
                   string))))

      (do-it string))))

(defun double-backslashes (string)
  (let ((string (format nil "~S" string)))
    (subseq string 1 (1- (length string)))))


(defun compact-backslashes (string)
  (let ((pos (search "\\\\" string)))
    (if pos
        (concatenate 'string 
                     (subseq string 0 pos)
                     "\\"
                     (compact-backslashes (subseq string (+ 2 pos))))
      string)))

(defun double-backslashes-if-needed (string)
  (let ((pos (search "\\" string)))
    (if pos
        (concatenate 'string 
                     (subseq string 0 pos)
                     "\\\\"
                     (if (char= (elt string (1+ pos)) #\\)
                         (double-backslashes-if-needed (subseq string (+ 2 pos)))
                       (double-backslashes-if-needed (subseq string (+ 1 pos)))))
      string)))

(defun ensure-list (arg)
  (if (listp arg)
      arg
    (list arg)))

(defun ensure-string (arg)
  (if (stringp arg)
      arg
    (if (symbolp arg)
        (symbol-name arg)
      (format nil "~A" arg))))

(defun file-is-readable-p (file)
  (harlequin-common-lisp:file-readable-p file))

(defun remove-url-prefixes (obj)
  (labels ((do-it (obj)
             (typecase obj
               (null obj)
               (number obj)
               (racer-boolean obj)
               (symbol
                (with-standard-pretty-printing
                 (read-from-string-preserving-case
                  (do-it (format nil "~S" obj))
                  nil nil)))
               (string
                (if (string-equal obj "nil")
                    obj
                  (let ((prefix1 nil)
                        (ns1 nil)
                        (lobj (length obj)))
                  
                    (loop as ns being the hash-key 
                          in (profile-namespace-2-prefix-hash (active-profile))
                          as prefix being the hash-value 
                          in (profile-namespace-2-prefix-hash (active-profile))
                        
                          do 
                          (let ((pos (search ns obj)))
                            (when (and pos 
                                       (not (zerop (length prefix)))
                                       (zerop pos))
                              (setf prefix1 prefix
                                    ns1 ns)
                              (return))))

                    (if ns1 
                        (format nil "#!~A:~A" prefix1 (subseq obj (length ns1)))
                      (let* ((def-ns
                              (gethash "" (profile-prefix-2-namespace-hash (active-profile))))
                             (pos (and def-ns (search def-ns obj)))
                             (ldef (length def-ns)))
                        (if (and pos (zerop pos)
                                 (not (zerop lobj)))
                            (cond ((> lobj ldef)
                                   (let ((char (elt obj ldef)))
                                     (if (or (char= char #\#)
                                             (char= char #\/))
                                         (format nil "#!:~A" (subseq obj (1+ ldef)))
                                       (format nil "#!:~A" (subseq obj ldef)))))
                                  ((= lobj ldef)
                                   obj)
                                  (t
                                   (format nil "#!:~A" (subseq obj ldef))))
                          obj))))))
               (cons
                (cons (do-it (car obj))
                      (do-it (cdr obj))))
               (t nil))))
    (with-standard-pretty-printing
     (do-it obj))))
             

(defun extract-string (string)
  (if (blank-line-p string)
      ""
    (when (char= (elt string 0) #\")
      (multiple-value-bind (string2 to)
          (read-from-string string)
        (declare (ignorable string2))
        ;;; read-from-string nur zum Ermitteln der Grenzen 
        (if (whitespace-char-p (char string (1- to)))
            (values (read-from-string
                     (format nil "~S" (subseq string 1 (- to 2))))
                    (1- to))
          (values (read-from-string
                   (format nil "~S" (subseq string 1 (1- to))))
                  to))))))

        
(defun get-nil-for (active-profile)
  (if (eq (profile-case active-profile) :upcase)
      nil
    'racer-user::|nil|))
                 
;;;
;;;
;;; 

(defun read-from-string-preserving-case (string &rest args)
  (when string
    (typecase string
      (string 
       (let ((old-case 
              (readtable-case *readtable*)))
         (unwind-protect
             (progn 
               (setf (readtable-case *readtable*) 
                     :preserve)
               (apply #'read-from-string string args))
           (setf (readtable-case *readtable*) old-case))))
      (otherwise
       string))))


(defun my-read-from-string (string)

  (let ((*readtable* *lracer-readtable*))

    (labels ((read-literal (&key start end)
           
               (when (and start (not (zerop start)))
                 (error "!"))
             
               (let ((start (or start 0)))

                 (cond ((char= #\" (elt string 0))

                        (error "!") 
                        )
                       
                       (t

                        (multiple-value-bind (res to)

                            (handler-case   
                                (progn
                                  (multiple-value-bind (res to)
                                      (read-from-string-preserving-case 
                                       string
                                       nil nil 
                                       :start (or start 0) :end end)

                                    (if (and (symbolp res)
                                             (active-profile)
                                             (profile-alisp-racer (active-profile))
                                             (or (keywordp res)
                                                 (and (is-racer-command-p res)
                                                      (not (is-racer-mixed-case-command-p res)))))
                                      
                                        (progn 
                                          (read-from-string-preserving-case 
                                           (string-upcase string)
                                           nil nil 
                                           :start (or start 0) :end end))

                                      (progn 
                                        (values res to)))))

                              (error (error)
                                     
                                ;; Package not exists? (http: ...) 
                                   
                                (let* ((ostring (subseq string
                                                        (or start 0)
                                                        end))

                                       (open-bar (position #\| ostring))
                                       (close-bar (position #\| ostring :from-end t))
                                          
                                       (wss (remove nil 
                                                    (list (position-if #'whitespace-char-p ostring)
                                                          (position #\) ostring))))

                                       (ws (if (cdr wss)
                                               (apply #'min wss)
                                             (first wss)))

                                       (ostring (subseq ostring 0 ws))
                                          
                                       (bar-symbol-p (and open-bar close-bar
                                                          (if ws
                                                              (< open-bar ws close-bar)
                                                            t)))
                                       (ostring2
                                        (if bar-symbol-p 
                                            (double-backslashes ostring)
                                          (concatenate 'string
                                                       "|"
                                                       (double-backslashes ostring)
                                                       "|"))))
                                  (when *debug-reader*
                                    (format t "Info:  ~S ~S~%" bar-symbol-p ostring2))

                                  (multiple-value-bind (res to)
                                      (read-from-string-preserving-case ostring2)

                                    (values res 
                                            (- to 
                                               (- (length ostring2)
                                                  (length ostring))))))))
                     
                          (when *debug-reader* (format t "Read literal: Ret: ~S ~S ~S~%" res to string))
                     
                          (setf string (subseq string to))
                        
                          res)))))
                          
             (do-it ()
             
               (when *debug-reader* (princ "String: ") (write string) (terpri))
               (when *debug-reader* (princ "Package: ") (write *package*) (terpri))

               (multiple-value-bind (substring to)
                   (extract-string string)

                 (cond (substring 
                        (setf string (subseq string to))
                        substring)

                       (t
                      
                        (let* ((ws (or (position-if #'whitespace-char-p string) -1))

                               (quote (or (position #\' string) -1))
                               (backquote (or (position #\` string) -1))
                               (comma (or (position #\, string) -1))
                               (comma-at (or (position #\@ string) -1))

                               (colon (or (position #\: string) -1))
                    
                               (open (or (position #\( string) -1))
                               (close (or (position #\) string) -1))
                               (open-bar (or (position #\| string) -1))
                               (close-bar (or (and (not (= open-bar -1))
                                                   (position #\| string :start (1+ open-bar)))
                                              -1))

                               #|
                             (hashquote (or (position #\# string)
                                            -1))
                             (exclamation (or (position #\! string)
                                              -1))
                             |# 
                             
                               (min 
                                (apply #'min 
                                       (or 
                                        (remove -1 (list quote backquote comma comma-at
                                                         ;;hashquote
                                                         ws colon open close open-bar close-bar))
                                        '(-1)))))
               
                          (when *debug-reader* 
                            (format t "STRING ~S  QUOTE ~A  BACKQUOTE ~A  COMMA ~A  COMMA-AT ~A  WS ~A  COLON ~A  OPEN ~A  CLOSE ~A  BAR ~A  -> MIN ~A~%" 
                                    string quote backquote comma comma-at ws colon open close (list open-bar close-bar)  min))

               
                          (if (or (= min -1) (= min colon))
                              (progn 

                                (multiple-value-bind (res to)
                                    (read-literal)
                                
                                  (values res to)))

                            (cond #+:ignore
                                  ((and (= min hashquote)
                                        (= (1+ hashquote) exclamation))

                                   (let ((min 
                                          (cond ((= close -1) 
                                                 (cond ((= ws -1) nil)
                                                       (t ws)))
                                                ((= ws -1)
                                                 close)
                                                (t (min close ws)))))
                                   
                                     (if min
                                         (values (intern (subseq string hashquote min))
                                                 min)
                                       (values (intern string)))))

                                  ;;;
                                  ;;;
                                  ;;;

                                  ((= min ws)
                                   (let ((pos (position-if-not #'whitespace-char-p string)))
                                     (cond ((and pos 
                                                 (< ws pos))
                                            (setf string (subseq string pos))
                                            (do-it))
                                           (t
                                            (read-literal :start pos :end ws)))))

                                  ;;;
                                  ;;;
                                  ;;; 

                                  ((= min quote)
                                   (setf string (subseq string 1))
                                   (multiple-value-bind (res to)
                                       (do-it)
                                     (values `(:quote
                                               ,res)
                                             to)))
                                
                                  ((= min backquote)
                                   (setf string (subseq string 1))
                                   (multiple-value-bind (res to)
                                       (do-it)
                                     (values `(:backquote
                                               ,res)
                                             to)))
                                
                                  ;;;
                                  ;;;
                                  ;;;
                                
                                  ((= min comma)

                                   (if (not (and comma-at 
                                                 (= comma-at (1+ comma))))
                                       (progn 
                                         (setf string (subseq string 1))
                                         (multiple-value-bind (res to)
                                             (do-it)
                                           (values `(:bq-comma
                                                     ,res)
                                                   to)))
                                     (progn 
                                       (setf string (subseq string 2))
                                       (multiple-value-bind (res to)
                                           (do-it)
                                         (values `(:bq-comma-atsign
                                                   ,res)
                                                 to)))))
                                
                                  ;;;
                                  ;;;
                                  ;;;

                                  ((= min open)
                                   (let ((res nil))
                          
                                     (setf string (subseq string 1))
                        
                                     (loop
                                      ;; (princ "loop ")
                                      ;; (write string)  (terpri)

                                      (multiple-value-bind (a terminate ignore)
                                          (do-it)
                                        (unless ignore
                                          (push a res))
                                        (when terminate 
                                          (return))))

                                     (nreverse res)))
                             
                                  ((= min close)
                                   (cond ((zerop close)
                                          (setf string (subseq string 1))
                                          (values 
                                           nil
                                           t t))
                                         (t
                                          (read-literal :end close))))

                                  ((= min open-bar)
                                   (unless close-bar
                                     (report-error (format nil "Missing closing bar in ~S" string)))
                        
                                   (prog1
                                       (intern (subseq string (1+ open-bar) (- close-bar open-bar))) 
                                     (setf string (subseq string (1+ close-bar)))))))))))))
      
      (if (stringp string)
          (maptree (do-it)
                   #'(lambda (x) 
                       (if (and (symbolp x)
                                (string-equal (symbol-name x) "nil"))
                           (get-nil-for (active-profile))
                         x )))
        string))))


(defun maptree (tree fn)
  (when tree

    (if (consp tree)        
    
        (mapcar #'(lambda (x) 
                    (if (consp x)
                        (maptree x fn)
                      (funcall fn x)))
                tree)

      (funcall fn tree))))


(defun remove-inv (roles)
  (if (consp roles) 
      (remove-if #'consp roles)
    nil))


(defun remove-quotes (string)
  (let ((pos (position #\" string)))
    (if pos
        (subseq string 0 pos)
      string)))
           
;;;
;;;
;;;

(defmacro with-null-output (&rest body)
  `(let ((*error-output* system::*null-stream*)
         (*terminal-io*  system::*null-stream*)
         (*debug-io*  system::*null-stream*)
         (*standard-output* system::*null-stream*)
         (*trace-output* system::*null-stream*))
     ,@body))


(defmacro with-xor ((port) &body body)
  `(gp:with-graphics-state (,port
                            :foreground (gp:compute-xor-pixel ,port)
			    :operation boole-xor)
     ,@body))

(defun x-y-width-and-height (x1 y1 x2 y2)
  (values (min x1 x2)
          (min y1 y2)
          (abs (- x1 x2))
          (abs (- y1 y2))))

;;;
;;;
;;;

(define-condition continue-from-timeout () ())

(define-condition abort-execution () ())


#+:Lispworks
(defun with-sirius-timeout-1 (timeout timeout-function body-fn)
  (declare (dynamic-extent body-fn))
  (let ((tag 'tag-1)
        (process mp:*current-process*))
    
    (labels ((timeout-throw ()
               (invoke-restart 'abort-execution))

             (timeout-action ()
               (mp:process-interrupt process #'timeout-throw)))
      
      (declare (dynamic-extent #'timeout-throw #'timeout-action))

      (let ((timer (mp:make-named-timer 'timer #'timeout-action)))

        (catch tag
          (unwind-protect
              (restart-case 
                  (progn
                    (mp:schedule-timer-relative timer timeout)
                    (return-from with-sirius-timeout-1
                      (funcall body-fn)))

                (continue-from-timeout () 
                  (throw tag 
                         (when timeout-function
                           (funcall timeout-function))))

                (abort-execution () 
                  (throw tag
                         (when timeout-function
                           (funcall timeout-function)))))
            
            (mp:unschedule-timer timer)))))))

(defmacro with-sirius-timeout ((seconds . timeout-forms) &body body)
  `(with-sirius-timeout-1 ,seconds #'(lambda () ,@timeout-forms) #'(lambda () ,@body)))



(defun yes-or-no (x)
  (cond ((eq x :error)
         'error)
        (x 'yes)
        (t 'no)))

(defun yes-or-no-or-unknown (x)
  (cond ((eq x :dont-know)
         'unknown)
        ((eq x :error)
         'error)
        (x 'yes)
        (t 'no)))


;;;
;;;
;;;


(defun symbol-name-1 (x)
  ;;; evtl. ist ein Printer Error als String in 
  ;;; einer von service-request enthaltenen Ergebnisliste
  ;;; (Problem im racer-server-1-9-18)
  (if (stringp x)
      x
    (if (consp x)
        (format nil "~A" x)
      (symbol-name x))))

;;;
;;;
;;;

(defun compare-fn-concepts (x y)

  (labels ((comp ()
             (string< (symbol-name-1 x)
                      (symbol-name-1 y))))

    (with-profile-access 
      (cond (selected-first

             (let ((sel-x (selected-concept? x))
                   (sel-y (selected-concept? y))
                   (cur-x (equal x current-concept))
                   (cur-y (equal y current-concept)))

               (if cur-x 
                   (if cur-y
                       nil 
                     t)
                 (if cur-y
                     nil
                   (if sel-x
                       (if sel-y
                           (comp)
                         t)
                     (if sel-y
                         nil
                       (comp)))))))

            (t (comp))))))

(defun compare-fn-individuals (x y)

  (labels ((comp ()
             (string< (symbol-name-1 x)
                      (symbol-name-1 y))))

    (with-profile-access 
      (cond (selected-first

             (let ((sel-x (selected-individual? x))
                   (sel-y (selected-individual? y))
                   (cur-x (equal x current-individual))
                   (cur-y (equal y current-individual)))

               (if cur-x 
                   (if cur-y
                       nil 
                     t)
                 (if cur-y
                     nil
                   (if sel-x
                       (if sel-y
                           (comp)
                         t)
                     (if sel-y
                         nil
                       (comp)))))))

            (t (comp))))))

(defun compare-fn-roles (x y)

  (labels ((comp ()
             (string< (symbol-name-1 x)
                      (symbol-name-1 y))))

    (with-profile-access 
      (cond (selected-first

             (let ((sel-x (selected-role? x))
                   (sel-y (selected-role? y))
                   (cur-x (equal x current-role))
                   (cur-y (equal y current-role)))

               (if cur-x 
                   (if cur-y
                       nil 
                     t)
                 (if cur-y
                     nil
                   (if sel-x
                       (if sel-y
                           (comp)
                         t)
                     (if sel-y
                         nil
                       (comp)))))))

            (t (comp))))))
       

;;;
;;;
;;;

(defun item-equal (x y)
  (and (eq (type-of x) (type-of y))
       (typecase x 
         (symbol 
          (eq x y))
         (string 
          (string= x y))
         (list
          (equalp x y))
         (otherwise 
          (equalp x y)))))
