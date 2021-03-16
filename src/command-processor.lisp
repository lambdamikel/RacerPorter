;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defun input-command (input &key
                            describe-p 
                            query-p
                            (message :okay) 
                            original-input

                            is-shell-command-p 
                            dont-remove-trailing-newline-p 
                            (show-in-log-pane-p t)
                            (send-to-racer-p t)
                            (update-command-history-p send-to-racer-p)

                            (use-prompt-p send-to-racer-p)
                            always-increment-prompt-p 
                            button-command-p
                            
                            (keep-prompt-p nil)
                            
                            (show-in-shell-pane-p t)
                            
                            &allow-other-keys)

  ;;; input muss string sein!!!
  ;;; schickt string zu Racer 

  (with-sirius-app (app)
    (with-update-display-status
      (with-slots (info-pane) app
        (with-profile-access
          (let ((profile (active-profile)))

            (let* ((original-input (or original-input input))
                   (original-input 
                    (if (and (stringp original-input)
                             (not dont-remove-trailing-newline-p))
                        #| (subseq original-input 
                                   (1- (length original-input))) |# 
                      original-input
                      original-input))
                   (button-pretty-input nil))

              ;;;
              ;;;
              ;;; 

              (when (or show-in-log-pane-p
                        show-in-shell-pane-p)

                (let* ((prefix "")
                       (printed-input

                        (if (not send-to-racer-p) ;;; Dummy Command? 

                            (format nil "[*] ? ~A" 
                                    original-input)
                       
                          (let ((*package* (find-package :racer-user))
                                (s-expr 
                                 (if (or (profile-pretty-print-input profile)
                                         button-command-p)
                                     (read-from-string-preserving-case 
                                      (double-backslashes-if-needed input))
                                   original-input)))

                            ;;(display-message "~S" s-expr)

                            (prog1 

                                (cond (use-prompt-p

                                       (setf prefix (format nil "[~D] ? " (1+ counter)))

                                       (if (or button-command-p
                                               (profile-pretty-print-input profile))
                                           (with-pretty-printing ()
                                                                 (format nil "[~D] ? ~S" 
                                                                         (1+ counter)
                                                                         s-expr))
                                  
                                         (format nil "[~D] ? ~A" 
                                                 (1+ counter) s-expr)))

                                      (t

                                       (setf prefix (format nil "[*] ? "))
                                 
                                       (if (or (profile-pretty-print-input profile)
                                               button-command-p)

                                           (with-pretty-printing ()
                                                                 (format nil "[*] ? ~S" 
                                                                         s-expr))

                                         (format nil "[*] ? ~A" 
                                                 s-expr))))

                              (when button-command-p 
                                (setf button-pretty-input
                                      (with-pretty-printing
                                       ()
                                       (let ((string 
                                              (concatenate
                                               'string 
                                               (make-string (length prefix) :initial-element #\space)
                                               "~S")))
                                             
                                         (format nil string
                                                 s-expr))))

                                (setf button-pretty-input
                                      (subseq button-pretty-input (length prefix)))))))))
                
                  (when show-in-shell-pane-p
                    (clear-current-prompt-and-input)
                    (append-to-command-buffer printed-input))

                  (when show-in-log-pane-p
                    (clear-info)
                    (append-to-info-buffer printed-input))))

              ;;;
              ;;;
              ;;;
            
              (multiple-value-bind (res stdout error-p)
                  (if send-to-racer-p
                      (internal-process-racer-expr 
                       (if (profile-time-all-operations *active-profile*)
                           (concatenate 'string "(time " input ")")
                         input)
                       describe-p)
                    message)

                (cond (error-p
                       ;;; Racer error? 
                       (beep-pane)

                       (when update-command-history-p
                         (push (or button-pretty-input
                                   original-input)
                               commands)
                         (setf n (length commands))))

                      (t
                     
                       (when query-p 
                         (when (and (listp res)
                                    (every #'listp res))
                           (setf current-query-result res)
                           (when (eq active-tab :query-input)
                             (schedule-update))))

                       (when update-command-history-p 

                         (setf sirius-star-star-star
                               sirius-star-star
                         
                               sirius-star-star
                               sirius-star
                
                               sirius-star
                               res)

                         (setf sirius-plus-plus-plus
                               sirius-plus-plus
                
                               sirius-plus-plus 
                               sirius-plus
                
                               sirius-plus 
                               (or button-pretty-input original-input))

                         (push (or button-pretty-input original-input)
                               commands)
                       
                         (setf n (length commands)))))

                (when (or show-in-log-pane-p
                          show-in-shell-pane-p)
          
                  (let ((output 
                         (let ((*package* (find-package :racer-user)))
                         
                           (handler-case 
                               (concatenate 'string 

                                            (when (and stdout 
                                                       (not (blank-line-p stdout)))
                                              (format nil "~%~A" stdout))
                                  
                                            (with-pretty-printing (:print-doublequotes-p t)
                                                                  (format nil "~%[~D] > ~S~%~%" 
                                                                          (if use-prompt-p
                                                                              (1+ counter)
                                                                            '*)
                                                                          res)))
                          
                             (error (error) 
                               ;; string too big? -> workaround (ugly) 
                               ;; (princ "workaround enabled")
                               ;; (display-message (format nil "~A" error))

                               (if (confirm-yes-or-no
                                    (format nil "Warning! The result is very big (~A items)!~%Put only a subset into the Shell?" 
                                            (length res)))

                                   (let ((n 10000))
                                     (loop 

                                      (handler-case 
                                          (return 
                                           (concatenate 'string 
                                     
                                                        (when (and stdout 
                                                                   (not (blank-line-p stdout)))
                                                          (format nil "~%~A" stdout))
                                  
                                                        (with-pretty-printing ()
                                                                              (format nil "~%[~D] > ~S~%~%" 
                                                                                      (if use-prompt-p
                                                                                          (1+ counter)
                                                                                        '*)
                                                                                      (nconc (subseq res 0 n)
                                                                                             '(|...|))))))
                                        (error (n) 
                                          (setf n (- n 5000))
                                          (when (zerop n) 

                                            (format nil "~%[~D] > ~S~%~%" 
                                                    (if use-prompt-p
                                                        (1+ counter)
                                                      '*)
                                                    :aborted-result-too-big))))))

                                 nil))))))

                    (when show-in-shell-pane-p

                      (cond (output
                             (append-to-command-buffer output)

                             (when (and (profile-popup-message-box profile)
                                        (not is-shell-command-p)
                                        (> (length output) 100))
                               (let ((pos (+ 2 (position #\> output))))
                                 (sirius-message 
                                  (concatenate 
                                   'string
                                   (make-string (1- pos) :initial-element #\space)
                                   (subseq output pos))))))

                            (t
                             (let ((*package* (find-package :racer-user)))
                               (append-to-command-buffer  
                                (concatenate 'string
                                             (when (and stdout 
                                                        (not (blank-line-p stdout)))
                                               (format nil  "~%~A" stdout))
                                       
                                             (format nil "~%[~D] > "
                                                     (if use-prompt-p
                                                         (1+ counter)
                                                       '*))))
                               (append-to-command-buffer res)
                               (append-to-command-buffer 
                                (format nil "~%~%")))))
                
                      (unless (and *busy-marker* (eq res *busy-marker*))

                        (when *check-only-cache* 
                          (sleep +wait-before-replace-current-shell-input-sleep-time2+))

                        (if always-increment-prompt-p
                            (input-prompt)
                          (input-prompt 
                           ;;; nicht hochzaehlen, wenn: 
                           (or ;error-p
                            (not use-prompt-p) 
                            keep-prompt-p
                            (and *busy-marker* ; nicht fuer NIL!
                                 (eq res *busy-marker*)))))))

                    (when show-in-log-pane-p
                      (cond (output 
                             (append-to-info-buffer output))
                          
                            (t  (let ((*package* (find-package :racer-user)))
                                  (append-to-info-buffer  
                                   (concatenate 'string
                                                (when (and stdout 
                                                           (not (blank-line-p stdout)))
                                                  (format nil  "~%~A" stdout))
                                       
                                                (format nil "~%[~D] > "
                                                        (if use-prompt-p
                                                            (1+ counter)
                                                          '*))))
                                  (append-to-info-buffer res)
                                  (append-to-info-buffer 
                                   (format nil "~%~%")))))
                           
                      (when show-info-pane
                        (beginning-of-buffer info-pane))))

                  ;; (setf prompt-ready-p t)
                  )

                (force-push-buttons-update)

                ;; (setf prompt-ready-p t)

                res))))))))


(defun input-extended-command (text-input &rest args &key state-changing-command-p original-input &allow-other-keys)
  
  (handler-case
      (multiple-value-bind (text-input2)
          (replace-stars text-input)

        (when state-changing-command-p
          (reset-active-profile
           :before-state-changing-shell-command-p t))
        
        (values 
         (apply #'input-command text-input2 
                :original-input (or original-input text-input)
                args)
         text-input2))

    (error (error)

      ;;; notwendig, denn replace-stars bzw. read-from-string-preserving-case 
      ;;; produziert bei xxx:yyy-Symbolen
      ;;; einen Error
      (report-error
       (format nil "~A is not a valid Racer command, transmission denied.
Error was: ~A" 
               text-input
               error)))))

;;;
;;;
;;;

(defun transform-args (args)
  (let ((args
         (cond ((keywordp args)
                (intern 
                 (if (profile-alisp-racer (active-profile))
                     (symbol-name args)
                   (string-downcase (symbol-name args)))
                 :keyword))
               ((null args) nil)
               ((eq args 't) 'racer-user::|t|)
               ((consp args)
                (cons (transform-args (first args))
                      (transform-args (rest args))))
               (t args))))

    (if (consp args)
        (if (profile-alisp-racer (active-profile))
            args
          (mapcar #'(lambda (x) 
                      (if (not x) ; NIL? 
                          'racer-user::|nil|
                        x))
                  args))
      args)))

(defun create-racer-call-string (input) 
  (let ((input (ensure-list input)))
    (concatenate 'string 
                 (format nil "(~A"
                         (if (is-racer-mixed-case-command-p (first input))
                             (symbol-name (first input))
                           (string-downcase
                            (symbol-name (first input)))))

                 (with-io-printing ()
                                   (format nil "~{ ~S~})"
                                           (transform-args (rest input)))))))

;;;
;;; Programmatische Eingabe
;;;

(defun enter-result-command (command &key state-changing-command-p mouse-select-command-p)
  (let ((verbose
         (=> mouse-select-command-p
             (profile-show-mouse-select-commands (active-profile))))
        (*display-error* t))
    (enter-command :input command
                   :state-changing-command-p state-changing-command-p 
                   :button-command-p t
                   :update-command-history-p 
                   (and (profile-put-porter-commands-in-history (active-profile))
                        (not mouse-select-command-p))
                   :use-prompt-p 
                   (profile-put-porter-commands-in-history (active-profile))
                   :send-to-racer-p t
                   :show-in-log-pane-p verbose
                   :show-in-shell-pane-p verbose)))

(defun enter-state-changing-result-command (command)
  (enter-result-command command :state-changing-command-p t))

(defun enter-mouse-select-command (command)
  (without-caching
    (enter-result-command command :mouse-select-command-p t)))

(defun enter-dummy-command (text-input message)
  (input-command text-input 
                 :dont-remove-trailing-newline-p t
                 :send-to-racer-p nil
                 :message message))

;;;
;;;
;;;

(defun function-name (name)
  (if (gethash (symbol-name name)
               +mixed-case-commands-hash+)
      (symbol-name name)
    (if (profile-alisp-racer (active-profile))
        (string-upcase (symbol-name name))
      (string-downcase (symbol-name name)))))

(defun replace-stars (input)  
  (with-profile-access
    (let ((toplevel t)
          (lambda-found nil))

      (labels ((fname (expr)
                 (if (consp expr)
                     (cons
                      (read-from-string-preserving-case
                       (function-name (first expr)))
                      (rest expr))
                   expr))

               (do-it (expr &optional (replace-p t))
                 (cond ((and (symbolp expr)
                             (not lambda-found))

                        (case expr 
                          
                          ((lambda |lambda|
                             racer-user::lambda racer-user::|lambda|
                             :lambda :|lambda|
                             
                             evaluate |evaluate|
                             racer-user::evaluate racer-user::|evaluate|
                             :evaluate :|evaluate|
                             
                             define |define|
                             racer-user::define racer-user::|define|
                             :define :|define|)
                           
                           (setf lambda-found t)
                           expr)

                          (otherwise

                           (cond (replace-p
 
                                  (case expr 
                                    (racer-user::|*| sirius-star)
                                    (racer-user::|**| sirius-star-star)
                                    (racer-user::|***| sirius-star-star-star)
                                    (racer-user::|+| 
                                     (if toplevel
                                         (fname (my-read-from-string sirius-plus))
                                       expr))
                                    (racer-user::|++| 
                                     (if toplevel
                                         (fname (my-read-from-string sirius-plus-plus))
                                       expr))
                                    (racer-user::|+++| 
                                     (if toplevel 
                                         (fname (my-read-from-string sirius-plus-plus-plus))
                                       expr))
                                    (racer-user::|*t*| sirius-current-tbox)
                                    (racer-user::|*a*| sirius-current-abox)
                                    (racer-user::|*n*| sirius-current-namespace)
                                    (racer-user::|*i*| current-individual)
                                    (racer-user::|*c*| (first (ensure-list current-concept)))
                                    (racer-user::|*r*| current-role)
                                    (racer-user::|*qor*| current-query-or-rule)
                                    (racer-user::|*def*| (first current-definition))
                                    (racer-user::|*oo*| current-ontology)
                                    (racer-user::|*or*| current-reasoner)
                                    (racer-user::|*ax*| (first current-axiom))
                                    (otherwise expr)))

                                 (t expr)))))

                       ((stringp expr)
                        (escape-newlines-etc expr))

                       ((consp expr)
                        (mapcar #'do-it expr))

                       (t expr))))

        (let* ((*package* (find-package :racer-user))
               (expr (my-read-from-string input)))

          (when (consp expr)
            (setf toplevel nil))

          (let* ((expr 
                  (do-it expr))
                 (expr (fname expr))

                 (oexpr 
                  (do-it expr nil))
                 (oexpr (fname oexpr)))

            (with-io-printing ()
                              (values
                               (format nil "~S" expr)
                               (format nil "~S" oexpr)))))))))

;;;
;;;
;;; 

(defun is-prompt-p (string)
  (let ((n (length string)))
    (and (>= n 6)
         (char= (elt string 0) #\[)
         (char= (elt string (1- n)) #\space)
         (char= (elt string (- n 2)) #\?)
         (char= (elt string (- n 3)) #\space)
         (char= (elt string (- n 4)) #\]))))
         

(defun get-current-prompt ()
  (with-profile-access
    (or current-prompt
        (setf current-prompt 
              (format nil "[~D] ? " (1+ counter))))))

