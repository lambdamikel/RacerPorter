;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defun setup-shell-pane (&optional clear-p)

  (when clear-p 
    (clear-info)
    (clear-command))
  
  (with-profile-access
    
    ;;;
    ;;;
    ;;; 
    
    (with-command-buffer-operation
      (setf (editor-pane-text command-pane)
            (concatenate 'string (editor-pane-text command-pane)
                         shell-content))

      (move-cursor-to-end-of-expression)
      (input-prompt t))))

(defun abort-command (&rest args)
  (declare (ignorable args))
  (clear-current-prompt-and-input)
  (input-prompt t))

(defun previous-command (&rest args)
  (declare (ignorable args))
  (with-profile-access
    (if (> n 0)
        (progn 
          (decf n)
          (replace-current-input
           (nth n (reverse commands))))
      (beep-pane))))

(defun next-command (&rest args)
  (declare (ignorable args))
  (with-profile-access
    (if (< n (1- (length commands)))
        (progn 
          (incf n)
          (replace-current-input 
           (nth n (reverse commands))))
      (beep-pane))))

(defun tilde-command (&rest args)
  (declare (ignorable args)) 
  (with-command-buffer-operation
    (let ((point (editor:buffer-point (get-command-buffer))))
      (editor:insert-string point "~"))))

;;;
;;;
;;;

(defun save-shell-log ()
  (with-sirius-app (app)
    (with-slots (command-pane) app
      (let ((file (save-file-prompter "Specifiy a File for the Shell Log" 
                                      "*.log"
                                      nil
                                      (user-directory)
                                      
                                      )))
        (when file 
          (with-open-file (stream file :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (write-string (editor-pane-text command-pane) stream)))))))

(defun edit-shell-log ()
  (sirius-editor nil t))

;;;
;;;
;;;

(defun input-prompt (&optional current-p)
  (with-profile-access
    (with-command-buffer-operation
      (unless current-p
        (setf current-prompt nil)
        (incf counter))

      (let ((point (editor::copy-point (editor::buffer-point buffer) :before)))
        (setf current-prompt-point point)
          (editor:insert-string point (get-current-prompt))
          (move-cursor-to-end-of-expression)))))

(defun clear-current-prompt-and-input ()
  (with-profile-access
    (with-command-buffer-operation
      (when current-prompt-point
        (editor:move-point (editor:buffer-point buffer) current-prompt-point)
        (editor:set-current-mark (editor:buffer-point buffer))
        (editor:move-point (editor:buffer-point buffer)
                           (editor:buffers-end buffer))
        (editor:delete-region-command nil)))))

#+:mac
(defun move-cursor-to-end-of-expression ()
  ;;; nur das funktioniert auf dem Mac! 
  (with-sirius-slots (command-pane)
                     (call-editor command-pane "End Of Buffer")))

#-:mac
(defun move-cursor-to-end-of-expression ()
  (with-command-buffer-operation
    (editor:end-of-buffer-command buffer)))

(defun backward-character ()
  (with-command-buffer-operation
    (editor:delete-previous-character-command nil)))

;;;
;;;
;;;

(defmacro with-command-pane-or-editor-operation (editor-p &body body)
  `(let ((buffer 
          (if ,editor-p 
              (capi:editor-pane-buffer 
               (slot-value  *sirius-editor-app* 'sirius::editor-pane))
            (get-command-buffer))))
     (editor:use-buffer buffer
       ,@body)))

(defun goto-beginning-of-command (&optional (mode :current-form) editor-p)
  (with-profile-access
    (with-command-pane-or-editor-operation 
        editor-p
      (let* ((read-prompt nil)
             (opoint nil)
             (after-prompt-point nil)
             (missing-open-bracket-p nil))
          
        (setf opoint (editor::copy-point (editor::buffer-point buffer) :temporary))

        (cond (editor-p 

               (ecase mode
                 (:toplevel-form 
                  
                  (loop
                   (handler-case
                       (editor::backward-up-list-command 1)
                     (error ()
                       (return)))))

                 (:current-form
                  
                  (editor::move-point (editor:buffer-point buffer) opoint)
                  
                  (handler-case 
                      (editor::backward-up-list-command 1)
                    (error ()
                      (handler-case 
                          (progn 
                            (editor::backward-form-command 1)
                            (setf missing-open-bracket-p t))
                        (error ())))))

                 (:current-form-dont-move
                  
                  (handler-case 
                      (editor::backward-up-list-command 1)
                    (error ()
                      (progn 
                        (editor::backward-form-command 1)
                        (setf missing-open-bracket-p t))
                      (error ())))))
              
               (values (get-current-prompt)
                       missing-open-bracket-p))

              (t
              
               (handler-case 
         
                   (let ((point nil)
                         (count 0))

                     (editor:line-start (editor:buffer-point buffer))
                     (setf point (editor::copy-point (editor::buffer-point buffer) :temporary))
                     (editor:character-offset (editor::buffer-point buffer) 1)
                         
                     (loop while (not (string= "[" (editor:points-to-string                            
                                                    point
                                                    (editor:buffer-point buffer)
                                                    )))
                           do 
                           (incf count)

                           (when (> count 1000)
                             ;;; hack: nach 1000 zeilen noch nix gefunden? lass es sein! 
                             ;;; (wahrscheinlich editor bug...)
                             (editor::move-point (editor:buffer-point buffer) opoint)           
                             (return-from goto-beginning-of-command nil))
                          
                           (editor:line-offset (editor:buffer-point buffer) -1)
                           (editor:line-start (editor:buffer-point buffer))
                           (setf point (editor::copy-point (editor::buffer-point buffer) :temporary))
                           (editor:character-offset (editor::buffer-point buffer) 1))
                    
                     (editor:form-offset (editor::buffer-point buffer) 2)
                     (editor:character-offset (editor::buffer-point buffer) 1)
                    
                     (setf read-prompt
                           (editor:points-to-string                            
                            point
                            (editor:buffer-point buffer)))

                     (unless (is-prompt-p read-prompt)
                       (editor::move-point (editor:buffer-point buffer) opoint)
                       (return-from goto-beginning-of-command nil))

                     (setf after-prompt-point
                           (editor::copy-point (editor::buffer-point buffer) :temporary))

                     (ecase mode
                       (:toplevel-form )

                       (:current-form

                        (editor::move-point (editor:buffer-point buffer) opoint)

                        (handler-case 
                            (editor::backward-up-list-command 1)
                          (error () 
                            (handler-case 
                                (progn 
                                  (editor::backward-form-command 1)
                                  (if (editor:point< (editor:buffer-point buffer) after-prompt-point)
                                      (editor::forward-form-command 1))
                                  (setf missing-open-bracket-p t))
                              (error ())))))
                       
                       (:current-form-dont-move

                        (handler-case 
                            (editor::backward-up-list-command 1)
                          (error () 
                            (handler-case 
                                (progn 
                                  (editor::backward-form-command 1)
                                  (if (editor:point< (editor:buffer-point buffer) after-prompt-point)
                                      (editor::forward-form-command 1))
                                  (setf missing-open-bracket-p t))
                              (error ()))))))
              
                     (values read-prompt missing-open-bracket-p)))))))))


(defun in-evaluate-context-p (&optional editor-p)
  (with-profile-access
    (with-command-pane-or-editor-operation 
        editor-p
      
      (let* ((point nil)
             (opoint nil)
             (context nil))

        (setf opoint (editor::copy-point (editor::buffer-point buffer) :temporary))
          
        (when (goto-beginning-of-command :current-form editor-p)

          (editor:end-of-buffer-command buffer)
          
          (handler-case
              (loop
               (editor::backward-up-list-command 1)
               (setf point (editor::copy-point (editor::buffer-point buffer) :temporary))
               (editor:forward-word-command 1)
               (let ((token
                      (coerce
                       (remove-if #'whitespace-char-p
                                  (coerce 
                                   (subseq
                                    (editor:points-to-string                            
                                     point
                                     (editor:buffer-point buffer))
                                    1)
                                   'list))
                       'string)))
                 (push token context))
               (editor:move-point (editor:buffer-point buffer)
                                  point))
               
            (editor:editor-error (condition))))

        (editor:move-point (editor:buffer-point buffer)
                           opoint)

        (find-if (lambda (x) (search "evaluate" x)) context)))))


(defun get-current-input (&optional editor-p)

  ;;; kann nicht in apply-in-pane-process ausgefuehrt werden, 
  ;;; sonst bekomme ich NIL als Rueckgabewert

  (with-profile-access
    (with-command-pane-or-editor-operation 
        editor-p

      (let* ((orig-copy (editor::copy-point (editor::buffer-point buffer) :temporary))
             (copy-p nil)
             (read-prompt nil)
             (point nil)
             (string nil))
        
        (setf read-prompt
              (goto-beginning-of-command :toplevel-form editor-p))

        (labels ((nuked () 
                   (clear-current-prompt-and-input)
                   (beep-pane)
                   (input-prompt t)
                   :nuked))

          (cond (read-prompt
                 
                 (setf copy-p 
                       (not 
                        (string-equal read-prompt
                                      (get-current-prompt))))
                   
                 (setf point (editor::copy-point (editor::buffer-point buffer) :temporary))
                 (editor:form-offset (editor::buffer-point buffer) 1)

                 (setf string
                       (editor:points-to-string                            
                        point
                        (editor:buffer-point buffer)))

                 (cond ((blank-line-p string)
                           
                        (editor:line-end (editor::buffer-point buffer))
                           
                        (setf string
                              (editor:points-to-string                            
                               point
                               (editor:buffer-point buffer)))

                        (editor::move-point (editor::buffer-point buffer) orig-copy)

                        (when copy-p 
                          (clear-current-prompt-and-input) 
                          (input-prompt t)
                          (append-to-command-buffer string))
                           
                        (if (blank-line-p string)
                            (nuked)
                          (values string copy-p)))

                       (t
                           
                        (editor::move-point (editor::buffer-point buffer) orig-copy)

                        (when copy-p 
                          (clear-current-prompt-and-input) 
                          (input-prompt t)
                          (append-to-command-buffer string))

                        (values string copy-p))))

                (t 
                 (nuked))))))))
                             

(defun replace-current-input (string) 
  (clear-current-prompt-and-input)
  (let ((printed-input
         (let ((*package* (find-package :racer-user)))
           (format nil "~A"
                   (concatenate 'string
                                (get-current-prompt) string)))))
      
    (append-to-command-buffer printed-input)))


(defun get-function-name (&optional editor-p)
  (with-profile-access
    (with-command-pane-or-editor-operation
        editor-p
      (let* ((form-start-point nil)
             (command-start-point nil)
             (command-end-point nil)
             (form-end-point nil)
             (opoint nil)
             (input nil)
             (complete-form-p nil))

        (setf opoint (editor::copy-point (editor::buffer-point buffer) :temporary))

        (multiple-value-bind (prompt missing-open-bracket-p)
            (goto-beginning-of-command :current-form editor-p)

          ;; (editor::insert-character (editor::buffer-point buffer) #\*)

          (cond (prompt

                 (unless (is-prompt-p prompt)
                   (return-from get-function-name nil))

                 ;; (editor::insert-character (editor::buffer-point buffer) #\*)
                                                 
                 (setf form-start-point (editor::copy-point (editor::buffer-point buffer) :temporary))

                 ;; entweder ganze Form oder bis Buffer Ende lesen 

                 (handler-case
                     (progn 
                       (editor:forward-form-command 1)
                       (setf complete-form-p t))
                   (editor:editor-error (condition)
                     (handler-case
                         (editor:end-of-buffer-command buffer)
                       (editor:editor-error (condition)))))

                 (setf form-end-point (editor::copy-point (editor::buffer-point buffer) :temporary))

                 (setf input
                       (editor:points-to-string           
                        form-start-point
                        (editor:buffer-point buffer)))

                 ;; (display-message "~S" input)

                 (editor::move-point (editor::buffer-point buffer) opoint)
        
                 (let* ((command-start
                         (position-if-not #'(lambda (x) 
                                              (or (whitespace-char-p x)
                                                  (char= x #\()))
                                          input))
                        (command-end
                         (when command-start
                           (position-if #'whitespace-char-p input :start command-start))))

                   (when command-start                   
                     (setf command-start-point (editor:copy-point form-start-point :temporary))
                     (editor::move-point-to-offset command-start-point
                                                   (+ (editor:point-position form-start-point)
                                                      command-start))

                     (setf command-end-point (editor:copy-point form-start-point :temporary))
                     (editor::move-point-to-offset command-end-point 
                                                   (+ (editor:point-position form-start-point) command-start
                                                      (length (subseq input command-start command-end))))

                     (values (if (and complete-form-p)
                                 (subseq input command-start (if command-end
                                                                 command-end
                                                               (1- (length input))))
                               (subseq input command-start command-end) )
                             form-start-point form-end-point 
                             command-start-point command-end-point 
                             (and complete-form-p (not missing-open-bracket-p))
                             missing-open-bracket-p))))

                (t (editor::move-point (editor::buffer-point buffer) opoint)

                   nil)))))))

;;;
;;; 
;;;

(defvar *incremental-search-active-p* nil)

(defun sirius-enter-space-from-shell (&rest args)
  (cond ((or *incremental-search-active-p*)
         (apply #'sirius-gesture-spec-input args))
        (t
         (sirius-enter-space)
         (sirius-update-argument-display))))

(defun sirius-enter-backspace-from-shell (&rest args)
  (cond ((or *incremental-search-active-p*)
         (apply #'sirius-gesture-spec-input args))
        (t 
         (sirius-enter-backspace)
         (sirius-update-argument-display))))

(defun sirius-enter-closing-par-from-shell (&rest args)
  (cond ((or *incremental-search-active-p*)
         (apply #'sirius-gesture-spec-input args))
        (t 
         (sirius-enter-closing-par)
         (sirius-update-argument-display))))

(defun sirius-enter-opening-par-from-shell (&rest args)
  (cond ((or *incremental-search-active-p*)
         (apply #'sirius-gesture-spec-input args))
        (t 
         (sirius-enter-opening-par)
         (sirius-update-argument-display))))

(defun sirius-enter-newline-from-shell (&rest args)
  (cond ((or *incremental-search-active-p*)
         (apply #'sirius-gesture-spec-input args))
        (t 
         (sirius-enter-newline)
         (sirius-update-argument-display))))

;;;
;;;
;;;

(defun sirius-incr-search-forward (&rest args)
  (declare (ignorable args))
  ;;; reicht nicht f. Mac, s. editor-commands-***.lisp
  (if *incremental-search-active-p*
      (apply #'sirius-gesture-spec-input args)
    (let ((*incremental-search-active-p* t))
      (with-command-buffer-operation 
        (call-editor command-pane "Incremental Search")))))

(defun sirius-incr-search-backward (&rest args)
  (declare (ignorable args))
  ;;; reicht nicht f. Mac, s. editor-commands-***.lisp
  (if *incremental-search-active-p*
      (apply #'sirius-gesture-spec-input args)
    (let ((*incremental-search-active-p* t))
      (with-command-buffer-operation
        (call-editor command-pane "Reverse Incremental Search")))))

;;;
;;;
;;;

(defun expand-file-name-command (&rest args)
  (declare (ignorable args))
  (with-command-buffer-operation
    (call-editor command-pane "Expand File Name")))

;;;
;;;
;;;
                  

(defun get-lambda-for (&optional fn)
  (let ((name (or fn (get-function-name))))
    (multiple-value-bind (res foundp)
        (gethash name +lambda-hash+)
      (if foundp 
          (or res 'none)
        (multiple-value-bind (res foundp)
            (gethash (string-upcase name) +lambda-hash+)
          (if foundp 
              (or res 'none)
            nil))))))

#|
(defun show-lambda-doc (&rest args)
  (declare (ignorable args))
  (let ((message 
         (with-pretty-printing (:print-doublequotes-p nil)
                               (let ((fn (get-function-name))
                                     (lambda (get-lambda-for)))
                                 (if lambda
                                     (cond ((eq lambda 'none)
                                            (format nil "Function ~S has no arguments." fn))
                                           ((eq lambda 'unknown)
                                            (format nil "Arguments of ~S are unknown!" fn))
                                           (t
                                            (format nil  "~S"
                                                    (cons 
                                                     fn lambda))))
                                   (when fn
                                     (format nil "No information regarding ~A" fn)))))))
    (if message 
        (sirius-message message)
      (beep-pane))))

(defun sirius-update-argument-display ()
  (with-sirius-slots (lambda-list-pane) 
                     (setpc (title-pane-text lambda-list-pane)
                            (let* ((lambda (get-lambda-for))
                                   (fn (get-function-name)))
                              (if lambda 
                                  (format nil "Arguments of ~A: ~{ ~A~}" 
                                          (get-function-name)
                                          (ensure-list lambda))
                                (if fn 
                                    (format nil "Unknown function ~A" fn)
                                  ""))))))

(defun sirius-clear-argument-display ()
  (with-sirius-slots (lambda-list-pane) 
                     (setpc (title-pane-text lambda-list-pane) "")))
|# 

(defun sirius-show-modeline (string)
  (with-sirius-slots (command-pane)
     (editor::process-character `(editor::message "~A (Ctrl-g to remove this message)" ,string)
                                (capi::editor-window command-pane))))

(defun sirius-update-argument-display (&optional editor-p)
  (let* ((fn (get-function-name editor-p))
         (lambda (get-lambda-for fn))
         (string
          (if lambda 
              (format nil "Arguments of ~A (Ctrl-g to remove this message):~{ ~A~}" 
                      fn
                      (ensure-list lambda))
            (if fn 
                (format nil "No information regarding ~A (Ctrl-g to remove this message)" fn)
              ""))))
  (with-sirius-slots (command-pane)
     (editor::process-character `(editor::message "~A" ,string)
                                (if editor-p 
                                    (capi:editor-window (slot-value *sirius-editor-app* 'editor-pane))
                                  (capi::editor-window command-pane))))))


#|
(defun sirius-clear-argument-display ()
  (with-sirius-slots (command-pane)
      (editor::process-character "" (capi::editor-window command-pane))))
|# 

;;;
;;;
;;;


(defun sirius-enter-newline ()
  (move-cursor-to-end-of-expression)
  (append-to-command-buffer +newline-string+))

(defun sirius-enter-space ()
  (append-to-command-buffer " "))

(defun sirius-enter-backspace ()
  (backward-character))

(defun sirius-enter-closing-par ()
  (append-to-command-buffer ")"))

(defun sirius-enter-opening-par ()
  (append-to-command-buffer "("))

;;;
;;;
;;;

(defun clear-command ()
  (with-command-buffer-operation
   (setf (editor-pane-text command-pane) "")))

(defun clear-info ()
  (when (profile-show-info-pane (active-profile))
    (with-info-buffer-operation
      (setf (editor-pane-text info-pane) ""))))

;;; 
;;; Shell-Eingabe:
;;;

(defun sirius-enter-command-from-shell (&rest args)
  (declare (ignorable args))

  (cond ((or *incremental-search-active-p*)
         (apply #'sirius-gesture-spec-input args))

        (t
        
         (multiple-value-bind (input copy-p)
             (get-current-input)

           (cond ((and (not (eq input :nuked))
                       (eq (enter-command 
                            :input input
                            :process-command-p nil
                            :in-sirius-pane-process-p t)
                           :okay))

                  (cond (copy-p 
                                     
                         (replace-current-input 
                          input)
                         (move-cursor-to-end-of-expression)

                         (sirius-update-argument-display))

                        (t (sirius-enter-newline)
                                        
                           ;;; Aua, ich muss einen neuen Prozess starten, denn CAPI macht
                           ;;; kein Editor-Update bis nicht sirius-enter-command-from-shell 
                           ;;; zurueckkommt! 
                                        
                           (start-process "input process"
                             (progn 
                               (apply #'sirius-enter-command-from-shell1 
                                      :input input 
                                      args)
                               (when copy-p
                                 (with-command-buffer-operation
                                   (call-editor command-pane "End of Buffer")
                                   (call-editor command-pane "Indent"))))))))

                 (t
                  ;;; still inside expression? no newline then

                  (sirius-update-argument-display)
         
                  (start-process "input process"
                    (apply #'sirius-enter-command-from-shell1 
                           :input input 
                           :dont-remove-trailing-newline-p t 
                           args))))))))


(defun sirius-enter-command-from-shell1 (&rest args &key input dont-remove-trailing-newline-p &allow-other-keys)
  (declare (ignorable args))

  (let* ((res nil)
         (rewritten-input nil)
         (*busy-marker* :busy-backgrounding)
         (input (or input (get-current-input)))
         (old-profile (active-profile)))

    (declare (ignorable rewritten-input))

    (when (and (not (eq input :nuked))
               (or (not (stringp input))
                   (not (blank-line-p input))))

      (labels ((update (&optional dummy-result)
                 (declare (ignorable dummy-result))

                 (without-callback-function

                  (when (and (eq res *busy-marker*)
                             ;;; Ergebnis ist nun im Cache, holen (und redisplay)
                             (switch-to-old-tab old-profile :shell))

                    (clear-current-prompt-and-input)

                    (with-only-cache-lookup
                      (let ((*busy-marker* :xasxsaxs12312x)) 
                        (enter-command :input input 
                                       :send-to-racer-p t ; diesmal aus dem Cache!
                                       :is-shell-command-p t
                                       :dont-remove-trailing-newline-p 
                                       dont-remove-trailing-newline-p 
                                       :update-command-history-p t
                                       :show-in-log-pane-p t
                                       :show-in-shell-pane-p t))))
                                        
                  (with-promoted-lock 
                    (sirius-use-current-abox-tbox)))))
          
        (with-asynchronous-request
          (with-callback-function
           #'update
           
           (multiple-value-setq (res rewritten-input)
               (enter-command :input input
                              :state-changing-command-p t
                              :send-to-racer-p t
                              :is-shell-command-p t
                              ;; :always-increment-prompt-p t
                              :dont-remove-trailing-newline-p 
                              dont-remove-trailing-newline-p 
                              :show-in-log-pane-p t
                              :show-in-shell-pane-p t))

           (unless (eq res *busy-marker*)
             ;;; in diesem Fall wird die Callback-Fn n icht aufgerufen! 
             ;;; manuell aufrufen: 
             (update))

           res))))))


(defun enter-command (&rest args
                            &key 
                            in-sirius-pane-process-p 
                            input
                            is-shell-command-p
                            (process-command-p t)
                            (show-in-shell-pane-p t)
                            ignore-unknown-simple-commands-p 
                            disable-error-handler-p
                            &allow-other-keys)
  
  (let ((res nil)
        (rewritten-input nil)
        (profile (active-profile)))

    (unless (connected-and-socket-alive-p profile)
      (no-connection-error)
      (return-from enter-command nil))
    
    (let ((closure 
           #'(lambda ()

               (with-profile-access

                 (let ((input 
                        (or (when input
                              (if (stringp input)
                                  input
                                (create-racer-call-string input)))
                            (get-current-input)))
                       (original-input input))
                       
                   (unless (eq input :nuked)

                     (handler-case 

                         (let* ((special-syntax-p
                                 ;;; SPARQL? Funktioniert nur aus dem Editor heraus!
                                 (let ((input (string-upcase input)))
                                   (and (some #'(lambda (x)
                                                  (search x input))
                                              '("SELECT"  "PREFIX" "WHERE" "FILTER" "DISTINCT"))
                                        (let ((pos (position-if-not #'whitespace-char-p input)))
                                          (if pos
                                              ;;; beginnt mit Text? 
                                              (alpha-char-p (elt input pos))
                                            nil)))))

                                (direct-command-p 
                                 (char= (elt input 0) #\:))

                                (input 
                                 (if special-syntax-p
                                     (format nil sparql-pattern input)
                                   input))

                                (command
                                 ;;; wird nur zum Dispatchen verwendet!
                                 ;;; s. replace-stars f. "echtes" read
                                 (my-read-from-string input)))

                           (when (and special-syntax-p
                                      is-shell-command-p)
                             (sirius-message 
                              (format nil "Sorry, SPARQL syntax not directly supported by Shell
(I don't know where the SPARQL expression ends). 
Please evaluate using RacerEditor, or enclose SPARQL query as follows: 
(sparql-retrieve ~S)"  original-input))
                             (error "bad syntax"))
                               
                           (labels ((process-simple-command (command &optional (input input))
                                          
                                      (if (not process-command-p)

                                          (setf res :okay)

                                        (let ((describe-p 
                                               (and (consp command)
                                                    (member (first command) 
                                                            +list-of-describe-functions+
                                                            :test #'string-equal
                                                            :key #'symbol-name)))

                                              (is-query-p
                                               (and (consp command)
                                                    (member (first command)
                                                            +list-of-query-functions+
                                                            :test #'string-equal
                                                            :key #'symbol-name)))
                                                  
                                              (sirius-command-p 
                                               (and (symbolp command)
                                                    (member command 
                                                            '(|*| 
                                                              |**| 
                                                              |***| 
                                                              |+| 
                                                              |++| 
                                                              |+++| 
                                                              |*t*|
                                                              |*a*|
                                                              |*n*|
                                                              |*i*|
                                                              |*c*| 
                                                              |*r*|
                                                              |*oo*|
                                                              |*or*|
                                                              |*ax*|
                                                              |*qor*|
                                                              |*def*|)))))
                                              
                                          (multiple-value-setq (res rewritten-input)
                       
                                              (cond ((or (consp command)
                                                           sirius-command-p)
                                                               
                                                       (apply #'input-extended-command input 
                                                              :describe-p describe-p 
                                                              :query-p is-query-p
                                                              :original-input (when special-syntax-p 
                                                                                original-input)
                                                              args))

                                                      (direct-command-p 
                                                       (case command
                                                         ((:|quit| :quit
                                                           :|exit| :exit
                                                           :|bye| :bye)
                                                          (quit-sirius-and-shutdown))

                                                         ((:|disconnect| :disconnect)
                                                          (close-server-connection))

                                                         ((:|editor| :editor :|edit| :edit)
                                                          (sirius-editor))
                                                             
                                                         ((:|open| :open)
                                                          (open-kb))

                                                         ((:|load| :load)
                                                          (load-kb))

                                                         (otherwise
                                                          (report-error
                                                           (format nil "Unknown command ~A" command))))

                                                       (input-prompt t))
                         
                                                      ((integerp command)
                                                       (if (zerop command)
                                                       
                                                           (report-error (format nil "History entry ~A not found" command))
                                                   
                                                         (let* ((input (nth (1- command) (reverse commands)))
                                                                (describe-p 
                                                                 (and (consp input)
                                                                      (member (first input)
                                                                              +list-of-describe-functions+
                                                                              :test #'string-equal
                                                                              :key #'symbol-name)))
                                                                (is-query-p
                                                                 (and (consp input)
                                                                      (member (first input)
                                                                              +list-of-query-functions+
                                                                              :test #'string-equal
                                                                              :key #'symbol-name))))
                                                       
                                                           (if input
                                                               (apply #'input-extended-command input 
                                                                      :dont-remove-trailing-newline-p t
                                                                      :describe-p describe-p 
                                                                      :query-p is-query-p
                                                                      args)
                                                             (report-error 
                                                              (format nil "History entry ~A not found" command))))))
                    
                                                      (t

                                                       (when (or (not (symbolp command))
                                                                 (not ignore-unknown-simple-commands-p))
                                                         (report-error 
                                                          (format nil "Unknown command ~A" command))))))))))
                                 
                             (cond ((and (consp command)
                                         (member (first command)
                                                 (cons 'progn (mapcar #'first +list-of-with-macros+))
                                                 :test #'string-equal
                                                 :key #'symbol-name))
                                        
                                    (let* ((args (second (assoc (first command)
                                                                (cons '(progn 0) +list-of-with-macros+)
                                                                :test #'string-equal
                                                                :key #'symbol-name)))
                                           (commands (subseq command (1+ args)))
                                           (header (subseq command 0 (1+ args)))
                                           (progn-p (string-equal 
                                                     (symbol-name (first command))
                                                     "progn")))

                                      (if (cdr commands) ; sonst Endlosschleife!
                                          (dolist (command commands)
                                            (if progn-p 
                                                (process-simple-command 
                                                 command
                                                 (let ((*readtable* editor::*editor-readtable*))
                                                   ;;; sorgt dafuer, dass keine BARS etc. beim Printen entstehen
                                                   ;;; Leerzeichen am Ende wichtig - (Return-Ersatz)
                                                   (format nil "~S " command)))
                                              (process-simple-command 
                                               `(,@header ,command)
                                               (let ((*readtable* editor::*editor-readtable*))
                                                 ;;; sorgt dafuer, dass keine BARS etc. beim Printen entstehen
                                                 ;;; Leerzeichen am Ende wichtig - (Return-Ersatz)
                                                 (format nil "~S " `(,@header ,command))))))
                                            
                                        (if progn-p
                                            (process-simple-command
                                             (second command)
                                             (let ((*readtable* editor::*editor-readtable*))
                                               (format nil "~S " (second command))))
                                          (process-simple-command command)))))

                                   (t (process-simple-command command)))))

                       (error (error)
                         
                         ;; (error error)
                         ;; dieser Handler sorgt dafuer, dass beim Editieren einfach Enter gedrueckt
                         ;; werden kann fuer eine neue Zeile (Expression ist noch nicht vollstaendig) 
                         ;; der error wird von my-read-from-string geworfen
                         ;; (error error)

                         (if disable-error-handler-p 
                             (error error)
                           (when (and show-in-shell-pane-p process-command-p)
                             (with-command-buffer-operation
                               (call-editor command-pane "End of Buffer")
                               (call-editor command-pane "New Line")
                               (call-editor command-pane "Indent"))))))))))))

      (if in-sirius-pane-process-p 
          (with-command-buffer-operation
            (funcall closure))
        (funcall closure)))
         
    (values res rewritten-input)))




