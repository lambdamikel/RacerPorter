;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;; Editor Hacking and Patching 
;;;

(defconstant +editor-input-model+
  `((:GESTURE-SPEC editor-gesture-spec-input)
    ((:BUTTON-1 :PRESS) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-START-REGION)
    ((:BUTTON-1 :PRESS :SHIFT) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-BUTTON-1-SHIFT-ACTION)
    ((:BUTTON-1 :PRESS :SHIFT :CONTROL) CAPI::MOUSE-EDITOR-CALL EDITOR::MOUSE-SAVE-KILL)
    ((:MOTION :BUTTON-1) CAPI::MOUSE-EDITOR-CALL EDITOR::MOUSE-SET-POINT-AND-HIGHLIGHT)
    ((:BUTTON-1 :RELEASE) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-FINISH-REGION)
    ((:BUTTON-1 :RELEASE :SHIFT) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-CURSOR-RELEASE)
    ((:BUTTON-1 :SECOND-PRESS) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-START-MARK-FORM)
    ((:BUTTON-2 :PRESS) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-PANE-PASTE-SELECTION)
    ((:BUTTON-3 :PRESS :SHIFT) CAPI::MOUSE-EDITOR-CALL EDITOR::MOUSE-COPY-AND-PASTE)))

(defun editor-gesture-spec-input (&rest args) 
  (apply #'CAPI::GESTURE-SPEC-INPUT args))
                  
;;;
;;;
;;;                 

#-:sirius-dev 
(progn 

  ;;; #+:lispworks5.0
  (defmethod capi::interface-editor-pane-blink-rate ((interface (eql nil)) (pane t)) nil)

  (lw:undefine-action "Confirm when quitting image" "Save modified buffers")

  (defvar *old-process-character* (symbol-function 'editor:process-character))

  (let ((lw::*handle-warn-on-redefinition* :quiet))
    (defun editor::process-character (&rest args)
      (handler-case
          (apply *old-process-character* args)
        (error (error) (beep-pane)))))

  (defvar *old-goto-buffer* (symbol-function 'editor::goto-buffer))

  (let ((lw::*handle-warn-on-redefinition* :quiet))
    (defun editor::goto-buffer (buffer &rest args)
      (prog1
          (apply *old-goto-buffer* buffer args)
        (update-buffer-modeline buffer))))

  (defvar *old-insert-character* (symbol-function 'editor:insert-character))

  (let ((lw::*handle-warn-on-redefinition* :quiet))
    (defun editor::insert-character (point &rest args)
      (prog1
          (apply *old-insert-character* point args)
        (update-buffer-modeline (editor::point-buffer point)))))

  (defvar *old-change-to-buffer* (symbol-function 'editor::change-to-buffer))

  (let ((lw::*handle-warn-on-redefinition* :quiet))
    (defun editor::change-to-buffer (buffer &rest args)
      (prog1
          (apply *old-change-to-buffer* buffer args)
        (update-buffer-modeline buffer))))

  )

;;;
;;;
;;;

(defun transform-backquotes-etc (expr)
  (maptree expr
           #'(lambda (x) 
               (case x
                 (system::bq-list   :bq-list)
                 (system::bq-list*  :bq-list*)
                 (system::bq-append :bq-append)
                 (otherwise x)))))

;;;
;;; Eval
;;;

(defun update-tbox-and-abox ()
  (execute-with-interface *sirius-app*
                          #'(lambda () 
                              (sirius-use-current-abox-tbox)
                              (update-state-display))))

(defun racer-eval (expr) 
  ;;; Top Level Eval
  
  (let* ((result :wait))
    
    ;; (process-wait (profile-prompt-ready-p (active-profile)))
    ;; (setf (profile-prompt-ready-p (active-profile)) nil)

    (apply-in-pane-process *sirius-editor-app*
                           #'(lambda ()
                               (handler-case 
                                   (with-synchronous-request                                   
                                     (without-caching
                                       (setf result 
                                             (enter-command 
                                              :input
                                              (let ((*readtable* editor::*editor-readtable*))
                                                (format nil "~S" (transform-backquotes-etc expr)))
                                              :dont-remove-trailing-newline-p t
                                              :state-changing-command-p t
                                              :send-to-racer-p t
                                              :always-increment-prompt-p t
                                              :show-in-log-pane-p t
                                              :show-in-shell-pane-p t))))
                                 (error (error)
                                   ;;; sonst haengt der Process wait!
                                   (setf result `(:error ,(format nil "~A" error)))))))
           
    (process-wait (not (eq result ':wait)))
        
    (update-tbox-and-abox)

    (editor::process-character
     `(editor::message "Racer says: ~S" 
                       ,(truncate-if-longer-than 
                         (format nil "~S" result) 200))
     (capi:editor-window (slot-value *sirius-editor-app* 'editor-pane)))

    ;; (setf (profile-prompt-ready-p (active-profile)) t)

    result))

(defun sparql-eval (expr) 
  ;;; Top Level Eval
  
  (let* ((result :wait))

    (apply-in-pane-process *sirius-editor-app*
                           #'(lambda ()
                               (handler-case 
                                   (with-synchronous-request
                                     (without-caching
                                       (setf result 
                                             (enter-command 
                                              :input expr
                                              :dont-remove-trailing-newline-p t
                                              :state-changing-command-p nil
                                              :send-to-racer-p t
                                              :always-increment-prompt-p t
                                              :show-in-log-pane-p t
                                              :show-in-shell-pane-p t))))
                                 (error (error)
                                   (setf result `(:error ,(format nil "~A" error)))))))
    
    (process-wait (not (eq result ':wait)))
        
    (update-tbox-and-abox)

    result))

(defun racer-eval2 (expr) 
  ;;; Background Eval (s. editor-commands) 

  (let* ((result :wait))
    (apply-in-pane-process *sirius-editor-app*
                           #'(lambda ()
                               (handler-case 
                                   (with-synchronous-request
                                     (without-caching
                                       (setf result 
                                             (let ((*display-error* nil)) ; damit Error hier durchkommt!
                                               (enter-command 
                                                :input 
                                                (let ((*readtable* editor::*editor-readtable*))
                                                  (format nil "~S" (transform-backquotes-etc expr)))
                                                :disable-error-handler-p t
                                                :dont-remove-trailing-newline-p t
                                                :ignore-unknown-simple-commands-p nil ;;; t 
                                                :state-changing-command-p t
                                                :send-to-racer-p t
                                                :update-command-history-p nil
                                                :always-increment-prompt-p nil
                                                :show-in-log-pane-p nil
                                                :show-in-shell-pane-p nil
                                                :keep-prompt-p t)))))
                                 (error (error)
                                   (setf result `(:error ,(format nil "~A" error)))))))
    
    (process-wait (not (eq result ':wait)))

    result))

;;;
;;; Editor Class and Frame 
;;; 

(defclass sirius-editor-pane
          (capi:editor-pane)
  ()
  (:default-initargs
   :buffer-modes '("Lisp" "XML")
   :visible-min-height '(character 10)
   :visible-min-width '(character 10)   

   :input-model +editor-input-model+))

(define-interface sirius-editor ()
  ((mode :initarg :fundamental :accessor editor-mode)
   (saved-editor-pane :accessor saved-editor-pane  :initform nil))

  (:menus 
   (file-menu
    "File"
    ((:component
      (("New"
        :accelerator
        #+:mac "accelerator-n" 
        #-:mac "control-n" 
        :callback 'sirius-editor
        :callback-type :none)

       ("Open..."
        :accelerator 
        #+:mac "accelerator-o" 
        #-:mac "control-o" 
        :callback 'open-kb
        :callback-type :none)

       ("Load..."
        :accelerator
        #+:mac "accelerator-l"
        #-:mac "control-l"
        :callback 'load-kb2
        :callback-type :none)))))

   (edit-menu
    "Edit"
    ((:component
      (("Undo"
        :callback-type :none
        :callback 'editor-undo
        :accelerator 
        #+:mac "accelerator-z"
        #-:mac "control-meta-z")))

     (:component
      (("Cut"
        :callback-type :none
        :callback 'editor-cut
        :accelerator 
        #+:mac "accelerator-x"
        #-:mac "control-meta-x")

       ("Copy"
        :callback-type :none
        :callback 'editor-copy
        :accelerator
        #+:mac "accelerator-c"
        #-:mac "control-meta-c")

       ("Paste"
        :callback-type :none
        :callback 'editor-paste
        :accelerator
        #+:mac "accelerator-v"
        #-:mac "control-meta-v")))))

   (buffer-menu
    "Buffer"

    ((:component
      (("Complete Racer Command"
        :accelerator 
        #-:mac "control-a"
        #+:mac "accelerator-r"
        :callback 'complete-racer-command1
        :callback-type :none)

       ("Complete Racer Filename"
        :accelerator 
        #-:mac "control-7"
        #+:mac "accelerator-7"
        :callback 'complete-racer-filename1
        :callback-type :none)

       ("Complete Word"
        :accelerator 
        #-:mac "control-8"
        #+:mac "accelerator-8"
        :callback 'complete-word
        :callback-type :none)

       ("Racer Command Signature"
        :accelerator 
        #-:mac "control-i"
        #+:mac "accelerator-i"
        :callback 'racer-command-signature
        :callback-type :none)))
     
     (:component
      (("Evaluate Racer Expression"
        :accelerator 
        #+:mac "accelerator-e"
        #-:mac "control-shift-e"
        :callback 'eval-racer-expression
        :callback-type :none)
     
       ("Evaluate Sparql Expression"
        :accelerator 
        #+:mac "accelerator-shift-s" 
        #-:mac "control-shift-s"
        :callback 'eval-sparql-expression
        :callback-type :none)
       
       ("Evaluate Racer Buffer"
        :accelerator
        #+:mac "accelerator-h"
        #-:mac "control-shift-h"
        :callback 'eval-racer-buffer
        :callback-type :none)
       
       ("Evaluate OWL Buffer (.rdf, .rdf, or .ofn)"
        :accelerator
        #+:mac "accelerator-y"
        #-:mac "control-shift-y"
        :callback 'eval-owl-buffer
        :callback-type :none)))))

   (windows-menu
    "Window"

    ((:component
      (("Close"
        :accelerator
        "accelerator-w"
        :callback 'close-editor-window
        :callback-type :interface)))

     (:component
      (("RacerPorter"
        :callback 'raise-racerporter
        :callback-type :none)

       ("RacerEditor"
        :callback 'raise-racereditor
        :enabled-function (lambda (menu)
                            (declare (ignore menu))
                            *sirius-editor-app*)
        :callback-type :none))))))

  (:panes

   (modeline title-pane)

   (editor-pane sirius-editor-pane 
                :echo-area t
                :background +sirius-bc+
                :foreground +sirius-fc+))

  (:layouts
   
   (main-layout column-layout 
                '(editor-pane modeline)))

  (:menu-bar
   file-menu  
   edit-menu
   buffer-menu
   #+:mac windows-menu
   )
  
  (:default-initargs :title +editor-name+

   :auto-menus nil
   
   :best-height '(character 40)
   :best-width '(character 80)

   :destroy-callback #'editor-destroy
   
   #+:macosx :create-callback #+:macosx #'editor-create
   #-:macosx :top-level-hook  #-:macosx #'editor-top-level
   
   ))

;;;
;;; Hooks 
;;; 

(defun sirius-error-handler (condition me-or-my-encapsulation)
  (declare (ignore me-or-my-encapsulation))
  (let ((*display-error* t))
    (report-error (format nil "~A" condition))))

(defun editor-create (interface)
  (setf *debugger-hook* 'sirius-error-handler)
  (setf (saved-editor-pane interface) 
        (slot-value interface 'editor-pane))

  (setf editor::*current-evaluator* 'racer-eval))

(defun editor-top-level (continuation editor)
  (setf editor::*current-evaluator* 'racer-eval)
  (setf (saved-editor-pane editor) 
        (slot-value editor 'editor-pane))
  (handler-case 
      (progn 
        (funcall continuation))
    (error (condition)
      (report-error (format nil "~A" condition)))))

(defun editor-destroy (editor)
  (declare (ignorable editor))
  (setf *sirius-editor-app* nil)
  t)

;;;
;;;
;;; 

(defmethod capi:interface-keys-style ((self sirius-editor))
  :emacs)

;;;
;;; Modeline 
;;; 

(defun update-buffer-modeline (buffer-triggering-hook-function &rest args) 
  (declare (ignore args buffer-triggering-hook-function))
  (when *sirius-editor-app*
    (execute-with-interface 
     *sirius-editor-app*
     (lambda ()

       (with-slots (modeline editor-pane) *sirius-editor-app*
         (let* ((buffer (and editor-pane (capi:editor-pane-buffer editor-pane)))
                (pathname (and buffer (editor:buffer-pathname buffer))))
           (when modeline 
             (setf (title-pane-text modeline)
                   (let ((modeline-string
                          (format nil 
                                  "~A ~A ~A    ~A ~A"
                                  (if (connected-and-socket-alive-p)
                                      (get-server-description (active-profile) t)
                                    "")
                                  (cond ((not (editor::buffer-writable buffer))
                                         "-%%-")
                                        ((editor:buffer-modified buffer)
                                         "-**-")
                                        (t "----"))
                                  #|(if pathname 
                                        (format nil "~A.~A" 
                                                (pathname-name pathname)
                                                (pathname-type pathname))
                                      (format nil "~A" (editor:buffer-name buffer))
                                      )|#
                                  (format nil "~A" (editor:buffer-name buffer))
                                  (editor::buffer-mode-names buffer)
                                  (if pathname (namestring pathname) ""))))
                     (if (editor::buffer-windows buffer)
                         (subseq modeline-string 0 
                                 (min (length modeline-string)
                                      (editor::window-width (first (editor::buffer-windows buffer)))
                                      ))
                       (subseq modeline-string 0)))))))))))


(defun update-current-buffer-modeline ()
  (update-buffer-modeline editor::*current-buffer*))

(editor::add-global-hook editor::input-hook 'update-current-buffer-modeline)


;;;
;;; Commands 
;;;


(defun editor-undo ()
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Undo")))

(defun editor-cut ()
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Kill Region")))

(defun editor-copy ()
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Save Region")))

(defun editor-paste ()
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Un-kill")))

;;;
;;;
;;;

(defun close-editor-window (interface)
  (quit-interface interface))

(defun complete-racer-command1 (&rest args)
  (declare (ignore args))
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Complete Racer Command")))

(defun complete-racer-filename1 (&rest args)
  (declare (ignore args))
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Complete Racer Filename")))

(defun complete-word (&rest args)
  (declare (ignore args))
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Dynamic Completion")))

(defun racer-command-signature (&rest args)
  (declare (ignore args))
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Racer Command Signature")))

(defun eval-racer-expression (&rest args)
  (declare (ignore args))
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Evaluate Expression")))

(defun eval-sparql-expression (&rest args)
  (declare (ignore args))
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Evaluate Sparql Query")))

(defun eval-racer-buffer (&rest args)
  (declare (ignore args))
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Evaluate Buffer")))

(defun eval-owl-buffer (&rest args)
  (declare (ignore args))
  (let ((editor (slot-value *sirius-editor-app* 'editor-pane)))
    (call-editor editor "Evaluate OWL Buffer")))

(defun sirius-editor1 (&rest args)
  (declare (ignore args))
  (sirius-editor))

(defun open-kb1 (&rest args)
  (declare (ignore args))
  (open-kb))

(defun load-kb2 (&rest args)
  (declare (ignore args))
  (capi:raise-interface *sirius-app*)
  (execute-with-interface 
   *sirius-app*
   (lambda ()
     (load-kb))))

;;;
;;; Temp Directory 
;;; 

(defun generate-temp-filename ()
  (when (not (or (profile-sirius-temp-directory (active-profile))
                 (sirius-temp-directory)))
    (error "No Sirius Temp Directory Specified in Profile ~A!" (profile-name (active-profile))))

  (loop for i from 0 to 10000
        for temp-filename = (concatenate 'string 
                                         (namestring
                                          (or (profile-sirius-temp-directory (active-profile))
                                              (sirius-temp-directory)))
                                         "racer-" (format nil "~D" i) ".temp")
        unless (probe-file temp-filename)
        do 
        (return-from generate-temp-filename 
          (namestring temp-filename)))
  (error "Cannot create temporary file."))


(defmacro with-temp-file (((stream-var) &rest generator-forms) &body parser-forms)
  (let ((temp-filename-var (gensym)))
    `(let ((,temp-filename-var (generate-temp-filename)))
       (with-open-file (,stream-var 
                        ,temp-filename-var
                        :direction :output
                        :if-exists :supersede)
         .,generator-forms)
       (unwind-protect
           (with-open-file (,stream-var 
                            ,temp-filename-var
                            :direction :input)
             .,parser-forms)
         (delete-file ,temp-filename-var)))))


;;;
;;; OWL (RDF, OWL XML, OWL Functional) Evaluator 
;;; 

(defun sirius-eval-owl (stream)
  (with-suspended-server-pane-update
    (let ((number-of-bytes 0))
      (with-synchronous-request
      
        (with-temp-file ((temp-stream)
                       
                         ;;; keine Ahnung, warum das notwendig ist!!!
                         ;;; nicht mehr notwendig? 
                         ;;; (write-char #\newline temp-stream)

                         (loop for char = (read-char stream nil nil)
                               do
                               (when (null char)
                                 (return))
                               (incf number-of-bytes)
                               (write-char char temp-stream)))

          (let ((file1
                 (racer-function
                  (transmit-file "tmp" number-of-bytes)))
                #+:ignore
                (last-val 0))

            (unless (consp file1) ; kein error? 
              (with-sirius-app (sirius)
                (execute-with-interface sirius
                                        (lambda ()
                                          (enter-dummy-command 
                                           (format nil "Sending ~A to ~A:~A as ~A..." "Ontology"
                                                   (profile-host (active-profile))
                                                   (profile-port (active-profile))
                                                   file1)
                                           :okay))))

              (with-socket-lock
                (loop for char = (read-char temp-stream nil nil)
                      ;;for byte from 0 by 1 
                      ;;for val = (float (* 100 (/ byte number-of-bytes)))
                      do
                      ;; (princ " ")
                      ;; (princ char) 
                      
                      #+:ignore
                      (when (> (- val last-val) 1)
                        (sirius::set-progress-bar-to val t)
                        (setf last-val val))
                     
                      (when (null char)
                        (return))
                      (write-char char (profile-socket (active-profile))))
            
                ;; (princ "done!")

                (force-output (profile-socket (active-profile))))

              (with-sirius-app (sirius)
                (execute-with-interface sirius
                                        (lambda ()
                                          (with-synchronous-request
                                            (without-caching
                                              (unwind-protect
                                                  (enter-state-changing-result-command
                                                   `(owlapi-read-ontology 
                                                     ,(double-backslashes file1)
                                                     :maintain-owlapi-axioms 
                                                     ,(with-profile-access
                                                        editor-eval-maintain-axioms)))))))))))))))

  (update-tbox-and-abox))
  
;;;
;;; Start 
;;;  

(defvar *editor-counter* 0)

(defun sirius-editor (&optional filename from-shell-p)
  (with-capi-synchronization
    (with-suspended-server-pane-update
      (unless *sirius-editor-app*
        (setf *sirius-editor-app* (make-instance 'sirius-editor)))

      (display *sirius-editor-app*)

      (labels ((initialize-editor (buffer)
                 (let ((current-window 
                        (capi:editor-window (slot-value *sirius-editor-app* 'editor-pane))))
                   (capi:execute-with-interface
                    *sirius-editor-app*
                    (lambda ()
                      (let ((editor::*current-window* current-window))
                        (setf (editor:buffer-major-mode buffer) "Lisp")
                        (editor::goto-buffer-if-unflagged-current 0 buffer t)
                        (editor:move-point (editor:buffer-point buffer)
                                           (editor:buffers-start buffer))))))))
                                               
        (let ((buffer
               (if filename
                   (editor:find-file-buffer (pathname filename))
                 (if from-shell-p
                     (editor:buffer-from-name "Command")
                   (editor::make-buffer (format nil "Untitled-~A" (incf *editor-counter*)))))))

          (setf (slot-value *sirius-editor-app* 'editor-pane)
                (if from-shell-p 
                    (slot-value *sirius-app* 'command-pane)
                  (saved-editor-pane *sirius-editor-app*)))

          (if from-shell-p 
              (setf (output-pane-input-model  (slot-value *sirius-editor-app* 'editor-pane))
                    +sirius-input-model+)
            (setf (output-pane-input-model  (slot-value *sirius-editor-app* 'editor-pane))
                  +editor-input-model+))

          (initialize-editor buffer))))))


(defun sirius-editor-string (string)
  (with-capi-synchronization
    (with-suspended-server-pane-update
      (unless *sirius-editor-app*
        (setf *sirius-editor-app* (make-instance 'sirius-editor)))

      (display *sirius-editor-app*)

      (labels ((initialize-editor (buffer)
                 (let ((current-window 
                        (capi:editor-window (slot-value *sirius-editor-app* 'editor-pane))))
                   (capi:execute-with-interface
                    *sirius-editor-app*
                    (lambda ()
                      (let ((editor::*current-window* current-window))
                        (setf (editor:buffer-major-mode buffer) "Lisp")
                        (editor::goto-buffer-if-unflagged-current 0 buffer t)
                        (setf (editor-pane-text (slot-value *sirius-editor-app* 'editor-pane))
                              string)
                        (editor:move-point (editor:buffer-point buffer)
                                           (editor:buffers-start buffer))))))))
                                               
        (let ((buffer
               (editor::make-buffer (format nil "Untitled-~A" (incf *editor-counter*)))))

          (setf (slot-value *sirius-editor-app* 'editor-pane)
                (saved-editor-pane *sirius-editor-app*))

          (initialize-editor buffer))))))

;;;
;;;
;;;

(defun read-editor-config ()
  (with-profile-access
      (handler-case
	  (when (probe-file editor-configfile)
	    (with-open-file (stream editor-configfile  :direction :input)
	      (loop 
		(let ((line1 (read-line stream nil nil))
		      (line2 (read-line stream nil nil))
		      (line3 (read-line stream nil nil)))
		  (declare (ignorable line3))
		  ;; (pprint (list line1 line2))
		  (if (or (not line1) (not line2))
		      (return)
		    (if (editor::find-command line1)
			(editor:bind-key line1 line2)
		      (error-message "Error in RacerEditor Config File \"~A\": 
Unknown Command ~S" editor-configfile line1)))))))
	(error (error)
	  (error-message "Error in RacerEditor Config File \"~A\": 
~S" editor-configfile error)))))

;;; Bsp. Editor Config-Datei: 

#| 
Forward Word
Control-Right

Backward Word
Control-Left

Forward Form
Meta-Right

Backward Form
Meta-Left

Beginning of Line
Home

End of Line
End

Beginning of Buffer
Control-Home

End of Buffer
Control-End

|# 
