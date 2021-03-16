;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(setf CAPI-COCOA-LIBRARY::*DEFAULT-META-MODIFIER* :alt)

;;;
;;;
;;;

(define-interface sirius-application (capi:cocoa-default-application-interface)
  ()
  (:menus
   (application-menu
    
    +sirius-name+
    
    ((:component
      (( (format nil "About ~A" +sirius-name+)
         :callback 'show-sirius-logo
         :callback-type :none)))
     
     (:component
      (("Preferences..."
        :callback 'edit-profile
        :callback-type :none)))
     
     (:component
      ()
      ;; This is a special named component where the CAPI will
      ;; attach the standard Services menu.
      :name :application-services)
     
     (:component
      (( (format nil "Hide ~A" +sirius-name+)
         :accelerator "accelerator-h"
         :callback-data :hidden)
       
       ("Hide Others"
        :accelerator "accelerator-meta-h"
        :callback-data :others-hidden)
       
       ("Show All"
        :callback-data :all-normal))
      
      :callback #'(setf capi:top-level-interface-display-state)
      :callback-type :data-interface)
     
     (:component
      (( (format nil "Quit ~A & Shutdown RacerPro" +sirius-name+)
         :accelerator "accelerator-shift-q"
         :callback (lambda ()
                     (quit-sirius-and-shutdown))
         :callback-type :none)

       ( (format nil "Quit ~A" +sirius-name+)
         :accelerator "accelerator-q"
         :callback (lambda ()
                     (quit-sirius-and-ask-for-shutdown-of-server-started-by-this-racerporter))         
         :callback-type :none)))))
   
   (file-menu
    "File"
    ((:component
      (#+:sirius-editor
       ("New"
        :accelerator "accelerator-n"
        :callback 'sirius-editor
        :callback-type :none)
       ("Open..."
        :accelerator "accelerator-o"
        :callback 'open-kb
        :callback-type :none)
       ("Load..."
        :accelerator "accelerator-l"
        :callback 'load-kb
        :callback-type :none)))))
   
   (edit-menu
    "Edit"
    ((:component
      (("Undo"
        :callback-type :none
        :callback 'sirius-undo
        :accelerator "accelerator-z")))
     (:component
       (("Cut"
        :callback-type :none
        :callback 'sirius-cut
        :accelerator "accelerator-x")
       ("Copy"
        :callback-type :none
        :callback 'sirius-copy
        :accelerator "accelerator-c")
       ("Paste"
        :callback-type :none
        :callback 'sirius-paste
        :accelerator "accelerator-v")))))
   
   (windows-menu
    "Window"
    ((:component
      (("Close"
        :accelerator "accelerator-w"
        :callback 'quit-sirius-and-ask-for-shutdown-of-server-started-by-this-racerporter
        :callback-type :none)))
     (:component
      (("RacerPorter"
        :callback 'raise-racerporter
        :callback-type :none)
       #+:sirius-editor 
       ("RacerEditor"
        :callback 'raise-racereditor
        :enabled-function (lambda (menu) 
                            (declare (ignorable menu))
                            *sirius-editor-app*)

        :callback-type :none)))))
   
   )

  (:menu-bar application-menu file-menu edit-menu windows-menu
   )

  (:default-initargs
   :title +sirius-name+
   :application-menu 'application-menu
   :message-area nil 
   :message-callback 'sirius-mac-message
   ;:auto-menus t
   ))


(defun sirius-undo ()
  (with-sirius-app (sirius)
    (with-slots (command-pane) sirius 
    (editor:process-character "Undo" (editor-window command-pane)))))

(defun sirius-cut ()
  (with-sirius-app (sirius)
    (with-slots (command-pane) sirius 
    (editor:process-character "Kill Region" (editor-window command-pane)))))

(defun sirius-copy ()
  (with-sirius-app (sirius)
    (with-slots (command-pane) sirius 
    (editor:process-character "Save Region" (editor-window command-pane)))))

(defun sirius-paste ()
  (with-sirius-app (sirius)
    (with-slots (command-pane) sirius 
    (editor:process-character "Un-kill" (editor-window command-pane)))))

;;;
;;; 
;;; 

(defun sirius-mac-message (self message &rest args)
  (declare (ignore self))
  (case message
    (:open-file
     (handler-case
         (let ((filename (car args)))
           (if (interface-displayed-p *sirius-app*)
               (execute-with-interface *sirius-app*
                                       (lambda (file)
                                         (open-connection)
                                         (when (connected-and-socket-alive-p)
                                           (sirius-editor file)))
                                       filename)
             (setf (load-file-on-display *sirius-app*) filename)))
       (error (condition)
         (report-error (format nil "~A" condition)))))))


;;;
;;;
;;;


(defun show-sirius-logo ()
  (let* ((logo
          (make-instance 'output-pane
                         ;;:background +logo-bc+
                         :display-callback 'display-logo-pane
                         :visible-min-width 820
                         :visible-min-height 641
                         :horizontal-scroll nil
                         :vertical-scroll nil)))

    (popup-confirmer
     logo
     +version-info+
     ;;:background +logo-bc+
     :modal t
     :ok-button "OK"
     :cancel-button nil
     :callback-type :none
     :title (format nil "About ~A" +sirius-name+))))


