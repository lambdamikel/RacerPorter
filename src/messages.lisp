;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defconstant +quit-message+ (format nil "Really Quit ~A?" +sirius-name+))

(defconstant +no-connection-message+ "No Connection to RacerPro")

;;;
;;;
;;;

(defun message (string &optional (type :message))
  (let* ((editor 
          (make-instance
           'capi:editor-pane
           :accepts-focus-p nil
           :contents string))
         (title 
          (ecase type
            (:message "Message")
            (:error "Error")
            (:warning "Warning"))))
    (capi:popup-confirmer editor 
                          title
                          :cancel-button nil
                          :modal t
                          :title title
                          :visible-min-width 
                          '(character 100)
                          :visible-max-height 
                          '(character 40))))

(defun error-message (string &rest args)
  (message (apply #'format nil string args) :error))

(defun warning-message (string &rest args)
  (message (apply #'format nil string args) :warning))

(defun sirius-message (string &rest args)
  (message (apply #'format nil string args) :message))
