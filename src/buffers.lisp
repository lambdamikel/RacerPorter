;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defun get-log-buffer () 
  (editor:buffer-from-name "Log"))

(defun get-info-buffer () 
  (editor:buffer-from-name "Info"))

(defun get-command-buffer () 
  (editor:buffer-from-name "Command"))


;;;
;;;
;;; 

(defun append-to-buffer (buffer-name input)
  (let ((buffer (editor:buffer-from-name buffer-name)))
    (editor:use-buffer buffer
      (let ((point (editor::buffer-point buffer)))
        (typecase input
          (string (editor:insert-string point input))
          (list
                 
           (editor:insert-string point "(")
           (let* ((pos (editor:point-column point))
                  (spaces (format nil "~%~A" (make-string pos :initial-element #\space))))

             (maplist #'(lambda (item)
                          (let ((item (first item))
                                (lastp (not (cdr item))))
                                  
                            (editor:insert-string point 
                                                  (with-pretty-printing ()
                                                                        (format nil "~S" item)))
                                  
                            (unless lastp 
                              (editor:insert-string point spaces))))
                     
                      input)
                   
             (editor:insert-string point ")"))))))))
                 

(defun append-to-command-buffer (input)
  (with-sirius-slots (command-pane)
                     (apply-in-pane-process command-pane 
                         #'(lambda ()
                             (append-to-buffer "Command" input)))))

(defun append-to-info-buffer (input)
  (when (profile-show-info-pane (active-profile))
    (with-sirius-slots (info-pane)
                       (apply-in-pane-process info-pane
                                              #'(lambda ()
                                                  (append-to-buffer "Info" input))))))

;;;
;;;
;;;

(defun beginning-of-buffer (pane) 
  (apply-in-pane-process pane 
                         #'(lambda ()
                             (call-editor pane "Beginning of Buffer"))))

(defun beginning-of-line (pane)  
  (apply-in-pane-process pane 
                         #'(lambda ()
                             (call-editor pane "Beginning of Line"))))

(defun end-of-buffer (pane) 
  (apply-in-pane-process pane 
                         #'(lambda ()
                             (call-editor pane "End of Buffer"))))

;;;
;;;
;;; 

(defmacro with-command-buffer-operation (&body body)
  `(with-sirius-slots (command-pane)
                      (apply-in-pane-process command-pane
                                             #'(lambda ()
                                                 (let ((buffer (get-command-buffer)))
                                                   (editor:use-buffer buffer
                                                     ,@body))))))


(defmacro with-info-buffer-operation (&body body)
  `(with-sirius-slots (info-pane)
                      (apply-in-pane-process info-pane
                                             #'(lambda ()
                                                 (let ((buffer (get-info-buffer)))
                                                   (editor:use-buffer buffer
                                                     ,@body))))))




