;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defun get-time ()
  (multiple-value-bind (s mi h d mo year)
      (decode-universal-time (get-universal-time))
    (format nil "~2,'0D:~2,'0D:~2,'0D, ~2,'0D-~2,'0D-~2,'0D" 
            h mi s
            year mo d)))

(defun append-to-log (message &optional (header-p t))
  (unless *no-logging*
    (with-profile-access
      (with-capi-synchronization
        (when (or (and use-logfile log-stream)
                  use-logbuffer)
          (let ((message 
                 (if header-p
                     (concatenate 'string
                                  (get-time)
                                  ", " 
                                  message)
                   message)))

            (when use-logbuffer
              (with-sirius-app (sirius)
                (with-slots (log-length) sirius
                  (apply-in-pane-process sirius
                                         #'(lambda ()
                                             (let ((buffer (get-log-buffer)))
                                               (editor:use-buffer buffer
                                                 (let ((point (editor::buffer-point buffer)))
                                                   (when (> log-length 1000)
                                                     (clear-log))
                                                   (incf log-length)
                                                   (if (> (length message) 100)
                                                       (editor:insert-string point 
                                                                             (format nil "~A...\" (truncated)]~%" 
                                                                                     (subseq message 0 98)))
                                                     (editor:insert-string point message))))))))))

            (when (and use-logfile log-stream)
              (handler-case 
                  (progn 
                    (write-string message log-stream)
                    (stream:stream-flush-buffer log-stream))
                (error (error)
                  (error-message 
                   "Error with Logfile ~A:~%~A."  
                   (namestring porter-logfile)
                   error)
                  (close log-stream)
                  (setf log-stream nil
                        use-logfile nil))))))))))
          
(defun clear-log ()
  (with-sirius-app (app)
    (with-slots (log-pane log-length) app
      (setf log-length 0)
      (setf (editor-pane-text log-pane) "")
      (apply-in-pane-process 
       log-pane
       #'(lambda ()
           (call-editor log-pane "End of Buffer"))))))
