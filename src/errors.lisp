;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defun truncate-if-longer-than (string n)
  (if (> (length string) n)
      (let ((m (floor (/ n 2))))
        (concatenate 'string
                     (subseq string 0 m)
                     "... (truncated) ..."
                     (subseq string (- (length string) m))))
    string))

(defun return-error-answer (message) 
  (values `(:error ,message) nil t))

(defmethod report-error ((message string))
  (let* ((message 
          (remove #\~ message)))

    (if *display-error*
        (progn
          (when (active-profile)
            (setf (profile-current-communication-status (active-profile)) :error))
          (error-message message)

          ;;; neu  - funktioniert? im Auge behalten
          (when (connected-and-socket-alive-p)
            (clear-and-resync))

          (return-error-answer message))

      (error message))))

(defmethod report-error ((message condition))
  (report-error (make-string-from-condition message)))

(defun make-string-from-condition (condition)
  (format nil "~A" condition))

(defun no-connection-error ()
  (report-error (format nil "No Connection using Profile 
~A
to ~A" 
                        (profile-name (active-profile))
                        (get-server-description))))

(defun error-res-p (res)
  (or 
   (and (consp res)
        (eq (first res) :error))
   (eq res :not-found)
   (eq res :error)))


(defun cannot-startup (&optional reason)
  (if reason
      (format t "~%*** Cannot startup ~A!~%*** Reason: ~A~%" +sirius-name+ reason)
    (format t "~%*** Cannot startup ~A!~%" +sirius-name+)))



