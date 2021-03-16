;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defun open-connection (&optional silent-p)
  (with-profile ((active-profile))
    (let ((res
           (if (not (open-server-connection))
  
               (if silent-p 
                   nil
                 (let ((*display-error* t))
                   (no-connection-error)
                   (enter-dummy-command
                    (format nil "Using Profile ~A to Connect to ~A" 
                            (profile-name (active-profile))
                            (get-server-description (active-profile) t))
                    :error)
          
                   nil))

             (if silent-p 
                   
                 t

               (progn
           
                 (enter-dummy-command
                  (format nil "Using Profile ~A to Connect to ~A" 
                          (profile-name (active-profile))
                          (get-server-description (active-profile) t))
                  (list :okay (get-server-description (active-profile) t)))

                 t)))))

      (sleep +wait-after-connect-sleep-time+)

      (resync-state)

      res)))

(defun close-connection (&optional silent-p)
  (with-profile ( (active-profile) )
    (let ((server 
           (get-server-description (active-profile) t)))

      (close-server-connection)
      
      (unless silent-p 
        (enter-dummy-command
         (format nil "Disconnect from ~A" server)
         :okay)))

    (resync-state)))

(defun connect-or-disconnect ()
  (progn ;with-capi-synchronization
    (if (connected-and-socket-alive-p)
        (close-connection)
      (progn 
        (clear-current-prompt-and-input)
        (open-connection)
        (when (connected-and-socket-alive-p)
          (push-history-entry))))))

(defun connect ()
  (progn ;with-capi-synchronization
    (clear-current-prompt-and-input)
    (open-connection)
    (when (connected-and-socket-alive-p)
      (push-history-entry))))
