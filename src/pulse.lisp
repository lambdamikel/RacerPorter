;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

#-:pulse-pane
(defvar *pulse* 0)

#-:pulse-pane
(defvar *pulse-length* 8)

;;;
;;;
;;;



;;;
;;; 
;;; 

#+(and :pulse-pane :sirius-dev)
(defun get-pulse-image (status comm-status)  
  ;;; BMP-File Version
  (if (not (profile-show-status-led (active-profile)))
      *inline-noimage-image*
    (let ((file 
           (make-pathname
            :directory (pathname-directory (image-directory))
            :name 

            (let ((movie 

                   (or 
                  
                    (case status 
                      (:not-connected *not-connected-movie*)
                      (:died *error-movie*))

                    (case comm-status 
                      (:updating-display *updating-movie*)
                      (:connection-died-during-send *error-movie*)
                      (:connection-died-during-read *error-movie*)
                      (:connection-already-dead *error-movie*)
                      (:error *error-movie*))

                    (case status 
                      (:busy *busy-movie*)
                      (:ready *ready-movie*)
                      (:cache-hit *ready-movie*)))))

              (nth *pulse* movie))

            :type "bmp")))

      (when (probe-file file)
        (namestring file)))))

#+(and :pulse-pane (not :sirius-dev))
(defun get-pulse-image (status comm-status)
  (if (not (profile-show-status-led (active-profile)))
      *inline-noimage-image*
    (let ((movie 

           (or 
                  
            (case status 
              (:not-connected *not-connected-movie*)
              (:died *error-movie*))

            (case comm-status 
              (:updating-display *updating-movie*)
              (:connection-died-during-send *error-movie*)
              (:connection-died-during-read *error-movie*)
              (:connection-already-dead *error-movie*)
              (:error *error-movie*))

            (case status 
              (:busy *busy-movie*)
              (:ready *ready-movie*)
              (:cache-hit *ready-movie*)))))

      (nth *pulse* movie))))

(defun pulse ()
  (setf *pulse*
        (mod (1+ *pulse*) *pulse-length*)))
