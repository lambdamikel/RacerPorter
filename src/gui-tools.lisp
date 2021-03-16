;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)


;;;
;;;
;;;


(defun non-modal-message (message &optional sleep-time)
  (let ((button nil))

    (start-process-with-options
                                                 
        "cancel"
                                                   
        (:priority 900000)

      (setf button nil)

      (when sleep-time
        (sleep sleep-time))

      (contain 
       (setf button
             (make-instance 
              'row-layout
              :description
              (list 
               (make-instance
                'title-pane
                :text message))
              :adjust :center))

       :x :center
       :y :center
                                                         
       :auto-menus nil
       :toolbar nil
       :title "Wait!"))

    (process-wait button)
      
    button))


(defmethod please-wait-for-racer ((pooled-process pooled-process))
  (when (terminate-p pooled-process) 

    (let ((button 
           (non-modal-message 
            (format nil "Waiting for ~A to Finish ... Please Stand By"
                    (get-server-description))
            +wait-for-cancel-button-sleep-time+)))
              
      (process-wait (not (terminate-p pooled-process)))
      (process-wait button)    
        
      (apply-in-pane-process button
                             #'(lambda ()
                                 (quit-interface button))))))


#|

(defun non-modal-message (message &optional sleep-time)
  (declare (ignorable sleep-time))
  (let ((button nil))
    (contain 
     (setf button
           (make-instance 
            'row-layout
            :description
            (list 
             (make-instance
              'title-pane
              :text message))
            :adjust :center))

     :x :center
     :y :center
                                                         
     :auto-menus nil
     :toolbar nil
     :title "Wait!")

    button))

|#


;;;
;;;
;;;


(defun open-file-prompter (title filter &optional pathname)
  (multiple-value-bind (file successp)
      (prompt-for-file title 
                       :pathname pathname
                       :filter filter
                       :operation :open
                       #-:linux 
                       :ok-check
                       #-:linux
                       #'probe-file
                       :if-exists :ok
                       :if-does-not-exist :error)
    (when (and file successp)
      (substitute-backslashes-with-slashes
       (if (pathnamep file)
           (namestring file)
         file)))))

(defun save-file-prompter (title filter &optional dont-ask pathname)
  (multiple-value-bind (file successp)
      (prompt-for-file title 
                       :pathname pathname
                       :filter filter
                       :operation :save
                       :if-exists (if dont-ask :ok :prompt)
                       :if-does-not-exist :ok)

    (when (and file successp)

      #+:linux
      (when (and (not dont-ask)
                 (probe-file file))
        (unless
            (prompt-for-confirmation 
             (format nil "Warning! File ~A exists! Overwrite?" file))
          (return-from save-file-prompter nil)))
          
      
      (substitute-backslashes-with-slashes
       (if (pathnamep file)
           (namestring file)
         file)))))

(defun substitute-backslashes-with-slashes (string)
  (substitute #\/ #\\ string))

(defun substitute-slashes-with-backslashes (string)
  (substitute #\\ #\/ string))


(define-interface my-progress-bar ()
  ((cancel-p :accessor cancel-p :initform nil))
  (:panes
   (progress-bar
    capi:progress-bar
    :width 300
    :start 0
    :end 100
    :title ""
    :title-position :frame
    )
   (cancel-button push-button 
                  :text "Cancel"
                  :font *tabs-font*
                  :background +button-bc+
                  :foreground +button-fc+
                  :enabled t
                  :font *button-font*
                  :callback-type :none
                  :callback #'(lambda ()
                                (with-profile-access
                                  (setf cancel-p t)
                                  (beep-pane)))))

  (:layouts

   (main-layout
    column-layout
    '(progress-bar cancel-button)
    :adjust :center))

  (:default-initargs

   :message-area nil 
   :auto-menus nil

   :title "Transmitting"
   :best-width 300))



