;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-
 
(in-package sirius)

;;;
;;;
;;; 

(defmacro => (a b)
  `(or (not ,a) ,b))

(defmacro <=> (a b)
  `(and (=> ,a ,b)
	(=> ,b ,a)))

;;;
;;;
;;; 

(defmacro with-sirius-app ((app) &rest body)
  `(let ((,app *sirius-app*))
     ,@body))

(defmacro with-sirius-slots (slots &rest body)
  `(with-sirius-app (app)
     (with-slots ,slots app
       ,@body)))

(defmacro setpc (left right)
  (let ((l (gensym))
        (r (gensym)))
    `(let ((,l ,left)
           (,r ,right))
       (unless (equalp ,l ,r)
         (setf ,left ,r)))))

(defmacro with-update-display-status (&body body)
  (let ((profile (gensym)))
    ;;; Display nicht entkoppelt?
    `(let ((,profile (active-profile)))
       (when (eq ,profile *interface-active-profile*)
         (unwind-protect 
             (progn 
               (incf (profile-updating-counter ,profile))
               ,@body)
           (decf (profile-updating-counter ,profile)))))))

(defmacro with-suspended-server-pane-update (&body body)
  (let ((var (gensym))
        (prof (gensym)))
    `(let* ((,prof (active-profile))
            (,var (dont-update-server-pane ,prof)))
       (unwind-protect
           (progn 
             (setf (dont-update-server-pane ,prof) t)
             ,@body)
         (setf (dont-update-server-pane ,prof) ,var)))))

(defmacro without-history-entries (&body body)
  `(let ((*create-history-entries* nil))
     ,@body))


;;;
;;;
;;; 

(defmacro with-auto-tab (&body body)
  `(progn 
     (let ((old-tab active-tab)
           (old-profile (active-profile)))
       (prog1
           (progn
             ,@body)
         (switch-to-old-tab old-profile old-tab)))))

;;;
;;;
;;;

(defmacro with-graph-drawing-process (pane &body body)
  `(let* ((ready nil)
          (button nil))

     #+:mac (declare (ignorable button))
     
     (apply-in-pane-process
      ,pane
      #'(lambda ()
          
          (let ((cancel-process nil)
                (pane-process mp:*current-process*)
                (profile (active-profile)))

            #+:mac (declare (ignorable cancel-process pane-process))

            (with-update-display-status
		
             (block exit
                     
               (progn 
                  
                 (setf (profile-graph-drawing-aborted profile) nil)
  
                 #-:mac
                 (setf cancel-process

                       (start-process-with-options
                       
                        "cancel"
           
                        (:priority 900000)
                       
                        (sleep +wait-for-cancel-button-sleep-time+)
                               
                        (unless ready
                          (contain 
                           (setf button
                                 (make-instance 
                                  'row-layout
                                  :description
                                  (list 
                                   (make-instance
                                    'push-button 
                                    :text (format nil "Cancel Graph Drawing")
                                    :selection-callback
                                    #'(lambda (&rest args) 
                                        (declare (ignore args))
                                              
                                        (setf cancel-process nil)
                                        (setf autofocus nil)
                                           
                                        (setf (profile-graph-drawing-aborted profile) t)

                                        (mp::process-interrupt 
                                         pane-process
                                         #'(lambda () 
                                             (return-from exit :exit))))))
                                       
                                  :adjust :center))

                           :x :center
                           :y :center
                                
                           :auto-menus nil
                           :toolbar nil
                           :title "Wait!"))))
                  
                 ,@(rest (assoc :process body))
                  
                 (setf ready t)))
             
             (unless ready

               #-:mac 
               (apply-in-pane-process button
                                      #'(lambda ()
                                          (quit-interface button)))

               #| (apply-in-pane-process button
                                         #'(lambda ()
                                             (setf (layout-description button)
                                                   (list 
                                                    (make-instance
                                                     'push-button 
                                                     :text (format nil "OK, Graph Drawing Aborted")
                                                     :selection-callback
                                                     #'(lambda (&rest args) 
                                                         (declare (ignore args))
                                                         (quit-interface button))))))) |# 

               ,@(rest (assoc :cancel body)))

             #-:mac
             (when (and ready button)
               (apply-in-pane-process button
                                      #'(lambda ()
                                          (quit-interface button))))

             #-:mac
             (when cancel-process
               (mp:process-kill cancel-process))))))))


;;;
;;;
;;; 

(defmacro background-command (query query2 res-form1 dont-cache-p)
  (let ((res (gensym))
        (backgrounded-p (gensym)))

    `(let* ((,res nil)
            (,backgrounded-p t)
            (*busy-marker* :busy-backgrounding)
            (old-profile (active-profile))
            (old-tab (slot-value old-profile 'active-tab)))
                                  
       (labels ((update (,res)
                  (when ,backgrounded-p
                    (with-synchronous-request
                     (without-callback-function
                      (when (switch-to-old-tab old-profile old-tab)
                        (sleep +wait-before-replace-current-shell-input-sleep-time2+)
                        (let ((*remove-after-cache-hit* ,dont-cache-p)
                              (*use-cache* t)
                              (*check-only-cache* t))
                          ,query2)))))
                       
                  (unless (error-res-p ,res)
                    (,res-form1 ,res))))

         (with-asynchronous-request
          (with-callback-function
           #'update
           (setf ,res 
                 (,(if dont-cache-p 
                       'without-caching
                     'progn)
                  (let ((*update-cache* t))
                    ,query)))
           (unless (eq ,res *busy-marker*)
             (setf ,backgrounded-p nil)
             (update ,res))))))))

(defmacro background-result-command (query &optional (res-form '(lambda (res) res)))
  `(background-command (enter-result-command ,query) 
                       (enter-result-command ,query)
                       ,res-form 
                       nil))

(defmacro background-result-command-nc (query &optional (res-form '(lambda (res) res)))
  `(background-command (enter-result-command ,query) 
                       (enter-result-command ,query)
                       ,res-form 
                       t))

(defmacro background-state-changing-result-command (query &optional (res-form '(lambda (res) res)))
  `(background-command (enter-state-changing-result-command ,query)
                       (enter-result-command ,query)
                       ,res-form 
                       t))

