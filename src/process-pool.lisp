;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defvar *process* t)

(defvar *pool-counter* 0)

(defvar *all-processes* nil)

(defvar *process-pool* nil)

(defparameter *min-pool-size* 10)

(defparameter *max-pool-size* 50)

;;;
;;;
;;;

(defmacro start-process (name &body body)  
  `(mp:process-run-function 
          
    ,name
           
    nil
          
    #'(lambda (*error-output*
               *terminal-io*
               *debug-io*
               *standard-output*
               *trace-output*)

        (handler-case 
            (progn 
              ,@body)
          (error (error)
            (mp:map-process-backtrace mp:*current-process* 'print)
            (error-message "Error in Process ~A: ~A" ',name error)
            #+:sirius-dev (error error))))
    
    *error-output*
    *terminal-io*
    *debug-io*
    *standard-output*
    *trace-output*))


(defmacro start-process-with-options (name options &body body)  
  `(mp:process-run-function 
          
    ,name
           
    ',options
          
    #'(lambda (*error-output*
               *terminal-io*
               *debug-io*
               *standard-output*
               *trace-output*)

        (handler-case 
            (progn 
              ,@body)
          (error (error)
            (mp:map-process-backtrace mp:*current-process* 'print)
            (error-message "Error in Process ~A: ~A"  ',name error)
            #+:sirius-dev (error error))))

    *error-output*
    *terminal-io*
    *debug-io*
    *standard-output*
    *trace-output*))

(defun kill-process (process)
  (mp:process-kill process))

(defun make-lock ()
  (mp:make-lock :important-p t))

(defun process-sleep (time)
  (mp:process-wait-with-timeout "wait" time))

(defun unlock-lock (lock)
  #+:lispworks4
  (mp:release-lock lock)
  #-:lispworks4
  (mp:process-unlock lock nil))

;;;
;;;
;;;

(defvar *processes* nil)

(defvar *process-lock* 
  (make-lock))

(defvar *queue-lock*
  (make-lock))

(defvar *capi-lock* 
  (make-lock))

;;;
;;;
;;;

(defmacro with-process-lock (&body body)
  ;;; wird nur hier verwendet (acquire/release/abort process) 
  ;;; traegt niemals zu Deadlocks bei! 
  `(progn 
     (mp:with-lock (*process-lock*)
       ,@body)))

(defmacro with-update-queue-synchronization (&body body)
  `(progn 
     (mp:with-lock (*queue-lock*)
       ,@body)))

(defmacro with-capi-synchronization (&body body)
  `(progn 
     (mp:with-lock (*capi-lock*)
       ,@body)))
                   
;;;
;;;
;;;

(defun unlock-process-lock ()
  (unlock-lock *process-lock*))

;;;
;;;
;;;

(defmacro process-wait (&rest body)
  `(mp:process-wait "waiting"
                    (lambda ()
                      ,@body)))

;;;
;;;
;;;

(defclass pooled-process ()
  ((process :accessor process :initarg :process)
   (terminate-p :accessor terminate-p :initform nil)
   (pprocess-function :accessor pprocess-function :initform nil
                      :initarg :pprocess-function)))

(defmethod initialize-instance :after ((process pooled-process) &rest initargs)
  (declare (ignorable initargs))
  (incf *pool-counter*)
  (push process *all-processes*))

(defun kill-all-processes ()
  (mapc #'(lambda (x)
            (when (process x)
              (kill-process (process x))))
        *all-processes*)
  (setf *pool-counter* 0
        *all-processes* nil
        *process-pool* nil))
  
(defun make-pooled-process (&optional (name "Pooled Process" ))
  (with-process-lock
    (let* ((pp (make-instance 'pooled-process))
           (p (start-process name
                (loop
                 ;; (princ 1)
                 (process-wait (pprocess-function pp))
                 ;; (princ 2)
                 (funcall (pprocess-function pp))
                 ;; (princ 3)
               
                 (release-process pp)))))
    
      (setf (process pp) p)
    
      pp)))
   
;;;
;;;
;;;
              
(defun acquire-process ()
  (with-process-lock  
    (labels ((try-to-acquire ()
               (when *process-pool*
                 (return-from acquire-process
                   (pop *process-pool*)))))

      (when (zerop *pool-counter*)
        (init-process-pool))

      (try-to-acquire)

      (if (or (not *max-pool-size*)
              (< *pool-counter* *max-pool-size*))
          (progn
            (push (make-pooled-process) *process-pool*)
            (try-to-acquire))

        nil))))

(defun release-process (pooled-process)
  (with-process-lock
    (setf (pprocess-function pooled-process) nil)
    (push pooled-process *process-pool*)))

;;;
;;;
;;;

(defmethod abort-process ((pooled-process pooled-process))
  (with-process-lock 
    (unless (mp::process-dead-p (process pooled-process))
      (mp:process-reset (process pooled-process))
      (release-process pooled-process))))


(defmethod abort-process-please ((pooled-process pooled-process))
  (setf (terminate-p pooled-process) t))

(defmethod abort-request-confirmed ((pooled-process pooled-process))
  (setf (terminate-p pooled-process) nil))

;;;
;;;
;;;

(defmacro start-pooled-process (name &rest body)
  `(let ((process (acquire-process)))
     (when process
       (setf (mp:process-name (process process))
             ,name)
       
       (setf (pprocess-function process)
             #'(lambda ()
                 (let ((*process* process)
                       (*active-profile* (active-profile)))
                   ,@body))))
     process))

;;;
;;;
;;;

(defun init-process-pool ()
  (kill-all-processes)
  (setf *process-pool* 
        (loop as  i from 1 to *min-pool-size* 
              collect (make-pooled-process))))

(defun kill-pooled-process (pp)
  (with-process-lock
    (let ((name  (mp:process-name (process pp))))
      (mp:process-kill (process pp))
      (let* ((p (start-process name
                  (loop
                   ;; (princ 1)
                   (process-wait (pprocess-function pp))
                   ;; (princ 2)
                   (funcall (pprocess-function pp))
                   ;; (princ 3)
                
                   (release-process pp)))))
    
        (setf (process pp) p)
     
        pp))))

