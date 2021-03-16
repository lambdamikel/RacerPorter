;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-
 
(in-package sirius)

;;;
;;;
;;; 

(defconstant +answer-marker+ :answer)

(defconstant +ok-marker+ :ok)

(defconstant +error-marker+ :error)

;;;
;;;
;;;

(defclass list-character-input-stream (stream:fundamental-character-stream)
  ((characters :accessor characters :initarg :characters)))

(defmethod read-string-from-stream  ((x list-character-input-stream))
  (with-slots (characters) x
    
    (let* ((pos (position #\" characters))
           (start (1+ pos))
           (end nil))

      (loop while (not end) do
            (let ((pos2 (position #\" characters :start start)))
              (if (char= (elt characters (1- pos2)) #\\)
                  (incf start)
                (setf end pos2))))

      (handler-case
          (if (< (1+ pos) (1- end))
              (coerce (subseq characters 
                              (1+ pos) (1- end))
                      'string)
            "")
        (error ()
          (coerce (nconc
                   (subseq characters 
                           (1+ pos)
                           (min 4000000 (1- end)))
                   '(#\space #\. #\. #\.))
                  'string))))))
                       

(defun substitute-newlines (string)
  (let* ((pos1 (search "\\N" string))
         (pos2 (search "\\S" string))
         (pos (if pos1
                  (if pos2
                      (min pos1 pos2)
                    pos1)
                pos2)))      
    (if pos
        (concatenate 'string 
                     (subseq string 0 pos)
                     (if (and pos2 (= pos pos2) )
                         "\""
                       +newline-string+)
                     (substitute-newlines (subseq string (+ 2 pos))))
      string)))

(defmethod stream:stream-read-char ((x list-character-input-stream)) 
  (with-slots (characters) x

    (let ((char (pop characters)))

      (cond ((not char)
             :eof)

            ((symbolp char)
             (ecase char
               (:doublequote1 
                (push :doublequote2 characters)
                #\\)
               (:doublequote2
                #\S)
               (:newline1 
                (push :newline2 characters)
                #\\)
               (:newline2
                #\N)))

            ((characterp char)
             (cond ((char= char #\\)
                    (let ((next-ch (first characters)))
                      (case next-ch
                        (#\" 
                         (pop characters)
                         #\")
                       
                        (#\S
                         (pop characters)
                         (push :doublequote1 characters)                         
                         #\\)
                     
                        (#\N 
                         (pop characters)
                         (push :newline1 characters)
                         #\\)
                     
                        (#\\ 
                         (pop characters)
                         (push #\\ characters)
                         #\\)

                        (#\| 
                         (pop characters)
                         #\|)
                       
                        (otherwise #\\))))
                   (t char)))))))


(defmethod stream:stream-unread-char ((x list-character-input-stream) char) 
  (with-slots (characters) x
    (push char characters)
    nil))

;;;
;;;
;;;

#|
(defun substitute-newlines (string)
  (let ((chars (coerce string 'list))
        (res nil))

    (dolist (char chars)
      (case char
        (#\newline 
         (push #\\ res)
         (push #\N res)
         ;(push #\tab res)
         )
        (otherwise
         (push char res))))

    (coerce (nreverse res) 'string)))
|#


;;;
;;;
;;; 

(defvar *wait-for-n-seconds-for-racer-answer-before-return* 3)

(defvar *blocking-service-request* t)

(defvar *use-cache* t)

(defvar *update-cache* t)

(defvar *remove-after-cache-hit* nil)

(defvar *busy-marker* nil)

(defvar *check-only-cache* nil)

(defvar *report-connection-errors* t)

;;;
;;;
;;; 

(defmacro without-caching (&body body)
  `(let ((*use-cache* nil)
         (*update-cache* nil)
         (*check-only-cache* nil))
     ,@body))

(defmacro with-only-cache-lookup (&body body)
  `(let ((*check-only-cache* t)
         (*use-cache* t))
     ,@body))

;;;
;;;
;;; 

(defmacro with-synchronous-request (&body body)
  `(let ((*blocking-service-request* t))
     ,@body))

(defmacro with-asynchronous-request (&body body)
  `(let ((*blocking-service-request* nil))
     ,@body))


;;;
;;; Low Level Stuff 
;;; 

(defun open-socket (host port)
  (handler-case 
      (comm:open-tcp-stream host port :timeout (profile-open-connection-timeout (active-profile)))
    (error (condition)
      (return-from open-socket nil))))

(defun close-socket (socket)
  (handler-case 
      (with-sirius-timeout ((profile-close-connection-timeout (active-profile)) :timeout)
        (close socket))
    (error (error) 
           ;socket schon "kaputt"
      ))
  t)

(defun socket-alive-p (socket)
  (and socket
       ;;; lebt noch? 
       (or (eq socket t)
           (output-stream-p socket))))

;;;
;;;
;;;

(defmacro racer-function ((name &rest args) &optional (error-p t) error-marker)
  `(when (profile-socket (active-profile))
     (let ((*package* (find-package :racer-user))) ; notwendig! 
       (handler-case 
           (service-request
            (if *display-error*
                (create-racer-call-string (list ',name ,@args))
              (concatenate `string
                           "(without-progress "
                           (create-racer-call-string (list ',name ,@args))
                           ")")))
         (error (error)
           ;;; dieser Handler kann nur aktiv werden, 
           ;;; wenn kein Prozess gestartet wurde
           (if ,error-p 
               (report-error error)
             ,error-marker))))))

(defmacro racer-function1 ((name &rest args) &optional (error-marker :error))
  `(let ((*display-error* nil)) 
     (racer-function (,name ,@args) nil ,error-marker)))

(defun racer-function-evaluated (call &optional (error-p t) error-marker)
  (let ((name (first call))
        (args (rest call)))
    (when (profile-socket (active-profile))
      (let ((*package* (find-package :racer-user))) ; notwendig! 
        (handler-case 
            (progn
              (service-request
               (create-racer-call-string (cons name args))))
          (error (error)
            ;;; dieser Handler kann nur aktiv werden, 
            ;;; wenn kein Prozess gestartet wurde
            (if error-p 
                (report-error error)
              error-marker)))))))

;;;
;;;
;;;

(defun open-server-connection (&optional (profile (active-profile)))
  (reset-profile profile :before-open-server-connection-p t)

  (with-profile (profile)
    (with-synchronous-request
    
      (if (uses-internal-racer-p profile)
          
          (progn 

            ;;; RacerPlus 

            (setf (profile-case profile) 
                  :upcase)
            (setf (profile-alisp-racer profile) t)

            (setf (profile-socket profile) t
                  (profile-progress-socket profile) nil
                  (profile-connection-died profile) nil)
              
            (let ((racer-version 
                   (racer-function1 (get-racer-version) nil)))

              (setf (profile-racer-version profile) racer-version))
              
            (return-from open-server-connection t))


        (progn 

          ;;; RacerPro 

          (when (connected-and-socket-alive-p)
            (return-from open-server-connection t))

          (let ((socket
                 (open-socket (profile-host profile)
                              (read-from-string (profile-port profile))))
                (progress-socket 
                 (open-socket (profile-host profile)
                              (read-from-string (profile-control-port profile)))))
            
            (setf (profile-socket profile) socket
                  (profile-progress-socket profile) progress-socket
                  (profile-connection-died profile) nil)

            (unless (connected-and-socket-alive-p)
              (return-from open-server-connection nil))

            (with-sirius-timeout ( (profile-acquire-server-info-timeout (active-profile)) 
                                   (let ((*display-error* t)) 
                                     (close-server-connection)
                                     (return-from open-server-connection nil)))

              ;;; sende als (CURRENT-ABOX)... 

              (let ((racer-version 
                     (or 
                      (progn 
                        (setf (profile-alisp-racer profile) t)
                        (racer-function1 (get-racer-version) nil))
                      
                      (progn 
                        (setf (profile-alisp-racer profile) nil)
                        (racer-function1 (get-racer-version) nil)))))

                (unless racer-version 
                  (return-from open-server-connection nil))

                (setf (profile-racer-version profile) racer-version)

                (cond ((string-lessp racer-version "1.9.3")
                       (close-server-connection)
                       (report-error (format nil "Sorry, this version of RacerPorter doesn't support RacerPro ~A (at least 1.9.3 is required)"
                                             racer-version)))

                      #| ((string= racer-version "1.9.1")                     
                          (setf (profile-alisp-racer profile) t))  
                    
                      ((or (string= racer-version "1.9.2")                     
                           (string= racer-version "1.9.3"))
                       (setf (profile-alisp-racer profile) nil)) |# 

                      (t (setf (profile-alisp-racer profile) nil)))

                (let ((case (or (racer-function1 (server-case) nil)
                                :upcase)))

                  (setf (profile-case profile) 
                        case)

                  (when (eq case :upcase)
                    (setf (profile-alisp-racer profile) t))
                  
                  (setf (profile-unsafe profile)     
                        (if (uses-internal-racer-p profile)
                            t
                          (let ((res (racer-function1 (in-unsafe-mode?))))
                            (if (eq res :error)
                                :dont-know
                              res))))))))))))                      
  t)

(defun close-server-connection (&optional (profile (active-profile)))
  (with-profile (profile)
    (if (uses-internal-racer-p profile)
        (setf (profile-socket profile) nil
              (profile-progress-socket profile) nil)
              
      (progn
        (close-socket (profile-socket profile))
        (close-socket (profile-progress-socket profile))))

    (reset-profile profile :after-close-server-connection-p t)

    (set-progress-bar-to 0)

    t))

(defun close-all-connections ()
  (mapc #'close-server-connection *sirius-profiles*))

;;;
;;;
;;;

(defun connection-was-established-p  (&optional (profile (active-profile)))
  (and profile
       ;;; evtl. t bei internal-racer 
       (profile-socket profile)))

(defun connected-and-socket-alive-p (&optional (profile (active-profile)))
  (and profile 
       (connection-was-established-p profile) 
       (not (profile-connection-died profile))
       (socket-alive-p (profile-socket profile))))

(defun racer-ready-p (&optional (profile (active-profile)))
  (and profile 
       (connected-and-socket-alive-p profile)
       (eq (profile-current-command-status profile) :ready)))

(defun racer-busy-p (&optional (profile (active-profile)))
  (and profile 
       (connected-and-socket-alive-p profile)
       (eq (profile-current-command-status profile) :busy))) 

(defun ping-racer-p (&optional (profile (active-profile)))
  (and profile 
       (connected-and-socket-alive-p profile)
       (or (uses-internal-racer-p profile)
           (with-synchronous-request
             (without-caching
               (handler-case 
                   (with-profile (profile)
                     (let ((*display-error* nil)) 
                       (service-request "(current-abox)")
                       t))
                 (error () nil)))))))


(defun connection-died-p (&optional (profile (active-profile)))
  (and profile 
       (connection-was-established-p profile)
       (or (profile-connection-died profile)
           (unless (connected-and-socket-alive-p profile)
             (setf (profile-connection-died profile) t)
             t))))

;;;
;;;
;;; 

(defun profile-status (&optional (profile (active-profile)))
  (cond ((racer-busy-p profile) :busy)
        ((racer-ready-p profile) :ready)
        ((connection-died-p profile) :died)
        (t :not-connected)))

;;;
;;; aus LRacer 
;;;    

(defmacro with-standard-io-syntax-1 (&body body)
  (let ((package-sym (gensym)))
    `(let ((,package-sym *package*))
       (with-standard-io-syntax 
         (let ((*package* ,package-sym))
           .,body)))))


(defvar *message* nil)

(defvar *messages* nil)

(defvar *callback-fn* nil)

(defvar *running* nil)

(defmacro with-callback-function (fn &rest body)
  `(let ((*callback-fn* ,fn))
     ,@body))

(defmacro without-callback-function (&rest body)
  `(let ((*callback-fn* nil))
     ,@body))

(defun service-request (message)
  
  (let ((active-profile (active-profile)))

    (cond ((connected-and-socket-alive-p active-profile)

           (let* ((request-counter (incf (profile-request-counter active-profile)))
                  (server (get-server-description active-profile))
                  (exit-server-command-p (let ((res (search "(exit-server)" message)))
                                           (and res (zerop res))))
                  (request-type (if *blocking-service-request* 
                                    "S"
                                  "A"))
                  (request-message (format nil "(~A) REQUEST ~A FOR ~A:
     ~S"
                                           request-type
                                           request-counter
                                           server
                                           message))
                  (answer-message (format nil "(~A) ANSWER ~A FROM ~A:
     ~S"
                                          request-type
                                          request-counter
                                          server
                                          message)))

             (declare (ignorable process))
             
             (setf (profile-current-cache-hit active-profile) nil)
                         
             (multiple-value-bind (cached-answer present-p)

                 (when *use-cache*
                   (gethash message (profile-answer-hash active-profile)))
      
               (cond (present-p 

                      (setf (profile-current-cache-hit active-profile) (list request-counter message))
                      (append-to-log (format nil "~A -->~%" request-message))
                      (append-to-log (format nil "     CACHE HIT~%") nil)

                      (when *remove-after-cache-hit*
                        (remhash message (profile-answer-hash active-profile)))
                      
                      (values (first cached-answer)
                              (second cached-answer)))

                     (t 
                 
                      (when (and *use-cache* *check-only-cache*)
                        (return-from service-request nil))

                      (cond ((and (not (uses-internal-racer-p active-profile))
                                  (null (profile-socket active-profile)))
                             ;; (display-message (format nil "~S" message))

                             (no-connection-error))
                       
                            (t                                                    

                             (let ((package *package*)
                                   (busy-marker *busy-marker*)
                                   (callback-fn *callback-fn*)

                                   (use-cache *use-cache*)
                                   (update-cache *update-cache*)
                                   (check-only-cache *check-only-cache*)
                                   (remove-after-cache-hit *remove-after-cache-hit*)                                   

                                   (blocking-service-request *blocking-service-request* )                  

                                   (lracer-readtable *lracer-readtable*)
                                   (readtable *readtable*)
                                   (answer nil)
                                   (answer-confirmed nil)
                                   (parent-process mp:*current-process*))

                               (declare (ignorable parent-process))

                               ;;; Prozess-Koerper

                               (labels (

                                        #-:racer-with-sirius
                                        (do-it () 
                                          
                                          (let ((*package* package)
                                                (*busy-marker* busy-marker)
                                                (*display-error* nil)
                                                (*active-profile* active-profile)
                                                (*lracer-readtable* lracer-readtable)
                                                (*readtable* readtable)
                                                (*print-pretty* nil)
                                                (*callback-fn* callback-fn)

                                                (*use-cache* use-cache)
                                                (*update-cache* update-cache)
                                                (*check-only-cache* check-only-cache)
                                                (*remove-after-cache-hit* remove-after-cache-hit)

                                                (*blocking-service-request* blocking-service-request)
                                                
                                                (res nil))

                                            (setf (profile-current-command-status active-profile) :busy)
                                            (setf *running* message)
                                         
                                            (setf (profile-processing-command active-profile) 
                                                  (list request-counter message))

                                            (labels ((exit-do-it (&optional (release-p t))

                                                       (setf *running* nil)

                                                       (when release-p
                                                         (setf (profile-current-command-status active-profile)
                                                               :ready)

                                                         (setf (profile-current-communication-status active-profile) 
                                                               :ready))

                                                       (setf answer res)

                                                       (when (and (consp res)
                                                                  (consp (first res))
                                                                  (member (caar res)
                                                                          '(:display-request
                                                                            :|display-request|)))
                                                         (sirius-message "~A" 
									 (substitute-newlines 
									  (second (first res)))))

                                                       (process-wait answer-confirmed)
                                                       
                                                       (when callback-fn 
                                                         (funcall callback-fn (first res)))

                                                       (return-from do-it res)))
                                       
                                              (cond ((connection-died-p active-profile)
                                            
                                                     (append-to-log (format nil "~A -->~%" request-message))
                                                     (append-to-log (format nil "     ERROR, CONNECTION ALREADY DEAD!~%") nil)
                                            
                                                     ;;; KEIN Error, denn ein Error reicht (irgendein anderer Prozess
                                                     ;;; hat den Fehler bereits gemeldet, befor er connection-died-p 
                                                     ;;; setzte!
                                              
                                                     (setf (profile-current-communication-status active-profile) 
                                                           :connection-already-dead)

                                                     (setf res 
                                                           (multiple-value-call #'list
                                                             (return-error-answer "Connection already dead!")))

                                                     (setf callback-fn nil
                                                           answer-confirmed t)

                                                     (exit-do-it nil))

                                                    (t
                                            
                                                     (with-standard-io-syntax-1

                                                       (with-socket-lock
                                          
                                                         (setf (profile-current-communication-status active-profile) 
                                                               :sending-request)

                                                         (append-to-log (format nil "~A~%" request-message))

                                                         (handler-case 
                                                             (let* ((*package* (find-package :racer-user))
                                                                    ;;(message (substitute-newlines message))
                                                                    )
                                                               ;;(format (profile-socket active-profile) "~A~%" 
                                                               ;;        message)
                                                               (write-string message (profile-socket active-profile))
                                                               (terpri (profile-socket active-profile))
                                                               (force-output (profile-socket active-profile)))
                                     
                                                           (error (error) 
                                                             ;;; Socket Died? 

                                                             (setf (profile-connection-died active-profile) t)
                                                       
                                                             (append-to-log
                                                              (format nil "~A -->~%" request-message))
                                                             (append-to-log 
                                                              (format nil "     ERROR, CONNECTION DIED DURING SEND!~%") 
                                                              nil)
                                                       
                                                             (setf (profile-current-communication-status active-profile)
                                                                   :connection-died-during-send)
                                                                  
                                                             (setf res 
                                                                   (multiple-value-call #'list
                                                                     (return-error-answer "Connection died during send!")))
                                                       
                                                             (setf callback-fn nil
                                                                   answer-confirmed t)

                                                             (exit-do-it nil)))
                                                       
                                                         (multiple-value-bind (res2 stdout error-p chars truncated-p)
                                                             (handler-case 
                                                                 (progn

                                                                   (setf (profile-current-communication-status active-profile)
                                                                         :getting-racer-result)

                                                                   (let* ((*package* (find-package :racer-user))
                                                                          (stream (read-line-1
                                                                                   (profile-socket active-profile))))

                                                                     (cond (stream 

                                                                            (setf (profile-current-communication-status active-profile)
                                                                                  :parsing-racer-result)
                                                                       
                                                                            (let ((*report-connection-errors* 
                                                                                   (not exit-server-command-p)))
                                                                         
                                                                              (parse-racer-reply stream)))

                                                                           (t (unless exit-server-command-p 

                                                                                (setf (profile-connection-died active-profile) t)
                                                       
                                                                                (append-to-log
                                                                                 (format nil "~A -->~%" request-message))
                                                                                (append-to-log
                                                                                 (format nil "     ERROR, CONNECTION DIED DURING READ (NO TERMINATING LINEFEED FOUND)!~%") 
                                                                                 nil)
                                                                              
                                                                                (setf (profile-current-communication-status active-profile)
                                                                                      :connection-died-during-read)
                                                                              
                                                                                (setf res
                                                                                      (multiple-value-call #'list
                                                                                        (return-error-answer "Connection died during read!"))))

                                                                              (setf callback-fn nil
                                                                                    answer-confirmed t)
                                                                            
                                                                              ;;; wichtig! hier wird der Handler nicht aktiv!
                                                                              ;;;  (return-error-answer 
                                                                              (exit-do-it nil)))))

                                                               (error (error) 
                                                                      
                                                                 (setf (profile-current-communication-status active-profile)
                                                                       :error)
                                                                        
                                                                 (append-to-log (format nil "~A -->~%" request-message))
                                                                 (append-to-log (format nil "     ERROR ~A~%" error) nil)
                                                                   
                                                                 (setf res 
                                                                       (multiple-value-call #'list
                                                                         (return-error-answer (format nil "~A" error))))
                                                                 
                                                                 (cond ((not *blocking-service-request*)

                                                                        ;;; auch Error answers muessen in den Cache, sonst
                                                                        ;;; kann kein asynchrones Refresh gemacht werden
                                                                        
                                                                        (when *update-cache*
                                                                          (setf (gethash message
                                                                                         (profile-answer-hash active-profile)) 
                                                                                res))

                                                                        (setf (third res) :no-error-message) 
                                                                        
                                                                        ;;; wird sonst doppelt geliefert
                                                                        ;;; es ist notwendig, dass die Error message hier
                                                                        ;;; aus dem Prozess geliefert wird, sonst gehen 
                                                                        ;;; Error messages verloren

                                                                        ;;; der Prozess wird benötigt, damit nicht
                                                                        ;;; "timeout / busy backgrounding" zurück kommt
                                                                        ;;; (der Nutzer braucht eine Zeit, um den Dialog zu
                                                                        ;;; bestaetigen) 
                                                                          
                                                                        (start-process "error display"
                                                                          (let ((*display-error* t))
                                                                            (report-error error)))

                                                                        (exit-do-it nil))
                                                                       
                                                                       (t
                                                                        
                                                                        (setf callback-fn nil
                                                                              answer-confirmed t)
                                                                           
                                                                        (exit-do-it nil)))))

                                                           (setf res (list res2 stdout error-p))
                                                          
                                                           (setf (profile-current-communication-status  active-profile) :good)
                                                         
                                                           (append-to-log (format nil "~A -->~%" answer-message))
                                                           
                                                           (let ((string 
                                                                  (coerce chars 'string)))
                                                             (if truncated-p
                                                                 (append-to-log
                                                                  (format nil "     OKAY [~S... (truncated)]~%" string)
                                                                  nil)
                                                               (append-to-log
                                                                (format nil "     OKAY [~S]~%" string)
                                                                nil)))

                                                           (when *update-cache*
                                                             (setf (gethash message (profile-answer-hash active-profile)) 
                                                                   res))

                                                           (exit-do-it)))))))))

                                        #+:racer-with-sirius
                                        (do-it () 
                                          
                                          (let ((*package* package)
                                                (*busy-marker* busy-marker)
                                                (*display-error* nil)
                                                (*active-profile* active-profile)
                                                (*lracer-readtable* lracer-readtable)
                                                (*readtable* readtable)
                                                (*print-pretty* nil)
                                                (*callback-fn* callback-fn)

                                                (*use-cache* use-cache)
                                                (*update-cache* update-cache)
                                                (*check-only-cache* check-only-cache)
                                                (*remove-after-cache-hit* remove-after-cache-hit)

                                                (*blocking-service-request* blocking-service-request)
                                                
                                                (res nil))

                                            (setf (profile-current-command-status active-profile) :busy)
                                            (setf *running* message)
                                         
                                            (setf (profile-processing-command active-profile) 
                                                  (list request-counter message))

                                            (labels ((exit-do-it (&optional (release-p t))

                                                       (setf *running* nil)

                                                       (when release-p
                                                         (setf (profile-current-command-status active-profile)
                                                               :ready)

                                                         (setf (profile-current-communication-status active-profile) 
                                                               :ready))

                                                       (setf answer res)

                                                       (process-wait answer-confirmed)
                                                       
                                                       (when callback-fn 
                                                         (funcall callback-fn (first res)))

                                                       (return-from do-it res)))
                                       
                                              (with-standard-io-syntax-1
                                                (setf (profile-current-communication-status active-profile) 
                                                      :sending-request)

                                                (append-to-log (format nil "~A~%" request-message))
                                              
                                                (multiple-value-bind (res2 stdout error-p chars truncated-p)
                                                    (handler-case 
                                                        (progn

                                                          (setf (profile-current-communication-status active-profile)
                                                                :getting-racer-result)

                                                          (setf (profile-current-communication-status active-profile)
                                                                :parsing-racer-result)
                                                                       
                                                          (let* ((*report-connection-errors* 
                                                                  (not exit-server-command-p))

                                                                 (res-string nil)
                                                                 (stream nil))

                                                            (setf res-string
                                                                  (with-output-to-string (res-string) 
                                                                    (with-output-to-string (stdout-string) 
                                                                      (let ((*standard-output* stdout-string))
                                                                        (cl-user::process-racer-string 
                                                                         res-string
                                                                         message
                                                                         nil
                                                                         (find-package :racer-user)
                                                                         stdout-string)))))

                                                            (setf stream
                                                                  (make-instance 'list-character-input-stream
                                                                                 :characters 
                                                                                 (coerce res-string 'list)))
                                                          
                                                            (parse-racer-reply stream)))

                                                      (error (error) 
                                                        
                                                        (setf (profile-current-communication-status active-profile)
                                                              :error)
                                                                        
                                                        (append-to-log (format nil "~A -->~%" request-message))
                                                        (append-to-log (format nil "     ERROR ~A~%" error) nil)
                                                                   
                                                        (setf res 
                                                              (multiple-value-call #'list
                                                                (return-error-answer (format nil "~A" error))))
                                                                 
                                                        (cond ((not *blocking-service-request*)

                                                               ;;; auch Error answers muessen in den Cache, sonst
                                                               ;;; kann kein asynchrones Refresh gemacht werden
                                                                        
                                                               (when *update-cache*
                                                                 (setf (gethash message
                                                                                (profile-answer-hash active-profile)) 
                                                                       res))

                                                               (setf (third res) :no-error-message) 
                                                                        
                                                               ;;; wird sonst doppelt geliefert
                                                               ;;; es ist notwendig, dass die Error message hier
                                                               ;;; aus dem Prozess geliefert wird, sonst gehen 
                                                               ;;; Error messages verloren

                                                               ;;; der Prozess wird benötigt, damit nicht
                                                               ;;; "timeout / busy backgrounding" zurück kommt
                                                               ;;; (der Nutzer braucht eine Zeit, um den Dialog zu
                                                               ;;; bestaetigen) 
                                                                          
                                                               (start-process "error display"
                                                                 (let ((*display-error* t))
                                                                   (report-error error)))

                                                               (exit-do-it nil))
                                                                       
                                                              (t
                                                                        
                                                               (setf callback-fn nil
                                                                     answer-confirmed t)
                                                                           
                                                               (exit-do-it nil)))))

                                                  (setf res (list res2 stdout error-p))
                                                          
                                                  (setf (profile-current-communication-status  active-profile) :good)
                                                         
                                                  (append-to-log (format nil "~A -->~%" answer-message))

  
                                                  (let ((string 
                                                         (coerce chars 'string)))
                                                    (if truncated-p
                                                        (append-to-log
                                                         (format nil "     OKAY [~S... (truncated)]~%" string)
                                                         nil)
                                                      (append-to-log
                                                       (format nil "     OKAY [~S]~%" string)
                                                       nil)))
                                                  
                                                  (when *update-cache*
                                                    (setf (gethash message (profile-answer-hash active-profile)) 
                                                          res))

                                                  (exit-do-it)))))))
                                 
                                 ;;; neuen Prozess fuer Anfrage starten! 
                                 ;;; natuerlich darf aus diesem Prozess nicht auf 
                                 ;;; CAPI etc. zugegriffen werden!
                                 
                                 (cond ((not *blocking-service-request*)
                                        (or (start-pooled-process 
                                             "connection process"
                                             (with-profile-request
                                               (pushnew *process* (profile-evaluation-process (active-profile)))
                                               (do-it)))
                                            
                                            (let ((*display-error* t))
                                              (report-error 
                                               (format nil "Process Pool exceeded! Request ignored." )))))

                                       (t
                                        (with-profile-request
                                          (progn ; with-socket-lock
                                            (setf callback-fn nil
                                                  answer-confirmed t)
                                            (do-it)))))

                                 ;;; max. *wait-n-seconds-for-racer-answer-before-return* auf Racer-Antwort warten, 
                                 ;;; dann mit ":busy" zurueckkehren

                                 (labels ((return-answer-or-busy-marker ()

                                            (if answer
                                             
                                                (if (third answer)
                                                    (if (eq (third answer) :no-error-message)
                                                        (return-error-answer (second (first answer)))
                                                      (report-error (second (first answer))))

                                                  (if exit-server-command-p
                                                      (progn
                                                        (sleep +exit-server-command-before-close-connection-sleep-time+)
                                                        (close-server-connection))
                                             
                                                    (values (first answer)
                                                            (second answer)
                                                            (third answer))))

                                              *busy-marker*)))

                                   (cond (answer

                                          (return-answer-or-busy-marker))
                                    
                                         (t 
                                     
                                          (if (and 

                                               *wait-for-n-seconds-for-racer-answer-before-return* 
                                               
                                               (eq (with-sirius-timeout
                                                       
                                                       (*wait-for-n-seconds-for-racer-answer-before-return* 
                                                        :sirius-timeout)
                                                     ;;; (setf answer ... ) vom Prozess warten, 
                                                     ;;; wird als letzte Operation im Prozess ausgeführt 
                                                
                                                     (unless answer
                                                       (process-wait answer))

                                                     (setf callback-fn nil
                                                           answer-confirmed t)
                                                  
                                                     :done)

                                                   :sirius-timeout))

                                              (progn

                                                ;;; timeout occured? 
                                                
                                                (labels ((quit-button (&rest args) 
                                                           (declare (ignore args))
                                                           (quit-interface *message*)
                                                           (setf *message* nil)))
                                      
                                                  (unless t ; *message* 
                                                    (setf *message* 
                                                          (contain 
                                                           (make-instance 
                                                            'push-button 
                                                            :text (format nil "Please be patient, ~A is busy!"
                                                                          (get-server-description active-profile))
                                                            :selection-callback #'quit-button)
                                                           :destroy-callback #'quit-button
                                                           :x :center
                                                           :y :center
                                                           :auto-menus nil :title "Please Wait"))))

                                                (append-to-log (format nil "~A -->~%" request-message))
                                                (append-to-log (format nil "     BUSY, BACKGROUNDING, RETURNING ~A~%"
                                                                       *busy-marker*)
                                                               nil)

                                                (setf answer-confirmed t)
                                    
                                                (return-answer-or-busy-marker))

                                            (progn 
                                              (unless answer 
                                                (error "Something is wrong, sorry. Please try to disconnect and reconnect."))
                                              (return-answer-or-busy-marker)))))))))))))))

          (t 

           ;; (display-message (format nil "~S" message))
           
           (no-connection-error)
           
           ))))

;;;
;;; Unmarshalling 
;;; 

;; (setf *x* nil)
;; (setf *y* nil)

(defconstant +standard-protocol-keywords+ 
  (mapcar #'(lambda (x) 
              (coerce x 'list))
          ;; SPACE am Ende erforderlich! s. Vergleich!
          '(":answer "
            ":ok "
            ":abort "
            ":error ")))
          

(defmethod parse-racer-reply ((stream list-character-input-stream))

  (let ((*readtable* *lracer-readtable*)
        (old-case (readtable-case *readtable*))
        (chars1 (characters stream))
        (chars nil)
        (truncated-p nil)
        (simplified-p nil))
        
    (let ((count 0)
          (pointer nil))

      (loop while chars1 do
            (let ((entry (list (pop chars1))))
              (if pointer
                  (setf (cdr pointer) entry)
                (setf chars entry))
              (setf pointer entry)
              (incf count)
              #-:no-truncated-log-files 
              (when (= 100 count)
                (setf truncated-p t)
                (setf chars1 nil)))))

    (setf simplified-p
          ;; alles was mit ":answer ", ":error ", ":ok " anfängt, ist nicht simplified....
          (or (not chars)
              (let ((l (length chars)))
                (every #'(lambda (protocol-keyword)
                           (or (< l (length protocol-keyword))
                               (some #'(lambda (char1 char2)
                                         (not (char= char1 char2)))
                                     protocol-keyword chars)))
                       +standard-protocol-keywords+))))


    (if (not chars) ; kommt z.B. bei (define-prefix ...) vor, wenn simplified / smart protocol! 

        :okay

      (progn 
        
        ;; (setf *x* (characters stream))
        ;; (pprint *x*)
        ;; (pprint simplified-p)

        (unwind-protect
            (progn
          
              (setf (readtable-case *readtable*) :preserve)
         
              (cond (simplified-p 
                 
                     (let ((res nil))
                       (handler-case 
                           (setf res
                                 (transform-result
                                  (read-from-string-preserving-case
                                   (coerce (characters stream)
                                           'string))))
                         (error (error) 
                           (close-connection)
                           (report-error "Sorry, couldn't parse Racer reply. Closing connection, please recover by reconnecting by hand.")))

                       (cond ((member res '(:error :|error|))
                              (let ((message 
                                     (if t ;*recursive-service-request-p* 
                                         "Some Error Occured"
                                       (ignore-errors
                                         (let ((*recursive-service-request-p* t))
                                           (racer-function1 
                                            `(|OWLAPI-getLastAnswer| ,(profile-current-reasoner (active-profile)))
                                            nil))))))

                                (report-error message)))

                             ((member res '(:abort :|abort|))
                              (let ((message "Request aborted - please re-connect"))
                                (declare (ignorable message))

                                (setf (profile-current-communication-status (active-profile))
                                      :error)
                                (report-error message)))

                             ((member res '(:void :|void|))
                              (values res
                                      nil
                                      nil ; kein Error
                                      chars
                                      truncated-p))

                             (t
                              (values res
                                      nil
                                      nil ; kein Error
                                      chars
                                      truncated-p)))))
                
                    (t 

                     (let ((marker
                            (intern (string-upcase 
                                     (symbol-name (read stream)))
                                    :keyword)))

                       (cond ((eq marker +answer-marker+)

                              (handler-case 
                       
                                  (let* ((number (read stream))
                                         (char1 (read-char stream)) ; einfassende Anfuerhungszeichen konsumieren! 
                                         (result (read stream))
                                         (char2 (read-char stream))
                          
                                         (stdout
                                          (nsubstitute
                                           #\Newline #\Tab
                                           ;;; essentielle Optimierung gegenüber read!
                                           (read-string-from-stream stream)))

                                         (result1
                                          (transform-result result)))
                       
                                    (declare (ignorable number char1 char2))

                                    (when (profile-maintain-arguments (active-profile))
                                      (dolist (arg (ensure-list result1))
                                        (when (symbolp arg)
                                          (let ((arg (format nil "~S" arg)))
                                            (register-potential-arg arg arg)
                                            #|
                             (let ((arg (remove-url-prefixes arg)))
                               (unless (string= arg "")
                                 (register-potential-arg arg))) |# ))))

                                    (values result1
                                            stdout
                                            nil ; kein Error
                                            chars
                                            truncated-p))

                                (error (error) 

                                  (close-connection)
                                  (report-error "Sorry, couldn't parse Racer reply. Closing connection, please recover by reconnecting by hand."))))

                             ((eq marker +ok-marker+) 
           
                              (let ((number (read stream))
                                    (stdout 
                                     (nsubstitute
                                      #\Newline #\Tab
                                      (read stream))))

                                (declare (ignorable number))
      
                                (values :okay stdout)))
                         
                             ((eq marker +error-marker+)
                          
                              (let ((number (read stream))
                                    (message 
                                     (nsubstitute
                                      #\Newline #\Tab

                                      (handler-case 
                                          (coerce (characters stream) 'string)

                                        (error (error) 
                                          "Error")))))
                   
                                (declare (ignorable number))
                   
                                (let ((stdout (position #\" message
                                                        :end (max 0 (1- (length message)))
                                                        :from-end t)))
               
                                  (let* ((message
                                          (subseq message 0 stdout))
                                         (message-end
                                          (or (1+ (position-if-not #'lw:whitespace-char-p message :from-end t))
                                              (length message)))
                                         (message (subseq message 0 message-end)))

                                    (report-error message)))))

                             (t (if *report-connection-errors* 
                                    (progn 
                                      ;; (display-message (format nil "~S" 'xxx))
                                      (no-connection-error))
                                  :okay)))))))
          
          (setf (readtable-case *readtable*) old-case))))))


;;;
;;;
;;;    

(defun read-line-1 (input-stream &optional eof-value)
  (let ((line nil))

    (loop for char = (read-char input-stream nil t)
          do 
          (progn

            (cond ((eq char 't)
                   (return-from read-line-1 eof-value))
                    
                  ((or (char= char #\Linefeed)
                       (char= char #\Return))

                   ;;; ab RacerPro 17.1.2007 

                   (read-char-no-hang input-stream nil nil)

                   ;;; (pprint (coerce (reverse line) 'string))

                   (return-from read-line-1 
                     (make-instance 'list-character-input-stream
                                    :characters 
                                    (reverse line))))

                  (t
                   
                   (push char line)))))))

(defun transform-result (s-expr)
  (cond ((null s-expr)
         nil)
        ((consp s-expr)
         (mapcar #'transform-result s-expr))
        ((symbolp s-expr)
         (cond ((string-equal (symbol-name s-expr) "nil") nil) ; erforderlich!!! wirklich!!! 
               ((eq (symbol-package s-expr) (find-package "RACER-STRING"))
                (symbol-name s-expr))
               (t  
                (let* ((symbol-name (symbol-name s-expr))
                       (pos-colons (search "::" symbol-name))
                       (package-name (and pos-colons
                                          (subseq symbol-name 
                                                  0 
                                                  pos-colons)))
                       (symbol-name-1 (and pos-colons
                                           (subseq symbol-name (+ pos-colons 2)))))
                  (if (null package-name)
                      s-expr
                    (intern 
                     (concatenate 'string
                                  package-name 
                                  "::"
                                  symbol-name-1)))))))
        (t s-expr)))


;;;
;;;
;;;

(defun internal-process-racer-expr (input describe-p)
  (handler-case 
      (multiple-value-bind (output stdout error-p)
          
          (service-request input)

        (values (if (stringp output)
                    (if (and describe-p
                             (not error-p))
                        (my-read-from-string (expand-n-s-etc output))
                      output)
                  output)
                stdout
                error-p))

    (error (error)
      (report-error error))))
