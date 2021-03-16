;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(declaim (special user::*progress-certain*))

(defun update-active-tab ()
  (switch-to-active-tab)

  (with-profile-access
  
    (when (and *create-history-entries*
               (and (zerop busy-counter))
               (connected-and-socket-alive-p))
      (push-history-entry))

    (let ((old-tab active-tab))
      (declare (ignorable old-tab))
      (ecase active-tab
        (:log)
        (:logo)
        (:server) ; macht der Prozess
        (:shell 
         
         #+(and :lispworks6 :linux)
         (unless *shell-font*
           (with-sirius-app (sirius)
             (setf *shell-font* 
                   (gp:augment-font-description 
                    (gp:font-description
                     (gp:find-best-font (slot-value sirius 'command-pane)
                                        (apply
                                         #'gp::make-font-description 
                                         '(:stock :system-fixed-font))))
                    :size *global-font-size*))

             (dolist (pane '(command-pane info-pane 
                                          log-pane
                                          taxonomy-pane
                                          role-hierarchy-pane
                                          abox-graph-pane))
               (setf (simple-pane-font 
                      (slot-value sirius pane))
                     *shell-font*))))

         (move-cursor-to-end-of-expression))
        ;;;
        ;;; die starten ihre eigenen Prozesse
        ;;; 
        (:taxonomy (update-taxonomy-pane))
        (:role-hierarchy (update-role-hierarchy-pane))
        (:abox-graph (update-abox-graph-pane))
        ;;;
        ;;;
        ;;; 
        (:abox (start-pooled-process "update-abox-pane"
                                     (with-update-display-status
                                       (update-abox-pane))))
        (:tbox (start-pooled-process "update-tbox-pane"
                                     (with-update-display-status
                                       (update-tbox-pane))))
        (:query-input (start-pooled-process "update-query-input-pane"
                                            (with-update-display-status
                                              (update-query-input-pane))))
        (:queries (start-pooled-process "update-query-pane"
                                        (with-update-display-status
                                          (update-query-pane))))
        (:defined-queries (start-pooled-process "update-defined-queries-pane"
                                                (with-update-display-status
                                                  (update-defined-queries-pane))))
        (:concepts (start-pooled-process "update-concepts-pane"
                                         (with-update-display-status
                                           (update-concepts-pane))))
        (:roles (start-pooled-process "update-roles-pane"
                                      (with-update-display-status
                                        (update-roles-pane))))
        (:individuals (start-pooled-process "update-individual-pane"
                                            (with-update-display-status
                                              (update-individuals-pane))))
        (:assertions (start-pooled-process "update-assertions-pane"
                                           (with-update-display-status
                                             (update-assertions-pane))))
        (:axioms (start-pooled-process "update-axioms-pane"
                                       (with-update-display-status
                                         (update-axioms-pane)))))))

  (update-sirius-checkboxes)
  (force-push-buttons-update))

;;;
;;;
;;; 


(defun switch-to-old-tab (old-profile old-tab)
  (cond ((eq old-profile *interface-active-profile*)
         
         (setf (profile-active-tab old-profile) old-tab)
         (switch-to-active-tab)
         
         t)

        (t
         
         (when (confirm-yes-or-no
                (format nil "You have issued a request for ~A / ~A.~%Result is now available! Switch to corresponding tab?"
                        (profile-name old-profile)
                        (get-server-description old-profile)))
           
           (with-sirius-app (sirius)
             (apply-in-pane-process sirius
                                    #'(lambda ()
                                        (activate-profile old-profile)
                                        (update-server-pane t)
                                        (with-access-to-profile old-profile
                                          (setf (profile-active-tab old-profile) old-tab)
                                          (switch-to-active-tab))

                                        (schedule-update-no-history-entry old-tab))))

           t))))

;;;
;;;
;;; 

(defun kill-processes ()
  (with-capi-synchronization

    (when *communication-status-updater*
      (mp:process-kill *communication-status-updater*)
      (setf *communication-status-updater* nil))

    (when *progress-bar*
      (mp:process-kill *progress-bar*)
      (setf *progress-bar* nil))
    
    (when *updater*
      (mp:process-kill *updater*)
      (setf *updater* nil))))

(defun kill-pb-process ()
  (when *progress-bar*
      (mp:process-kill *progress-bar*)
      (setf *progress-bar* nil)))

(defun start-pb-process ()
  (setf *progress-bar*
        (start-process-with-options
            "Progress Bar Updater"
            (:priority 900000)
          (loop
           (#-:sirius-dev 
            ignore-errors
            #+:sirius-dev 
            progn
               
            (sleep +progress-bar-update-process-sleep-time+)

            (with-sirius-app (sirius)
              (progn ; with-capi-synchronization 
                (when sirius
                  (update-progress-bar-display)))))))))   

(defun start-processes ()  
  (setf *communication-status-updater*
        (start-process-with-options
            "Comm Updater"
            (:priority 900000)
          (loop
           (#-:sirius-dev 
            ignore-errors
            #+:sirius-dev 
            progn
               
            (sleep +update-process-sleep-time+)
                  
            (with-sirius-app (sirius)
              (with-capi-synchronization
                (when sirius
                  (apply-in-pane-process sirius
                                         #'(lambda ()
                                             (update-request-response-display))))))))))
  
  #+:progress-bar-pane
  (start-pb-process)
  
  (setf *updater* 
        (start-process-with-options
            "Comm Updater"
            (:priority 900000)
          (let ((val nil))
            (loop
             (#-:sirius-dev 
              ignore-errors
              #+:sirius-dev 
              progn
            
              (process-wait (setf val (profile-update-queue (active-profile))))

              (with-capi-synchronization
                
                (with-update-queue-synchronization 
                  (setf val (pop  (profile-update-queue (active-profile)))))

                (with-sirius-app (sirius)

                  (when sirius

                    (let ((tab (second val))
                          (type (first val))
                          (profile (third val)))

                      (ecase type
                        (:update
                         (apply-in-pane-process sirius
                                                #'(lambda ()
                                                    (with-profile (profile)
                                                      (with-profile-access
                                                        (setf active-tab tab)
                                                        (update-active-tab))))))

                        (:update-no-history-entry
                         (apply-in-pane-process sirius
                                                #'(lambda ()
                                                    (with-profile (profile)
                                                      (with-profile-access
                                                        (setf active-tab tab)
                                                        (without-history-entries
                                                          (update-active-tab)))))))
                    
                        (:focus
                         (apply-in-pane-process sirius
                                                #'(lambda ()
                                                    (with-profile (profile) 
                                                      (with-profile-access
                                                        (setf active-tab tab)
                                                        (without-history-entries
                                                          (let ((*current-history-position-is-top-of-stack* 
                                                                 nil))

                                                            #+:ignore
                                                            (case active-tab
                                                              (:taxonomy
                                                               (setf taxonomy-pane-command :install))
                                                              (:abox-graph
                                                               (setf abox-graph-pane-command :install))
                                                              (:role-hierarchy-graph
                                                               (setf role-hierarchy-pane-command :install)))
                                                              
                                                            (update-active-tab)))))))))))))))))))


(defun schedule-update-item (entry)
  (with-update-queue-synchronization 
    (when (eq (active-profile)
              *interface-active-profile*)
      ;;; ignore updates for non-visibile profiles
      (with-profile-access
        (if (not update-queue)
            (setf update-queue
                  (list entry)
                  update-queue-last-pointer
                  update-queue)
          (setf (cdr update-queue-last-pointer)
                (list entry)))))))
               
(defun schedule-update (&optional tab)
  (with-profile-access
    (schedule-update-item
     (list :update (or tab active-tab) (active-profile)))))

(defun schedule-focus-update (&optional tab)
  (with-profile-access
    (schedule-update-item
     (list :focus (or tab active-tab) (active-profile)))))

(defun schedule-update-no-history-entry (&optional tab)
  (with-profile-access
    (schedule-update-item
     (list :update-no-history-entry (or tab active-tab) (active-profile)))))


;;;
;;;
;;; 


#+(and (not :native-progress-bar) :progress-bar)
(defun set-progress-bar-to (val &optional (certain t))
  (with-sirius-app (app)
    (with-slots (progress-bar) app
      (gp:clear-graphics-port progress-bar)
      (if certain
          (gp:draw-rectangle progress-bar
                             0 0
                             (* (round (capi::pane-width progress-bar))
                                (/ val 100))
                             (round (capi::pane-height progress-bar))
                             :foreground :green
                             :background :green 
                             :filled t)
        (let* ((w (capi::pane-width progress-bar))
               (center (round (* w 0.5)))
               (length (* w 0.8))
               (size (round (/ (* length (sin (* (/ val 100) pi))) 2))))

          (declare (ignorable size center))

          (gp:draw-rectangle progress-bar
                             (- center size)
                             0 
                             (* 2 size)
                             (round (capi::pane-height progress-bar))
                             :foreground :green
                             :background :green 
                             :filled t))))

    (setf (profile-last-progress-value (active-profile)) val)))        

#+(and :native-progress-bar :progress-bar)
(defun set-progress-bar-to (val &optional (certain t))
  (with-sirius-app (app)
    (with-slots (progress-bar) app
      (if (or (zerop val) certain)

          (setf (range-slug-start progress-bar) val)

        (let* ((w 100)
               (center (round (* w 0.5)))
               (length (* w 0.8))
               (size (round (/ (* length (sin (* (/ val 100) pi))) 2))))
          
          (declare (ignorable size center))

          (unless (eql val (profile-last-progress-value (active-profile)))
            ;;; really changed? 
            (setf (range-slug-start progress-bar)
                  (if (= (range-slug-start progress-bar) 50)
                      60
                    50))))))

    (setf (profile-last-progress-value (active-profile))
          val)))

#-:progress-bar
(defun set-progress-bar-to (val &optional (certain t))
  (declare (ignorable val certain))
  val)

;;;
;;;
;;;  

(defun update-request-response-display ()
  (pulse)

  ;;; wird periodisch durch einen Update-Prozess gerufen

  (with-sirius-app (app)
    (when (and app
               (interface-displayed-p app))
              
      (update-state-display)
        
      (with-slots (server-request-pane                   
                   server-response-pane
                   #+:pulse-pane 
                   server-request-pulse-pane
                   ) app
          
        (with-profile-access

          (let* ((profile (active-profile))

                 (cache-hit (profile-current-cache-hit profile))

                 (status 
                  (let ((s (profile-status profile)))
                    (cond ((and (eq s :ready) cache-hit)
                           :cache-hit)
                          ((and (eq s :ready) 
                                (or (not (zerop (profile-updating-counter profile)))
                                    (profile-update-queue profile)))
                           :updating-display)
                          (t s))))

                 (comm-status 
                  (let ((s (profile-current-communication-status profile)))
                    (cond ((and (eq s :ready) cache-hit)
                           :cache-hit)
                          ((and (eq s :ready) 
                                (or (not (zerop (profile-updating-counter profile)))
                                    (profile-update-queue profile)))
                           :updating-display)
                          (t s))))

                 (request-color 
                  (case status 
                    (:cache-hit +cache-hit-color+)
                    (:updating-display +updating-display-color+)
                    (:busy +req-busy-color+)
                    (:ready +req-ready-color+)
                    (:died +req-died-color+)
                    (:not-connected +req-not-connected-color+)
                    (otherwise +req-otherwise-color+)))

                 (response-color
                  (case comm-status 
                    (:cache-hit +cache-hit-color+)
                    (:updating-display +updating-display-color+)
                    (:sending-request +res-sending-request-color+)
                    (:connection-died-during-send +res-connection-died-during-send-color+)
                    (:connection-died-during-read +res-connection-died-during-read-color+)
                    (:connection-already-dead +res-connection-already-dead-color+)
                    (:getting-racer-result +res-getting-racer-result-color+)
                    (:error +res-error-color+)
                    (:good +res-good-color+)
                    (:ready +res-ready-color+)
                    (otherwise 
                     (case status 
                       (:not-connected +res-otherwise-color+)
                       (:ready +res-ready-color+)
                       (otherwise  +res-otherwise-color+))))))
            
            ;;;
            ;;;
            ;;;
            
            (setpc (simple-pane-background server-request-pane)
                   request-color)
            
            (setpc (title-pane-text server-request-pane)
                   (cond ((eq comm-status :updating-display)
                          (format nil "Updating Display, Please Stand By"))
                         (cache-hit
                          #|(format nil "~A (~A thread~:P, ~A left) : ~A..."
                                    (first cache-hit)
                                    (- *pool-counter* 
                                       (length *process-pool*))
                                    (length *process-pool*)
                                    (subseq (second cache-hit)
                                            1
                                            (min 14 (length (second cache-hit))))) |# 
                          (format nil "~A : ~A..."
                                  (first cache-hit)
                                  (subseq (second cache-hit)
                                          0
                                          (min 60 (length (second cache-hit))))))                            
                         ((profile-processing-command profile)
                          #|(format nil "~A (~A thread~:P, ~A left) : ~A"
                                    (first (profile-processing-command profile))
                                    (- *pool-counter* 
                                       (length *process-pool*))
                                    (length *process-pool*)
                                    (subseq (second (profile-processing-command profile))
                                            1
                                            (min 14 (length (second (profile-processing-command profile)))))) |#

                          (remove #\| 
                                  (compact-backslashes
                                   (format nil "~A : ~A"
                                           (first (profile-processing-command profile))
                                           (subseq (second (profile-processing-command profile))
                                                   0
                                                   (min 60 (length (second (profile-processing-command profile)))))) )))

                         ((eq status :ready)
                          "Ready")

                         (t "Waiting for Connect (Press \"Connect\" Button)")))

            (setpc (simple-pane-background server-response-pane) 
                   response-color)

            (setpc (title-pane-text server-response-pane)
                   (cond ((eq comm-status :updating-display)
                          (format nil "Updating Display, Please Stand By"))
                         (cache-hit
                          (format nil "~A : ~A" 
                                  (first cache-hit)
                                  (if (eq comm-status :error)
                                      "ERROR (PUSH \"RECOVER\" TO CONTINUE)"
                                    comm-status)))
                         ((profile-processing-command profile)
                          (format nil "~A : ~A" 
                                  (first (profile-processing-command profile))
                                  (if (eq comm-status :error)
                                      "ERROR (PUSH \"RECOVER\" TO CONTINUE)"
                                    comm-status)))
                         ((eq status :ready)
                          "Ready")
                         (t "")))

            (if (and (zerop busy-counter)
                     (or (eq request-color +req-ready-color+)
                         (eq request-color +cache-hit-color+)))
                (enable-push-buttons)
              (disable-push-buttons))
                  
            (when (and (connection-died-p)
                       (not *dialog-displayed*))
              (setf *dialog-displayed* t)
              (with-profile-access
                (error-message
                 "RacerPro connection died!~%Request ~A was: ~A.~%Status: ~A.~%Connection to ~A closed!"
                 (first processing-command)
                 (second processing-command)
                 current-communication-status
                 (get-server-description)))

              (close-connection)
              (setf *dialog-displayed* nil))

            (when (and (zerop (mod *pulse* 4))
                       (zerop updating-counter)
                       (eq active-tab :server))
              (update-server-pane))

            #+:pulse-pane
            (let ((image (get-pulse-image status comm-status)))
              (unless (eq image (image-pinboard-object-image server-request-pulse-pane))
                  
                (setf (image-pinboard-object-image server-request-pulse-pane)
                      image)))))))))

(defun read-line-no-hang (stream)
  (let ((chars nil))
    (loop as char = (read-char stream nil) do
          (if (not (char= char #\return))
              (push char chars)
            (return)))
    (coerce (reverse chars) 'string)))

#-:racer-with-sirius
(defun update-progress-bar-display ()

  ;;; wird periodisch durch einen Update-Prozess gerufen

  (with-sirius-app (app)
    (with-profile-access
      (when show-progress-bar
        (when (and app
                   (interface-displayed-p app))
        
          (with-slots (#+:progress-bar-pane 
                       racer-request-pane
                       ) app
          
            #+:progress-bar-pane 
            (cond ((and (connected-and-socket-alive-p) ;; Verbindung zu Racer erfolgreich? 
                        (socket-alive-p progress-socket)
                        ;; (zerop (mod *pulse* 2))
                        ;; (zerop updating-counter)
                        ;; NEIN!
                        )

                   (let ((stream progress-socket)
                         (progress nil)
                         (progress-certain nil)
                         (request nil))

                     (declare (ignorable abort))

                     (handler-case
                         (progn
                      
                           (cond (abort-requested-p 
                                  ;; (setf abort-requested-p nil)
                                  (setf abort-requested-p nil)
                                  (write-string  "(abort)" stream)
                                  (terpri stream)
                                  (force-output stream)
                                  (read-line-no-hang stream)
                                  (setf abort-confirmed-p t)
                                  (dolist (x evaluation-process)
                                    (kill-pooled-process x))
                                  (display-message "Request was aborted - Racer may take some time to recover.~%Please reconnect shortly.")
                                  (close-connection)
                                  (clear-and-resync))
                           
                                 (t
                                  ;;;
                                  ;;;
                                  ;;;
                                  
                                  (write-string "(get-progress-indicator)" stream)
                                  (terpri stream)
                                  (force-output stream)
                                  #+:ignore
                                  (setf progress 
                                        (handler-case 
                                            (parse-integer
                                             (read-line-no-hang stream))
                                          (error ()
                                            nil)))

                                  ;;;
                                  ;;;
                                  ;;;
                      
                                  (write-string "(progress-certain?)" stream)
                                  (terpri stream)
                                  (force-output stream)
                                  (setf progress-certain
                                        (and progress
                                             (member (read-line-no-hang stream)
                                                     '("t" "|t|" "T") 
                                                     :test #'string-equal)))
                      
                                  ;;;
                                  ;;;
                                  ;;;
                      
                                  (write-string "(get-current-request)" stream)
                                  (terpri stream)
                                  (force-output stream)
                                  (setf request (read-line-no-hang stream))

                                  (when request

                                    (cond ((search "Idle" request)
                                           (setpc (simple-pane-background racer-request-pane) +req-ready-color+)
                                           (setpc (title-pane-text racer-request-pane) "Nothing"))
                                          (t
                                           (cond ((search "ABORT" request)
                                                  (setpc (simple-pane-background racer-request-pane) +updating-display-color+)
                                                  (setpc (title-pane-text racer-request-pane)
                                                         (remove #\| request)))
                                                 (t
                                                  (setpc (simple-pane-background racer-request-pane) +req-busy-color+)
                                                  (setpc (title-pane-text racer-request-pane)
                                                         (remove #\| request))))))

                                    (when progress
                                      (set-progress-bar-to progress progress-certain))))))
                      
                       (error (error)
                         (declare (ignorable error))
                         (setpc (title-pane-text racer-request-pane) 
                                (format nil "ERROR: ~A (PUSH \"RECOVER\" TO CONTINUE)" error))
                         (setpc (simple-pane-background racer-request-pane) +res-error-color+)
                         (set-progress-bar-to 0 t)
                         (close-connection)
                         ))))

                  (t 

                   (setpc (title-pane-text racer-request-pane) "Not Connected")
                   (setpc (simple-pane-background racer-request-pane) *background-color*)
                   (set-progress-bar-to 0 t)))))))))


#+:racer-with-sirius
(defun update-progress-bar-display ()

  ;;; wird periodisch durch einen Update-Prozess gerufen

  (with-sirius-app (app)
    (with-profile-access
     (when show-progress-bar
       (when (and app
                  (interface-displayed-p app))
        
         (with-slots (#+:progress-bar-pane 
                      racer-request-pane
                      ) app
          
           #+:progress-bar-pane 
           (cond ((and (connected-and-socket-alive-p) ;; Verbindung zu Racer erfolgreich? 
                       ;; (zerop (mod *pulse* 2))
                       ;; (zerop updating-counter)
                       ;; NEIN!
                       )

                  (handler-case
                      (progn
                   
                        (let ((progress nil)
                              (progress-certain nil)
                              (request nil))
               
                          (when abort-requested-p 
                            (setf abort-requested-p nil)
                            (user::request-abort))
               
                          ;;;
                          ;;;
                          ;;; 

                          (setf progress (user::get-progress-value))
               
                          (setf progress-certain
                                (and progress
                                     user::*progress-certain*))

                          (setf request 
                                (racer::substring
                                 (user::get-current-request)
                                 80))

                          ;;;
                          ;;;
                          ;;;

                          (cond ((not request)
                                 (setpc (simple-pane-background racer-request-pane) +req-ready-color+)
                                 (setpc (title-pane-text racer-request-pane) "Nothing"))
                                (t
                                 (cond ((search "ABORT" request)
                                        (setpc (simple-pane-background racer-request-pane) +updating-display-color+)
                                        (setpc (title-pane-text racer-request-pane)
                                               (remove #\| request)))
                                       (t
                                        (setpc (simple-pane-background racer-request-pane) +req-busy-color+)
                                        (setpc (title-pane-text racer-request-pane)
                                               (remove #\| request))))))

                          (when progress
                            (set-progress-bar-to progress progress-certain))))
                      
                    (error (error)
                      (declare (ignorable error))
                      (setpc (title-pane-text racer-request-pane) 
                             (format nil "ERROR: ~A (PUSH \"RECOVER\" TO CONTINUE)" error))
                      (setpc (simple-pane-background racer-request-pane) +res-error-color+)
                      (set-progress-bar-to 0 t))))

                 (t
                  
                  (setpc (title-pane-text racer-request-pane) "Not Connected")
                  (setpc (simple-pane-background racer-request-pane) *background-color*)
                  (set-progress-bar-to 0 t)))))))))

;;;
;;;
;;;

(defun update-tbox-pane ()
  (with-sirius-app (app)
    (with-slots (tbox-pane) app
      (with-profile-access
        (with-auto-tab
          (let* ((profile (active-profile))
                 (items 
                  (msort (racer-function1 (all-tboxes) '(:error))
                         #'string<
                         :key #'symbol-name))

                 (items (mapcar
                         #'(lambda (tbox)

                             (list (list tbox
                                         (eq tbox (racer-function1 (current-tbox))))

                                   (yes-or-no (racer-function1 (tbox-classified-p tbox)))

                                   (let ((mcs (racer-function1 (get-meta-constraint tbox))))
                                     (yes-or-no
                                      (not (eq mcs
                                               (if alisp-racer
                                                   'racer-user::top
                                                 'racer-user::|top|)))))

                                   (yes-or-no
                                    (racer-function1 (tbox-cyclic? tbox)))

                                   (racer-function1 (get-tbox-language tbox))
                                                           
                                   (if (string= (profile-racer-version profile) "1.9.0")
                                       (let ((res (racer-function1 (all-atomic-concepts tbox))))
                                         (if (eq res :error)
                                             :error
                                           (length res)))
                                     (racer-function1 (all-atomic-concepts tbox :count t)))

                                   (if (string= (profile-racer-version profile) "1.9.0")
                                       (let ((res (racer-function1 (all-roles tbox))))
                                         (if (eq res :error)
                                             :error
                                           (length res)))
                                     (racer-function1 (all-roles tbox :count t)))
                                   
                                   (racer-function1 (associated-aboxes tbox))))
                         
                         items)))

            (apply-in-pane-process tbox-pane
                                   #'(lambda ()
                                       (setf (collection-items tbox-pane)
                                             items)
                                                               
                                       (setf (choice-selected-items tbox-pane)
                                             sirius-current-tbox)))))))))




(defun update-abox-pane ()
  (with-sirius-app (app)
    (with-slots (abox-pane) app
      (with-profile-access
        (with-auto-tab
          (let* ((profile (active-profile))
                 (items 
                  (msort (racer-function1 (all-aboxes) '(:error))
                         #'string<
                         :key #'symbol-name))
                  
                 (items (mapcar 
                         #'(lambda (abox) 
                             (let ((tbox
                                    (racer-function1 (associated-tbox abox))))

                               (list (list abox
                                           (eq abox (racer-function1 (current-abox))))

                                     tbox
                                                             
                                     (yes-or-no (racer-function1 (abox-realized-p abox)))
                                                             
                                     (racer-function1 (get-abox-language abox))


                                     (if (string= (profile-racer-version profile) "1.9.0")
                                         (let ((res (racer-function1 (all-individuals abox))))
                                           (if (eq res :error)
                                               :error
                                             (length res)))
                                       (racer-function1 (all-individuals abox :count t)))

                                     (if (string= (profile-racer-version profile) "1.9.0")
                                         (let ((res (racer-function1 (all-concept-assertions abox))))
                                           (if (eq res :error)
                                               :error
                                             (length res)))
                                       (racer-function1 (all-concept-assertions abox :count t)))

                                     (if (string= (profile-racer-version profile) "1.9.0")
                                         (let ((res (racer-function1 (all-role-assertions abox))))
                                           (if (eq res :error)
                                               :error
                                             (length res)))
                                       (racer-function1 (all-role-assertions abox :count t))))))
                         items)))

            (apply-in-pane-process abox-pane
                                   #'(lambda ()
                                       (setf (collection-items abox-pane)
                                             items)
                                                               
                                       (setf (choice-selected-items abox-pane)
                                             sirius-current-abox)))))))))

;;;
;;;
;;;

(defun next-update-refresh-role-hierarchy ()
  (with-profile-access
    (setf graph-drawing-aborted nil)
    (setf role-hierarchy-pane-command :refresh)))

(defun next-update-layout-role-hierarchy ()
  (with-profile-access
    (setf graph-drawing-aborted nil)
    (setf role-hierarchy-pane-command :draw)))

(defun next-update-compute-and-layout-role-hierarchy ()
  (with-profile-access
    (setf graph-drawing-aborted nil)
    (setf role-hierarchy-pane-command :compute-and-draw)))
                                               

(defun update-role-hierarchy-pane ()
  (with-sirius-app (app)
    (with-profile-access
      (with-slots (role-hierarchy-pane
                   role-hierarchy-tab) app

        (when role-hierarchy-pane-command
          (ecase role-hierarchy-pane-command
            (:compute-and-draw 
             (setf role-hierarchy-graph nil
                   saved-role-hierarchy-graph-pane nil
                   role-hierarchy-graph-pane nil))
            (:draw 
             (setf saved-role-hierarchy-graph-pane nil
                   role-hierarchy-graph-pane nil))
            (:refresh 
             (setf role-hierarchy-graph-pane nil))))

        (setf role-hierarchy-pane-command nil)

        (let* ((running (typep role-hierarchy-graph 'pooled-process))
               (no-role-hierarchy (null role-hierarchy-graph))
               (role-hierarchy-computed (and role-hierarchy-graph (not running) (not (symbolp role-hierarchy-graph))))
               (role-hierarchy-layouted role-hierarchy-graph-pane)
                 
               (connected (connected-and-socket-alive-p))
               (busy (racer-busy-p))
               (error (eq role-hierarchy-graph :error))
               (empty (eq role-hierarchy-graph :empty)))

          ;;; update-role-hierarchy-display ist reine Status-Anzeige, 
          ;;; wenn nicht Autofocus. Dann wird auch request-display
          ;;; eingeleitet

          (cond ((and (not running)
                      no-role-hierarchy ; wichtig! 
                      autofocus

                      connected
                      (not busy))

                 (with-auto-tab
                   (request-role-hierarchy)
                   (rescroll-role-hierarchy-pane)))

                ((and (not running)
                      role-hierarchy-computed
                      (not role-hierarchy-layouted)
                      autofocus
                       
                      connected
                      (not busy))
                       
                 ;; notwendig, falls NUR layout geaendert wurde (z.B. Tree -> Graph)

                 (establish-role-hierarchy-pane)
                 (rescroll-role-hierarchy-pane))
                  
                  
                (connected

                 ;;; Statusanzeige
                   
                 (apply-in-pane-process app
                                        #'(lambda ()
                                              
                                            (cond (role-hierarchy-layouted

                                                   t)

                                                  (t

                                                   (setf (layout-description role-hierarchy-tab)
                                                         (remove nil
                                                                 (list (when show-status-pane
                                                                         'state-pane1)
                                                                       'navi-pane
                                                                       *role-hierarchy-info-pane*
                                                                       'role-hierarchy-buttons1
                                                                       'role-hierarchy-buttons2
                                                                       'roles-buttons2
                                                                       (when show-info-pane
                                                                         'info-pane))))
                                                                                           
                                                   (setf (editor-pane-text role-hierarchy-pane)
                                                         (cond (graph-drawing-aborted
                                                                (setf graph-drawing-aborted nil)
                                                                (clear-and-resync)
                                                                +role-hierarchy-aborted-text+)
                                                               (error 
                                                                +role-hierarchy-error-text+)
                                                               (empty 
                                                                (get-role-hierarchy-empty-text))
                                                               (running
                                                                +role-hierarchy-running-text+)
                                                               (no-role-hierarchy 
                                                                +role-hierarchy-request-text+)
                                                               (role-hierarchy-computed
                                                                (concatenate 'string
                                                                             +role-hierarchy-computed-text+
                                                                             +newline-string+
                                                                             (format nil "The Role Hierarchy contains ~A nodes." 
                                                                                     ;;; "top role" abziehen
                                                                                     (1- (hash-table-count role-hierarchy-graph))))))))))))

                  
                (t 

                 (apply-in-pane-process app
                                        #'(lambda ()
                                              
                                            (setf (layout-description role-hierarchy-tab)
                                                  (remove nil 
                                                          (list (when show-status-pane 
                                                                  'state-pane1)
                                                                'navi-pane
                                                                *role-hierarchy-info-pane*
                                                                'role-hierarchy-buttons1
                                                                'role-hierarchy-buttons2
                                                                'roles-buttons2
                                                                (when show-info-pane
                                                                  'info-pane))))
                                              
                                            (setf (editor-pane-text role-hierarchy-pane) "")))))
              
          (set-pane-focus role-hierarchy-pane))))))
        


(defun next-update-refresh-taxonomy ()
  (with-profile-access
    (setf graph-drawing-aborted nil)
    (setf taxonomy-pane-command :refresh)))

(defun next-update-layout-taxonomy ()
  (with-profile-access
    (setf graph-drawing-aborted nil)
    (setf taxonomy-pane-command :draw)))

(defun next-update-compute-and-layout-taxonomy ()
  (with-profile-access
    (setf graph-drawing-aborted nil)
    (setf taxonomy-pane-command :compute-and-draw)))
                                               

(defun update-taxonomy-pane ()
  (with-sirius-app (app)
    (with-profile-access
      (with-slots (taxonomy-pane
                   taxonomy-tab) app
        
        (when taxonomy-pane-command
          (ecase taxonomy-pane-command
            (:compute-and-draw 
             (setf taxonomy-graph nil
                   saved-taxonomy-graph-pane nil
                   taxonomy-graph-pane nil))
            (:draw 
             (setf saved-taxonomy-graph-pane nil
                   taxonomy-graph-pane nil))
            (:refresh 
             (setf taxonomy-graph-pane nil))
            (:install
             ;; geht nur, wenn mit Kopien der graph-panes  gearbeitet wird... 
             ;; disabled 
             (establish-taxonomy-pane)
             (rescroll-taxonomy-pane)
             (set-pane-focus taxonomy-pane)
             (return-from update-taxonomy-pane nil))))

        (setf taxonomy-pane-command nil)
          
        (let* ((running (typep taxonomy-graph 'pooled-process))
               (no-taxonomy (null taxonomy-graph))
               (taxonomy-computed (and taxonomy-graph (not running) (not (symbolp taxonomy-graph))))
               (taxonomy-layouted taxonomy-graph-pane)
                 
               (connected (connected-and-socket-alive-p))
               (busy (racer-busy-p))
               (error (eq taxonomy-graph :error))
               (empty (eq taxonomy-graph :empty)))

          ;;; update-taxonomy-display ist reine Status-Anzeige, 
          ;;; wenn nicht Autofocus. Dann wird auch request-display
          ;;; eingeleitet

          (cond ((and (not running)
                        
                      no-taxonomy ; wichtig! 
                       
                      autofocus
                        
                      connected
                      (not busy))

                 (with-auto-tab
                   (request-taxonomy)
                   (rescroll-taxonomy-pane)))

                ((and (not running)
                      (and taxonomy-computed
                           (not taxonomy-layouted))
                      autofocus
                        
                      connected
                      (not busy))

                 ;; graph vorhanden, nur die Pane wurde invalidiert
                 ;; notwendig, falls NUR layout geaendert wurde (z.B. Tree -> Graph)

                 (establish-taxonomy-pane)
                 (rescroll-taxonomy-pane))
                  
                (connected

                 ;;; Statusanzeige

                 (apply-in-pane-process app
                                        #'(lambda ()
                                              
                                            (cond (taxonomy-layouted
                                                     
                                                   t)

                                                  (t

                                                   (setf (layout-description taxonomy-tab)
                                                         (remove nil 
                                                                 (list (when show-status-pane
                                                                         'state-pane1)
                                                                       'navi-pane
                                                                       *taxonomy-info-pane*
                                                                       'taxonomy-buttons1
                                                                       'taxonomy-buttons2
                                                                       'concepts-buttons2
                                                                       (when show-info-pane
                                                                         'info-pane))))
                                                                                           
                                                   (setf (editor-pane-text taxonomy-pane)
                                                         (cond (graph-drawing-aborted
                                                                (setf graph-drawing-aborted nil)
                                                                (clear-and-resync)
                                                                +taxonomy-aborted-text+)
                                                               (error 
                                                                +taxonomy-error-text+)
                                                               (empty  ;;; kommt jetzt nicht mehr vor!
                                                                       (get-taxonomy-empty-text))
                                                               (running
                                                                +taxonomy-running-text+)
                                                               (no-taxonomy 
                                                                +taxonomy-request-text+)
                                                               (taxonomy-computed
                                                                (concatenate 'string
                                                                             +taxonomy-computed-text+
                                                                             +newline-string+
                                                                             (format nil "The Taxonomy contains ~A nodes." 
                                                                                     (hash-table-count taxonomy-graph)))))))))))
                  
                (t 

                 (apply-in-pane-process app
                                        #'(lambda ()
                                              
                                            (setf (layout-description taxonomy-tab)
                                                  (remove nil
                                                          (list (when show-status-pane
                                                                  'state-pane1)
                                                                'navi-pane
                                                                *taxonomy-info-pane*
                                                                'taxonomy-buttons1
                                                                'taxonomy-buttons2
                                                                'concepts-buttons2
                                                                (when show-info-pane
                                                                  'info-pane))))
                                              
                                            (setf (editor-pane-text taxonomy-pane) "")))))
              
          (set-pane-focus taxonomy-pane))))))

(defun next-update-refresh-abox-graph ()
  (with-profile-access
    (setf graph-drawing-aborted nil)
    (setf abox-graph-pane-command :refresh)))

(defun next-update-layout-abox-graph ()
  (with-profile-access
    (setf graph-drawing-aborted nil)
    (setf abox-graph-pane-command :draw)))

(defun next-update-compute-and-layout-abox-graph ()
  (with-profile-access
    (setf graph-drawing-aborted nil)
    (setf abox-graph-pane-command :compute-and-draw)))
                                               


(defun update-abox-graph-pane ()
  (with-sirius-app (app)
    (with-profile-access

      (with-slots (abox-graph-pane
                   abox-graph-tab) app
             
        (when abox-graph-pane-command
          (ecase abox-graph-pane-command
            (:compute-and-draw 
             (setf abox-graph nil
                   saved-abox-graph-graph-pane nil
                   abox-graph-graph-pane nil))
            (:draw 
             (setf saved-abox-graph-graph-pane nil
                   abox-graph-graph-pane nil))
            (:refresh 
             (setf abox-graph-graph-pane nil))))

        (setf abox-graph-pane-command nil)

        (let* ((running (typep abox-graph 'pooled-process))
               (no-abox-graph (null abox-graph))
               (abox-graph-computed (and abox-graph (not running) (not (symbolp abox-graph))))
               (abox-graph-layouted abox-graph-graph-pane)
                 
               (connected (connected-and-socket-alive-p))
               (busy (racer-busy-p))
               (error (eq abox-graph :error))
               (empty (eq abox-graph :empty)))

          ;;; update-abox-graph-display ist reine Status-Anzeige, 
          ;;; wenn nicht Autofocus. Dann wird auch request-display
          ;;; eingeleitet

          (cond ((and (not running)
                      no-abox-graph ; wichtig! 
                      autofocus

                      connected
                      (not busy))

                 (with-auto-tab
                   (request-abox-graph)
                   (rescroll-abox-graph-graph-pane)))

                ((and (not running)
                      abox-graph-computed
                      (not abox-graph-layouted)
                      autofocus
                       
                      connected
                      (not busy))
                       
                 ;; notwendig, falls NUR layout geaendert wurde (z.B. Tree -> Graph)

                 (establish-abox-graph-pane)
                 (rescroll-abox-graph-graph-pane))
                  
                  
                (connected

                 ;;; Statusanzeige

                 (apply-in-pane-process app
                                        #'(lambda ()
                                              
                                            (cond (abox-graph-layouted

                                                   t)

                                                  (t


                                                   (setf (layout-description abox-graph-tab)
                                                         (remove nil 
                                                                 (list (when show-status-pane 
                                                                         'state-pane1)
                                                                       'navi-pane
                                                                       *abox-graph-info-pane*
                                                                       'abox-graph-buttons1
                                                                       'abox-graph-buttons2
                                                                       'abox-graph-buttons3
                                                                       'abox-graph-buttons4
                                                                       (when show-info-pane
                                                                         'info-pane))))
                                                                                           
                                                   (setf (editor-pane-text abox-graph-pane)
                                                         (cond (graph-drawing-aborted
                                                                (setf graph-drawing-aborted nil)
                                                                (clear-and-resync)
                                                                +abox-graph-aborted-text+)
                                                               (error 
                                                                +abox-graph-error-text+)
                                                               (empty 
                                                                (get-abox-graph-empty-text))
                                                               (running
                                                                +abox-graph-running-text+)
                                                               (no-abox-graph 
                                                                +abox-graph-request-text+)
                                                               (abox-graph-computed
                                                                (concatenate 'string
                                                                             +abox-graph-computed-text+
                                                                             +newline-string+
                                                                             (format nil "The ABox Graph contains ~A nodes." 
                                                                                     (hash-table-count abox-graph)))))))))))
                  
                (t 

                 (apply-in-pane-process app
                                        #'(lambda ()
                                              
                                            (setf (layout-description abox-graph-tab)
                                                  (setf (layout-description abox-graph-tab)
                                                        (remove nil
                                                                (list (when show-status-pane 
                                                                        'state-pane1)
                                                                      'navi-pane
                                                                      *abox-graph-info-pane*
                                                                      'abox-graph-buttons1
                                                                      'abox-graph-buttons2
                                                                      'abox-graph-buttons3
                                                                      'abox-graph-buttons4
                                                                      (when show-info-pane
                                                                        'info-pane)))))

                                            (setf (editor-pane-text abox-graph-pane) "")))))
              
          (set-pane-focus abox-graph-pane))))))


;;;
;;;
;;; 


(defun install-role-hierarchy-pane ()
  (with-profile-access
    (with-sirius-slots (role-hierarchy-tab)

                       (with-sirius-app (sirius)
                         (apply-in-pane-process sirius
                                                #'(lambda ()
                                                    (setf (layout-description role-hierarchy-tab)
                                                          (remove nil 
                                                                  (list (when show-status-pane 
                                                                          'state-pane1)
                                                                        'navi-pane
                                                                        role-hierarchy-graph-pane ; Profil 
                                                                        'role-hierarchy-buttons1
                                                                        'role-hierarchy-buttons2
                                                                        'roles-buttons2
                                                                        (when show-info-pane
                                                                          'info-pane))))

                                                    (update-push-buttons)))))))
              
                     
(defun establish-role-hierarchy-pane (&rest args)
  (declare (ignore args))

  (with-profile-access
    (with-sirius-slots (role-hierarchy-tab role-hierarchy-pane) 

                       (when (and role-hierarchy-graph
                                  (not role-hierarchy-graph-pane))
                         (request-role-hierarchy-pane))

                       (cond ((not (graph-pane-roots role-hierarchy-graph-pane)) ; empty? 
                                     
                              (apply-in-pane-process app
                                                     #'(lambda ()
                                                                
                                                         (setf (layout-description role-hierarchy-tab)
                                                               (remove nil
                                                                       (list (when show-status-pane
                                                                               'state-pane1)
                                                                             'navi-pane
                                                                             *role-hierarchy-info-pane*
                                                                             'role-hierarchy-buttons1
                                                                             'role-hierarchy-buttons2
                                                                             'roles-buttons2
                                                                             (when show-info-pane
                                                                               'info-pane))))
                                                                                           
                                                         (setf (editor-pane-text role-hierarchy-pane)
                                                               (get-role-hierarchy-empty-text)))))
                             
                             (t

                              (with-graph-drawing-process

                                  role-hierarchy-tab

                                (:process
                                 (install-role-hierarchy-pane))
                                (:cancel
                                 (next-update-layout-role-hierarchy)
                                 (schedule-update-no-history-entry :role-hierarchy))))))))

                            
(defun rescroll-role-hierarchy-pane ()
  (with-profile-access
    (when (consp role-hierarchy-scroll)
      (when (and role-hierarchy-scroll 
                 role-hierarchy-graph-pane)
        (scroll role-hierarchy-graph-pane
                :pan :move
                role-hierarchy-scroll)))))

;;;
;;;
;;;

(defun install-taxonomy-pane ()
  (with-profile-access
                         
    (with-sirius-slots (taxonomy-tab)

                       (with-sirius-app (sirius)
                         (apply-in-pane-process sirius
                                                #'(lambda ()
                                                    (setf (layout-description taxonomy-tab)
                                                          (remove nil 
                                                                  (list (when show-status-pane
                                                                          'state-pane1)
                                                                        'navi-pane
                                                                        taxonomy-graph-pane ; Profil 
                                                                        'taxonomy-buttons1
                                                                        'taxonomy-buttons2
                                                                        'concepts-buttons2
                                                                        (when show-info-pane
                                                                          'info-pane))))

                                                    (update-push-buttons)))))))


(defun establish-taxonomy-pane (&rest args)
  (declare (ignore args))
  (with-profile-access
    (with-sirius-slots (taxonomy-tab taxonomy-pane) 

                       (when (and taxonomy-graph
                                  (not taxonomy-graph-pane))
                         (request-taxonomy-pane))

                       (cond ((not (graph-pane-roots taxonomy-graph-pane)) ; empty? 

                              (apply-in-pane-process app
                                                     #'(lambda ()
                                                                
                                                         (setf (layout-description taxonomy-tab)
                                                               (remove nil 
                                                                       (list (when show-status-pane
                                                                               'state-pane1)
                                                                             'navi-pane
                                                                             *taxonomy-info-pane*
                                                                             'taxonomy-buttons1
                                                                             'taxonomy-buttons2
                                                                             'concepts-buttons2
                                                                             (when show-info-pane
                                                                               'info-pane))))
                                                                                           
                                                         (setf (editor-pane-text taxonomy-pane)
                                                               (get-taxonomy-empty-text)))))
                             
                             (t
                                     
                              (with-graph-drawing-process
                                         
                                  taxonomy-tab

                                (:process
                                 (install-taxonomy-pane))
                                (:cancel
                                 (next-update-layout-taxonomy)
                                 (schedule-update-no-history-entry :taxonomy))))))))


(defun rescroll-taxonomy-pane ()
  (with-profile-access
    (let ((x (copy-list taxonomy-scroll)))
      (handler-case 
          (when (and x taxonomy-graph-pane)
            (scroll taxonomy-graph-pane
                    :pan :move
                    x))
        (error (error) (beep-pane))))))


;;;
;;;
;;;

(defun install-abox-graph-pane ()
  (with-profile-access
    (with-sirius-slots (abox-graph-tab) 

                       (setf (layout-description abox-graph-tab)
                             (remove nil 
                                     (list (when show-status-pane
                                             'state-pane1)
                                           'navi-pane
                                           abox-graph-graph-pane
                                           'abox-graph-buttons1
                                           'abox-graph-buttons2
                                           'abox-graph-buttons3
                                           'abox-graph-buttons4
                                           (when show-info-pane
                                             'info-pane))))

                       (update-push-buttons))))


(defun establish-abox-graph-pane (&rest args)
  (declare (ignore args))

  (with-sirius-slots (abox-graph-tab)
                     (with-profile-access
                       (when (and abox-graph
                                  (not abox-graph-graph-pane))
                         (request-abox-graph-pane))

                       (cond ((not (graph-pane-roots abox-graph-graph-pane)) ; empty? 
                              
                              (apply-in-pane-process app
                                                     #'(lambda ()
                                                                
                                                         (setf (layout-description abox-graph-tab)
                                                               (remove nil 
                                                                       (list (when show-status-pane 
                                                                               'state-pane1)
                                                                             'navi-pane
                                                                             *abox-graph-info-pane*
                                                                             'abox-graph-buttons1
                                                                             'abox-graph-buttons2
                                                                             'abox-graph-buttons3
                                                                             'abox-graph-buttons4
                                                                             (when show-info-pane
                                                                               'info-pane))))

                                                         (setf (editor-pane-text *abox-graph-info-pane*)
                                                               (get-abox-graph-empty-text)))))
                                    
                             (t
                                     
                              (with-graph-drawing-process

                                  abox-graph-tab

                                (:process
                                 (install-abox-graph-pane))
                                (:cancel
                                 (next-update-layout-abox-graph)
                                 (schedule-update-no-history-entry :abox-graph))))))))



(defun rescroll-abox-graph-graph-pane ()
  (with-profile-access
    (let ((x (copy-list abox-graph-scroll)))
      (handler-case 
          (when (and x abox-graph-graph-pane)
            (scroll abox-graph-graph-pane
                    :pan :move
                    x))
        (error (error) (beep-pane))))))

;;;
;;;
;;;

(defun update-individuals-pane ()
  (with-sirius-app (app)
    (with-slots (individuals-pane individuals-tab) app
      (with-profile-access      
        (with-auto-tab
          (let* ((items (racer-function1 (all-individuals sirius-current-abox) :error))
                 (items (if (eq items :error)
                            (list items)
                          (remove-if #'(lambda (x) 
                                         (and selected-only
                                              (not (selected-individual? x))))
                                     items)))
                 (items 
                  (msort items
                         #'compare-fn-individuals )))
                                    
            (apply-in-pane-process individuals-pane 

                                   ;;; wichtig! sonst update-probleme aufm mac!
                                   ;;; ist notwendig, weil Prozesse fuer diese und die
                                   ;;; folgenden update-Funkionen gestartet werden
                                   ;;; (anders als bei den Graph-Panes)! 

                                   #'(lambda ()

                                       (cond (items

                                              (setf (layout-description individuals-tab)
                                                    (remove nil
                                                            (list (when show-status-pane
                                                                    'state-pane1)
                                                                  'navi-pane
                                                                  'individuals-pane
                                                                  'individuals-buttons1
                                                                  (when show-info-pane
                                                                    'info-pane))))

                                              (setf (collection-items individuals-pane) 
                                                    items)
                                              
                                              (setf (choice-selected-items individuals-pane)
                                                    (profile-all-selected-individuals)))

                                             (t

                                              (setf (layout-description individuals-tab)
                                                    (remove nil
                                                            (list (when show-status-pane
                                                                    'state-pane1)
                                                                  'navi-pane
                                                                  *abox-graph-info-pane*
                                                                  'individuals-buttons1
                                                                  (when show-info-pane
                                                                    'info-pane))))

                                              (setf (editor-pane-text *abox-graph-info-pane*)
                                                    (get-individuals-pane-empty-text))))))))))))
                
(defun update-concepts-pane ()
  (with-sirius-app (app)
    (with-slots (concepts-pane concepts-tab) app
      (with-profile-access
        (with-auto-tab
          (let* ((items
                  (case concept-list-mode 
                    (:all
                     (racer-function1 (all-atomic-concepts sirius-current-tbox) :error))

                    (:defined 
                     (let ((concepts 
                            (racer-function1 (all-atomic-concepts sirius-current-tbox)
                                             :error)))

                       (remove-if #'(lambda (x) 
                                      (let ((x (first (ensure-list x))))
                                        (racer-function1 (concept-is-primitive-p x sirius-current-tbox))))
                                  concepts)))
                                              
                    (:primitive 
                     (let ((concepts
                            (racer-function1 (all-atomic-concepts sirius-current-tbox)
                                             :error)))
                         
                       (remove-if-not #'(lambda (x) 
                                          (let ((x (first (ensure-list x))))
                                            (racer-function1 (concept-is-primitive-p x sirius-current-tbox))))
                                      concepts)))
                                                  
                    (:unsatisfiable
                     (racer-function1 (atomic-concept-synonyms
                                       (if alisp-racer
                                           'racer-user::BOTTOM
                                         'racer-user::|bottom|) 
                                       sirius-current-tbox)
                                      :error))))

                 (items (if (eq items :error)
                            (list items)
                          (remove-if #'(lambda (x) 
                                         (and selected-only
                                              (not (selected-concept? x))))
                                     items)))

                 (items (msort items #'compare-fn-concepts)))

            (apply-in-pane-process concepts-pane
                                   #'(lambda ()
                                       (cond (items

                                              (setf (layout-description concepts-tab)
                                                    (remove nil
                                                            (list (when show-status-pane
                                                                    'state-pane1)
                                                                  'navi-pane
                                                                  'concepts-pane
                                                                  'concepts-buttons1
                                                                  'concepts-buttons2
                                                                  (when show-info-pane
                                                                    'info-pane))))

                                              (setf (collection-items concepts-pane) 
                                                    items)

                                              (setf (choice-selected-items concepts-pane)
                                                    (profile-all-selected-concepts)))
                                             
                                             (t
                                              
                                              (setf (layout-description concepts-tab)
                                                    (remove nil
                                                            (list (when show-status-pane
                                                                    'state-pane1)
                                                                  'navi-pane
                                                                  *taxonomy-info-pane* 
                                                                  'concepts-buttons1
                                                                  'concepts-buttons2
                                                                  (when show-info-pane
                                                                    'info-pane))))

                                              (setf (editor-pane-text *taxonomy-info-pane*)
                                                    (get-concepts-pane-empty-text))))))))))))


(defun update-roles-pane ()
  (with-sirius-app (app)
    (with-slots (roles-pane roles-tab) app
      (with-profile-access
        (with-auto-tab
          (let* ((items 
                  (ecase role-list-mode
                                                       
                    (:roles
                     (racer-function1 (all-roles sirius-current-tbox) :error))
                    
                    (:unsatisfiable
                     (let ((roles (racer-function1 (all-roles sirius-current-tbox) :error)))
                       (if (eq roles :error)
                           :error
                         (remove-if #'(lambda (role) (racer-function1 (role-satisfiable-p role sirius-current-tbox))) roles))))
                    
                    (:transitive-roles
                     (racer-function1 (all-transitive-roles sirius-current-tbox) :error))
                     
                    (:features 
                     (racer-function1 (all-features sirius-current-tbox) :error))
                     
                    (:cd-attributes 
                     (racer-function1 (all-attributes sirius-current-tbox) :error))
                     
                    (:datatype-properties
                     (let ((roles 
                            (racer-function1
                             (all-roles sirius-current-tbox)
                             :error)))

                       (if (eq roles :error) 
                           :error
                         (remove-if-not #'(lambda (x) 
                                            (and (symbolp x)
                                                 (racer-function1
                                                  (role-used-as-datatype-property-p x sirius-current-tbox))))
                                        roles))))

                    (:annotation-properties
                     (let ((roles 
                            (racer-function1
                             (all-roles sirius-current-tbox)
                             :error)))
                                               
                       (if (eq roles :error)
                           :error
                         (remove-if-not #'(lambda (x) 
                                            (and (symbolp x)
                                                 (racer-function1
                                                  (role-used-as-annotation-property-p x sirius-current-tbox))))
                                        (remove-inv roles)))))))

                 (items (if (eq items :error)
                            (list items)
                          (if selected-only
                              (remove-if-not #'selected-role? items)
                            items)))

                 (items
                  (if selected-first
                      (let ((selected-items
                             (msort 
                              (remove-if-not #'selected-role? items)
                              #'compare-fn-roles))
                                                                                 
                            (other-items 
                             (msort
                              (remove-if #'selected-role? items)
                              #'compare-fn-roles)))
                                                                             
                        (nconc selected-items
                               other-items))

                    (msort items #'compare-fn-roles))))
                                    
            (apply-in-pane-process roles-pane
                                   #'(lambda ()
                                       (let ((pitems 
                                              (mapcar #'(lambda (role)
                                                          (list role
                                                                (if (consp role)
                                                                    (second role)
                                                                  (let ((role
                                                                         (racer-function1
                                                                          (atomic-role-inverse role sirius-current-tbox))))
                                                                    (unless (consp role) 
                                                                      role)))
                                                                (racer-function1
                                                                 (atomic-role-domain role sirius-current-tbox))
                                                                (racer-function1
                                                                 (atomic-role-range role sirius-current-tbox))
                                                                (racer-function1
                                                                 (atomic-role-parents role sirius-current-tbox))))

                                                      (remove-if #'consp
                                                                 items))))

                                         (cond (pitems

                                                (setf (layout-description roles-tab)
                                                      (remove nil
                                                              (list (when show-status-pane
                                                                      'state-pane1)
                                                                    'navi-pane
                                                                    roles-pane
                                                                    'roles-buttons1
                                                                    'roles-buttons2
                                                                    (when show-info-pane
                                                                      'info-pane))))

                                                (setf (collection-items roles-pane) pitems)
                                                
                                                (setf (choice-selected-items roles-pane) 
                                                      (remove-if-not #'selected-role? pitems
                                                                     :key #'first)))

                                               (t

                                                (setf (layout-description roles-tab)
                                                      (remove nil
                                                              (list (when show-status-pane
                                                                      'state-pane1)
                                                                    'navi-pane
                                                                    *role-hierarchy-info-pane*
                                                                    'roles-buttons1
                                                                    'roles-buttons2
                                                                    (when show-info-pane
                                                                      'info-pane))))
                                                                                           
                                                (setf (editor-pane-text *role-hierarchy-info-pane*)
                                                      (get-roles-pane-empty-text)))))))))))))
    

(defun get-all-assertions ()
  (with-profile-access
    (ecase assertion-list-mode
                                           
      (:concept-assertions
       (msort (remove-if-not #'(lambda (x)
                                 (or (eq x :error)
                                     (and (let ((x (second x)))
                                            (ecase taxonomy-root-kind 
                                              (:all t)
                                              (:selected (selected-concept? x))
                                              (:current (eq x current-concept))))
                                          (let ((from (first x)))
                                            (ecase abox-graph-root-kind 
                                              (:all t)
                                              (:selected (selected-individual? from))
                                              (:current (eq from current-individual)))))))
                             (ensure-list
                              (racer-function1
                               (all-concept-assertions sirius-current-abox))))
              #'string<
              :key #'(lambda (x) (symbol-name-1 (first x)))))

      (:role-assertions
       (msort (remove-if-not #'(lambda (x) 
                                 (or (eq x :error)
                                     (and (let ((x (second x)))
                                            (ecase role-hierarchy-root-kind 
                                              (:all t)
                                              (:selected (selected-role? x))
                                              (:current (eq x current-role))))
                                          (let ((from (first (first x)))
                                                (to (second (first x))))
                                            (ecase abox-graph-root-kind 
                                              (:all t)
                                              (:selected (or (selected-individual? from)
                                                             (selected-individual? to)))
                                              (:current (or (eq from current-individual)
                                                            (eq to current-individual))))))))
                                                                                             
                             (ensure-list 
                              (racer-function1
                               (all-role-assertions sirius-current-abox))))
              #'string<
              :key #'(lambda (x) (format nil "~S" x))))

      (:same-as-assertions
       (msort (remove-if-not #'(lambda (x) 
                                 (or (eq x :error)
                                     (and (let ((from (second x))
                                                (to (third x)))
                                            (ecase abox-graph-root-kind 
                                              (:all t)
                                              (:selected (or (selected-individual? from)
                                                             (selected-individual? to)))
                                              (:current (or (eq from current-individual)
                                                            (eq to current-individual))))))))
                                                                                             
                             (ensure-list 
                              (racer-function1
                               (all-same-as-assertions sirius-current-abox))))
              #'string<
              :key #'(lambda (x) (format nil "~S" x))))
                                
      (:different-from-assertions
       (msort (remove-if-not #'(lambda (x) 
                                 (or (eq x :error)
                                     (and (let ((from (second x))
                                                (to (third x)))
                                            (ecase abox-graph-root-kind 
                                              (:all t)
                                              (:selected (or (selected-individual? from)
                                                             (selected-individual? to)))
                                              (:current (or (eq from current-individual)
                                                            (eq to current-individual))))))))
                                                                                             
                             (ensure-list 
                              (racer-function1
                               (all-different-from-assertions sirius-current-abox))))
              #'string<
              :key #'(lambda (x) (format nil "~S" x))))
                                

      (:attribute-assertions
       (msort (remove-if-not #'(lambda (x) 
                                 (or (eq x :error)
                                     (let ((from (second x))
                                           (to (third x))
                                           (role (fourth x)))
                                       (and (ecase role-hierarchy-root-kind 
                                              (:all t)
                                              (:selected (selected-role? role))
                                              (:current (eq role current-role)))
                                            (ecase abox-graph-root-kind 
                                              (:all t)
                                              (:selected (or (selected-individual? from)
                                                             (selected-individual? to)))
                                              (:current (or (eq from current-individual)
                                                            (eq to current-individual))))))))
                                                                  
                             (ensure-list
                              (racer-function1
                               (all-attribute-assertions sirius-current-abox))))

              #'string<
              :key #'(lambda (x) (format nil "~S" x))))

      (:constraint-assertions
       (msort (ensure-list (racer-function1 (all-constraints sirius-current-abox)))
              #'string< 
              :key #'(lambda (x) (format nil "~S" x))))

      (:annotation-concept-assertions
       (msort (remove-if-not #'(lambda (x)
                                 (or (eq x :error)
                                     (and (let ((x (second x)))
                                            (ecase taxonomy-root-kind 
                                              (:all t)
                                              (:selected (selected-concept? x))
                                              (:current (eq x current-concept))))
                                          (let ((from (first x)))
                                            (ecase abox-graph-root-kind 
                                              (:all t)
                                              (:selected (selected-individual? from))
                                              (:current (eq from current-individual)))))))
                             (ensure-list
                              (racer-function1
                               (all-annotation-concept-assertions sirius-current-abox))))
              #'string<
              :key #'(lambda (x) (symbol-name-1 (first x)))))

      (:annotation-role-assertions
       (msort (remove-if-not #'(lambda (x) 
                                 (or (eq x :error)
                                     (and (let ((x (second x)))
                                            (ecase role-hierarchy-root-kind 
                                              (:all t)
                                              (:selected (selected-role? x))
                                              (:current (eq x current-role))))
                                          (let ((from (first (first x)))
                                                (to (second (first x))))
                                            (ecase abox-graph-root-kind 
                                              (:all t)
                                              (:selected (or (selected-individual? from)
                                                             (selected-individual? to)))
                                              (:current (or (eq from current-individual)
                                                            (eq to current-individual))))))))
                             (ensure-list
                              (racer-function1
                               (all-annotation-role-assertions sirius-current-abox))))
              #'string<
              :key #'(lambda (x) (format nil "~S" x)))))))

         
(defun update-assertions-pane ()
  (with-sirius-app (app)
    (with-slots (assertions-pane assertions-tab) app
      (with-profile-access
        (with-auto-tab
          (let* ((items 
                  (get-all-assertions))
                 (items 
                  (remove-if #'(lambda (x)
                                 (and selected-only
                                      (not (selected-assertion?  
                                            (list assertion-list-mode x)))))
                             items))


                 (selected-items 
                  (remove-if-not #'(lambda (x) 
                                     (selected-assertion? 
                                      (list assertion-list-mode 
                                            x)))
                                 items))
                                        
                 (other-items 
                  (remove-if #'(lambda (x) 
                                 (selected-assertion? 
                                  (list assertion-list-mode 
                                        x)))
                             items)))
                                        
                     
            (apply-in-pane-process assertions-pane
                                   #'(lambda ()

                                       (let ((items
                                              (if selected-first
                                                  (nconc selected-items other-items)
                                                items)))

                                         (cond (items

                                                (setf (layout-description assertions-tab)
                                                      (remove nil
                                                              (list (when show-status-pane
                                                                      'state-pane1)
                                                                    'navi-pane
                                                                    'assertions-pane
                                                                    'assertions-buttons1
                                                                    'assertions-buttons3
                                                                    'assertions-buttons2
                                                                    (when show-info-pane
                                                                      'info-pane))))
                                                
                                                (setf (collection-items assertions-pane)
                                                      items)

                                                (setf (choice-selected-items assertions-pane)
                                                      (mapcar #'second
                                                              (profile-all-selected-assertions))))
                                               
                                               (t
                                                               
                                                (setf (layout-description assertions-tab)
                                                      (remove nil
                                                              (list (when show-status-pane
                                                                      'state-pane1)
                                                                    'navi-pane
                                                                    *abox-graph-info-pane*
                                                                    'assertions-buttons1
                                                                    'assertions-buttons3
                                                                    'assertions-buttons2
                                                                    (when show-info-pane
                                                                      'info-pane))))

                                                (setf (editor-pane-text *abox-graph-info-pane*)
                                                      (get-assertions-pane-empty-text)))))))))))))


(defun update-axioms-pane ()
  (with-sirius-app (app)
    (with-slots (axioms-pane reasoner-selection-panel 
                             ontology-selection-panel
                             axioms-selector) app
                              
      (with-profile-access

        (with-auto-tab
          
          (let ((scroll
                 (get-scroll-position axioms-pane :vertical)))

            (apply-in-pane-process 
             axioms-pane
             #'(lambda ()
                 (setf (collection-items axioms-pane) 
                       (list (list nil 'PLEASE-WAIT nil)))
                 (setf (choice-selected-item axioms-pane) nil)))

            (let* ((items nil)
                                         
                   (types1 current-axiom-types)

                   (show-loaded1-p (member 'racer-user::|Loaded Axiom| current-axiom-types))
                 
                   (show-unloaded1-p (member 'racer-user::|Unloaded Axiom| current-axiom-types))

                   (show-loaded-p (and (or show-loaded1-p (not (or show-loaded1-p show-unloaded1-p)))
                                       types1))
                 
                   (show-unloaded-p (and (or show-unloaded1-p (not (or show-loaded1-p show-unloaded1-p)))
                                         types1))
                                         
                   (types (mapcar #'(lambda (x) 
                                      (intern 
                                       (concatenate 'string "OWL" 
                                                    (symbol-name x))
                                       :racer-user))
                                  (remove 'racer-user::|Unloaded Axiom|
                                          (remove 'racer-user::|Loaded Axiom| types1))))

                   (loaded-axioms
                    (when current-ontology
                      (when show-loaded-p
                        (racer-function1 
                         (owlapi-get-axioms-of-type-in types current-ontology current-reasoner t :loaded)
                         nil))))
                 
                   (unloaded-axioms
                    (when current-ontology
                      (when show-unloaded-p
                        (racer-function1 
                         (owlapi-get-axioms-of-type-in types current-ontology current-reasoner t :unloaded)
                         nil))))

                   (hash (make-hash-table :size (length loaded-axioms) :test #'eql))

                   (loaded-axioms 
                    (remove-if #'(lambda (x) 
                                   (and selected-only
                                        (not (selected-axiom? x))))
                               loaded-axioms))
                 
                   (unloaded-axioms 
                    (remove-if #'(lambda (x) 
                                   (and selected-only
                                        (not (selected-axiom? x))))
                               unloaded-axioms))

                   (selected-loaded-axioms
                    (remove-if-not #'selected-axiom? loaded-axioms))
                 
                   (selected-unloaded-axioms
                    (remove-if-not #'selected-axiom? unloaded-axioms))

                   (other-loaded-axioms 
                    (remove-if #'selected-axiom? loaded-axioms))

                   (other-unloaded-axioms 
                    (remove-if #'selected-axiom? unloaded-axioms))

                   (axioms 
                    (if selected-first
                        (nconc 
                       
                         (sort 
                          (nconc 
                           (when show-loaded-p
                             selected-loaded-axioms)
                           (when show-unloaded-p
                             selected-unloaded-axioms))
                          #'(lambda (x y)
                              (let ((cur-id (first current-axiom))
                                    (x-id (first x))
                                    (y-id (first y)))
                                (cond ((and cur-id (= x-id cur-id))
                                       t)
                                      ((and cur-id (= y-id cur-id))
                                       nil)
                                      (t (< x-id y-id))))) )
                              
                         (sort 
                          (nconc
                           (when show-loaded-p
                             other-loaded-axioms)
                           (when show-unloaded-p
                             other-unloaded-axioms))
                          #'< :key #'first))

                      (sort 
                       (nconc 
                        (when show-loaded-p
                          selected-loaded-axioms)
                        (when show-unloaded-p
                          selected-unloaded-axioms)
                        (when show-loaded-p
                          other-loaded-axioms)
                        (when show-unloaded-p
                          other-unloaded-axioms))
                       #'< :key #'first))))

              (dolist (ax loaded-axioms)
                (setf (gethash (first ax) hash) t))

              (dolist (axiom (nreverse axioms))
                (let* ((id (first axiom))
                       (axiom (second axiom))
                       (lambda 
                               (remove "&OPTIONAL"
                                       (gethash (symbol-name (first axiom)) +lambda-hash+)
                                       :test #'string-equal))
                       (type (intern (symbol-name (first axiom))))
                       (lambda-ax (mapcar #'(lambda (lambda ax)
                                              (list lambda ax))
                                          (cons (first axiom) (butlast lambda)) ; reasoner und optional weg
                                          (cons nil (butlast (rest axiom))))))

                  (loop as (lambda ax) in (reverse lambda-ax) do

                        (if ax
                            (push (list id (intern lambda) ax) items)
                          (progn 
                            (push (list 
                                   id
                                   (intern 
                                    (subseq (symbol-name type) 13))
                                   (if (eq id (first current-axiom))
                                       (if (gethash id hash)
                                           'loaded-current-axiom
                                         'unloaded-current-axiom)
                                     (if (gethash id hash)
                                         'loaded
                                       'unloaded))
                                   t) ; Marker f. Axiom-Beginn
                                  items))))

                  (push (list nil nil nil) items)))

              (pop items)

              ;;;
              ;;;
              ;;;

              (when (and (not items)
                         (not editor-eval-maintain-axioms)
                         (not (member :no-axioms *messages-shown*)))
                (push :no-axioms *messages-shown*)
                (warning-message "No OWLAPI axioms were found. Make sure the
correct \"Reasoner Container\" is selected, and also the correct 
\"Ontology Container\" from that reasoner container. Perhaps no 
OWLAPI axioms have been created in this ontology container? 
In case the ontology container was created via \"owl-read-file\", make
sure that \":maintain-owlapi-axioms t\" was supplied.")) 

              ;;;
              ;;;
              ;;;

              (apply-in-pane-process 
               axioms-pane
               #'(lambda ()
                   ;;; erzwinge Redisplay:
                   (setf (collection-items axioms-selector) 
                         (collection-items axioms-selector))

                   (setf (choice-selected-items axioms-selector) types1)
                                           
                   (setf (collection-items reasoner-selection-panel)
                         (racer-function1
                          (owlapi-get-reasoners)
                          nil))
                                           
                   (setf (choice-selected-item reasoner-selection-panel)
                         current-reasoner)

                   (setf (simple-pane-enabled reasoner-selection-panel) 
                         (when current-reasoner t))                                    

                   (setf (collection-items ontology-selection-panel)
                         (let ((loaded 
                                (racer-function1 
                                 (owlapi-get-loaded-ontologies current-reasoner) nil)))
                           (mapcar #'(lambda (ont)
                                       (list ont
                                             (if (member ont loaded)
                                                 "loaded"
                                               "not loaded")))
                                   (racer-function1
                                    (owlapi-get-ontologies current-reasoner)
                                    nil))))

                   (setf (choice-selected-item ontology-selection-panel)
                         current-ontology)

                   (setf (simple-pane-enabled ontology-selection-panel) 
                         (when current-ontology t))
                                    
                   (setf (collection-items axioms-pane) items)
               
                   (setf (choice-selected-items axioms-pane)
                         (remove-if-not #'(lambda (ax)
                                            (or (and (selected-axiom? ax)
                                                     (fourth ax))
                                                ;;;(equal ax current-axiom)
                                                ))
                                        items))

                   (unless selected-first 
                     (scroll axioms-pane :vertical :move scroll)))))))))))


;;;
;;;
;;; 

(defun update-query-input-pane ()
  (with-sirius-app (app)
    (with-slots (query-result-pane) app
      (with-profile-access
        (let ((res nil)
              (count 0))

          (dolist (tuple current-query-result)
            (incf count)
            (dolist (var-val tuple)
              (cond ((consp var-val)
                     (if (cdr var-val)
                         (push (cons count var-val) res)
                       (push (cons count (cons '? var-val)) res)))
                    (t 
                     (push (list count '? var-val) res))))
            ;; (push (list "" "") res)
            )

          (setf res (nreverse res))

          (apply-in-pane-process query-result-pane
                                 #'(lambda ()
                                     (setf (collection-items query-result-pane)
                                           res)
                                       
                                     (setf (choice-selected-items query-result-pane) 
                                           (union (profile-all-selected-individuals)
                                                  (profile-all-selected-concepts))))))))))

                                  
(defun update-query-pane ()
  (with-sirius-app (app)
    (with-slots (queries-pane) app
      (with-profile-access
        (with-auto-tab
          (without-caching
            (let* ((items1 
                    (append 
                     (ecase query-list-mode
                       (:all
                        (racer-function1 (all-queries :abox sirius-current-abox :type-of-substrate :all)))

                       (:accurate
                        (racer-function1 (accurate-queries :abox sirius-current-abox :type-of-substrate :all)))

                       (:inaccurate
                        (racer-function1 (inaccurate-queries :abox sirius-current-abox :type-of-substrate :all)))

                       (:running
                        (racer-function1 (running-queries :abox sirius-current-abox :type-of-substrate :all)))

                       (:waiting
                        (racer-function1 (waiting-queries :abox sirius-current-abox :type-of-substrate :all)))

                       (:cheap-waiting
                        (racer-function1 (waiting-cheap-queries :abox sirius-current-abox :type-of-substrate :all)))

                       (:expensive-waiting
                        (racer-function1 (waiting-expensive-queries :abox sirius-current-abox :type-of-substrate :all)))

                       (:processed
                        (racer-function1 (processed-queries :abox sirius-current-abox :type-of-substrate :all)))

                       (:ready
                        (racer-function1 (ready-queries :abox sirius-current-abox :type-of-substrate :all))))))

                   (items1 (mapcar #'(lambda (id)
                                       (if (eq id :error)
                                            
                                           (list :error 
                                                 :error 
                                                 :error 
                                                 :error 
                                                 :error )
                                                  
                                         (let ((query 
                                                (racer-function1 (describe-query id nil))))

                                           (if (eq query :error)
                                                
                                               (list :error 
                                                     :error 
                                                     :error 
                                                     :error 
                                                     :error )
                                         
                                             (list id
                                                   (second query)
                                                   (yes-or-no (racer-function1 (cheap-query-p id)))
                                                   (yes-or-no (racer-function1 (next-tuple-available-p id)))
                                                   (third query))))))

                                   (ensure-list items1)))

                   (items2 
                                            
                    (ecase  query-list-mode ;;; rule-list-mode wird nicht mehr benoetigt 
                      (:all 
                       (racer-function1 (all-rules :abox sirius-current-abox :type-of-substrate :all)))

                      (:accurate
                       (racer-function1 (accurate-rules :abox sirius-current-abox :type-of-substrate :all)))

                      (:inaccurate
                       (racer-function1 (inaccurate-rules :abox sirius-current-abox :type-of-substrate :all)))

                      (:running
                       (racer-function1 (running-rules :abox sirius-current-abox :type-of-substrate :all)))

                      (:waiting
                       (racer-function1 (waiting-rules :abox sirius-current-abox :type-of-substrate :all)))

                      (:cheap-waiting
                       (racer-function1 (waiting-cheap-rules :abox sirius-current-abox :type-of-substrate :all)))

                      (:expensive-waiting
                       (racer-function1 (waiting-expensive-rules :abox sirius-current-abox :type-of-substrate :all)))

                      (:processed
                       (racer-function1 (processed-rules :abox sirius-current-abox :type-of-substrate :all)))

                      (:ready
                       (racer-function1 (ready-rules :abox sirius-current-abox :type-of-substrate :all)))))

                   (items2 (mapcar #'(lambda (id) 
                                       (if (eq id :error)
                                           (list :error
                                                 :error
                                                 :error
                                                 :error
                                                 :error)
                                         (let ((rule (racer-function1 (describe-rule id nil))))
                                           (if (eq rule :error)
                                               (list :error
                                                     :error
                                                     :error
                                                     :error
                                                     :error)    
                                             (list id
                                                   (second rule)
                                                   (yes-or-no (racer-function (cheap-rule-p id)))
                                                   (yes-or-no (racer-function 
                                                               (next-set-of-rule-consequences-available-p 
                                                                id)))
                                                   (third rule))))))
                                   (ensure-list items2))))

              (apply-in-pane-process queries-pane
                                     #'(lambda ()
                                         (setf (collection-items queries-pane)
                                               (append items1 items2))
                                                                 
                                         (setf (choice-selected-items queries-pane)
                                               current-query-or-rule))))))))))


;;;
;;;
;;;       

(defun update-defined-queries-pane ()
  (with-sirius-app (app)
    (with-slots (defined-queries-pane) app
      (with-profile-access      
        (with-auto-tab
          (let ((items 
                 (racer-function1
                  (describe-all-definitions :tbox sirius-current-tbox :error-p nil)
                  :error)))
            (apply-in-pane-process defined-queries-pane 
                                   #'(lambda ()
                                       (let ((items 
                                              (if (eq items :error)
                                                  nil
                                                (msort 
                                                 items
                                                 #'string<
                                                 :key #'(lambda (x)
                                                          (symbol-name (second x)))))))
                                                                   
                                         (setf (collection-items defined-queries-pane) items
                                               (choice-selected-items defined-queries-pane)
                                               (remove-if-not #'(lambda (x) 
                                                                  (and (comp-fn (second x)
                                                                                (first current-definition))
                                                                       (equal (third x) 
                                                                              (second current-definition))))
                                                              items)))))))))))


;;;
;;;
;;;

(defun update-server-pane (&optional force-p)
  (let ((profile (active-profile)))
    (when profile
      (unless (dont-update-server-pane profile)
        (with-sirius-app (app)
          (with-slots (connect-button server-pane) app

            (unless (equalp 
                     (collection-items server-pane)
                     *sirius-profiles*)

              (setf (collection-items server-pane)
                    *sirius-profiles*))

            (unless (eq (choice-selected-item server-pane)
                        profile)
              (setf (choice-selected-item server-pane) profile))
          
            (let ((new-text 
                   (if (connection-was-established-p)
                       "Disconnect "
                     "Connect"))
                
                  (old-text
                   (slot-value connect-button 'capi-internals::text)))


              ;;; why can this happen? 

              (unless (stringp old-text) 
                (setf old-text "?"))

              (unless (stringp new-text) 
                (setf new-text "?"))    
          
          
              (when (or (not (string-equal old-text new-text))
                        force-p)

                (setf (capi:item-text connect-button) 
                      new-text)))))))))



(defun update-state-display ()
  (with-sirius-app (app)
    (let ((profile (active-profile)))
      (when (profile-show-status-pane profile)
        (with-only-cache-lookup
          (without-history-entries

            (with-slots (cur-tbox-pane
                         cur-abox-pane
                         cur-concept-pane
                         cur-no-selected-concepts-pane
                         cur-individual-pane
                         cur-no-selected-individuals-pane
                         cur-role-pane
                         cur-no-selected-roles-pane
                         cur-query-or-rule-pane
                         cur-definition-pane
                         cur-namespace-pane
                         cur-history-pane
                         cur-profile-pane
                         cur-axiom-pane
                         cur-no-selected-axioms-pane
                         cur-reasoner-pane
                         cur-ontology-pane) app
      
              (with-profile-access

                ;;;
                ;;;
                ;;;
                  
                (setpc (title-pane-text cur-history-pane)
                       (if (profile-history profile)
                           (format nil "~A / ~A" 
                                   history-position
                                   (length (profile-history profile)))
                         ""))
      
                ;;;
                ;;;
                ;;;

                (with-pretty-printing ()

                                      (setpc (title-pane-text cur-tbox-pane)
                                             (line-item-printer-no-doublequotes sirius-current-tbox))
              
                                      (setpc (title-pane-text cur-abox-pane)
                                             (line-item-printer-no-doublequotes sirius-current-abox))
              
                                      (setpc (title-pane-text cur-concept-pane)
                                             (line-item-printer-no-doublequotes current-concept))

                                      (setpc (title-pane-text cur-no-selected-concepts-pane)
                                             (format nil "  ~A" 
                                                     (hash-table-count all-selected-concepts-hash)))
              
                                      (setpc (title-pane-text cur-individual-pane)
                                             (line-item-printer-no-doublequotes current-individual))
         
                                      (setpc (title-pane-text cur-axiom-pane)
                                             (line-item-printer-no-doublequotes
                                              (when current-axiom
                                                (format nil "~A" 
                                                        (first current-axiom)))))

                                      (setpc (title-pane-text cur-no-selected-axioms-pane)
                                             (format nil "  ~A" 
                                                     (hash-table-count all-selected-axioms-hash)))
               
                                      (setpc (title-pane-text cur-no-selected-individuals-pane)
                                             (format nil "  ~A" 
                                                     (hash-table-count all-selected-individuals-hash)))
              
                                      (setpc (title-pane-text cur-query-or-rule-pane)
                                             (line-item-printer-no-doublequotes current-query-or-rule))

                                      (setpc (title-pane-text cur-role-pane)
                                             (line-item-printer-no-doublequotes current-role))

                                      (setpc (title-pane-text cur-definition-pane)
                                             (line-item-printer-no-doublequotes (when current-definition
                                                                                  (cons (first current-definition)
                                                                                        (second current-definition)))))

                                      (setpc (title-pane-text cur-no-selected-roles-pane)
                                             (format nil "  ~A" 
                                                     (hash-table-count all-selected-roles-hash)))

                                      (setpc (title-pane-text cur-namespace-pane)
                                             (line-item-printer-no-doublequotes 
                                              sirius-current-namespace))
              
                                      (setpc (title-pane-text cur-profile-pane)
                                             (line-item-printer-no-doublequotes 
                                              (profile-name profile)))
         
                                      (setpc (title-pane-text cur-reasoner-pane)
                                             (line-item-printer-no-doublequotes 
                                              current-reasoner))
              
                                      (setpc (title-pane-text cur-ontology-pane)
                                             (line-item-printer-no-doublequotes current-ontology)))))))))))


