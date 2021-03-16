;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(defvar *command* nil)

(defun escape-spaces (string) 
  (let ((string (coerce string 'list))
        (res nil))
    (loop while string do 
          (let ((char (pop string)))
            (case char
              (#\space 
               (push #\\ res)
               (push char res))
              (otherwise
               (push char res)))))
    (coerce (reverse res) 'string)))

(defun escape-spaces-win (string) 
  string)


#+(or :lispworks5 :lispworks6)
(defun tokenize (string)
  (let ((pos (position #\space string)))
    (if pos
        (cons (subseq string 0 pos)
              (tokenize (subseq string (1+ pos))))
      (list string))))

(defun start-server (&optional (message-p t) (also-connect-if-specified-p t))
  (with-capi-synchronization
    (let ((profile (active-profile))
          (already-running nil))
      
      (with-slots (host 
                   port 
                   dig-port
                   unsafe
                   debug
                   no-http-console-log
                   verbose
                   dig-1-1
                   owllink
                   racerpro-executable 
                   logging-file
                   temp-directory
                   license-file
                   startup-arguments
                   auto-connect
                   server-started) profile

        (setf server-started nil)
	
	(when (or (not racerpro-executable)
		  (blank-line-p racerpro-executable)
		  (not (probe-file racerpro-executable)))
	  
	  (setf racerpro-executable
	    (find-racerpro-executable)))
	
        (cond ((or (not racerpro-executable)
		   (blank-line-p racerpro-executable)
                   (not (probe-file racerpro-executable)))
               
               (enter-dummy-command
                (format nil "Cannot find RacerPro Executable! Please specify path to RacerPro using \"Edit Profile -> RacerPro Executable\"!")
                :error)

               (return-from start-server :no-server))

              (t
              
               (if (open-connection t)

                   (progn
                     (when message-p
                       (sirius-message "I have not started ~A, since it is already running!" 
                                       (get-server-description)))
                     (enter-dummy-command
                      (format nil "Cannot start ~A, server is already running!" 
                              (get-server-description))
                      :error)
                     (setf already-running t)
                     (close-connection t))
               
                 (let* ((arguments 
                         `(("-license-file " ,license-file)
                           (" -p " ,port)
                           (" -host " ,host)
                           (" -http " ,dig-port)
                           (" -logging " ,logging-file)
                           (" -temp " ,temp-directory)))
             
                        (arguments 
                         (concatenate 'string
                                      (reduce #'(lambda (x y)
                                                  (concatenate 'string x y))
                                              (append
                                               (apply #'append
                                                      (mapcar #'(lambda (x)
                                                                  (list (first x)
                                                                        (format nil "~A" (second x))))
                                                              (remove-if #'(lambda (x) 
                                                                             (or (not x)
                                                                                 (string-equal x "")))
                                                                         arguments
                                                                         :key #'second)))
                                               `(,@(unless verbose
                                                     '(" -silent"))
                                                 ,@(when dig-1-1
                                                     '(" -dig11"))
                                                 ,@(when owllink
                                                     '(" -protocol owllink"))
                                                 ,@(when debug
                                                     '(" -debug"))
                                                 ,@(when unsafe
                                                     '(" -u"))
                                                 ,@(when no-http-console-log 
                                                     '(" -nohttpconsolelog")))))
                                      " "
                                      startup-arguments))
             
                        (server 
                         #+:win32 (format nil "~A"
                                          (substitute-slashes-with-backslashes 
                                           racerpro-executable))
              
                         #+(or :linux :mac)
                         (format nil "~A "
                                 (escape-spaces racerpro-executable))
                         )

                        (command #-:mac
                                 (concatenate 'string 
                                              server
                                              #-(or :lispworks6 :lispworks5)
                                              "+cm --"
                                              arguments)
                                 #+:mac
                                 (if (profile-start-in-terminal profile)
                                     (format nil
					     "osascript -e 'tell application \"Terminal\" to do script \"\\\"~A\\\" ~A\"'"
					     racerpro-executable ; We do not want to escape spaces here!
					     arguments)
                                   (concatenate 'string 
                                                server
                                                #-(or :lispworks6 :lispworks5)
                                                "+cm --"
                                                arguments))))
                 
                   (setf *command* command)

                   #+(or :linux :mac)
                   (let ((failed
                          #+:linux
                          (with-sirius-timeout
                           ( ;;+linux-or-mac-wait-for-server-start-until-success-assumed+ 
                            (profile-wait-for-started-server-before-connect-time profile)
                            :timeout)
                           (sys:call-system command
                                            :wait t
                                            #+(or :linux :mac) 
                                            :shell-type
                                            #+(or :linux :mac) 
                                            "/bin/sh"))
                          #+:mac
                          (if (profile-start-in-terminal profile)
                              (progn
                                (sys:call-system command
                                                 :wait t
                                                 #+(or :linux :mac) 
                                                 :shell-type
                                                 #+(or :linux :mac) 
                                                 "/bin/sh")
                                :timeout)
                            (with-sirius-timeout
                             ( ;;+linux-or-mac-wait-for-server-start-until-success-assumed+ 
                              (profile-wait-for-started-server-before-connect-time profile)
                              :timeout)
                             (sys:call-system command
                                              :wait t
                                              #+(or :linux :mac) 
                                              :shell-type
                                              #+(or :linux :mac) 
                                              "/bin/sh")))))
            
                     (enter-dummy-command (format nil "Shell Command: ~S" command)
                                          (cond ((eq failed :timeout)
                                                 ;; server running

                                                 (setf server-started t)

                                                 :okay)
                                                (t
                                                 (report-error 
                                                  (format nil "Shell command ~S failed!"
                                                          command))))))

                   #+:win32
                   (let ((res (handler-case 
                                  #+(or :lispworks5 :lispworks6)
                                (sys:call-system (cons server (cons "+cm" (cons "--" (tokenize arguments)))) :wait nil)
                                #-(or :lispworks5 :lispworks6)
                                (sys:call-system command :wait nil)
                                (error () nil))))

                     (when res
                       (setf server-started t))

                     (enter-dummy-command (format nil "Shell Command: ~S" command) 
                                          (if res 
                                              :okay
                                            (report-error
                                             (format nil "Shell command ~S failed!"
                                                     command)))))))
                  
               ;;;
               ;;;
               ;;;
          
               (sleep (profile-wait-for-started-server-before-connect-time profile))
               (clear-current-prompt-and-input)
          
               (when (and also-connect-if-specified-p 
                            (or server-started 
                                already-running)
                            auto-connect)
                   (open-connection t))))))))
  

(defun kill-server (&optional silent-p dont-ask-p)
  (if (string-lessp (profile-racer-version (active-profile))
                    "1.9.1")
      (report-error 
       (format nil "Sorry, the (kill-server) command is only supported in RacerPro > 1.9.0.~%Please use the operating system to kill ~A."
               (get-server-description (active-profile))))
    
    (let ((*use-cache* nil))

      (unless (profile-unsafe (active-profile))
        
        (report-error 
         (format nil "Sorry, (kill-server) command is only permitted when RacerPro is running in unsafe mode.~%Please use the operating system to kill ~A." (get-server-description (active-profile))))
        (return-from kill-server nil))
      
      (when (or dont-ask-p 
                (confirm-yes-or-no 
                 (format nil "Really Shutdown ~A?" (get-server-description))))

        (if silent-p 

            (ignore-errors
              (let* ((*package* (find-package :racer-user)))
                (format (profile-socket (active-profile)) "~A~%" 
                        (create-racer-call-string '(exit-server)))
                (close-connection t)
                (force-output (profile-socket (active-profile)))))

          (progn 
            (enter-result-command
             `(exit-server))
            (close-connection t)))))))

(defun show-license-details ()
  (let ((profile (active-profile)))
    (with-slots (racerpro-executable license-file) profile

      (if (or (not racerpro-executable)
              (blank-line-p racerpro-executable))
          (report-error (format nil 
                                "Please use the \"Edit Profile...\" button to specifiy the path to the 
RacerPro executable (and optionally, also the path to the .racerlicense file) 
for profile ~A, then try again." (profile-name (active-profile))))
        
        (let* ((temp-file
                #+:win32 (generate-temp-filename))

               (server 
                #+:win32 (format nil "\"~A\" +c -d \"~A\" -- "
                                 (escape-spaces-win
                                  (substitute-slashes-with-backslashes racerpro-executable))
                                 (escape-spaces-win 
                                  (substitute-backslashes-with-slashes temp-file)))

                #+(or :linux :mac) 
                (format nil "~A -- "
                        (escape-spaces racerpro-executable)))

               (command (concatenate 'string 
                                     server
                                     (if (and license-file 
                                              (not (blank-line-p license-file)))
                                         (concatenate 'string 
                                                      "-license-file " 
                                                      license-file)
                                       "")
                                     (format nil " -p 0 -http 0 -nowait -dump-license-info "))))

          #-:win32 (declare (ignorable temp-file))

          (setf *command*  command)

          #+(or :linux :mac)
          (multiple-value-bind (ret string)
              (sys:call-system-showing-output command
                                              :prefix nil
                                              :output-stream nil
                                              :wait 
                                              t
                                              #+(or :linux :mac) 
                                              :shell-type
                                              #+(or :linux :mac) 
                                              "/bin/sh")
            (declare (ignorable ret))
            
            (enter-dummy-command (format nil "Shell Command: ~S" command)
                                 string))
          
          #+:win32
          (progn
            
            (sys:call-system-showing-output command
                                            :prefix nil
                                            :output-stream nil
                                            :wait t)
            (handler-case 
            
                (enter-dummy-command (format nil "Shell Command: ~S" command)
                                     (let ((chars nil))
                                       (with-open-file (stream temp-file
                                                               :direction :input)
            
                                         (loop for char = (read-char stream nil nil)
                                               do
                                               (cond ((null char)
                                                      (return))
                                                     (t (push char chars))))

                                         (coerce (reverse chars) 'string))))

              (error ()
                (enter-dummy-command (format nil "Shell Command: ~S" command) 
                                     (report-error
                                      (format nil "Shell command ~S failed!"
                                              command)))))
                  
            
            (when (probe-file temp-file) 
              (delete-file temp-file))))))))





#|
osascript -e 'tell application "Terminal" to do script "\"/ 
Applications/RacerPro-1-9-2 beta/RacerPro\""'

|#

