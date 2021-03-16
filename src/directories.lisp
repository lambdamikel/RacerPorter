;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(defun sirius-directory () 
  #+(and :sirius-dev (not :mac))
  (make-pathname :directory (list :relative "sirius"))
  #+(and :sirius-dev :mac)
  (make-pathname :directory (list :absolute "Users" "mwe" "sirius"))
  #+(and :sirius-dev :win32)
  (make-pathname :directory (list :absolute "sirius"))

  #-:sirius-dev
  (harlequin-common-lisp:get-working-directory))


(defun find-racerpro-executable () 
  (let* ((dir (program-directory)))
    (when dir
      (let ((racer 
             (namestring
              (make-pathname
               :host
               (pathname-host dir)
               :directory
               (pathname-directory dir)
               :name
               #+:win32
               "RacerPro.exe"
               #-:win32
               "RacerPro"))))
	
	;; (display-message (format nil "DIR: ~A" racer))
	
        (when (probe-file racer)
          racer)))))

(defun program-directory () 
  #+:win32 
  (or 
   (let ((reg
          (let ((val
                 (win32::registry-value
                  "Software\\Classes\\Applications\\RacerPorter.exe\\shell\\open\\command"
                  ""
                  :errorp nil)))
            (declare (ignorable val))
            #+:ignore-previously-installed-racer-version
            (lw:current-pathname)
            #-:ignore-previously-installed-racer-version
            (if val
                (if (char= (elt val 0) #\")
                    (subseq val 1 (1- (length val)))
                  val)
              nil))))
     
     (when reg
       (namestring  
        (make-pathname
         :host 
         (pathname-host reg)
         :directory
         (pathname-directory reg)))))
   
   (concatenate 'string 
                (lw:environment-variable "programfiles")
                "\\"))

  #+:linux 
  (lw:current-pathname)

  #+:mac
  ;;; gehe vom Porter-Verzeichnis nach oben, um Racer zu finden
  (get-parent-directory
   (get-parent-directory
    (get-parent-directory
     (first system:*line-arguments-list*)))))

(defun get-parent-directory (dir)
  (make-pathname :directory (butlast (pathname-directory dir))))

(defun user-directory (&optional path) 
  (handler-case 
      (let ((dir 
             (or
              
              #+:win32
              (concatenate 'string 
                           (lw:environment-variable "userprofile")
                           "\\")
               
              (user-homedir-pathname)
               
              #+(or :mac :linux)
              (pathname "~/")
               
              ;;; nicht verwenden! fuehrt zum CAPI Deadlock!
              ;;; #+:mac (sys:get-folder-path :documents)

              )))

        (make-pathname 
         :directory (append (pathname-directory dir)
                            (ensure-list path))
         :host (pathname-host dir)))
    (error () nil)))

(defun sirius-temp-directory (&optional (probe-p t))
  (handler-case 
      (let ((name
             (namestring 
              (or (lw:environment-variable "TEMP")
                  #+(or :linux :mac) "/tmp/"
                  #+:win32 (user-directory "temp")))))
                  
                 
        (if probe-p 
            (probe-file name)
          name))
    (error () nil)))

#+:sirius-dev
(defun image-directory ()
  (or *image-directory*
      (setf *image-directory*
            (handler-case 
                (make-pathname 
                 :directory 
                 (append (pathname-directory 
                          (namestring (sirius-directory)))
                         '("images")))
              (error () nil)))))

;;;
;;;
;;; 

(defconstant +config-file+ 
  "profiles")

(defun profile-extension ()
  #+:racer-with-sirius
  "racerplus"
  #-:racer-with-sirius
  "racerporter")

(defun editor-config-extension ()
  "racereditor")

(defun sirius-profiles-file ()
  (let ((dir 
         (or
          #+(and :mac (not :racer-with-sirius))
          (pathname "~/Library/Preferences/com.racer-systems.racerporter/")
          #+(and :mac :racer-with-sirius)
          (pathname "~/Library/Preferences/com.racer-systems.racerplus/")
          #-:mac 
          nil
          (user-directory))))

    (make-pathname 
     :directory (pathname-directory dir)
     :host (pathname-host dir)
     :name +config-file+
     :type (profile-extension))))

(defconstant +sirius-logfile+ 
  #+:racer-with-sirius
  "racerplus-logfile"
  #-:racer-with-sirius
  "racerporter-logfile")

(defconstant +editor-configfile+ 
  "racereditor-config")

(defun sirius-logfile-file ()
  (let ((dir 
         (user-directory)))

    (make-pathname 
     :directory (pathname-directory dir)
     :host (pathname-host dir)
     :name +sirius-logfile+
     :type "log")))

(defun sirius-editor-configfile ()
  (let ((dir 
         (or
          #+(and :mac (not :racer-with-sirius))
          (pathname "~/Library/Preferences/com.racer-systems.racerporter/")
          #+(and :mac :racer-with-sirius)
          (pathname "~/Library/Preferences/com.racer-systems.racerplus/")
          #-:mac 
          nil
          (user-directory))))

    (make-pathname 
     :directory (pathname-directory dir)
     :host (pathname-host dir)
     :name +editor-configfile+
     :type (editor-config-extension))))