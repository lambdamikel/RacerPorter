;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-
 
(in-package :cl-user)

(setf (logical-pathname-translations "sirius") 
      (list `("**;*.*" "z:/racer/sirius/**/*.*")))

(setf (logical-pathname-translations "sirius-dev") 
      (list `("**;*.*" "z:/racer/sirius/**/*.*")))

(load "sirius:define-system.lisp")

;;; (push :sonic *features*)

(require "comm")
(require "dynamic-complete")

(push :no-truncated-log-files *features*)

;; #-:mac 
;; (push :pulse-pane *features*)

;; doesn't work well enough
;; (push :progress-bar *features*)

#+(and (not :native-progress-bar) :progress-bar)
(push :progress-bar-pane *features*)

;;; fuer Versionen ohne Installer!
(push :ignore-previously-installed-racer-version *features*)

;;;#+:mac
(push :native-progress-bar *features*)

;;;
;;;
;;;

(define-system sirius
               #-:sirius-dev (:default-pathname "sirius:")
               #+:sirius-dev (:default-pathname "sirius:")
               (:serial
                "sirius-package"
                "version"
                "parameters"
                "specials"
                "fonts"
                
                "macros"

                "registry-new"
                "readtables"
                "messages"
                "manual"
                "io"
                "directories"
                "profiles"
                "printer"
                "tools"
                "process-pool"
                "gui-tools"
                "graph"
                "buttons"
                "sirius-connection"

                #+(and (not :sirius-dev) :pulse-pane) 
                "movie-data" 

                #+(and       :sirius-dev :pulse-pane) 
                "movies"

                #-:lispworks6 
                image-data
		
                "axiom-editor"
                "connection"
                "history"
                "selection"
                "sirius-capi-frame"
                "errors"
                "buffers"
                "pulse"
                "shell"
                "completion"
                "log"
                "loader"
                "command-processor"
                "updates"
                "role-hierarchy"
                "taxonomy"
                "abox-graph"
                "server"
                "logo"
                
                ;;; #+(and :image-data :linux) 
		;;; linux-image-data
                                
                #+:mac 
                "sirius-mac"
                
                "start"                

                #+:sirius-editor 
                sirius-editor))

(define-system sirius-editor
               #-:sirius-dev (:default-pathname "sirius:editor;")
               #+:sirius-dev (:default-pathname "sirius:editor;")
               (:serial
                "editor"
                #-:sirius-dev 
                "delete-commands"
                "xml-mode"
                "tilde"
                "editor-commands"))

(define-system image-data
    #+:racer-with-sirius (:default-pathname "sirius:racerplus-image-data;")
    #-:racer-with-sirius (:default-pathname "sirius:racerporter-image-data;")
    (:serial
     "image-data1"))

(define-system linux-image-data
    #+:racer-with-sirius (:default-pathname "sirius:racerplus-image-data;")
    #-:racer-with-sirius (:default-pathname "sirius:racerporter-image-data;")
    (:serial
     "image-data2"))


;;;
;;;
;;;

(defun racerporter-dev (&optional (force-p nil))

  (push :sirius *features*)
  (push :sirius-editor *features*)
  (push :sirius-dev *features*)

  (compile-system 'sirius :load t :force force-p)

  (princ "(sirius::sirius)"))

(racerporter-dev t)