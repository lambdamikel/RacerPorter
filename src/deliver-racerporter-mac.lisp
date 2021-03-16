(in-package cl-user)

(setf system::*sg-default-size* (* 100 16000))

(setf (logical-pathname-translations "sirius-dev")
      (list  '("**;*.*" "/Users/mwe/sirius/**/*.*")))

(push :lracer *features*)
(pushnew :porter *features*)
(pushnew :sirius-editor *features*)

(defun create-directories (directory)
  (unless (consp directory)
    (setf directory (pathname-directory (pathname directory))))
  (let ((dir1 (list :absolute)))
    (loop for dir in (rest directory) do
          (setf dir1 (nconc  dir1 (list dir)))
          (let ((pathname (make-pathname :directory dir1)))
          (unless (probe-file pathname)
            (system:make-directory 
	     #+:win32 (concatenate 'string "c:" (namestring pathname))
	     #-:win32 pathname))))))


(load "~mwe/lispworks/define-system.lisp")
(load "sirius-dev:sirius-sysdcl.lisp")


(setf *features* (delete :racer-server *features*))

(compile-system 'sirius :load t :force t)

(load "sirius-dev:macos-application-bundle.lisp")

;;; ======================================================================

(deliver 'sirius::sirius
	 (write-macos-application-bundle 
          "~mwe/Desktop/RacerPorter"
          :template-bundle (pathname-location 
			    (concatenate 'string
			      (namestring
			       (translate-logical-pathname 
				"sirius-dev:templates;"))
                               "RacerPorter.app/")))
	 0
         :console :input
         :packages-to-keep '("RACER-USER")
         :keep-editor t
         :editor-style :emacs
         :editor-commands-to-keep :full-editor
         :KEEP-PRETTY-PRINTER t 
	 :interface :capi
         :quit-when-no-windows t)

(quit)
