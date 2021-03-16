(in-package "CL-USER")

(load-all-patches)

(push :sirius-editor *features*)

(setf system::*sg-default-size* (* 100 16000))

(setf (logical-pathname-translations "sirius")
      (list  '("**;*.*" "~/sirius/**/*.*")))

(setf (logical-pathname-translations "sirius-dev")
      (list  '("**;*.*" "~/sirius/**/*.*")))

(load "~/lispworks/define-system.lisp")

(princ (translate-logical-pathname "sirius:sirius-sysdcl.lisp"))

(load "sirius:sirius-sysdcl.lisp")

(compile-system 'sirius :load t :force t)


#+:mac
(progn
  (load "sirius:macos-application-bundle.lisp")
  (let ((path (pathname
               (concatenate 'string (namestring (translate-logical-pathname "sirius:"))
                            "Sirius"))))
    
    (deliver 'sirius::sirius
             (write-macos-application-bundle 
              path          
              :template-bundle (pathname-location 
                                (concatenate 'string
                                             (namestring
                                              (translate-logical-pathname "sirius:templates;"))
                                             "RacerPorter.app/")))
             0
             :KEEP-PRETTY-PRINTER t 
             :packages-to-keep '("RACER-USER")
             :interface :capi
             :quit-when-no-windows t)

    ))


(quit)


