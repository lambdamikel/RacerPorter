(in-package "CL-USER")

(load-all-patches)

(setf system::*sg-default-size* (* 100 16000))

(push :sirius-editor *features*)
(push :image-data *features*)

(setf (logical-pathname-translations "sirius")
      (list  '("**;*.*" "~/sirius/**/*.*")))

(setf (logical-pathname-translations "sirius-dev")
      (list  '("**;*.*" "~/sirius/**/*.*")))

(load "~/lispworks/define-system.lisp")

(princ (translate-logical-pathname "sirius:sirius-sysdcl.lisp"))

(load "sirius:sirius-sysdcl.lisp")

(compile-system 'sirius :load t :force t)

(deliver 'sirius::sirius
         "sirius-dev-exe"
	 0
         :packages-to-keep '("RACER-USER")
         :keep-editor t
         :editor-style :emacs
         :editor-commands-to-keep :full-editor
         :KEEP-PRETTY-PRINTER t 
	 :interface :capi
         :quit-when-no-windows t)

(quit)


