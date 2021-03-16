(in-package "CL-USER")

(load-all-patches)

(setf (logical-pathname-translations "sirius")
      (list  '("**;*.*" "c:/sirius/**/*.*")))

(setf (logical-pathname-translations "sirius-dev")
      (list  '("**;*.*" "c:/sirius/**/*.*")))

(compile-file "sirius:test-app.lisp")

(load "sirius:test-app")

(deliver 'test-app 
         "test-app-exe"
         0
         :interface :capi)

(quit)


