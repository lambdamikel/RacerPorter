(in-package "CL-USER")

(load-all-patches)

(load-logical-pathname-translations "RACER")

(push :sirius *features*)

(load "racer:code;define-system.lisp")

(load "racer:sirius;sirius-sysdcl.lisp")


(compile-system 'sirius :load t :force t)

#-:mac
(deliver 'sirius::sirius 
         (translate-logical-pathname
          #+:win32 
          (format nil "racer:sirius;sirius-windows;sirius-~A.exe" 
                  (substitute #\- #\. (format nil "~A" sirius::+sirius-version+)))
          #+:linux
          (format nil "racer:sirius;sirius-linux;sirius-~A" 
                  (substitute #\- #\. (format nil "~A" sirius::+sirius-version+))))
         0
         :icon-file "racer:sirius;RacerPorter.ico"
         :KEEP-PRETTY-PRINTER t
         :packages-to-keep '("RACER-USER")
 	 :interface :capi)

#+:mac
(progn
  (load "racer:sirius;macos-application-bundle.lisp")
  (let ((path (pathname
               (concatenate 'string (namestring (translate-logical-pathname "racer:sirius;"))
                            "Sirius"))))
    
    (deliver 'sirius::sirius
             (write-macos-application-bundle 
              path          
              :template-bundle (pathname-location 
                                (concatenate 'string
                                             (namestring
                                              (translate-logical-pathname "racer:sirius;templates;"))
                                             "Sirius.app/")))
             0
             :KEEP-PRETTY-PRINTER t 
             :packages-to-keep '("RACER-USER")
             :interface :capi
             :quit-when-no-windows t)

    ))


(quit)
