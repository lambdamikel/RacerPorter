;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(declaim (special cl-user::*racer-version* cl-user::*build-version*))

(defparameter +sirius-version+ 
  (if (boundp 'cl-user::*racer-version*)
      cl-user::*racer-version*
    ;; "Undefined"
    "2.0 preview"
    ))

(defparameter +build-version+ 
  (if (boundp 'cl-user::*build-version*)
      cl-user::*build-version*
    ;; "Undefined"
    "2013-06-16"))

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn

  #+(not :racer-with-sirius)
  (progn 
    (defparameter +sirius-name+ "RacerPorter")
    (push :porter *features*))
  
  #+:racer-with-sirius
  (defparameter +sirius-name+ "RacerPlus")))

(defconstant +editor-name+ "RacerEditor")

