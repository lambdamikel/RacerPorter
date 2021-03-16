;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CL-USER -*-
 
(in-package :cl-user)

#-:racer-with-sirius
(defpackage racer
  (:nicknames "racer"))

#-:racer-with-sirius
(defpackage racer-user
  (:nicknames "racer-user"))

(defpackage sirius 
  (:nicknames "sirius")
  (:use common-lisp capi racer racer-user)
  (:export "SIRIUS"))
   

