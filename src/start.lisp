;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: CL-USER; Base: 10 -*-

(in-package cl-user)

(defun run-sirius ()
  (sirius::sirius))

(defun start-sirius ()
  (sirius::sirius))

#-:sirius-editor
(defun sirius::sirius-editor ()
  t)

