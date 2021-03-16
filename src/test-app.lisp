;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(defun test-app ()
  (capi::display-message (format nil "~S" 
                           system:*line-arguments-list*)))
