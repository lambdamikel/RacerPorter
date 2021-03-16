;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun b2f (x)
    (float (/ x 255))))
;;;
;;;
;;; 

(defconstant +selected-color+ (color:make-rgb (b2f #x00) (b2f #x33) (b2f #xFF)))

(defconstant +graph-is-a-color+ (color:make-rgb (b2f #x00) (b2f #x00) (b2f #x00)))

(defconstant +updating-display-color+ (color:make-rgb (b2f #xFF) (b2f #xFF) (b2f #x66)))

(defconstant +req-busy-color+ (color:make-rgb (b2f #xFF) (b2f #xBE) (b2f #x57)))

(defconstant +req-ready-color+ (color:make-rgb (b2f #xAA) (b2f #xFF) (b2f #x80)))

(defconstant +req-died-color+  (color:make-rgb (b2f #xFF) (b2f #x00) (b2f #x00)))

(defconstant +req-not-connected-color+ (color:make-rgb (b2f #xFF) (b2f #x80) (b2f #x80)))

(defconstant +req-otherwise-color+ (color:make-rgb (b2f #xCC) (b2f #xCC) (b2f #xCC)))

(defconstant +res-sending-request-color+ (color:make-rgb (b2f #x80) (b2f #xff) (b2f #xf4)))

(defconstant +res-connection-died-during-send-color+ (color:make-rgb (b2f #xFF) (b2f #x00) (b2f #x00)))

(defconstant +res-connection-died-during-read-color+ (color:make-rgb (b2f #xFF) (b2f #x20) (b2f #x20)))

(defconstant +res-connection-already-dead-color+ (color:make-rgb (b2f #xFF) (b2f #x80) (b2f #x80)))

(defconstant +res-getting-racer-result-color+ (color:make-rgb (b2f #x80) (b2f #xBF) (b2f #xFF)))

(defconstant +res-error-color+ (color:make-rgb (b2f #xFF) (b2f #x80) (b2f #x80)))

(defconstant +res-good-color+ (color:make-rgb (b2f #xAA) (b2f #xFF) (b2f #x80)))

(defconstant +res-ready-color+ (color:make-rgb (b2f #xAA) (b2f #xFF) (b2f #x80)))

(defconstant +res-parsing-racer-result (color:make-rgb (b2f #xCC) (b2f #xB2) (b2f #xFF)))

(defconstant +res-otherwise-color+ (color:make-rgb (b2f #xCC) (b2f #xCC) (b2f #xCC)))

(defconstant +res-busy-color+  (color:make-rgb (b2f #xFF) (b2f #xBE) (b2f #x57)))

(defconstant +cache-hit-color+ (color:make-rgb (b2f #xaa) (b2f #xff) (b2f #x80)))

(defconstant +logo-bc+ (color:make-rgb (b2f #xDD) (b2f #xDD) (b2f #xDD)))

;;;
;;;
;;;

(defconstant +open-connection-timeout+ 3)

(defconstant +wait-after-connect-sleep-time+ 0.5)

(defconstant +close-connection-timeout+ 3)

(defconstant +acquire-server-info-timeout+ 3)

(defconstant +wait-for-started-server-before-connect-timeout+ 4)

(defconstant +wait-until-canceled-sleep-time+ 1)

(defconstant +exit-server-command-before-close-connection-sleep-time+ 0.5)

;;;
;;;
;;;

(defconstant +sirius-listener-height+ 14)

(defconstant +sirius-info-height+ 10)

(defconstant +sirius-info-max-height+ 10)

;;;
;;;
;;;

(defconstant +button-bc+ nil)
(defconstant +button-fc+ nil)

(defconstant +sirius-bc+ nil)
(defconstant +sirius-fc+ nil)

(defconstant +state-bc+ nil)
(defconstant +state-fc+ nil)

;;;
;;;
;;;

(defconstant +update-process-sleep-time+ 0.3)

(defconstant +progress-bar-update-process-sleep-time+ 0.4)

(defconstant +wait-for-cancel-button-sleep-time+ 1)

(defconstant +wait-before-replace-current-shell-input-sleep-time2+ 0.3)

;;;
;;;
;;; 

(defconstant +maxdepth-marker+ '?)

;;;
;;;
;;;

(defparameter *old-pprint-dispatch* (copy-pprint-dispatch))

(defparameter *pprint-dispatch1* (copy-pprint-dispatch))

(defparameter *pprint-dispatch2* (copy-pprint-dispatch))

