;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defvar *global-font-size* 
  #+(and :lispworks6 (not :mac))
  8
  #-(and :lispworks6 (not :mac))
  10)

;;;
;;;
;;;

(defvar *pe-font*)

(defvar *pe-title-font*)

(defvar *pe-title-bold-font*)

;;;
;;;
;;;

(defvar *tabs-font*)

(defvar *button-font*)

(defvar *racer-button-font*)

(defvar *title-font*)

(defvar *info-font*)

(defvar *checkbox-font*)

(defvar *radiobox-font*)

(defvar *list-font*)

(defvar *shell-font*)

(defvar *graph-font*)

;;;
;;;
;;; 

(defun get-helvetica-font (&optional (n *global-font-size*))
  `(:family "helvetica" :size ,n))

(defun get-helvetica-bold-font (&optional (n *global-font-size*))
  `(:family "lucida" :weight :bold :size ,n))

(defun get-system-font (&optional (n *global-font-size*))
  #+:win32 
  `(:FAMILY "Lucida Console" :WEIGHT :NORMAL :SIZE ,n)
  #+:lispworks6
  `(:WEIGHT :NORMAL :FAMILY "Arial" :SIZE ,n)
  #-(or :win32 :lispworks6)
  `(:stock :system-fixed-font :size ,(+ 1 n)))

(defun get-system-bold-font (&optional (n *global-font-size*))
  #+:win32 
  `(:FAMILY "Lucida Console" :WEIGHT :BOLD :SIZE ,n)
  #+:lispworks6
  `(:WEIGHT :BOLD :FAMILY "Arial" :SIZE ,n)
  #-(or :win32 :lispworks6)
  `(:stock :system-fixed-font :size ,(+ 1 n)))

(defun get-system-large-font (&optional (n *global-font-size*))
  #+:win32 
  `(:FAMILY "Lucida Console" :WEIGHT :NORMAL :SIZE ,n)
  #+:lispworks6
  `(:WEIGHT :NORMAL :FAMILY "Arial" :SIZE ,(+ 2 n))
  #-(or :win32 :lispworks6)
  `(:stock :system-fixed-font :size ,(+ 2 n)))

