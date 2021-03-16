;;; -*- mode: lisp; syntax: ansi-common-lisp; package: sirius; base: 10 -*-

(in-package sirius)

;;;
;;;
;;; 

(defvar *messages-shown* nil)

(defvar *background-color* nil)

(defvar *no-logging* nil)

(defvar *confirm-quit* t)

(defvar *printing* nil)

(defvar *create-history-entries* t)

(defvar *recursive-service-request-p* nil)

(defvar *profiles-loaded-from* nil)

(defvar *warn-before-overwriting-profiles* nil)

(defvar *image-directory* nil)

(defvar *dialog-displayed* nil)

(defvar *image-size* nil)

(defvar *sirius-app* nil)

(defvar *communication-status-updater* nil)

(defvar *progress-bar* nil)

(defvar *updater* nil)

(defvar *sirius-logo-image* nil)

(defvar *sirius-logo-rgb-data* nil)

(defvar *inline-logo-image* nil)

(defvar *sirius-profiles* nil)

(defvar *profiles-to-save* nil)

(defvar *active-profile* nil)

(defvar *interface-active-profile* nil)

(defvar *current-history-position-is-top-of-stack* t)

(defvar *role-hierarchy-info-pane* nil)

(defvar *taxonomy-info-pane* nil)

(defvar *abox-graph-info-pane* nil)

(defvar *display-error* t)

;;;
;;;
;;;

(defvar *sirius-editor-app* nil)

(defvar *vertical-scroll* nil)

(defvar *horizontal-scroll* nil)

(defvar *sirius-field-width* nil)

(defvar *sirius-history-field-width* '(:character 15))

;;;
;;;
;;;

(defvar *TAB-TITLE-SHELL*)

(defvar *TAB-TITLE-ABOUT*)
 
(defvar *TAB-TITLE-QUERIES*)

(defvar *TAB-TITLE-ROLE-HIERARCHY*)

(defvar *TAB-TITLE-INDIVIDUALS*)

(defvar *TAB-TITLE-PROFILES*)

(defvar *TAB-TITLE-LOG*)

(defvar *TAB-TITLE-QUERY-IO*)

(defvar *TAB-TITLE-TAXONOMY*)

(defvar *TAB-TITLE-ROLES*)

(defvar *TAB-TITLE-TBOXES*)

(defvar *TAB-TITLE-ABOXES*)

(defvar *TAB-TITLE-DEF-QUERIES*)

(defvar *TAB-TITLE-ABOX-GRAPH*)

(defvar *TAB-TITLE-ASSERTIONS*)

(defvar *TAB-TITLE-CONCEPTS*)

(defvar *TAB-TITLE-OWLAPI*)
