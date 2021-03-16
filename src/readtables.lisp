;;; -*- mode: lisp; syntax: ansi-common-lisp; package: sirius; base: 10 -*-

(in-package sirius)

(defun sirius-resource-reader (stream subchar arg)
  (declare (ignore subchar arg))
  
  (let* ((prefix 
          (coerce 
           (loop as 
                 x = (read-char stream nil)
                 while (and x (not (char-equal x #\:)))
                 collect x)
           'string))
         (resource 
          (let ((old-case 
                 (readtable-case *readtable*)))
            (unwind-protect
                (progn (setf (readtable-case *readtable*) :preserve)
                  (let ((char (peek-char nil stream nil)))
                    (when (and char 
                               (not (whitespace-char-p char))
                               (not (char= char #\))))
                      (read stream))))
              (setf (readtable-case *readtable*) old-case))))
         (namespace (if (string= prefix "")
                        (profile-sirius-current-namespace (active-profile))
                      (second (assoc prefix (profile-sirius-current-namespace-prefixes (active-profile))
                                     :test #'string-equal))))
         (namespace-sep  
	  (when namespace
            (let ((n (1- (length  namespace))))
              (elt namespace n))))

         (namespace 
          (when namespace
            (case namespace-sep
              ((#\# #\/) namespace)
              (otherwise
               (format nil "~A#" namespace))))))

    (if resource
        (if namespace
            (intern (format nil "~A~A" namespace resource))
          (intern (format nil "~A" resource)))
      (if namespace
          (intern (format nil "~A" namespace))
        nil))))

(defun sirius-resource-reader2 (stream subchar arg)
  (declare (ignore subchar arg))
  
  (let* ((prefix 
          (coerce 
           (loop as 
                 x = (read-char stream nil)
                 while (and x (not (char-equal x #\:)))
                 collect x)
           'string))
         (resource 
          (let ((old-case 
                 (readtable-case *readtable*)))
            (unwind-protect
                (progn (setf (readtable-case *readtable*) :preserve)
                  (let ((char (peek-char nil stream nil)))
                    (when (and char 
                               (not (whitespace-char-p char))
                               (not (char= char #\))))
                      (read stream))))
              (setf (readtable-case *readtable*) old-case))))
         (namespace (if (string= prefix "")
                        (profile-sirius-current-namespace (active-profile))
                      (second (assoc prefix (profile-sirius-current-namespace-prefixes (active-profile))
                                     :test #'string-equal))))

         (namespace-sep  
          (when namespace
            (let ((n (1- (length  namespace))))
              (elt namespace n))))
         
         (namespace 
          (when namespace
            (case namespace-sep
              ((#\# #\/) namespace)
              (otherwise
               (format nil "~A#" namespace))))))

    (if resource
        (if namespace
            (intern (format nil "*~A~A" namespace resource))
          (intern (format nil "*~A" resource)))
      (if namespace
          (intern (format nil "*~A" namespace))
        nil))))

;;;
;;;
;;;

(defstruct (racer-boolean 
            (:predicate racer-boolean-p)
            (:conc-name boolean-))
  value)

(defmethod print-object ((object racer-boolean) stream)
  (declare (ignorable stream))
  (if (string= (boolean-value object) "false")
      (write-string "#F" stream)
    (write-string "#T" stream)))

(defparameter *true* (make-racer-boolean :value "true"))

(defparameter *false* (make-racer-boolean :value "false"))

(defmethod make-load-form ((object racer-boolean) &optional env)
  (declare (ignore env))
  (cond ((eq object *true*) '*true*)
        ((eq object *false*) '*false*)))

(defun sirius-true-value-reader (stream subchar arg)
  (declare (ignore stream subchar arg))
  *true*)

(defun sirius-false-value-reader (stream subchar arg)
  (declare (ignore stream subchar arg))
  *false*)

(defun true? (object)
  (and (racer-boolean-p object)
       (string= (boolean-value object) "true")))

(defun false? (object)
  (and (racer-boolean-p object)
       (string= (boolean-value object) "false")))



;;;
;;; Am Ende setzen! Wegen eingetragener Reader Macros!
;;; 


(defparameter *lracer-readtable* (copy-readtable *readtable*))

(defparameter editor::*editor-readtable* (copy-readtable *lracer-readtable*))

(setf (readtable-case editor::*editor-readtable*) :preserve)


;;; sind nicht mehr erforderlich in RacerPro 2.0 
;;; DOCH! Sonst funktionieren die im Editor nicht... 

(dolist (*readtable* (list *readtable* *lracer-readtable* editor::*editor-readtable*))
  
  (set-dispatch-macro-character #\# #\T 'sirius-true-value-reader)

  (set-dispatch-macro-character #\# #\F 'sirius-false-value-reader)
  
  (set-dispatch-macro-character #\# #\t 'sirius-true-value-reader)
  
  (set-dispatch-macro-character #\# #\f 'sirius-false-value-reader)

  (set-dispatch-macro-character #\# #\! 'sirius-resource-reader)

  (set-dispatch-macro-character #\# #\& 'sirius-resource-reader2))


