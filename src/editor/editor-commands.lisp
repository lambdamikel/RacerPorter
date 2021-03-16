;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: EDITOR; Base: 10 -*-

(in-package editor)

(defvar *in-region-eval-p* nil)

;;;
;;; Hooks
;;;

(define-file-type-hook ("owl" "owx" "rdf" "rdfs" "daml" "xml")
    (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "XML"))

(define-file-type-hook ("racer" "funct" "ofn" "lisp")
    (buffer type)
  (declare (ignore type))
  (setf (buffer-major-mode buffer) "Lisp"))

;;;
;;;
;;;

(defun report-region-lisp-eval-error (message &rest args)
  (let ((message (apply #'format nil message args)))
    (process-character `(sirius::report-error ,message)
                       (current-window))
    (process-character `(message ,message)
                       (current-window))))

;;;
;;; Region Eval Stuff 
;;; Quelle: /usr/local/lib/LispWorks/lib/4-4-0-0/src/editor/
;;; 
;;; (patched, s. readtable-case) 
;;; 
;;; Benoetigt von racer-eval (Evaluate Expression)
;;; 

(defmacro with-compilation-environment-at-point ((point
                                                  &key
                                                  (compilep nil)
                                                  start-message
                                                  end-message)
                                                 &body body)
  (with-unique-names (buffer)
    `(with-compilation-environment-at-point-fn
      ,point ,start-message ,end-message
      #'(lambda (,buffer)
          (let* ((,(if compilep '*compile-file-pathname* '*load-pathname*)
                  (buffer-pathname ,buffer))
                 (,(if compilep '*compile-file-truename* '*load-truename*)
                  (buffer-pathname ,buffer)) ; buffer-pathname _is_ a truename
                 )
            ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (let ((lw::*handle-warn-on-redefinition* :quiet))

    (defmacro with-input-from-region ((var start end) &body body &environment env)
      "With-Input-From-Region (Var Region) {Declaration}* {Form}*
  During the evaluation of Forms, Var is bound to a stream which
  returns input from Region."
      (multiple-value-bind (forms decls)
          (dspec:separate-declarations body env)
        `(let ((,var (pop *free-editor-region-streams*)))
           ,@decls
           (if ,var
               (setq ,var (modify-editor-region-stream ,var ,start ,end))
             (setq ,var (make-editor-region-stream ,start ,end)))
           (unwind-protect
               (progn ,@forms)
             (free-region-stream ,var)))))))

(defmacro with-output-to-editor-string ((stream-var) &body body)
  `(with-output-to-string 
       (,stream-var nil #+DBCS-ENV :element-type #+DBCS-ENV 'simple-char)
     ,@body))

;;;
;;;
;;;

(defun region-lisp-eval (buffer start end print)

  (let ((res "Finished evaluating"))

    (with-compilation-environment-at-point (start
                                            :start-message (when *in-region-eval-p* "Evaluating...")
                                            :end-message (and (not (windowp print))
                                                              *in-region-eval-p*
                                                              res))

      (let ((*readtable* *editor-readtable*))
      
        (with-input-from-region (stream start end)
          (let ((out-stream (if (streamp print) print (buffer-stream buffer))))
                  
            (handler-case 
            
                (common-utilities:load-text-stream
                 stream 
                 :exit-load-p t
                 :eval-function #'(lambda (form)
                                    (multiple-value-list
                                     (progn 
                                       (sirius::set-progress-bar-to 
                                        (float (* 100 (/ (file-position stream)
                                                         (file-length stream))))
                                        t)
                                       (setf res (editor-eval buffer form))
                                       (if (sirius::error-res-p res)
                                           (if (and *in-region-eval-p*
                                                    (capi:confirm-yes-or-no  
                                                     (format nil 
                                                             "Error: ~A
Continue Buffer Evaluation?" (second res))))
                                               res
                                             (error (second res)))
                                         res))))
                 :print-function
                 #'(lambda (result)
                     (and print
                          (if (windowp print)
                              (process-character
                               `(message ,*values-format-string* ,result)
                               print)
                            (in-output-eval-results out-stream result)))))
          
              (end-of-file (x)
                (report-region-lisp-eval-error "Incomplete s-expression in region " x)
                (return-from region-lisp-eval nil))
              (reader-error (x)
                (report-region-lisp-eval-error "Error while reading: ~a " x)
                (return-from region-lisp-eval nil))
              (error (x)
                (report-region-lisp-eval-error "Evaluation aborted: ~a " x)))))))))


;;;
;;; Benoetigt von Evaluate Buffer
;;; 

(defun region-eval2 (buffer start end &key print warnings after-function)
  (multiple-value-bind (pane window)
      (choose-lispeval-pane buffer nil)
    (funcall-background-job
     pane
     'background-region-eval2
     window
     buffer
     (copy-point start)
     (copy-point end)
     print
     warnings
     after-function)))

(defun background-region-eval2 (window buffer start end print warnings
                                       after-function)
  (let ((*current-evaluator* 'sirius::racer-eval2)
        (*package* (find-package "RACER-USER"))
        (*in-region-eval-p* t))
    (background-region-eval window buffer start end print warnings
                            after-function)
    (sirius::update-tbox-and-abox)))

;;;
;;; Benoetigt von Evaluate OWL Buffer
;;; 

(defun region-eval-owl1 (buffer start end &key print warnings after-function)
  (multiple-value-bind (pane window)
      (choose-lispeval-pane buffer nil)
    (funcall-background-job
     pane
     'background-region-eval3
     window
     buffer
     (copy-point start)
     (copy-point end)
     print
     warnings
     after-function)))

(defun background-region-eval3 (window buffer start end print warnings
                                       after-function)
  (let ((*current-evaluator* 'sirius::racer-eval2)
        (*package* (find-package "RACER-USER"))
        (*in-region-eval-p* t))
    (background-region-eval-owl window buffer start end print warnings
                                after-function)
    :okay))

(defun background-region-eval-owl (window buffer start end print warnings
                                          after-function)
  (unwind-protect
      (use-buffer-and-window buffer window
        (funcall 'region-eval-owl buffer start end print)
        (when warnings
          (let ((single-message (if (cdr warnings)
                                    (with-output-to-editor-string (out)
                                      (dolist (warning warnings)
                                        (format out "~&~A" warning))
                                      out)
                                  (car warnings))))
            (process-character `(message ,single-message) window)))
        (when after-function
          (process-character (list after-function) window))
        ;; Pekka 02May96: moved recording here, in the hope it will get
        ;; generalized some day.  Compilation always had it in a similar place.
        (region-eval-record-definitions :buffer buffer :start start :end end))
    (delete-point start)
    (delete-point end)))

(defun region-eval-owl (buffer start end print)
  (declare (ignore buffer))
  (with-compilation-environment-at-point (start
                                          :start-message "Evaluating OWL..."
                                          :end-message (and (not (windowp print))
                                                            "Finished evaluating"))
    (with-input-from-region (stream start end)
      (handler-case 
          (sirius::sirius-eval-owl stream)
          
        (end-of-file (x)
          (report-region-lisp-eval-error "Incomplete OWL expression in region " x)
          (return-from region-eval-owl nil))
        (reader-error (x)
          (report-region-lisp-eval-error "Error while reading: ~a " x)
          (return-from region-eval-owl nil))))))

;;; 
;;; Editor Commands Definitions 
;;; 

(defmacro with-defvar-action ((buffer warnings) &body body)
  (with-unique-names (dspec variable)
    `(let* ((,dspec (dspec-if-defvar ,buffer))
            (,variable (when ,dspec
                         (second ,dspec)))
            (,warnings
             (when (and ,variable (boundp ,variable))
               (defvar-warnings ,dspec))))
       (makunbound-defun-if-required ,buffer)
       ,@body)))

;;;
;;;
;;; 

(let ((lw::*handle-warn-on-redefinition* :quiet))

  (defcommand "Evaluate Expression" (p &optional (buffer (current-buffer)))
       (declare (ignore p))
       (if (sirius::connected-and-socket-alive-p)
           (with-defvar-action (buffer warnings)
             (call-on-buffer-defun buffer 'region-eval :print nil :warnings warnings))
         (sirius::no-connection-error)))

  
  (defcommand "Evaluate Sparql Query" (p &optional (buffer (current-buffer)))
       (declare (ignore p))
       (handler-case
           (if (sirius::connected-and-socket-alive-p)
               (let ((b (or buffer (current-buffer))))

                 (handler-case

                     (editor:use-buffer b

                       (let* ((end 
                               (or 
                                (forward-search-command b
                                                        (coerce '(#\newline #\newline) 'string)
                                                        (copy-point (editor::buffer-point b)))
                                (buffer-end b)))
                              (start 
                               (backward-search-command b
                                                        (coerce '(#\newline #\newline) 'string)
                                                        (copy-point (editor::buffer-point b))))

                              (query 
                               (editor:points-to-string  
                                start end)))

                         (when query
                           (let ((query 
                                  (subseq query 2 (- (length query) 2))))

                             (let ((pane
                                    (choose-lispeval-pane b nil)))
                               (funcall-background-job
                                pane
                                'sirius::sparql-eval
                                query))))))

                   (error ()
                     (sirius::report-error "Sparql Error: Sparql regions must be enclosed by blank lines!")))) 
                     
             (sirius::no-connection-error))
         (error (condition)  
           (sirius::report-error (format nil "Sparql Error: ~A" condition))))))
  


(let ((lw::*handle-warn-on-redefinition* :quiet))

  (defcommand "Evaluate Buffer" (p &key buffer)
       (declare (ignore p))
       (if (sirius::connected-and-socket-alive-p)
           (let ((b (or buffer (current-buffer))))
             (region-eval2 b
                           (buffer-%start b)
                           (buffer-%end b)
                           :print nil))
         (sirius::no-connection-error))))

(defcommand "Evaluate OWL Buffer" (p &key buffer)
     (declare (ignore p))
     (if (sirius::connected-and-socket-alive-p)
         (let ((b (or buffer (current-buffer))))
           (region-eval-owl1 b
                             (buffer-%start b)
                             (buffer-%end b)
                             :print nil))
       (sirius::no-connection-error)))

;;;
;;;
;;; 

(defcommand "Quit RacerPorter" (p)
     (declare (ignore p))
     (sirius::quit-sirius-and-shutdown))

;;;
;;;
;;; 

(defcommand "Racer Command Signature" (p)
     (declare (ignore p))
     (sirius::sirius-update-argument-display t))

(defcommand "Complete Racer Command" (p)
     (declare (ignore p))
     (sirius::complete-racer-command t))

(defcommand "Complete Racer Filename" (p)
     (declare (ignore p))
     (editor-complete-file nil nil t))

;;;
;;;
;;; 

(defun unsaved-buffers1 ()
  (loop for buffer in *buffer-list*
        for pathname = (buffer-pathname buffer)
        when (and (buffer-modified buffer)
                  (and (file-buffer-p buffer)
                       pathname))
        collect buffer))

(defun window-save-all-files ()
  (let ((unsaved-buffers (unsaved-buffers1)))
    (multiple-value-bind (save-list successp)
        (if (null unsaved-buffers)
            (values unsaved-buffers t)
          (check-list unsaved-buffers
                      :title "Save Selected Buffers"
                      :initial-state :set
                      :name-function 'buffer-name
                      :visible-items 20))
      (when successp
        (dolist (save save-list)
          (let ((pn (buffer-pathname save)))
            (if pn
                (write-da-file save pn)
              (write-file-command nil nil save)))))
      successp)))

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (let ((lw::*handle-warn-on-redefinition* :quiet))

    (defcommand "Incremental Search" (p)
         "Searches for input string as characters are provided.
  These are the default I-Search command characters: Ctrl-q quotes the
  next character typed.  Backspace cancels the last character typed. Ctrl-s
  repeats forward, and Ctrl-r repeats backward.  Ctrl-r or Ctrl-s with empty
  string either changes the direction or yanks the previous search string.
  Altmode exits the search unless the string is empty.  Altmode with
  an empty search string calls the non-incremental search command.
  Other control characters cause exit and execution of the appropriate
  command.  If the search fails at some point, Ctrl-g and backspace may be
  used to backup to a non-failing point; also, Ctrl-s and Ctrl-r may be used
  to look the other way.  Ctrl-g during a successful search aborts and returns
  point to where it started."
         "Search for input string as characters are typed in.
  It sets up for the recursive searching and checks return values."
      (setf sirius::*incremental-search-active-p* t)
      (unwind-protect
          (incremental-search-somehow 
           (if p :forward-regexp :forward))
        (setf sirius::*incremental-search-active-p* nil)))

    (defcommand "Reverse Incremental Search" (p)
         "Searches for input string as characters are provided.
  These are the default I-Search command characters: Ctrl-q quotes the
  next character typed.  Backspace cancels the last character typed.  Ctrl-s
  repeats forward, and Ctrl-r repeats backward.  Ctrl-r or Ctrl-s with empty
  string either changes the direction or yanks the previous search string.
  Altmode exits the search unless the string is empty.  Altmode with
  an empty search string calls the non-incremental search command.
  Other control characters cause exit and execution of the appropriate
  command.  If the search fails at some point, Ctrl-g and backspace may be
  used to backup to a non-failing point; also, Ctrl-s and Ctrl-r may be used
  to look the other way.  Ctrl-g during a successful search aborts and returns
  point to where it started."
         "Search for input string as characters are typed in.
  It sets up for the recursive searching and checks return values."
      (setf sirius::*incremental-search-active-p* t)
      (unwind-protect
          (incremental-search-somehow 
           (if p :backward-regexp :backward))
        (setf sirius::*incremental-search-active-p* nil)))))
