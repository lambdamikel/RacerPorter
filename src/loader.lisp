;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defun transmit-if-necessary (file)
  (unless (local-connection-p)
    (let ((n-bytes 0)
          (profile (active-profile)))
      
      (with-open-file (in-stream file :direction :input :element-type 'unsigned-byte)
        (loop for byte = (read-byte in-stream nil nil)
              do
              (if (null byte)
                  (return)
                (incf n-bytes))))

      (cond ((or (not (profile-ask-before-transmitting-large-files profile))
                 (and (confirm-yes-or-no (format nil "Really transmit ~A bytes to ~A?"  n-bytes 
                                                 (get-server-description)))
                      (> n-bytes (* 1024 1024))))

             (let* ((file1
                     (racer-function
                      (transmit-file (pathname-type (pathname file)) n-bytes))))

               (if (error-res-p file1)

                   (values nil t)

                 (progn 

                   (enter-dummy-command (format nil "Sending KB ~A to ~A:~A as ~A..." file
                                                (profile-host profile)
                                                (profile-port profile)
                                                file1)
                                        :okay)

                   (let* ((interface (contain (make-instance 'my-progress-bar
                                                             :title
                                                             (format nil "Transmitting KB ~A to ~A:~A..."
                                                                     file
                                                                     (profile-host profile)
                                                                     (profile-port profile))
                                                             :x :center
                                                             :y :center)))

                          (bar (slot-value interface 'progress-bar))
                          (count 0)
                          (last 0))
          
                     (with-open-file (in-stream file :direction :input :element-type 'unsigned-byte)

                       (with-socket-lock
                         (loop for byte = (read-byte in-stream nil nil)
                               do
                               (let ((cur
                                      (round (* 100 
                                                (/ (incf count)
                                                   n-bytes)))))
                      
                                 (unless (= cur last)
                                   (setf last cur)
                                   (setf (capi:range-slug-start bar) cur)))

                               (when (or (null byte)
                                         (cancel-p interface))
                                 (return))
                    
                               (write-byte byte (profile-socket profile))))

                       (apply-in-pane-process interface
                                              #'(lambda ()
                                                  (quit-interface interface))))
                     
                     (when (cancel-p interface)
                       ;;; flush stream bytes!
                       (close-connection t)
                       (open-connection t))

                     (values (substitute-backslashes-with-slashes file1)
                             (cancel-p interface)))))))

            (t (values nil t))))))

;;;
;;;
;;;

(defun load-kb ()
  (unless (active-profile)
    (no-connection-error))

  (let ((file
         (open-file-prompter
          "Load" "*.*" 
          (profile-last-load-directory (active-profile)))))
    (when file
      (setf (profile-last-load-directory (active-profile))
            (directory-namestring file))
      (load-kb1 file))))

(defun load-kb1 (file)

  (multiple-value-bind (file1 canceled-p)
      (transmit-if-necessary file)
    
    (let ((file (or file1 file))
          (axioms-p nil))

      (when (and file (not canceled-p)
                 (not (error-res-p file1)))

        (let ((type (string-downcase (pathname-type file))))

          (cond ((or (equal type "racer")
                     (equal type "lisp"))

                 (background-state-changing-result-command
                  `(|racer-read-file| ,file)
                  (lambda (res)
                    (declare (ignorable res))
                    (with-promoted-lock
                      (sirius-use-current-abox-tbox))
                    res)))

                ((member type '("funct" "owf" "ofn" "xml" "owx" "owl" "rdf" "rdfs")
                         :test #'string-equal)

                 (setf axioms-p 
                       (or (profile-editor-eval-maintain-axioms (active-profile))
                           #+:mac
                           (confirm-yes-or-no "Shall I create OWLAPI axioms (\"Axioms\" tab)?

Note: Reasoning also works without OWLAPI axioms. OWLAPI axioms are only required if axioms shall be edited in the \"Axioms\" tab, or the NOSA interface will be used, e.g., for rendering services. However, OWLAPI axioms require much more memory.")
                           #-:mac
                           (confirm-yes-or-no "Shall I create OWLAPI axioms (\"Axioms\" tab)?

Note: Reasoning also works without OWLAPI axioms. 
OWLAPI axioms are only required if axioms shall be
edited in the \"Axioms\" tab, or the NOSA interface
will be used, e.g., for rendering services. However, 
OWLAPI axioms require much more memory.")))
                 
                 (background-state-changing-result-command
                  `(|OWLAPI-readOntology| 
                    ,file
                    :maintain-owlapi-axioms ,axioms-p)
                  (lambda (res)
                    (declare (ignorable res))
                    (with-promoted-lock
                      (sirius-use-current-abox-tbox))
                    res)))

                #+:ignore
                ((or (equal type "funct")
                     (equal type "owf"))

                 (setf axioms-p 
                       (or (profile-editor-eval-maintain-axioms (active-profile))
                           (confirm-yes-or-no "Shall I create OWL axioms (\"Axioms\" tab)?")))
                 
                 (background-state-changing-result-command
                  `(|OWLAPI-readFunctionalOntologyFile| 
                    ,file
                    :maintain-owlapi-axioms ,axioms-p)
                  (lambda (res)
                    (declare (ignorable res))
                    (with-promoted-lock
                      (sirius-use-current-abox-tbox))
                    res)))
                
                #+:ignore
                ((or (equal type "xml")
                     (equal type "owx"))

                 (setf axioms-p 
                       (or (profile-editor-eval-maintain-axioms (active-profile))
                           (confirm-yes-or-no "Shall I create OWL axioms (\"Axioms\" tab)?")))
                 
                 (background-state-changing-result-command
                  `(|OWLAPI-readXMLOntologyFile| 
                    ,file
                    :maintain-owlapi-axioms ,axioms-p)
                  (lambda (res)
                    (declare (ignorable res))
                    (with-promoted-lock
                      (sirius-use-current-abox-tbox))
                    res)))
                               
                #+:ignore
                ((or (equal type "owl")
                     (equal type "rdf")
                     (equal type "rdfs"))

                 (setf axioms-p 
                       (or (profile-editor-eval-maintain-axioms (active-profile))
                           (confirm-yes-or-no "Shall I create OWL axioms (\"Axioms\" tab)?")))
                 
                 (background-state-changing-result-command 
                  (if (profile-load-into-default (active-profile))
                      (if (profile-alisp-racer (active-profile))
                          `(owl-read-file ,file :kb-name 'default :maintain-owlapi-axioms ,axioms-p)
                        `(owl-read-file ,file :kb-name |default| :maintain-owlapi-axioms ,axioms-p))
                    `(owl-read-file ,file :maintain-owlapi-axioms ,axioms-p))
                  (lambda (res)
                    (declare (ignorable res))
                    (with-promoted-lock
                      (sirius-use-current-abox-tbox))
                    res)))
                
                (t (report-error (format nil "Unsupported file type ~A" type)))))))))

;;;
;;;
;;;

(defun restore-image ()
  (unless (active-profile)
    (no-connection-error))

  (let* ((file 
          (open-file-prompter
           "Restore RacerPro Image (Specify *.SUB.IMG or *.KB.IMG)" "*.IMG" 
           (profile-last-image-directory (active-profile)))))
    (when file
      (setf (profile-last-image-directory (active-profile))
            (directory-namestring file))
      
      (loop while (pathname-type file) do
            (setf file
                  (substitute-backslashes-with-slashes 
                   (namestring
                    (make-pathname :directory (pathname-directory file)
                                   :host (pathname-host file)
                                   :name (pathname-name file))))))
    

      (restore-image1 
       (substitute-backslashes-with-slashes file)))))

(defun restore-image1 (file)

  (multiple-value-bind (file1 canceled-p)
      (transmit-if-necessary file)
    
    (let ((file (or file1 file)))

      (when (and file (not canceled-p)
                 (not (error-res-p file)))

        (background-state-changing-result-command
         `(restore-server-image ,file)
         (lambda (res)
           (with-promoted-lock
             (sirius-use-current-abox-tbox))
           res))))))

;;;
;;;
;;;

(defun store-image ()
  (unless (active-profile)
    (no-connection-error))

  (let ((file
         (save-file-prompter "Store RacerPro Image" "*.IMG" nil (user-directory))))

    (when file     

      (let ((rem nil))
        (loop while (pathname-type file) do
              (push (format nil ".~A" (pathname-type file)) rem)
              (setf file
                    (substitute-backslashes-with-slashes 
                     (namestring
                      (make-pathname :directory (pathname-directory file)
                                     :host (pathname-host file)
                                     :name (pathname-name file))))))

        (let* ((file1 (make-pathname :directory (pathname-directory file)
                                     :host (pathname-host file)
                                     :type "KB.IMG"
                                     :name (pathname-name file)))
               (file2 (make-pathname :directory (pathname-directory file)
                                     :type "SUB.IMG"
                                     :host (pathname-host file)
                                     :name (pathname-name file))))
          
          (when (cdr rem)
            (warning-message
             "Sorry, I have to remove the ~S extension.~%Will create~%~S~%and~%~S." 
             (apply #'concatenate 'string (butlast rem))
             (substitute-backslashes-with-slashes (namestring file1))
             (substitute-backslashes-with-slashes (namestring file2))))
          
          (when (or (not (pathname-type file))
                    (confirm-yes-or-no "Removing Extension ~A. Continue?" 
                                       (pathname-type file)))


            (when (and (or (not (probe-file file1))
                           (confirm-yes-or-no (format nil "File ~A exists! Overwrite?" file1)))
                       (or (not (probe-file file2))
                           (confirm-yes-or-no (format nil "File ~A exists! Overwrite?" file2))))
                    
              (let ((file
                     (substitute-backslashes-with-slashes 
                      (namestring
                       (make-pathname :directory (pathname-directory file1)
                                      :host (pathname-host file1)
                                      :name (pathname-name file1))))))

                (background-state-changing-result-command
                 `(store-server-image ,file))))))))))


;;;
;;;
;;; 

#+:sirius-editor
(defun open-kb ()
  (let ((file (open-file-prompter "Open" "*.*"  
                                  (profile-last-editor-directory (active-profile)))))
    (when file
      (setf (profile-last-editor-directory (active-profile))
            (directory-namestring file))
      
      (setf editor::*current-evaluator* 'racer-eval)
      (sirius-editor file))))

