;; -*- Mode: Lisp; rcs-header: "$Header: /hope/xanhope1-cam/hope.0/compound/23/LISPexamples/RCS/configuration:macos-application-bundle.lisp,v 1.5.1.1 2004/08/31 21:26:54 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/configuration/macos-application-bundle.lisp
;;
;; This example creates a Mac OS application bundle.
;;
;; The function WRITE-MACOS-APPLICATION-BUNDLE creates a Mac OS
;; application bundle for the given target-path. It returns the full
;; path of the executable in the bundle, suitable for passing to
;; SAVE-IMAGE.
;;
;; See the file examples/configuration/save-macos-application.lisp for
;; an example configuration script using this code.
;;
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2004 Xanalys Inc. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(defun write-macos-application-bundle (target-path &key
                                                   template-bundle
                                                   signature
                                                   application-icns
                                                   identifier
                                                   (version (if template-bundle
                                                                nil
                                                              (lisp-implementation-version)))
                                                   (build (if template-bundle
                                                              nil
                                                            "internal"))
                                                   document-types
                                                   executable-name)
  (let* ((target-path
          (make-pathname :name nil
                         :type nil
                         :version nil
                         :directory (append (pathname-directory
                                             target-path)
                                            (list (file-namestring
                                                   (make-pathname
                                                    :type "app"
                                                    :defaults target-path))))
                         :defaults target-path))
         (bundle-name (pathname-name
                       (car (last (pathname-directory
                                   target-path)))))
         (executable-name (or executable-name bundle-name)))
    (multiple-value-bind (target-contents-path
                          target-contents-macos-path
                          target-resources-path)
        (make-application-subpaths target-path)
      (let ((source-path (choose-template-bundle-path template-bundle)))
        (multiple-value-bind (source-contents-path
                              source-contents-macos-path
                              source-resources-path)
            (make-application-subpaths source-path)
          (declare (ignore source-contents-macos-path))
          (ensure-directories-exist target-contents-macos-path)
          (ensure-directories-exist target-resources-path)
          (copy-modified-info-plist source-contents-path
                                    target-contents-path
                                    executable-name
                                    bundle-name
                                    application-icns
                                    identifier
                                    signature
                                    version
                                    build
                                    document-types)
          (if signature
              (with-open-file (pkginfo (merge-pathnames "PkgInfo" target-contents-path)
                                       :element-type 'character
                                       :if-exists :supersede
                                       :direction :output)
                (write-string "APPL" pkginfo)
                (write-string signature pkginfo))
            (copy-file (merge-pathnames "PkgInfo" source-contents-path)
                       (merge-pathnames "PkgInfo" target-contents-path)))
          (let ((resource-to-copy '())
                (resource-to-skip '()))
            (when application-icns
              (push (translate-logical-pathname application-icns)
                    resource-to-copy)
              ;; Skip the default app.icns.
              (push (merge-pathnames (make-pathname :name "app" :type "icns")
                                     source-resources-path)
                    resource-to-skip))
            (when document-types
              (dolist (type document-types)
                (destructuring-bind (name extensions icns-file &optional os-types role)
                    type
                  (declare (ignore name extensions os-types))
                  (push (translate-logical-pathname icns-file)
                        resource-to-copy)))
              ;; Skip all other icns files.
              (push (merge-pathnames (make-pathname :name :wild :type "icns")
                                     source-resources-path)
                    resource-to-skip))
            (push (parse-namestring (concatenate 'string 
                                                 (namestring source-resources-path)
                                                 ".svn/"))
                  resource-to-skip)
            (copy-files resource-to-copy
                        target-resources-path)
            (copy-files (directory 
                         (merge-pathnames (make-pathname :name :wild 
                                                         :type :wild) 
                                          source-resources-path))
                        target-resources-path
                        resource-to-skip))
          (merge-pathnames executable-name
                           target-contents-macos-path))))))

(defun choose-template-bundle-path (template-bundle)
  (if template-bundle
      (let ((path (probe-file template-bundle)))
        (unless (and path
                     (probe-file (merge-pathnames "Contents/Info.plist"
                                                  path)))
          (error "Cannot find template bundle ~A."
                 template-bundle))
        path)
    (let* ((source-executable-name (truename (lisp-image-name)))
           (source-executable-dir (pathname-directory source-executable-name))
           (source-executable-dir-len (length source-executable-dir)))
      (unless (and (> source-executable-dir-len 3)
                   (equalp (subseq source-executable-dir
                                   (- source-executable-dir-len 2))
                          '("Contents" "MacOS")))
        (error "Cannot find application directory for ~A."
               source-executable-name))
      (make-pathname :name nil
                     :type nil
                     :version nil
                     :directory (subseq source-executable-dir
                                        0
                                        (- source-executable-dir-len 2))
                     :defaults source-executable-name))))


(defun make-application-subpaths (path)
  (let* ((contents-path (merge-pathnames
                         (make-pathname :directory '(:relative "Contents"))
                         path))
         (contents-macos-path (merge-pathnames
                               (make-pathname :directory '(:relative "MacOS"))
                               contents-path))
         (resource-path (merge-pathnames
                         (make-pathname :directory '(:relative "Resources"))
                         contents-path)))
    (values contents-path contents-macos-path resource-path)))

(defun copy-modified-info-plist (source-contents-path target-contents-path
                                                      executable-name
                                                      bundle-name
                                                      application-icns
                                                      identifier
                                                      signature
                                                      version
                                                      build
                                                      document-types)
  (with-open-file (info-in (merge-pathnames "Info.plist"
                                            source-contents-path))
    (with-open-file (info-out (merge-pathnames "Info.plist"
                                               target-contents-path)
                              :if-exists :supersede
                              :direction :output)
      (let ((execp nil)
            (bundlep nil)
            (appicnsp nil)
            (identifierp nil)
            (signaturep nil)
            (versionp nil)
            (versionstring (and version build
                                (format nil "~A~@[ (Build ~A)~]"
                                        version build)))
            (versionstringp nil)
            (wroteversionstringp nil))
        (loop (let ((line (read-line info-in nil nil)))
                (unless line
                  (return))
                (when (cond ((and document-types
                                  (search "<key>CFBundleDocumentTypes</key>" line))
                             (write-document-types document-types info-in info-out)
                             nil)
                            ((search "<key>CFBundleExecutable</key>" line)
                             (setq execp t))
                            ((and application-icns
                                  (search "<key>CFBundleIconFile</key>" line))
                             (setq appicnsp t))
                            ((search "<key>CFBundleName</key>" line)
                             (setq bundlep t))
                            ((and identifier
                                  (search "<key>CFBundleIdentifier</key>" line))
                             (setq identifierp t))
                            ((and signature
                                  (search "<key>CFBundleSignature</key>" line))
                             (setq signaturep t))
                            ((and version
                                  (search "<key>CFBundleVersion</key>" line))
                             (setq versionp t))
                            ((and versionstring
                                  (search "<key>CFBundleShortVersionString</key>" line))
                             (setq versionstringp t))
                            ((and versionstring
                                  (search "</dict>" line))
                             (unless wroteversionstringp
                               (write-char #\Tab info-out)
                               (write-line "<key>CFBundleShortVersionString</key>" info-out)
                               (write-char #\Tab info-out)
                               (format info-out "<string>~A</string>~%" versionstring))
                             t)
                            (execp
                             (write-modified-info-value
                              line executable-name info-out)
                             (setq execp nil))
                            (bundlep
                             (write-modified-info-value
                              line bundle-name info-out)
                             (setq bundlep nil))
                            (appicnsp
                             (write-modified-info-value
                              line
                              (file-namestring (translate-logical-pathname application-icns))
                              info-out)
                             (setq appicnsp nil))
                            (identifierp
                             (write-modified-info-value
                              line identifier info-out)
                             (setq identifierp nil))
                            (signaturep
                             (write-modified-info-value
                              line signature info-out)
                             (setq signaturep nil))
                            (versionp
                             (write-modified-info-value
                              line version info-out)
                             (setq versionp nil))
                            (versionstringp
                             (write-modified-info-value
                              line versionstring info-out)
                             (setq wroteversionstringp t)
                             (setq versionstringp nil))
                            (t t))
                  (write-line line info-out))))))))

(defun write-document-types (document-types info-in info-out)
  ;; First skip the original document types
  (assert (search "<array>" (read-line info-in nil nil)) () "Malformed Info.plist")
  (let ((array-depth 1)
        (dict-depth 0))
    (loop until (and (zerop array-depth) (zerop dict-depth))
          as line = (read-line info-in nil nil)
          when (null line)
            do (error "Malformed Info.plist")
          do (cond ((search "<array>" line)
                    (incf array-depth))
                   ((search "</array>" line)
                    (decf array-depth))
                   ((search "<dict>" line)
                    (incf array-depth))
                   ((search "</dict>" line)
                    (decf array-depth)))))
  ;; Now write the new definitions
  (flet ((writer (string tab-count)
           (dotimes (i tab-count)
             (write-char #\Tab info-out))
           (write-line string info-out))
         (string-writer (string tab-count)
           (dotimes (i tab-count)
             (write-char #\Tab info-out))
           (write-string "<string>" info-out)
           (write-string string info-out)
           (write-line "</string>" info-out)))
    (writer "<key>CFBundleDocumentTypes</key>" 1)
    (writer "<array>" 1)
    (dolist (type document-types)
      (destructuring-bind (name extensions icns-file &optional
                                (os-types '("****"))
                                (role "Editor"))
          type
        (writer "<dict>" 2)
        (writer "<key>CFBundleTypeExtensions</key>" 3)
        (writer "<array>" 3)
        (dolist (extension extensions)
          (string-writer extension 4))
        (writer "</array>" 3)
        (writer "<key>CFBundleTypeName</key>" 3)
        (string-writer name 3)
        (writer "<key>CFBundleTypeOSTypes</key>" 3)
        (writer "<array>" 3)
        (dolist (os-type os-types)
          (string-writer os-type 4))
        (writer "</array>" 3)
        (writer "<key>CFBundleTypeIconFile</key>" 3)
        (string-writer (file-namestring (translate-logical-pathname icns-file)) 3)
        (writer "<key>CFBundleTypeRole</key>" 3)
        (string-writer role 3)
        (writer "</dict>" 2)))
    (writer "</array>" 1)
    nil))

(defun write-modified-info-value (line value info-out)
  (let ((start (search "<string>" line))
        (end (search "</string>" line)))
    (unless (and start end (< start end))
      (error "Expected string key, found ~S."
             line))
    (write-string line info-out
                  :end (+ start 8))
    (write-string value info-out)
    (write-line line info-out
                :start end)))

(defun copy-files (source-paths target-directory-path
                                &optional source-paths-to-skip)
  (dolist (file source-paths)
    (unless (find file source-paths-to-skip :test 'pathname-match-p)
      (copy-file file 
                 (merge-pathnames (make-pathname :name (pathname-name file) 
                                                 :type (pathname-type file))
                                  target-directory-path)))))

#-:lispworks6.1
(defun copy-file (source-path target-path)
  (with-open-file (in source-path :element-type '(unsigned-byte 8))
    (with-open-file (out target-path :element-type '(unsigned-byte 8)
                         :if-exists :supersede
                         :direction :output)
      (loop for byte = (read-byte in nil nil)
            while byte
            do
            (write-byte byte out)))))

