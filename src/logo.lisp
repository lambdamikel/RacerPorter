;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defconstant +logo-image+
    #+:racer-with-sirius
  "sirius:images;racerplus-logo.jpg"
    #-:racer-with-sirius
    "sirius:images;racerporter-logo.jpg"))
  
#-:lispworks6
(defun display-logo-pane (logo-pane &rest args)
  (declare (ignorable args))

  (handler-case
      (with-sirius-app (app)
        (with-slots (image ext-image) app
          (unless 
              #+:macosx nil
            #-:macosx image 
            (let ((external-image *inline-logo-image*))
              (setf ext-image external-image
                    image (gp:load-image logo-pane external-image))
              (when image
                ;(gp:invalidate-rectangle logo-pane)
                (set-horizontal-scroll-parameters logo-pane 
                                                  :min-range 0 :max-range
                                                  (if image (gp:image-width image) 0))
                (set-vertical-scroll-parameters logo-pane
                                                :min-range 0 :max-range
                                                (if image (gp:image-height image) 0)))))

          (when image

            (let ((xoff (- (/ (gp:port-width logo-pane) 2) 
                           (/ (gp:image-width image) 2)))
                  (yoff (- (/ (gp:port-height logo-pane) 2) 
                           (/ (gp:image-height image) 2))))
        
              (gp:draw-image logo-pane image xoff yoff)))))

    (error (error) 
           (apply #'display-logo-pane2 logo-pane args))))

#+:lispworks6
(defun display-logo-pane (logo-pane &rest args)
  (declare (ignorable args))

  (handler-case
      (with-sirius-app (app)
        (with-slots (image ext-image) app
          (unless  
              #+:macosx nil
            #-:macosx image 
            (let ((external-image 
                   #.(gp:read-external-image +logo-image+)))

              (setf ext-image external-image
                    image (gp:load-image logo-pane external-image))
              (when image
                ;(gp:invalidate-rectangle logo-pane)
                (set-horizontal-scroll-parameters logo-pane 
                                                  :min-range 0 :max-range
                                                  (if image (gp:image-width image) 0))
                (set-vertical-scroll-parameters logo-pane
                                                :min-range 0 :max-range
                                                (if image (gp:image-height image) 0)))))

          (when image

            (let ((xoff (- (/ (gp:port-width logo-pane) 2) 
                           (/ (gp:image-width image) 2)))
                  (yoff (- (/ (gp:port-height logo-pane) 2) 
                           (/ (gp:image-height image) 2))))
        
              (gp:draw-image logo-pane image xoff yoff)))))

    (error (error) 
      )))

#-:lispworks6
(defun display-logo-pane2 (logo-pane &rest args)
  (declare (ignorable args))

  (when *sirius-logo-rgb-data*

    (let* ((rgb (copy-list *sirius-logo-rgb-data*))
           (dim (pop rgb))
           (width (first dim))
           (height (second dim)))

      (set-horizontal-scroll-parameters logo-pane  
                                        :min-range 0 :max-range width)
      (set-vertical-scroll-parameters logo-pane
                                      :min-range 0 :max-range height)
      
      (let* ((xoff (round (- (/ (gp:port-width logo-pane) 2)
                             (/ width 2))))
             (yoff (round (- (/ (gp:port-height logo-pane) 2) 
                             (/ height 2)))))
        
        (dotimes (y height)
          (declare (fixnum y))
          (dotimes (x width)
            (declare (fixnum x))
            (let* ((x1 (+ xoff x))
                   (y1 (+ yoff y)))
              (declare (fixnum x1) (fixnum y1))
              (gp:draw-point logo-pane  x1 y1
                             :foreground (pop rgb)))))))))

;;;
;;;
;;;

#+:sirius-dev
(defun logo-converter (in-file)

  (cond ((probe-file in-file)

         (setf *inline-logo-image* nil
               *sirius-logo-rgb-data* nil)

         (let* ((dialog nil)
                (ext-image nil)
                (image nil)

                (logo
                 (make-instance 'output-pane
                                :display-callback 
                                #'(lambda (pane &rest args)
                                    (declare (ignorable args))

                                    (unless image
                                      (setf ext-image (gp:read-external-image in-file))
                                      (setf image (gp:load-image pane ext-image))

                                      (gp:invalidate-rectangle pane)
                                      (set-horizontal-scroll-parameters pane 
                                                                        :min-range 0 :max-range
                                                                        (if image (gp:image-width image) 0))
                                      (set-vertical-scroll-parameters pane
                                                                      :min-range 0 :max-range
                                                                      (if image (gp:image-height image) 0)))

                                    (when image
                                        
                                      (let ((xoff (- (/ (gp:port-width pane) 2) 
                                                     (/ (gp:image-width image) 2)))
                                            (yoff (- (/ (gp:port-height pane) 2) 
                                                     (/ (gp:image-height image) 2))))
                                          
                                        (gp:draw-image pane image xoff yoff))))
                                  
                                :background +logo-bc+
                                :visible-min-width 813
                                :visible-min-height 641
                                :horizontal-scroll nil
                                :vertical-scroll nil))

                (ok 
                 (make-instance 'push-button
                                :callback-type :none
                                :callback #'(lambda ()

                                              (with-open-file
                                                  (stream (translate-logical-pathname 
                                                           #+:racer-with-sirius
                                                           "sirius:racerplus-image-data;image-data1.lisp"
                                                           #-:racer-with-sirius
                                                           "sirius:racerpro-image-data;image-data1.lisp"
                                                           )
                                                          :direction :output
                                                          :if-does-not-exist :create
                                                          :if-exists :supersede)

                                                (format stream ";;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-~%")
                                                (format stream ";;; generated by (logo-converter ~S) defined in logo*.lisp~%~%" in-file)

                                                (format stream "(in-package sirius)~%~%")
                                                (format stream "(defconstant +image-data+ ~S)~%~%" (slot-value ext-image 'gp::data))
                                                (format stream "(setf *inline-logo-image*
        (make-instance 
         'gp:external-image
         :data +image-data+
         :transparent-color-index nil
         :type :jpg))~%~%"))

                                              (with-open-file 
                                                  (stream (translate-logical-pathname
                                                           #+:racer-with-sirius
                                                           "sirius:racerplus-image-data;image-data2.lisp"
                                                           #-:racer-with-sirius
                                                           "sirius:racerpro-image-data;image-data2.lisp")
                                                          :direction :output
                                                          :if-does-not-exist :create
                                                          :if-exists :supersede)

                                                (format stream ";;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-~%")
                                                (format stream ";;; generated by (logo-converter ~S) defined in logo*.lisp~%~%" in-file)

                                                (format stream "(in-package sirius)~%~%")
                                                (format stream "#+:linux~%")
                                                        
                                                (let ((access
                                                       (gp:make-image-access logo image))
                                                      (rgb-data nil))
                                                          
                                                  (gp:image-access-transfer-from-image access)

                                                  (push (list (gp:image-access-width access)
                                                              (gp:image-access-height access))
                                                        rgb-data)
                                                
                                                  (dotimes (y (gp:image-access-height access))
                                                    (dotimes (x (gp:image-access-width access))
                                                      (let* ((pixel (gp:image-access-pixel access x y)))
                                                        (push pixel rgb-data))))

                                                  (gp:free-image-access access)
                                                  
                                                  (format stream "(setf *sirius-logo-rgb-data* (quote ~S))~%"
                                                          (reverse rgb-data))))

                                              (quit-interface dialog))

                                :text "Save")))
        
           (contain 
            (setf dialog 
                  (make-instance 'column-layout
                                 :description
                                 (list logo ok)
                                 :adjust :center))
         
            :auto-menus nil
            :title (format nil "Convert Image ~A -> \"sirius:image-data1(2).lisp\"." in-file))))

        (t (display-message "Cannot find ~A!" in-file))))


#+:sirius-dev
(defun batch-converter (directory file list-of-files init-form)

  (with-open-file (stream file 
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)

    (format stream ";;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-~%")
    (format stream ";;; generated by (batch-converter ~S ~S '~S) defined in logo*.lisp~%~%" 
            directory
            file
            list-of-files)

    (format stream "(in-package sirius)~%~%")

    (dolist (in-file list-of-files)
      (let ((in-file
             (make-pathname :directory (pathname-directory directory)
                            :name in-file
                            :type "bmp")))
        
        (cond ((probe-file in-file)

               (setf *inline-logo-image* nil
                     *sirius-logo-rgb-data* nil)

               (let* ((dialog nil)
                      (ext-image nil)
                      (image nil)

                      (logo
                       (make-instance 'output-pane
                                      :display-callback 
                                      #'(lambda (pane &rest args)
                                          (declare (ignorable args))

                                          (unless image
                                            (setf ext-image (gp:read-external-image in-file))
                                            (setf image (gp:load-image pane ext-image))

                                            (gp:invalidate-rectangle pane)
                                            (set-horizontal-scroll-parameters pane 
                                                                              :min-range 0 :max-range
                                                                              (if image (gp:image-width image) 0))
                                            (set-vertical-scroll-parameters pane
                                                                            :min-range 0 :max-range
                                                                            (if image (gp:image-height image) 0)))

                                          (when image
                                        
                                            (let ((xoff (- (/ (gp:port-width pane) 2) 
                                                           (/ (gp:image-width image) 2)))
                                                  (yoff (- (/ (gp:port-height pane) 2) 
                                                           (/ (gp:image-height image) 2))))
                                          
                                              (gp:draw-image pane image xoff yoff))

                                            ;;;
                                            ;;; Save
                                            ;;; 

                                            (format stream "(defconstant +~A-data+ ~S)~%~%" 
                                                    (pathname-name in-file)
                                                    (slot-value ext-image 'gp::data))
                                            (format stream "(defvar *inline-~A-image*
        (make-instance 
         'gp:external-image
         :data +~A-data+
         :transparent-color-index nil
         :type :jpg))~%~%" 
                                                    (pathname-name in-file)
                                                    (pathname-name in-file)))

                                          (sleep 0.3)
                                          
                                          (exit-dialog dialog))
                                          
                                      :background +logo-bc+
                                      :visible-min-width 100
                                      :visible-min-height 100
                                      :horizontal-scroll nil
                                      :vertical-scroll nil)))
                      
                 (setf dialog 
                        (make-instance 'column-layout
                                       :description
                                       (list logo)
                                       :adjust :center))

                 #-:mac (contain dialog
                                 :auto-menus nil
                                 :title (format nil "Convert Image ~A" 
                                                (format nil "~A -> ~A~A-data.lisp" 
                                                        in-file
                                                        (truename (image-directory))
                                                        (pathname-name (namestring in-file)))))

                 #+:mac 
                 (display 
                  (setf dialog 
                        (make-container
                   dialog
                   :title 
                   (format nil "Convert Image ~A" 
                           (format nil "~A -> ~A~A-data.lisp" 
                                   in-file
                                   (truename (image-directory))
                                   (pathname-name (namestring in-file)))))))))

              (t (display-message "Cannot find ~A!" in-file))))

      (sleep 0.5))

    (pprint init-form stream)))

      

