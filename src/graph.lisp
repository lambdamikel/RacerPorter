;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;


(defclass network-node (item-pinboard-object graph-node)
  ((text :accessor text :initform nil :initarg :text)
   (items :accessor items :initform nil :initarg :items)
   (highlighted :accessor highlighted :initform nil :initarg :highlighted)
   (selected :accessor selected :initform nil :initarg :selected)))

(defclass network-edge (labelled-arrow-pinboard-object graph-edge)
  ((text :accessor text :initform nil :initarg :text)
   (highlighted :accessor highlighted :initform nil :initarg :highlighted)
   (selected :accessor selected :initform nil :initarg :selected)
   (capi::thickness :initform 10)
   (capi::head-length :initform 10)
   (capi::head-breadth :initform 10)))

(defmethod draw-pinboard-object (pinboard (network-node network-node) &key)
  (with-geometry network-node
    (let* ((highlighted (highlighted network-node))
           (selected (selected network-node))
           (foreground (if selected
                           +selected-color+
                         (simple-pane-foreground pinboard)))
           (background (simple-pane-background pinboard)))         
           
      (let ((items 
             (items network-node))
            (text 
             (text  network-node))
            (y 0)
            (first t))
          
        (dolist (item (cons text items))
          (let* ((item
                  (if nil ;; (> (length item) 50)
                      (format nil "~A..." (subseq item 0 50))
                    item))
                 (height
                  (multiple-value-bind (left top right bottom)
                      (gp:get-string-extent pinboard item)
                    (declare (ignore left right))
                    (- bottom top))))

            (when (and *printing* 
                       (zerop y))
              (setf y height))

            (unless first
              (setf foreground
                    +graph-is-a-color+))

            (setf first nil)

            (if t ; (not *printing*)
                (gp:draw-x-y-adjusted-string pinboard
                                             item
                                             capi:%x%
                                             (+ capi:%y% (round y))
                                             :y-adjust :top
                                             :foreground (if highlighted background foreground)
                                             :background (if highlighted foreground background)
                                             :block t)

              (gp:draw-x-y-adjusted-string pinboard
                                           item
                                           capi:%x%
                                           (round y)
                                           :y-adjust :top
                                           :block t))
            (incf y height)))))))
        
  
(defmethod draw-pinboard-object-highlighted (pinboard (network-node network-node) &key)
  (setf (highlighted network-node) t)
  (draw-pinboard-object pinboard network-node))  

(defmethod draw-pinboard-object-unhighlighted (pinboard (network-node network-node) &key)
  (setf (highlighted network-node) nil)
  (draw-pinboard-object pinboard network-node))

