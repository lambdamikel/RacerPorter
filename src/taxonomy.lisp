;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(defmacro all-top-concepts ()
  ``(racer-user::*top*
     racer-user::|*top*| 
     racer-user::top
     racer-user::|top|))

(defmacro all-bottom-concepts ()
  ``(racer-user::*bottom*
     racer-user::|*bottom*| 
     racer-user::bottom
     racer-user::|bottom|))

(defun get-top-concept ()
  (with-profile-access
    (if alisp-racer
        `(racer-user::*top* racer-user::top)
      `(racer-user::|*top*| racer-user::|top|))))

(defun get-bottom-concept () 
  (with-profile-access
    (if alisp-racer
        '(racer-user::*bottom* racer-user::bottom)
      '(racer-user::|*bottom*| racer-user::|bottom|))))


(defun make-taxonomy-graph (hash taxonomy)
  (with-profile-access

    (loop as (con nil children) in taxonomy do

          (let* ((con (ensure-list con))
                 
                 (children (mapcar #'ensure-list children))
                 
                 (children 
                  
                  (remove-if #'(lambda (x) 
                                 (or 
                                  
                                  (and (not show-bottom)
                                       (intersection x (get-bottom-concept)))

                                  (and (not show-top)
                                       (intersection x (get-top-concept)))))

                             children))
                 
                 (children (sort children #'compare-fn-concepts :key #'first))
                 (entry (list con children)))

            (dolist (node con)
              (cond ((member node (get-top-concept))
                     (dolist (node (get-top-concept))
                       ;;; hier muss sichergestellt werden, dass auch 
                       ;;; Eintraege fuer *top* und top vorliegen, denn
                       ;;; evtl. wird SelectParents *top* (und nicht top)
                       ;;; als current selektieren -> würde nur "" im Graphen liefern 
                       (setf (gethash node hash) entry)))
                    ((member node (get-bottom-concept))
                     (dolist (node (get-bottom-concept))
                       ;;; hier muss sichergestellt werden, dass auch 
                       ;;; Eintraege fuer *top* und top vorliegen, denn
                       ;;; evtl. wird SelectParents *top* (und nicht top)
                       ;;; als current selektieren -> würde nur "" im Graphen liefern 
                       (setf (gethash node hash) entry)))
                      (t
                       (setf (gethash node hash) entry))))))
    
    hash))


(defun build-taxonomy-graph ()
  (with-profile-request

    (with-profile-access

      (let ((hash (make-hash-table :test #'equal 
                                   :size 1000
                                   :rehash-size 2.0)))

        (let ((tax
               (racer-function1
                (taxonomy sirius-current-tbox))))

          (make-taxonomy-graph hash tax))))))

                   
(defun request-taxonomy ()
  (with-profile-access 
    (start-pooled-process "request taxonomy"
                          ;;; notwendig, damit abbrechbar!
                          
                          (setf taxonomy-graph-pane nil)
                          (setf taxonomy-graph *process*)

                          ;;; Informieren ("Computing Taxonomy...")
                          (schedule-update-no-history-entry :taxonomy)
    
                          (handler-case 
                              (progn 
                                (setf taxonomy-graph
                                      (build-taxonomy-graph))
                                
                                (setf (taxonomy-graph-in-history)
                                      taxonomy-graph))
                            (error (error)
                              (let ((error (format nil "~A" error)))
                                (when (search "aborted" error)
                                  ;;(display-message error)
                                  (setf graph-drawing-aborted t))
                                (setf autofocus nil))))

                          (schedule-update-no-history-entry :taxonomy))))



(defun clear-taxonomy-roots ()
  (with-profile-access
    (setf taxonomy-graph-pane-roots nil
          saved-taxonomy-graph-pane nil)))


(defun compute-taxonomy-roots ()
  (with-profile-access
    (when sirius-current-tbox
      (if (and freeze-graph 
               taxonomy-graph-pane-roots)
          taxonomy-graph-pane-roots
        (let* ((all-concepts 
                (delete-if 
                 #'(lambda (x) 
                     (or (member x (all-top-concepts))
                         (and (not show-bottom)
                              (member x (all-bottom-concepts)))))
                    
                 (mapcar #'(lambda (x) 
                             (let ((x (ensure-list x)))
                               (first x)))

                         (let ((res (racer-function1 (all-atomic-concepts sirius-current-tbox) :error)))
                           (if (eq res :error)
                               (return-from compute-taxonomy-roots :error)
                             res)))))
               (roots 
                (ecase taxonomy-root-kind
                  (:all
                   (if show-top
                       (list (second (get-top-concept)))
                     all-concepts))
                  (:current 
                   (if current-concept
                       (list current-concept)
                     (unless (eq 'unbounded taxonomy-max-depth)
                       ;;; Drill Down? Dann Wurzel-Existenz sicherstellen
                       (list (second (get-top-concept))))))
                  (:selected 
                   (or (profile-all-selected-concepts)
                       (unless (eq 'unbounded taxonomy-max-depth)
                         (list (second (get-top-concept))))))))

               (roots
                (remove-duplicates
                 (mapcar #'(lambda (x) 
                             (first (first (gethash x taxonomy-graph))))
                         roots)))
             
               (roots
                (mapcar #'(lambda (x) 
                            (list x 0))
                        (sort (copy-list roots)
                              #'compare-fn-concepts))))

          (setf taxonomy-graph-pane-roots roots)

          roots)))))
                                        

(defun request-taxonomy-pane ()
  (with-profile-access
    (when sirius-current-tbox
      (let* ((roots
              (compute-taxonomy-roots)))

        (cond (saved-taxonomy-graph-pane
             
               (setf taxonomy-graph-pane 
                     saved-taxonomy-graph-pane
                     
                     (graph-pane-roots taxonomy-graph-pane)
                     roots))
              
              (t

               (setf taxonomy-graph-pane
                     (make-instance 'graph-pane
                                    
                                    :font *graph-font*
                                    
                                    :pane-menu nil
                                  
                                    :layout-function taxonomy-orientation
                           
                                    :test-function #'(lambda (x y)
                                                       (ecase taxonomy-kind
                                                         (:tree nil)
                                                         (:graph      
                                                          (and (not (third x))
                                                               (not (third y))
                                                               (equalp (first x)
                                                                       (first y))))))

                                    :highlight-style :draw

                                    :callback-type :data

                                    :selection-callback 
                                    #'(lambda (data)

                                        (let* ((maxdepth-p (third data))
                                               (data (first data))
                                               (syns  
                                                (first 
                                                 (gethash data taxonomy-graph))))

                                          (cond ((member (enter-mouse-select-command
                                                          `(concept? ,data ,sirius-current-tbox))
                                                         '(t racer-user::\t))

                                                 (cond (maxdepth-p 

                                                        (setf taxonomy-graph
                                                              (copy-hash-table taxonomy-graph))
                                                      
                                                        (dolist (node syns)
                                                          (setf (gethash node taxonomy-graph)
                                                                (append (gethash node taxonomy-graph) '(t)))))
                                                
                                                       (t
                                               
                                                        (toggle-concepts (list data))))

                                                 (next-update-refresh-taxonomy)
                                        
                                                 (schedule-update :taxonomy))

                                                (t (beep-pane)))))
                                  
                                    :node-pane-function
                                    #'(lambda (graph-pane node1)
                                        (declare (ignore graph-pane))
                                        (let* ((node (first node1))
                                               (maxdepth-p (third node1))
                                               (syns
                                                (first 
                                                 (gethash  
                                                  (first (ensure-list node))
                                                  taxonomy-graph))))

                                          (make-instance 'network-node
                                                         :selected 
                                                         (some #'(lambda (x) 
                                                                   (and (selected-concept? x)
                                                                        (not (equal x current-concept))))
                                                               syns)
                                                         :highlighted 
                                                         (member current-concept syns
                                                                 :test #'equal)
                                                         :text 
                                                         (cond (maxdepth-p
                                                                (line-item-printer +maxdepth-marker+))
                                                               (t (line-item-printer syns))))))

                                    :children-function 
                                    #'(lambda (node1)        
                                        (let* ((node (first node1))
                                               (depth (second node1))
                                               (maxdepth-p (third node1))

                                               (children 
                                                (second 
                                                 (gethash node taxonomy-graph))))

                                          (when (and children
                                                     (or (not maxdepth-p)
                                                         (third (gethash node taxonomy-graph))))
                                              
                                            (if (and (numberp taxonomy-max-depth)
                                                     (>= depth taxonomy-max-depth)
                                                     ;;; expanded manually? 
                                                     (not (third (gethash node taxonomy-graph))))

                                                (list (list node (1+ depth) t))

                                              (mapcar #'(lambda (x) 
                                                          (list (first (ensure-list x)) (1+ depth)))
                                                      children)))))

                                    :roots roots)
                    
                     saved-taxonomy-graph-pane taxonomy-graph-pane)))))))
