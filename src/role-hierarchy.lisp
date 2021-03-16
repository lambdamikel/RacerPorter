;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(defun get-top-role ()
  (with-profile-access
    (if alisp-racer
        `racer-user::*top-object-role*
      `racer-user::|*top-object-role*|)))

(defun get-bottom-role () 
  (with-profile-access
    (if alisp-racer
        `racer-user::*bottom-object-role*
      `racer-user::|*bottom-object-role*|)))


(defun make-role-hierarchy-graph (hash role-hierarchy)
  (with-profile-access

    (loop as (con nil children) in role-hierarchy do

          (dolist (node con)
            (setf (gethash node hash)
                  (list con children))))

    hash))


(defun build-role-hierarchy-graph ()
  (with-profile-request

    (with-profile-access

      (let ((hash (make-hash-table :test #'equal 
                                   :size 1000
                                   :rehash-size 2.0)))

        (let ((rh
               (racer-function1
                (get-role-hierarchy sirius-current-tbox))))

          (make-role-hierarchy-graph hash rh))))))

                   
(defun request-role-hierarchy ()
  (with-profile-access 
    (start-pooled-process "request role hierarchy"
                          ;;; notwendig, damit abbrechbar!
                          
                          (setf role-hierarchy-graph-pane nil)
                          (setf role-hierarchy-graph *process*)

                          ;;; Informieren ("Computing Role-Hierarchy...") 
                          (schedule-update-no-history-entry :role-hierarchy)
    
                          (handler-case 
                              (progn 
                                (setf role-hierarchy-graph
                                      (build-role-hierarchy-graph))
                                
                                (setf (role-hierarchy-graph-in-history)
                                      role-hierarchy-graph))
                            (error (error)
                              (let ((error (format nil "~A" error)))
                                (when (search "aborted" error)
                                  ;;(display-message error)
                                  (setf graph-drawing-aborted t))
                                (setf autofocus nil))))

                          (schedule-update-no-history-entry :role-hierarchy))))

(defun clear-role-hierarchy-roots ()
  (with-profile-access
    (setf role-hierarchy-graph-pane-roots nil
          saved-role-hierarchy-graph-pane nil)))
  

(defun compute-role-hierarchy-roots ()
  (with-profile-access
    (when sirius-current-tbox
      (if (and freeze-graph 
               role-hierarchy-graph-pane-roots)
          role-hierarchy-graph-pane-roots
    
        (let* ((roots 
                (ecase role-hierarchy-root-kind
                  (:all
                   (list (get-top-role)))
                  (:current 
                   (if current-role
                       (list current-role)
                     (unless (eq 'unbounded role-hierarchy-max-depth)
                       (list (get-top-role)))))
                  (:selected 
                   (or (profile-all-selected-roles)
                       (unless (eq 'unbounded role-hierarchy-max-depth)
                         (list (get-top-role)))))))

               (roots
                (remove-duplicates
                 (mapcar #'(lambda (x) 
                             (first (first (gethash x role-hierarchy-graph))))
                         roots)))
           
               (roots
                (mapcar #'(lambda (x) 
                            (list x 0))
                        (sort (copy-list roots)
                              #'compare-fn-roles))))

          (setf role-hierarchy-graph-pane-roots roots)

          roots)))))
    

(defun request-role-hierarchy-pane ()
  (with-profile-access
    (when sirius-current-tbox
      (let* ((roots 
              (compute-role-hierarchy-roots)))

        (cond (saved-role-hierarchy-graph-pane
             
               (setf role-hierarchy-graph-pane
                     saved-role-hierarchy-graph-pane
                     
                     (graph-pane-roots role-hierarchy-graph-pane)
                     roots))              
              (t
               
               (setf role-hierarchy-graph-pane
                     (make-instance 'graph-pane

                                    :font *graph-font*
                           
                                    :pane-menu nil
                           
                                    :layout-function role-hierarchy-orientation
                           
                                    :test-function #'(lambda (x y)
                                                       (ecase role-hierarchy-kind
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
                                                 (gethash data role-hierarchy-graph))))

                                          (cond ((or (eq data (get-top-role))
                                                     (member (enter-mouse-select-command
                                                              `(role? ,data ,sirius-current-tbox))
                                                             '(t racer-user::\t)))

                                                 (cond (maxdepth-p 

                                                        (setf role-hierarchy-graph
                                                              (copy-hash-table role-hierarchy-graph))
                                                        
                                                        (dolist (node syns)
                                                          (setf (gethash node role-hierarchy-graph)
                                                                (append (gethash node role-hierarchy-graph)
                                                                        '(t))))
                                                        
                                                        (next-update-refresh-role-hierarchy)
                                                        (schedule-update :role-hierarchy))
                                                
                                                       (t
                                               
                                                        (toggle-roles (list data))

                                                        (schedule-update :role-hierarchy))))

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
                                                  role-hierarchy-graph))))

                                          (make-instance 'network-node
                                                         :selected 
                                                         (some #'(lambda (x) 
                                                                   (and (selected-role? x)
                                                                        (not (equal x current-role))))
                                                               syns)
                                                         :highlighted 
                                                         (member current-role syns
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
                                                 (gethash node role-hierarchy-graph))))

                                          (when (and children
                                                     (or (not maxdepth-p)
                                                         (third (gethash node role-hierarchy-graph)))
                                                     (not (equal children '((racer-user::bottom-role))))
                                                     (not (equal children '((racer-user::*bottom-object-role*))))
                                                     (not (equal children '((racer-user::|bottom-role|))))
                                                     (not (equal children '((racer-user::|*bottom-object-role*|)))))
                                              
                                            (if (and (numberp role-hierarchy-max-depth)
                                                     (>= depth role-hierarchy-max-depth)
                                                     ;;; expanded manually? 
                                                     (not (third (gethash node role-hierarchy-graph))))

                                                (list (list node (1+ depth) t))

                                              (mapcar #'(lambda (x) 
                                                          (list (first (ensure-list x)) (1+ depth)))
                                                      children)))))

                                    :roots roots)
                    
                   
                     saved-role-hierarchy-graph-pane role-hierarchy-graph-pane)))))))

