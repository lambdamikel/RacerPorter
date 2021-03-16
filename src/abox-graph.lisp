;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(defvar *abox-node-id-counter* 0)

(defvar *cur-graph*) ; for debugging

(defun make-abox-graph (hash abox)
  (with-profile-access

    (loop as (inds succs dtps ans) in abox do 
          (if (member succs '(:|successors-found|
                              :successors-found))
            
              (dolist (ind inds)
                (setf (gethash ind hash) (list (list ind) :succs-found))
                (setf (gethash (list ind :label) hash) 
                      (if told-only
                          (remove-if #'consp
                                     (mapcar #'second
                                             (racer-function-evaluated
                                              `(racer-user::all-concept-assertions-for-individual ,ind ,sirius-current-abox))))
                        (tflatten 
                         (racer-function-evaluated
                          `(racer-user::individual-direct-types ,ind ,sirius-current-abox))))))
                        
            
            (let ((entry 
                   (list inds 
                         (reduce #'append (mapcar #'second succs))
                         (reduce #'append (mapcar #'second dtps))
                         (reduce #'append (mapcar #'second ans)))))
            
              (dolist (ind inds)
                (setf (gethash ind hash) entry)
                (setf (gethash (list ind :label) hash) 
                      (if told-only
                          (remove-if #'consp
                                     (mapcar #'second
                                             (racer-function-evaluated
                                              `(racer-user::all-concept-assertions-for-individual ,ind ,sirius-current-abox))))
                        (tflatten 
                         (racer-function-evaluated
                          `(racer-user::individual-direct-types ,ind ,sirius-current-abox))))))

              (let ((i (first inds)))
                (dolist (succ succs)
                  (let ((role (first succ))
                        (inds (mapcar #'first (second succ))))
                    (dolist (j inds)

                      (if (gethash (cons i j) hash)
                          (pushnew role (gethash (cons i j) hash))
                        (setf (gethash (cons i j) hash)
                              (list role))))))
                
                (dolist (dtp dtps)
                  (let ((role (first dtp))
                        (literals (second dtp)))
                    (dolist (j literals)
                      (setf (gethash j hash) (list j))
                      (if (gethash (cons i j) hash)
                          (pushnew role (gethash (cons i j) hash))
                        (setf (gethash (cons i j) hash)
                              (list role))))))
            
                (dolist (an ans)
                  (let ((role (first an))
                        (literals (second an)))
                    (dolist (j literals)
                      (setf (gethash j hash) (list j))
                      (if (gethash (cons i j) hash)
                          (pushnew role (gethash (cons i j) hash))
                        (setf (gethash (cons i j) hash)
                              (list role))))))))))

    (setf *cur-graph* hash)))


(defun build-abox-graph ()
  (with-profile-request

    (with-profile-access

      (let ((hash (make-hash-table :test #'equal 
                                   :size 1000
                                   :rehash-size 2.0))
            (roots nil))

        (let* ((abox
                (let* ((*display-error* nil)

                       (args
                        `(,@(when (not datatype-fillers)
                              '(:for-datatype-properties nil))

                          ,@(when (not datatype-fillers)
                              '(:for-annotation-properties nil))

                          ,@(when show-top-role 
                              '(:no-top-role-p nil))

                          ,@(when told-only
                              '(:told-only-p t))

                          ,@(when (not include-transitive-roles)
                              '(:no-transitives-p t))
                        
                          :only-successors-in-selected-individuals-p 
                          ,only-selected-successors 
                        
                          ,@(when (and only-selected-successors 
                                       ;;; sonst NIL, denn dann werden per
                                       ;;; Default (wie bei root-individuals)
                                       ;;; ALLE Individuen  (auf Server-Seite)
                                       ;;; genommen 
                                       (not (eq abox-graph-root-kind :all)))
                              `(:selected-individuals ,(profile-all-selected-individuals)))
                        
                          ,@(setf roots
                                  (ecase abox-graph-root-kind
                                    (:all nil)
                                    (:current 
                                     (if current-individual
                                         `(:root-individuals (,current-individual))
                                       `(:root-individuals nil)))
                                    (:selected
                                     `(:root-individuals ,(profile-all-selected-individuals)))))
                        
                          ,@(ecase role-hierarchy-root-kind
                              (:all nil)
                              (:current 
                               (if current-role
                                   `(:for-roles (,current-role))
                                 `(:for-roles nil)))
                              (:selected
                               `(:for-roles ,(profile-all-selected-roles))))

                          ,@(when (numberp abox-graph-max-depth)
                              `(:depth ,(1+ abox-graph-max-depth))))))
                  
                  (setf (gethash :roots hash) (second roots))
                  
                  (racer-function-evaluated
                   `(racer-user::get-abox-graph ,sirius-current-abox
                                                ,@args)))))

          (make-abox-graph hash abox))))))


(defun expand-abox-graph (hash new-roots)
  (with-profile-request

    (with-profile-access

      (let* ((abox
              (let* ((*display-error* nil)

                     (args
                      `(,@(when (eq (second (gethash (first new-roots) hash))
                                    :succs-found)
                            '(:browsing-mode-p nil))
                        
                        ,@(when (not datatype-fillers)
                            '(:for-datatype-properties nil))

                        ,@(when (not datatype-fillers)
                            '(:for-annotation-properties nil))

                        ,@(when told-only
                            '(:told-only-p t))

                        ,@(when (not include-transitive-roles)
                            '(:no-transitives-p t))
                      
                        :only-successors-in-selected-individuals-p 
                        ,only-selected-successors                         

                        ,@(when (and only-selected-successors 
                                     ;;; sonst NIL, denn dann werden per
                                     ;;; Default (wie bei root-individuals)
                                     ;;; ALLE Individuen  (auf Server-Seite)
                                     ;;; genommen 
                                     (not (eq abox-graph-root-kind :all)))
                            `(:selected-individuals ,(profile-all-selected-individuals)))
                      
                        ,@(ecase role-hierarchy-root-kind
                            (:all nil)
                            (:current 
                             (if current-role
                                 `(:for-roles (,current-role))
                               `(:for-roles nil)))
                            (:selected
                             `(:for-roles ,(profile-all-selected-roles))))

                        :root-individuals ,new-roots

                        ,@(when (numberp abox-graph-max-depth)
                            `(:depth ,(1+ abox-graph-max-depth))))))
                  
                  ;(setf (gethash :roots hash) (second roots))
                  
                (racer-function-evaluated
                 `(racer-user::get-abox-graph ,sirius-current-abox
                                              ,@args)))))

        (make-abox-graph hash abox)))))



(defun request-abox-graph ()
  (with-profile-access 
    (start-pooled-process "request abox graph"
                          ;;; notwendig, damit abbrechbar!
                          
                          (setf abox-graph-graph-pane nil)
                          (setf abox-graph *process*)

                          ;;; Informieren ("Computing Abox-Graph...") 
                          (schedule-update-no-history-entry :abox-graph)

                          (handler-case 
                              (progn 
                                (setf abox-graph
                                      (build-abox-graph))

                                (setf (abox-graph-in-history)
                                      abox-graph))
                            (error (error)
                              (let ((error (format nil "~A" error)))
                                (when (search "aborted" error)
                                  ;;(display-message error)
                                  (setf graph-drawing-aborted t))
                                (setf autofocus nil))))
                          
                          (schedule-update-no-history-entry :abox-graph))))
   
      

(defun clear-abox-graph-roots ()
  (with-profile-access
    (setf abox-graph-graph-pane-roots nil
          saved-abox-graph-graph-pane nil)))
  

(defun compute-abox-graph-roots ()
  (with-profile-access
    (when sirius-current-abox
      (if (and freeze-graph 
               abox-graph-graph-pane-roots)
          abox-graph-graph-pane-roots
        (let* ((roots 
                (if (eq abox-graph-root-kind :all)
                    (let ((res 
                           (racer-function1
                            (all-individuals sirius-current-abox) :error)))
                      (if (eq res :error)
                          (return-from compute-abox-graph-roots :error)
                        res))
                  (gethash :roots abox-graph)))

               (roots 
                (remove-duplicates 
                 (remove nil
                         (mapcar #'(lambda (x) 
                                     ;;; first synonym nehmen 
                                     (when (gethash x abox-graph)
                                       (first (first (gethash x abox-graph)))))
                                 roots))))

               (roots
                (mapcar #'(lambda (x) 
                            (list (list x x) 0))
                        (remove-duplicates
                         (sort (copy-list roots)
                               #'compare-fn-individuals)))))

          (setf abox-graph-graph-pane-roots roots)

          roots)))))


(defun compute-width-and-height (strings)
  (with-profile-access
    (let ((h 0)
          (w 0))
      (dolist (string strings)
        (let ((string 
               (if (> (length string) 30)
                   (format nil "~A..." (subseq string 0 30))
                 string)))
          (multiple-value-bind (left top right bottom)
              (gp:get-string-extent abox-graph-graph-pane string)
            (incf h (abs (- bottom top)))
            (setf w (max w (abs (- right left)))))))
      (values w h))))


(defun request-abox-graph-pane ()
  (with-profile-access
    (when sirius-current-abox
      (let* ((temp-hash 
              (make-hash-table :test #'equal 
                               :size 1000
                               :rehash-size 2.0))
               
             (roots 
              (handler-case
                  (compute-abox-graph-roots)
                (error (error)
                  (error error)))))
        
        (cond (saved-abox-graph-graph-pane

               (setf abox-graph-graph-pane 
                     saved-abox-graph-graph-pane

                     (graph-pane-roots abox-graph-graph-pane)
                     roots))

              (t

               
               ;;; damit Font fuer compute-width-and-height
               ;;; verfuegbar: 
               (setf abox-graph-graph-pane
                     (make-instance 'graph-pane
                                    :font *graph-font*))
                                    

               (setf abox-graph-graph-pane
                     (make-instance 'graph-pane
                                  
                                    :font *graph-font*
                                    
                                    :pane-menu nil
                                  
                                    :layout-function abox-graph-orientation
                                    
                                    :highlight-style :draw

                                    :callback-type :data
                                    
                                    :selection-callback 
                                    #'(lambda (data)
                                        
                                        (if (consp (first (first data))) ; d-literal?

                                            (let ((literal 
                                                   (with-pretty-printing 
                                                    ()
                                                    (format nil "~S" 
                                                            (first (first data))))))
                                              ;;(if (> (length literal) 100)
                                              ;;    (sirius-editor-string literal)
                                                (sirius-message literal))

                                          (let* ((maxdepth-p (third data))
                                                 (ind (first (first data)))
                                                 (graph (second (first data)))
                                                 (syns 
                                                  (or (first (gethash ind abox-graph))
                                                      (progn 
                                                        (setf maxdepth-p t)
                                                        (list ind)))))

                                            (cond (maxdepth-p 
                                                   (setf abox-graph
                                                         (copy-hash-table abox-graph))

                                                   (dolist (node syns)
                                                     (when (or (not (gethash node abox-graph))
                                                               (eq (second (gethash node abox-graph))
                                                                   :succs-found))
                                                       (expand-abox-graph abox-graph (list node)))

                                                     (setf (gethash (list node graph) abox-graph)
                                                           (append (gethash node abox-graph)
                                                                   '(t))))

                                                   (next-update-refresh-abox-graph)
                                                   ;(next-update-compute-and-layout-abox-graph)
                                                   (schedule-update :abox-graph))
                                                
                                                  ((member (enter-mouse-select-command
                                                            `(individual? ,ind ,sirius-current-abox))
                                                           '(t racer-user::\t))
                                                   
                                                   (toggle-individuals (list ind))
                                                   (schedule-update :abox-graph))

                                                  (t (beep-pane))))))
                                  
                                    :node-pane-function
                                    #'(lambda (graph-pane node1)
                                        (declare (ignore graph-pane))

                                        (let* ((node (first node1))
                                               (ind (first node))
                                               (maxdepth-p (third node1))
                                               (back-to-p (fourth  node1))
                                               (syns
                                                (or (first 
                                                     (gethash  
                                                      (first (ensure-list node))
                                                      abox-graph))
                                                    (progn 
                                                      (setf maxdepth-p t)
                                                      nil)))
                                               
                                               (text
                                                (cond
                                                 (maxdepth-p
                                                  (line-item-printer +maxdepth-marker+))
                                                 (back-to-p
                                                  (line-item-printer 
                                                   `(:goto ,ind)))
                                                 (t 
                                                  (line-item-printer ind))))

                                               (node-labels
                                                (mapcar #'(lambda (x) 
                                                            (format nil "is a ~A" (line-item-printer x)))
                                                        (and node-labels
                                                             (not back-to-p)
                                                             (gethash (list (first syns) :label) abox-graph)))))

                                          (multiple-value-bind (width height)
                                              (compute-width-and-height (cons text node-labels))

                                            (make-instance 'network-node
                                                           :selected 
                                                           (some #'(lambda (x) 
                                                                     (and (selected-individual? x)
                                                                          (not (equal x current-individual))))
                                                                 syns)
                                                           :highlighted 
                                                           (member current-individual syns 
                                                                   :test #'equal)
                                                           :visible-min-height 
                                                           height
                                                           :visible-min-width 
                                                           width
                                                           :text text
                                                           :items node-labels))))
                                    
                                    :edge-pane-function 
                                    #'(lambda (self from to)
                                        (declare (ignore self))

                                        ;;(display-message (format nil "~A" (list self from to)))
                                                    
                                        (let* ((from (caar from))
                                               (to (caar to)) 
                                               (roles 
                                                (gethash (cons from to) abox-graph))

                                               (print (if roles
                                                          (mapcar #'(lambda (x) 
                                                                      (line-item-printer x nil))
                                                                  roles)
                                                        "?")))

                                          (with-profile-access
                                            (make-instance 'network-edge
                                                           :from from
                                                           :to to
                                                           :data print))))

                                    :children-function 
                                    #'(lambda (node1)  
                                        (handler-case 
                                            (let* ((node (first node1))
                                                   (ind (first node))
                                                   (graph (second node))
                                                   (depth (second node1))
                                                   (maxdepth-p (third node1))
                                                   (children 
                                                    (second (gethash ind abox-graph))))

                                              (when (zerop depth)
                                                (clrhash temp-hash))
                                                  
                                              (unless (gethash node temp-hash)
                                            
                                                (setf (gethash node temp-hash) t)

                                                (if (eq children :succs-found)
                                                    
                                                    (list (list node (1+ depth) t))

                                                  (let* ((children 
                                                          (remove-duplicates
                                                           children
                                                           :test #'equal))

                                                         (literals
                                                          (remove-duplicates
                                                           (append
                                                            ;;; DTP succs
                                                            (third 
                                                             (gethash ind abox-graph))
                                                            ;;; AN succs
                                                            (fourth 
                                                             (gethash ind abox-graph)))
                                                           :test #'equal))

                                                         (children 
                                                          (sort 
                                                           (remove-if #'(lambda (to) 
                                                                          (not 
                                                                           (gethash (cons ind (first to)) 
                                                                                    abox-graph)))
                                                                      children)
                                                           #'compare-fn-individuals
                                                           :key #'first)))

                                                    (when (and (or children literals)
                                                               (or (not maxdepth-p)
                                                                   ;; expanded by hand
                                                                   (fifth (gethash node abox-graph))))
                                              
                                                      (if (and (numberp abox-graph-max-depth)
                                                               (>= depth abox-graph-max-depth)
                                                               (not (fifth (gethash node abox-graph))))

                                                          (list (list node (1+ depth) t))

                                                        (let ((children
                                                               (append 

                                                                (mapcar #'(lambda (x) 
                                                                            (let ((node (list (first (ensure-list x))
                                                                                              graph)))
                                                                              (if (gethash node temp-hash)
                                                                                  (list node (1+ depth) nil t)
                                                                                (list node (1+ depth)))))

                                                                        children)

                                                                (mapcar #'(lambda (x) 
                                                                            (list (list x graph)
                                                                                  (1+ depth)))
                                                                        literals))))

                                                          ;; (display-message (format nil "~S ~S ~S" depth abox-graph-max-depth children))
                                                          children)))))))

                                          (error (error) 
                                            (error "Child: ~A" error))))
                                                 
                                    :roots roots)
                    
                     saved-abox-graph-graph-pane abox-graph-graph-pane)))))))


