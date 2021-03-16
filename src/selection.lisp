;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defun invalidate-role-specific-panes ()
  (with-profile-access
    (when t 
      (case active-tab
        (:role-hierarchy
         (if freeze-graph
             (progn 
               (next-update-compute-and-layout-abox-graph)
               (next-update-layout-role-hierarchy))
           (progn 
             (next-update-compute-and-layout-abox-graph)
             (next-update-compute-and-layout-role-hierarchy))))
        (t (next-update-compute-and-layout-abox-graph)
           (next-update-compute-and-layout-role-hierarchy))))))

(defun unselect-all-roles ()
  (with-profile-access
    (clrhash all-selected-roles-hash)
    (setf current-role nil)
    (invalidate-role-specific-panes)))

(defun select-roles (roles)
  (with-profile-access
    (setf current-role (first roles))
    (dolist (role roles)
      (setf (gethash role all-selected-roles-hash) t))
    (invalidate-role-specific-panes)))

(defun selected-role? (role)
  (with-profile-access
    (and role
         (or (eq current-role role)
             (gethash role all-selected-roles-hash)))))

(defun unselect-roles (roles)
  (with-profile-access

    (dolist (role roles)
      (remhash role all-selected-roles-hash)

      (cond ((eq role current-role)
             (setf current-role nil)
             (when (zerop (1- (hash-table-count all-selected-roles-hash)))
               (setf current-role (first (profile-all-selected-roles)))))

            ((zerop (hash-table-count all-selected-roles-hash))
             (setf current-role nil))))
           
    (invalidate-role-specific-panes)))

(defun profile-all-selected-roles (&optional (profile (active-profile)))
  (with-access-to-profile profile
    (loop as role being the hash-key of all-selected-roles-hash 
          collect role)))

(defun toggle-roles (roles)
  (with-profile-access 

    (if (and (cdr roles) #+:mac nil)
        (dolist (role roles)
          (cond ((eq role current-role)
                 (setf current-role nil)
                 (invalidate-role-specific-panes))

                ((selected-role? role)
                 (unselect-roles (list role)))
                
                (t 
                 (select-roles (list role)))))

      ;;; intuitiver
      (let ((role (first roles)))
        (cond ((eq role current-role)
               (unselect-roles (list role)))
            
              ((selected-role? role)
               (setf current-role role)
               (invalidate-role-specific-panes))

              (t 
               (select-roles (list role))))))))


;;;
;;;
;;;

(defun invalidate-concept-specific-panes ()
  (with-profile-access
    (when t 
      (if (and freeze-graph (eq active-tab :taxonomy))
          (next-update-layout-taxonomy)
        (next-update-compute-and-layout-taxonomy)))))

(defun unselect-all-concepts ()
  (with-profile-access
    (clrhash all-selected-concepts-hash)
    (setf current-concept nil)
    (invalidate-concept-specific-panes)))
    

(defun select-concepts (concepts)
  (with-profile-access
    (setf current-concept (first concepts))
    (dolist (concept concepts)
      (setf (gethash concept all-selected-concepts-hash) t))
    (invalidate-concept-specific-panes)))

(defun selected-concept? (concept)
  (with-profile-access
    (and concept
         (or (eq current-concept concept)
             (gethash concept all-selected-concepts-hash)))))

(defun unselect-concepts (concepts)
  (with-profile-access

    (dolist (concept concepts)
      (remhash concept all-selected-concepts-hash)
    
      (cond ((eq concept current-concept)
             (setf current-concept nil)
             (when (zerop (1- (hash-table-count all-selected-concepts-hash)))
               (setf current-concept (first (profile-all-selected-concepts)))))

            ((zerop (hash-table-count all-selected-concepts-hash))
             (setf current-concept nil))))
           
    (invalidate-concept-specific-panes)))

(defun profile-all-selected-concepts (&optional (profile (active-profile)))
  (with-access-to-profile profile
    (loop as concept being the hash-key of all-selected-concepts-hash 
          collect concept)))

(defun toggle-concepts (concepts)
  (with-profile-access 
    
    (if (and (cdr concepts) #+:mac nil)
        (dolist (concept concepts)
          (cond ((eq concept current-concept)
                 (setf current-concept nil)
                 (invalidate-concept-specific-panes))

                ((selected-concept? concept)
                 (unselect-concepts (list concept)))

                (t 
                 (select-concepts (list concept)))))
      
      ;;; intuitiver
      (let ((concept (first concepts)))
        (cond ((eq concept current-concept)
               (unselect-concepts (list concept)))
            
              ((selected-concept? concept)
               (setf current-concept concept)
               (invalidate-concept-specific-panes))

              (t 
               (select-concepts (list concept))))))))

;;;
;;;
;;; 

(defun invalidate-assertion-specific-panes ()
  )

(defun unselect-all-assertions ()
  (with-profile-access
    (clrhash all-selected-assertions-hash)
    (setf current-assertion nil)
    (invalidate-assertion-specific-panes)))
    
(defun select-assertions (asses)
  (with-profile-access
    (setf current-assertion (first asses))
    (dolist (ass asses)
      (setf (gethash ass all-selected-assertions-hash) t))
    (invalidate-assertion-specific-panes)))

(defun selected-assertion? (ass)
  (with-profile-access
    (or (equal current-assertion ass)
        (gethash ass all-selected-assertions-hash))))

(defun unselect-assertions (asses)
  (with-profile-access
    (dolist (ass asses)
      (remhash ass all-selected-assertions-hash)

      (when (or (equal ass current-assertion)
                (not (profile-all-selected-assertions)))
        (setf current-assertion nil)))

    (invalidate-assertion-specific-panes)))

(defun profile-all-selected-assertions (&optional (profile (active-profile)))
  (with-access-to-profile profile
    (loop as assertion being the hash-key of all-selected-assertions-hash 
          collect assertion)))

(defun toggle-assertions (asses)
  (with-profile-access 

    ;;; es gibt noch keine current-assertion 
    ;;; Update: gibt es schon, aber sie hat keine
    ;;; besondere Bedeutung... 

    #+:mac
    (let ((ass (first asses)))
      (cond ((equal ass current-assertion)
             (unselect-assertions (list ass)))
            
            ((selected-assertion? ass)
             (setf current-assertion ass)
             (invalidate-assertion-specific-panes))

            (t 
             (select-assertions (list ass)))))

    #-:mac
    (let ((ass (first asses)))
      (cond ((selected-assertion? ass)
             (unselect-assertions (list ass)))
            
            (t 
             (select-assertions (list ass)))))
    
    (invalidate-assertion-specific-panes)))

;;;
;;;
;;; 

(defun invalidate-axiom-specific-panes ()
  )

(defun unselect-all-axioms ()
  (with-profile-access
    (clrhash all-selected-axioms-hash)
    (setf current-axiom nil)
    (invalidate-axiom-specific-panes)))    

(defun select-axioms (axes)
  (with-profile-access
    (setf current-axiom (first axes))
    (dolist (ax axes)
      (setf (gethash (first ax) all-selected-axioms-hash) t))
    (invalidate-axiom-specific-panes)))

(defun selected-axiom? (ax)
  (with-profile-access
    (or (eql (first current-axiom) (first ax))
        (gethash (first ax) all-selected-axioms-hash))))

(defun unselect-axioms (axes)
  (with-profile-access
    
    (dolist (ax axes)
      (remhash (first ax) all-selected-axioms-hash)

      (when (or (eql (first ax) (first current-axiom))
                (not (profile-all-selected-axioms)))
        (setf current-axiom nil)))

    (invalidate-axiom-specific-panes)))

(defun profile-all-selected-axioms (&optional (profile (active-profile)))
  (with-access-to-profile profile
    (loop as axiom being the hash-key of all-selected-axioms-hash 
          collect axiom)))

(defun toggle-axioms (axes)
  (with-profile-access 

    (if (and (cdr axes) #+:mac nil)
        (dolist (ax axes)
          (cond ((equal (first ax) (first current-axiom))
                 (setf current-axiom nil)
                 (invalidate-axiom-specific-panes))

                ((selected-axiom? ax)
                 (unselect-axioms (list ax)))

                (t 
                 (select-axioms (list ax)))))

      (let ((ax (first axes)))

        (cond ((and current-axiom 
                    (= (first ax) (first current-axiom)))
               (unselect-axioms (list ax)))
            
              ((selected-axiom? ax)
               (setf current-axiom ax)
               (invalidate-axiom-specific-panes))

              (t 
               (select-axioms (list ax))))))))

;;;
;;;
;;;

(defun select-parents ()
  (with-profile-access
    (let ((concepts (profile-all-selected-concepts)))
      (when concepts
        (let ((parents 
               (remove :error
                       (mapcar #'(lambda (c)
                                   (racer-function1 
                                    (atomic-concept-parents c sirius-current-tbox)))
                               concepts))))
          (when parents
            (unselect-all-concepts)
            (dolist (parent parents)
              (dolist (parent parent)
                (select-concepts (ensure-list parent))))))))))


(defun select-children ()
  (with-profile-access
    (let ((concepts (profile-all-selected-concepts)))
      (when concepts
        (let ((children 
               (remove :error
                       (mapcar #'(lambda (c)
                                   (racer-function1 
                                    (atomic-concept-children c sirius-current-tbox)))
                               concepts))))
          (when children 
            (unselect-all-concepts)
            (dolist (child children)
              (dolist (child child)
                (select-concepts (ensure-list child)) ))))))))

  
;;;
;;;
;;;


(defun invalidate-individual-specific-panes ()
  (with-profile-access
    (when t 
      (if (and freeze-graph (eq active-tab :abox-graph))
          (next-update-layout-abox-graph)
        (next-update-compute-and-layout-abox-graph)))))

(defun unselect-all-individuals ()
  (with-profile-access
    (clrhash all-selected-individuals-hash)
    (setf current-individual nil)
    (invalidate-individual-specific-panes)))
    

(defun select-individuals (inds)
  (with-profile-access
    (setf current-individual (first inds))
      
    (dolist (ind inds)
      (setf (gethash ind all-selected-individuals-hash) t))

    (invalidate-individual-specific-panes)))

(defun selected-individual? (ind)
  (with-profile-access
    (and ind
         (or (eq current-individual ind)
             (gethash ind all-selected-individuals-hash)))))

(defun unselect-individuals (inds)
  (with-profile-access
    
    (dolist (ind inds)
      (remhash ind all-selected-individuals-hash)
      (cond ((eq ind current-individual)
             (setf current-individual nil)
             (when (zerop (1- (hash-table-count all-selected-individuals-hash)))
               (setf current-individual (first (profile-all-selected-individuals)))))

            ((zerop (hash-table-count all-selected-individuals-hash))
             (setf current-individual nil))))
    
    (invalidate-individual-specific-panes)))

(defun profile-all-selected-individuals (&optional (profile (active-profile)))
  (with-access-to-profile profile
    (loop as individual being the hash-key of all-selected-individuals-hash 
          collect individual)))

(defun toggle-individuals (individuals)
  (with-profile-access 
    
    (if (and (cdr individuals) #+:mac nil)
        (dolist (ind individuals)
          (cond ((eq ind current-individual)
                 (setf current-individual nil)
                 (invalidate-individual-specific-panes))

                ((selected-individual? ind)
                 (unselect-individuals (list ind)))

                (t 
                 (select-individuals (list ind)))))

      (let ((ind (first individuals)))
        (cond ((eq ind current-individual)
               (unselect-individuals (list ind)))
            
              ((selected-individual? ind)
               (setf current-individual ind)
               (invalidate-individual-specific-panes))

              (t 
               (select-individuals (list ind))))))))
