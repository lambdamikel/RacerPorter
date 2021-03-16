;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +history-slots+ 
    '(active-tab  
      selected-first
      selected-only
      sirius-current-tbox
      sirius-current-abox
      sirius-current-namespace
      current-concept
      current-individual
      current-assertion
      current-role
      current-query-or-rule
      current-axiom
      current-reasoner
      current-ontology
      current-axiom-types
      new-axiom-type 
      show-bottom
      show-top
      remove-urls
      query-list-mode
      role-list-mode
      concept-list-mode
      assertion-list-mode
      taxonomy-kind
      abox-graph-kind
      role-hierarchy-kind
      taxonomy-max-depth
      abox-graph-max-depth
      role-hierarchy-max-depth
      include-transitive-roles
      only-selected-successors
      show-top-role
      told-only
      node-labels
      datatype-fillers
      taxonomy-orientation
      role-hierarchy-orientation
      abox-graph-orientation
      role-hierarchy-graph
      taxonomy-graph
      abox-graph
      taxonomy-root-kind
      role-hierarchy-root-kind
      abox-graph-root-kind
      taxonomy-scroll
      role-hierarchy-scroll
      abox-graph-scroll
      sirius-current-namespace-prefixes
      freeze-graph
      taxonomy-pane-command
      abox-graph-pane-command
      role-hierarchy-pane-command
      
      taxonomy-graph-pane-roots
      role-hierarchy-graph-pane-roots
      abox-graph-graph-pane-roots)))

(defmacro role-hierarchy-graph-in-history ()
  (let ((pos (position 'role-hierarchy-graph +history-slots+)))
    `(nth ,pos (second (get-history-entry-at-history-position)))))

(defmacro taxonomy-graph-in-history ()
  (let ((pos (position 'taxonomy-graph +history-slots+)))
    `(nth ,pos (second (get-history-entry-at-history-position)))))

(defmacro abox-graph-in-history ()
  (let ((pos (position 'abox-graph +history-slots+)))
    `(nth ,pos (second (get-history-entry-at-history-position)))))

;;;
;;;
;;;

(defun at-history-end-p () 
  (let ((profile (active-profile)))
    (= (length (profile-history profile))
       (profile-history-position profile))))

(defun at-history-start-p () 
  (zerop (1- (profile-history-position (active-profile)))))

(defun get-history-entry-at-history-position ()
  (let ((profile (active-profile)))
    (when (and (profile-history profile)
               (not (minusp (- (length (profile-history profile))
                               (profile-history-position profile)))))
      (nth (- (length (profile-history profile))
              (profile-history-position profile))
           (profile-history profile)))))

(defun get-latest-history-entry ()
  (first (profile-history (active-profile))))

#|
(defmethod (setf profile-history-position) :after (val (profile profile))
  (when (eq profile (active-profile))
    ))
|#

(defun push-history-entry ()
  (when *create-history-entries*

    (with-profile-access
      (let ((profile (active-profile)))
      (when taxonomy-graph-pane
        (setf taxonomy-scroll
              (list (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS taxonomy-graph-pane :slug-position)
                    (CAPI:GET-VERTICAL-SCROLL-PARAMETERS taxonomy-graph-pane :slug-position))))

      (when abox-graph-graph-pane
        (setf abox-graph-scroll
              (list (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS abox-graph-graph-pane :slug-position)
                    (CAPI:GET-VERTICAL-SCROLL-PARAMETERS abox-graph-graph-pane :slug-position))))
       
      (when role-hierarchy-graph-pane
        (setf role-hierarchy-scroll
              (list (CAPI:GET-HORIZONTAL-SCROLL-PARAMETERS role-hierarchy-graph-pane :slug-position)
                    (CAPI:GET-VERTICAL-SCROLL-PARAMETERS role-hierarchy-graph-pane :slug-position))))
        
      (unless (eq active-tab :logo)

        (let* ((focus-entry 
                (list 
                 (list
                  (profile-all-selected-concepts)
                  (profile-all-selected-roles)
                  (profile-all-selected-individuals)
                  (profile-all-selected-assertions)
                  (profile-all-selected-axioms))
                 (mapcar #'(lambda (slot) 
                             (slot-value profile slot))
                         +history-slots+))))
            
          (unless (tree-equal focus-entry
                              (first (profile-history profile))
                              :test #'equal)

            (when (or *current-history-position-is-top-of-stack* 
                      (not (profile-history-position profile)))
              (setf (profile-history-position profile)
                    (1+ (length (profile-history profile)))))

            (push focus-entry (profile-history profile)))))))))



(defun use-history-entry-at-history-position ()
  (let* ((current 
          (get-history-entry-at-history-position))
         (hash-tables 
          (first current))
         (history-slot-values 
          (second current))
         (profile (active-profile)))

    (with-sirius-slots (button-status)
                       (setf button-status nil))

    (with-profile-access
      (setf all-selected-concepts-hash
            (make-hash-table-from (pop hash-tables))

            all-selected-roles-hash
            (make-hash-table-from (pop hash-tables))
       
            all-selected-individuals-hash
            (make-hash-table-from (pop hash-tables))

            all-selected-assertions-hash
            (make-hash-table-from (pop hash-tables))
            
            all-selected-axioms-hash
            (make-hash-table-from (pop hash-tables))))

    (loop as slot in +history-slots+
          as value in history-slot-values do
          (setf (slot-value profile slot) value))))

(defun focus-prev ()
  (let ((profile (active-profile)))
    (when (> (profile-history-position profile) 1)
    (decf (profile-history-position profile))
    (use-history-entry-at-history-position)
    (schedule-focus-update))))

(defun focus-start ()
  (let ((profile (active-profile)))
    (when (profile-history profile)
    (setf (profile-history-position profile) 1)
    (use-history-entry-at-history-position)        
    (schedule-focus-update))))

(defun focus-next ()
  (let ((profile (active-profile)))
  (when (< (profile-history-position profile)
           (length (profile-history profile)))
    (incf (profile-history-position profile))
    (use-history-entry-at-history-position)
    (schedule-focus-update))))

(defun focus-end ()
  (let ((profile (active-profile)))
    (when (profile-history profile)
    (setf (profile-history-position profile)
          (length (profile-history profile)))
    (use-history-entry-at-history-position)       
    (schedule-focus-update))))

;;;
;;;
;;; 

(defun delete-focus ()
  (let ((profile (active-profile)))
  (when (and (profile-history-position profile)
             (profile-history profile))      
    (setf (profile-history profile)
          (delete (get-history-entry-at-history-position)
                  (profile-history profile)))
    (focus-end))))

;;;
;;;
;;;

(defun clear-history ()
  (with-profile-access

    (reset-active-profile
     :clear-history-p t)

    (clear-input)
    (clear-command)
    (input-prompt t)
    (schedule-update)))

(defun activate-profile (profile)
  (progn ;with-capi-synchronization
    (with-suspended-server-pane-update

      ;;; wichtig:
      (without-history-entries
        (with-sirius-app (sirius)
          (with-slots (command-pane) sirius
            
            (clear-current-prompt-and-input)

            (with-profile-access 

              (setf update-queue nil)

              ;;; evt. laufenden Prozess etc. sichern

              (when (get-history-entry-at-history-position)
                (setf (taxonomy-graph-in-history)
                      taxonomy-graph)
                (setf (abox-graph-in-history)
                      abox-graph)
                (setf (role-hierarchy-graph-in-history)
                      role-hierarchy-graph))

              (setf shell-content
                    (editor-pane-text command-pane)))

            (make-active-profile profile)
            
            (setup-shell-pane t)
            
            (with-profile-access
              (setf update-queue nil)            
              (unless (connected-and-socket-alive-p)
                (when auto-connect
                  (connect))))
            
            (use-history-entry-at-history-position)

            (with-access-to-profile profile
              (unless (typep taxonomy-graph 'pooled-process)
                (next-update-layout-taxonomy))
              (unless (typep abox-graph 'pooled-process)
                (next-update-layout-abox-graph))
              (unless (typep role-hierarchy-graph 'pooled-process)
                (next-update-layout-role-hierarchy)))

            (schedule-update-no-history-entry)))))))
