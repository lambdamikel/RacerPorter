;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun true () t))

;;;
;;;
;;;

(defconstant +racer-command-buttons+
  (list (list 'SELECT-ALL-CONCEPTS-BUTTON (lambda () (with-profile-access sirius-current-tbox)))
        (list 'SELECT-ALL-ROLES-BUTTON (lambda () (with-profile-access sirius-current-tbox)))
        (list 'SELECT-ALL-INDIVIDUALS-BUTTON (lambda () (with-profile-access sirius-current-abox)))
        (list 'SELECT-ALL-ASSERTIONS-BUTTON (lambda () (with-profile-access sirius-current-abox)))
        (list 'SELECT-CHILDREN-CONCEPTS-BUTTON (lambda () (with-profile-access current-concept)))
        (list 'SELECT-PARENT-CONCEPTS-BUTTON (lambda () (with-profile-access current-concept)))

        (list 'TAXONOMY-REQUEST-BUTTON (lambda () (with-profile-access (and sirius-current-tbox (not taxonomy-graph)))))

        (list 'ROLE-HIERARCHY-REQUEST-BUTTON (lambda () (with-profile-access (and sirius-current-tbox (not role-hierarchy-graph)))))
        (list 'ABOX-GRAPH-REQUEST-BUTTON (lambda () (with-profile-access (and sirius-current-abox (not abox-graph)))))

        (list 'FULL-RESET-BUTTON (lambda () ;(connected-and-socket-alive-p)
                                     (and *active-profile*
                                          (connection-was-established-p *active-profile*) 
                                          (not (profile-connection-died *active-profile*))
                                          (socket-alive-p (profile-socket *active-profile*)))))

        (list 'RELOAD-SERVER-BUTTON (lambda () (connected-and-socket-alive-p)))
        (list 'DUMP-SERVER-BUTTON (lambda () (and (connected-and-socket-alive-p)
                                                  (profile-unsafe (active-profile)))))
        (list 'LOAD-KB-BUTTON (lambda () (connected-and-socket-alive-p)))
        
        (list 'ABOX-CONSISTENT-BUTTON (lambda () (with-profile-access sirius-current-abox)))
        (list 'REALIZE-ABOX-BUTTON (lambda () (with-profile-access sirius-current-abox)))
        (list 'FORGET-ABOX-BUTTON (lambda () (with-profile-access sirius-current-abox)))
        (list 'TBOX-COHERENT-BUTTON (lambda () (with-profile-access sirius-current-tbox)))
        (list 'FORGET-TBOX-BUTTON (lambda () (with-profile-access sirius-current-tbox)))
        (list 'TBOX-CLASSIFY-BUTTON (lambda () (with-profile-access sirius-current-tbox)))
        (list 'DESCRIBE-CONCEPT-BUTTON (lambda () (with-profile-access current-concept)))
        (list 'DESCRIBE-ROLE-BUTTON (lambda () (with-profile-access current-role)))
        (list 'ROLE-DOMAIN-BUTTON (lambda () (with-profile-access current-role)))
        (list 'ROLE-RANGE-BUTTON (lambda () (with-profile-access current-role)))        
        (list 'ROLE-PARENTS-BUTTON (lambda () (with-profile-access current-role)))        
        (list 'ROLE-CHILDREN-BUTTON (lambda () (with-profile-access current-role)))        
        (list 'ROLE-SYNONYMS-BUTTON (lambda () (with-profile-access current-role)))        
        (list 'DESCRIBE-INDIVIDUAL-BUTTON (lambda () (with-profile-access current-individual)))
        (list 'DELETE-ASSERTIONS-BUTTON (lambda () 
                                          (with-profile-access
                                            (not (zerop (hash-table-count all-selected-assertions-hash))))))
        (list 'DIRECT-TYPES-BUTTON (lambda () (with-profile-access current-individual)))
        (list 'ALL-TYPES-BUTTON (lambda () (with-profile-access current-individual)))
        (list 'INDIVIDUAL-SYNONYMS-BUTTON (lambda () (with-profile-access current-individual)))
        (list 'INDIVIDUAL-ANTONYMS-BUTTON (lambda () (with-profile-access current-individual)))
        (list 'DELETE-INDIVIDUALS-BUTTON (lambda () (with-profile-access 
                                                      (not (zerop (hash-table-count all-selected-individuals-hash))))))

        (list 'CHANGE-CURRENT-TBOX-BUTTON (lambda () (with-profile-access sirius-current-tbox)))
        (list 'DESCRIBE-TBOX-BUTTON (lambda () (with-profile-access sirius-current-tbox)))
        (list 'CHANGE-CURRENT-ABOX-BUTTON (lambda () (with-profile-access sirius-current-abox)))
        (list 'DESCRIBE-ABOX-BUTTON (lambda () (with-profile-access sirius-current-abox)))
        (list 'INSTANCE-QUERY-BUTTON (lambda () (with-profile-access (and sirius-current-abox current-concept))))
        (list 'CONCEPT-SYNONYMS-BUTTON (lambda () (with-profile-access current-concept)))
        (list 'DESCRIBE-QUERY-BUTTON (lambda () (with-profile-access current-query-or-rule)))
        (list 'DESCRIBE-ORIGINAL-QUERY-BUTTON (lambda () (with-profile-access current-query-or-rule)))
        
        (list 'DELETE-QUERY-BUTTON (lambda () (with-profile-access current-query-or-rule)))
        (list 'ABORT-QUERY-BUTTON (lambda () (with-profile-access current-query-or-rule)))
        (list 'ANSWER-QUERY-BUTTON (lambda () (with-profile-access current-query-or-rule)))
        (list 'EXECUTE-QUERY-BUTTON (lambda () (with-profile-access current-query-or-rule)))
        (list 'REEXECUTE-QUERY-BUTTON (lambda () (with-profile-access current-query-or-rule)))
        (list 'REPREPARE-QUERY-BUTTON (lambda () (with-profile-access current-query-or-rule)))
        (list 'GET-NEXT-TUPLE-BUTTON (lambda () (with-profile-access current-query-or-rule)))

        (list 'DELETE-DEFINITION-BUTTON (lambda () (with-profile-access current-definition)))

        (list 'NRQL-STATUS-BUTTON (lambda () (connected-and-socket-alive-p)))

        (list 'DELETE-AXIOMS-BUTTON (lambda () (with-profile-access (and current-reasoner current-ontology 
                                                                         (not (zerop (hash-table-count all-selected-axioms-hash)))))))
        (list 'EDIT-AXIOM-BUTTON (lambda () (with-profile-access (and current-reasoner current-ontology 
                                                                      current-axiom))))
        (list 'NEW-REASONER-BUTTON (lambda () (connected-and-socket-alive-p)))
        (list 'SAVE-REASONER-BUTTON (lambda () (with-profile-access (and (profile-unsafe (active-profile)) current-reasoner))))
        (list 'NEW-ONTOLOGY-BUTTON (lambda () (with-profile-access current-reasoner)))
        (list 'LOAD-ONTOLOGY-BUTTON (lambda () (with-profile-access (and current-ontology current-reasoner))))
        (list 'LOAD-AXIOMS-BUTTON (lambda () (with-profile-access (and current-ontology current-reasoner
                                                                       (not (zerop (hash-table-count all-selected-axioms-hash)))))))
        (list 'SAVE-ONTOLOGY-BUTTON (lambda () (with-profile-access (and (profile-unsafe (active-profile)) current-ontology current-reasoner))))
        (list 'UNLOAD-ONTOLOGY-BUTTON (lambda () (with-profile-access (and current-ontology current-reasoner))))
        (list 'UNLOAD-AXIOMS-BUTTON (lambda () (with-profile-access (and current-ontology current-reasoner
                                                                         (not (zerop (hash-table-count all-selected-axioms-hash)))))))
        (list 'DELETE-REASONER-BUTTON (lambda () (with-profile-access current-reasoner)))
        (list 'DELETE-ONTOLOGY-BUTTON (lambda () (with-profile-access current-ontology)))
       
        (list 'NEW-AXIOM-BUTTON (lambda () (with-profile-access (and current-reasoner current-ontology new-axiom-type))))


        ;;;
        ;;;
        ;;;

        #+:sonic (list 'SONIC-LCS (lambda () (with-profile-access 
                                               (>= (hash-table-count all-selected-concepts-hash) 2))))

        #+:sonic (list 'SONIC-SCS (lambda () (with-profile-access 
                                               (>= (hash-table-count all-selected-concepts-hash) 2))))

        #+:sonic (list 'SONIC-APPROX (lambda () (with-profile-access 
                                                  current-concept)))

        #+:sonic (list 'SONIC-MSC (lambda () (with-profile-access 
                                               current-individual)))

        #+:sonic (list 'SONIC-GENERALIZE (lambda () (with-profile-access 
                                                      (>= (hash-table-count all-selected-individuals-hash) 2))))

        ;;;
        ;;;
        ;;;
        
        #-:sonic (list 'MSC-k-BUTTON  (lambda () (with-profile-access 
                                          (and sirius-current-abox
                                               current-individual))))
        ))


        
                                                                             


(defconstant +sirius-command-buttons+ ; senden keine RacerPro Commands 

  (list (list 'focus-next (lambda ()
                            (and (buttons-enabled-p)
                                 (profile-history (active-profile))
                                 (not (at-history-end-p)))))

        (list 'focus-start (lambda ()
                             (and (buttons-enabled-p)
                                  (and (profile-history (active-profile))
                                       (not (at-history-start-p))))))

        (list 'focus-prev (lambda ()
                            (and (buttons-enabled-p)
                                 (and (profile-history (active-profile))
                                      (not (at-history-start-p))))))

        (list 'focus-end (lambda ()
                           (and (buttons-enabled-p)
                                (profile-history (active-profile))
                                (not (at-history-end-p)))))

        (list 'focus-delete (lambda ()
                              (and (buttons-enabled-p)
                                   (profile-history (active-profile)))))

        (list 'reset-sirius-state-button (lambda ()
                                           (and (buttons-enabled-p)
                                                (profile-history (active-profile)))))
        
        (list 'CONNECT-BUTTON #'true)
        (list 'CLEAR-LOG-BUTTON #'true)
        (list 'SHOW-INFO-BUTTON #'true)
        (list 'RESYNC-SIRIUS-STATE-BUTTON #'true)
                                            
        ;;; (list 'LAMBDA-INFO-BUTTON (lambda () (buttons-enabled-p)))
        
        (list 'TAXONOMY-CANCEL-REQUEST-BUTTON (lambda () (with-profile-access 
                                                           (typep taxonomy-graph 'pooled-process))))

        (list 'TAXONOMY-DISPLAY-BUTTON (lambda () (with-profile-access 
                                                    (and taxonomy-graph
                                                         (not taxonomy-graph-pane)
                                                         (not (typep taxonomy-graph 'pooled-process))))))

        (list 'TAXONOMY-PRINT-BUTTON (lambda () (with-profile-access 
                                                  taxonomy-graph-pane)))

        (list 'TAXONOMY-RESET-BUTTON (lambda () (with-profile-access 
                                                  (and taxonomy-graph
                                                       (not (typep taxonomy-graph 'pooled-process))))))        

        (list 'ROLE-HIERARCHY-CANCEL-REQUEST-BUTTON (lambda () (with-profile-access 
                                                                 (typep role-hierarchy-graph 'pooled-process))))

        (list 'ROLE-HIERARCHY-DISPLAY-BUTTON (lambda () (with-profile-access 
                                                          (and role-hierarchy-graph
                                                               (not role-hierarchy-graph-pane)
                                                               (not (typep role-hierarchy-graph 'pooled-process))))))

        (list 'ROLE-HIERARCHY-PRINT-BUTTON (lambda () (with-profile-access 
                                                        role-hierarchy-graph-pane)))

        (list 'ROLE-HIERARCHY-RESET-BUTTON (lambda () (with-profile-access 
                                                        (and role-hierarchy-graph
                                                             (not (typep role-hierarchy-graph 'pooled-process))))))

        (list 'ABOX-GRAPH-CANCEL-REQUEST-BUTTON (lambda () (with-profile-access 
                                                             (typep abox-graph 'pooled-process))))

        (list 'ABOX-GRAPH-DISPLAY-BUTTON (lambda () (with-profile-access 
                                                      (and abox-graph
                                                           (not abox-graph-graph-pane)
                                                           (not (typep abox-graph 'pooled-process))))))

        (list 'ABOX-GRAPH-PRINT-BUTTON (lambda () (with-profile-access 
                                                    abox-graph-graph-pane)))

        (list 'ABOX-GRAPH-RESET-BUTTON (lambda () (with-profile-access 
                                                    (and abox-graph
                                                         (not (typep abox-graph 'pooled-process))))))
        
        (list 'KILL-SERVER-BUTTON (lambda () (connected-and-socket-alive-p)))

        (list 'QUIT-AND-SHUTDOWN-BUTTON (lambda () (connected-and-socket-alive-p)))

        (list 'START-SERVER-BUTTON (lambda () (and (profile-racerpro-executable (active-profile))
                                                   (not (string= (profile-racerpro-executable (active-profile)) "")))))

        (list 'LICENSE-BUTTON #'true) ; wegen informativer Fehlermeldung, wenn der Nutzer klickt und kein Exe eingetragen ist!
        
        (list 'clear-assertion-selection-button (lambda ()
                                                  (with-profile-access
                                                    (and (buttons-enabled-p)
                                                         (not (zerop (hash-table-count all-selected-assertions-hash)))))))

        (list 'clear-concept-selection-button (lambda ()
                                                (with-profile-access
                                                  (and (buttons-enabled-p)
                                                       (not (zerop (hash-table-count all-selected-concepts-hash)))))))
        (list 'clear-role-selection-button (lambda ()
                                             (with-profile-access
                                               (and (buttons-enabled-p)
                                                    (not (zerop (hash-table-count all-selected-roles-hash)))))))

        (list 'clear-individual-selection-button (lambda ()
                                                   (with-profile-access
                                                     (and (buttons-enabled-p)
                                                          (not (zerop (hash-table-count all-selected-individuals-hash)))))))
        
        (list 'assign-selected-concepts-from-result-button (lambda ()
                                                             (with-profile-access
                                                               (and (buttons-enabled-p)
                                                                    (connected-and-socket-alive-p)
                                                                    (consp sirius-star)))))

        (list 'assign-selected-roles-from-result-button (lambda ()
                                                          (with-profile-access
                                                            (and (buttons-enabled-p)
                                                                 (connected-and-socket-alive-p)
                                                                 (consp sirius-star)))))

        (list 'assign-selected-individuals-from-result-button (lambda ()
                                                                (with-profile-access
                                                                  (and (buttons-enabled-p)
                                                                       (connected-and-socket-alive-p)
                                                                       (consp sirius-star)))))
	
	(list 'CLEAR-AXIOM-SELECTION-BUTTON (lambda ()  (with-profile-access (and (buttons-enabled-p)
                                                                                  (not (zerop (hash-table-count all-selected-axioms-hash)))))))

        (list 'SELECT-AXIOMS-BUTTON (lambda ()  (with-profile-access (and current-reasoner current-ontology 
                                                                          (buttons-enabled-p)
                                                                          (or 
                                                                           (not (zerop (hash-table-count all-selected-individuals-hash)))
                                                                           (not (zerop (hash-table-count all-selected-concepts-hash)))
                                                                           (not (zerop (hash-table-count all-selected-roles-hash))))))))))

;;;
;;;
;;;

(defun buttons-enabled-p ()
  (with-sirius-slots (button-status)
                     (eq button-status :enabled)))


(defun buttons-disabled-p ()
  (with-sirius-slots (button-status)
                     (eq button-status :disabled)))


(defun switch-all-panes (mode)
  (with-sirius-app (sirius)
    (dolist (pane '(;SERVER-PANE
                   
                    search-item
                    COMMAND-PANE
                    LAMBDA-LIST-PANE
                    ;;LAMBDA-INFO-BUTTON

                    SELECT-CHILDREN-CONCEPTS-BUTTON
                    SELECT-PARENT-CONCEPTS-BUTTON

                    clear-concept-selection-button 
                    clear-role-selection-button 
                    clear-individual-selection-button 
                    
                    assign-selected-concepts-from-result-button
                    assign-selected-roles-from-result-button
                    assign-selected-individuals-from-result-button

                    selected-first-checkbox
                    selected-only-checkbox

                    query-result-pane
                    
                    TOLD-ONLY-CHECKBOX
                    DATATYPE-FILLERS-CHECKBOX
                                        
                    TBOX-PANE
                    ABOX-PANE
                    CONCEPTS-PANE
                    ROLES-PANE
                    INDIVIDUALS-PANE
                    ASSERTIONS-PANE
                    TAXONOMY-PANE
                    ROLE-HIERARCHY-PANE
                    ABOX-GRAPH-PANE
                    QUERIES-PANE
                    DEFINED-QUERIES-PANE
                    CONCEPTS-RADIO-PANEL
                    ROLES-RADIO-PANEL
                    ASSERTIONS-RADIO-PANEL
                    QUERIES-RADIO-PANEL
                    TBOX-TITLE-PANE
                    ABOX-TITLE-PANE
                    REASONER-TITLE-PANE
                    ONTOLOGY-TITLE-PANE
                    CUR-REASONER-PANE
                    CUR-ONTOLOGY-PANE
                    QUERY-OR-RULE-TITLE-PANE
                    ROLE-TITLE-PANE
                    CONCEPT-TITLE-PANE
                    INDIVIDUAL-TITLE-PANE
                    NAMESPACE-TITLE-PANE
                    PROFILE-NAME-PANE
                    SERVER-REQUEST-TITLE-PANE
                    SERVER-RESPONSE-TITLE-PANE
                    CUR-TBOX-PANE
                    CUR-ABOX-PANE
                    CUR-QUERY-OR-RULE-PANE
                    CUR-ROLE-PANE
                    CUR-INDIVIDUAL-PANE
                    CUR-CONCEPT-PANE
                    CUR-NAMESPACE-PANE
                    CUR-PROFILE-PANE
                    CUR-DEFINITION-PANE
                    REMOVE-URLS-CHECKBOX
                    AUTOFOCUS-CHECKBOX
                    CUR-history-TITLE-PANE
                    CUR-history-PANE
                    
                    SHOW-BOTTOM-CHECKBOX
                    SHOW-TOP-CHECKBOX
                    
                    TAXONOMY-ROOTS-PANEL
                    TAXONOMY-ORIENTATION-PANEL
                    TAXONOMY-KIND-PANEL
                    TAXONOMY-DEPTH-PANEL
                    
                    ROLE-ROOTS-PANEL
                    ROLE-ORIENTATION-PANEL
                    ROLE-HIERARCHY-KIND-PANEL
                    ROLE-HIERARCHY-DEPTH-PANEL
                    
                    ABOX-GRAPH-ROOTS-PANEL
                    ABOX-GRAPH-ORIENTATION-PANEL
                    ABOX-GRAPH-KIND-PANEL
                    ABOX-GRAPH-DEPTH-PANEL
                    
                    INCLUDE-TRANSITIVE-ROLES-CHECKBOX
                    ONLY-SELECTED-SUCCESSORS-CHECKBOX
                    show-top-role-checkbox

                    selected-first-checkbox
                    selected-only-checkbox
                    maintain-arguments-checkbox
                    freeze-graph-checkbox
                 
                    focus-prev 
                    focus-next
                    focus-start 
                    focus-end
                    focus-delete
                    reset-sirius-state-button
                    ))
      
      (let ((pane1 (slot-value sirius pane)))
        (when (typep pane1 'simple-pane)
          (setf (simple-pane-enabled pane1) mode))))))




(defun disable-push-buttons ()
  (with-sirius-app (sirius)
    (with-slots (button-status) sirius
      (setf button-status :disabled)
      (update-push-buttons))))


(defun enable-push-buttons ()
  (with-sirius-app (sirius)
    (with-slots (button-status) sirius
      (setf button-status :enabled)
      (update-push-buttons))))

(defun force-push-buttons-update ()
  (with-sirius-app (sirius)
    (with-slots (last-button-status) sirius
      (setf last-button-status :force))))

(defun update-push-buttons ()
  (with-sirius-app (app)

    (with-slots (button-status 
                 last-button-status) app

      (unless (eq button-status last-button-status)
    
        (setf last-button-status button-status)
        
        (with-profile-access
          (case button-status 
            (:enabled 
             
             (when (zerop busy-counter)
               (switch-all-panes t)
               
               (loop as (button condition) in +racer-command-buttons+ do
                     (setf (button-enabled
                            (slot-value app button))
                           (funcall condition)))

               (loop as (button condition) in +sirius-command-buttons+ do
                     (setf (button-enabled
                            (slot-value app button))
                           (funcall condition)))))
            
            (:disabled 

             (switch-all-panes nil)

             (loop as (button) in +racer-command-buttons+ do
                   (unless (eq button 'full-reset-button)
                     (setf (button-enabled
                            (slot-value app button))
                           nil)))

             (loop as (button condition) in +sirius-command-buttons+ do
                   (setf (button-enabled
                          (slot-value app button))
                         (funcall condition))))))))))



;;;
;;;
;;;

(defun update-sirius-checkboxes ()
  (with-sirius-app (app)
    (let ((profile (active-profile)))

      (with-slots (fold-or-unfold-pane 
                   autofocus-checkbox
                   freeze-graph-checkbox
                   selected-first-checkbox
                   selected-only-checkbox
                   maintain-arguments-checkbox
                   remove-urls-checkbox
                   show-bottom-checkbox
                   show-top-checkbox
                   include-transitive-roles-checkbox
                   only-selected-successors-checkbox
                   show-top-role-checkbox
                   told-only-checkbox
                   node-labels-checkbox
                   datatype-fillers-checkbox
                 
                   concepts-radio-panel
                   roles-radio-panel
                   assertions-radio-panel
                 
                   taxonomy-kind-panel 
                   role-hierarchy-kind-panel
                   abox-graph-kind-panel
                 
                   role-hierarchy-depth-panel 
                   taxonomy-depth-panel 
                   abox-graph-depth-panel 
                 
                   role-roots-panel
                   abox-graph-roots-panel
                   taxonomy-roots-panel
                 
                   role-orientation-panel
                   abox-graph-orientation-panel
                   taxonomy-orientation-panel

                   queries-radio-panel
                 ; rules-radio-panel

                 ;select-children-concepts-button
                 ;select-parent-concepts-button

                   ) app

        (with-profile-access
        
          (setf (button-selected show-bottom-checkbox) show-bottom
                (button-selected show-top-checkbox) show-top
                (button-selected autofocus-checkbox) autofocus
                (button-selected selected-first-checkbox) selected-first
                (button-selected selected-only-checkbox) selected-only
                (button-selected include-transitive-roles-checkbox) include-transitive-roles
                (button-selected show-top-role-checkbox) show-top-role
                (button-selected only-selected-successors-checkbox) only-selected-successors
                (button-selected told-only-checkbox) told-only
                (button-selected node-labels-checkbox) node-labels
                (button-selected datatype-fillers-checkbox) datatype-fillers
                (button-selected remove-urls-checkbox) remove-urls
                (button-selected freeze-graph-checkbox) freeze-graph
                (button-selected maintain-arguments-checkbox) maintain-arguments)


        
          (setf (choice-selected-item fold-or-unfold-pane)
                (cond ((and show-info-pane 
                            show-status-pane)
                       "Classic Layout")
                      ((and show-info-pane 
                            (not show-status-pane))
                       "No Status")
                      ((and (not show-info-pane)
                            show-status-pane)
                       "No Info")
                      ((and (not show-info-pane)
                            (not show-status-pane))
                       "Lean Layout")))


          #| ;;; sind momentan immer an, zu aufwendig die zu verwalten, reicht so noch nicht

        (setf (button-enabled select-children-concepts-button)
              current-concept 
              (button-enabled select-parent-concepts-button)
              current-concept)
          |#     

          ;;;
          ;;;
          ;;;
        

          (case active-tab 
            ((:concepts
              :roles
              :individuals
              :taxonomy
              :role-hierarchy
              :abox-graph
              :assertions
              :axioms)
                      
             (setf (button-enabled selected-first-checkbox) (connected-and-socket-alive-p profile))
             (setf (button-enabled selected-only-checkbox) (connected-and-socket-alive-p profile)))
                      
            (otherwise 
             (setf (button-enabled selected-first-checkbox) nil)
             (setf (button-enabled selected-only-checkbox) nil)))

          ;;;
          ;;;
          ;;; 

          (setf (choice-selected-item taxonomy-kind-panel)
                (ecase taxonomy-kind
                  (:tree "Tree")
                  (:graph "Graph")))

          (setf (choice-selected-item abox-graph-kind-panel)
                (ecase abox-graph-kind
                  (:tree "Tree")
                  (:graph "Graph")))

          (setf (choice-selected-item role-hierarchy-kind-panel)
                (ecase role-hierarchy-kind
                  (:tree "Tree")
                  (:graph "Graph")))

          ;;;
          ;;;
          ;;; 

          (setf (choice-selected-item role-orientation-panel)
                (ecase role-hierarchy-orientation 
                  (:left-right "Hor.")
                  (:top-down "Ver.")))

          (setf (choice-selected-item taxonomy-orientation-panel)
                (ecase taxonomy-orientation 
                  (:left-right "Hor.")
                  (:top-down "Ver.")))

          (setf (choice-selected-item abox-graph-orientation-panel)
                (ecase abox-graph-orientation 
                  (:left-right "Hor.")
                  (:top-down "Ver.")))

          ;;;
          ;;;
          ;;;
        
          (setf (choice-selected-item role-roots-panel)
                (ecase role-hierarchy-root-kind
                  (:all "All Roles")
                  (:current "Cur. Role")
                  (:selected "Sel. Roles")))

          (setf (choice-selected-item taxonomy-roots-panel)
                (ecase taxonomy-root-kind
                  (:all "All Concepts")
                  (:current "Cur. Concept")
                  (:selected "Sel. Concepts")))

          (setf (choice-selected-item abox-graph-roots-panel)
                (ecase abox-graph-root-kind
                  (:all "All Inds.")
                  (:current "Cur. Ind.")
                  (:selected "Sel. Inds.")))

          ;;;
          ;;;
          ;;; 
        
          (setf (choice-selected-item role-hierarchy-depth-panel)
                role-hierarchy-max-depth)

          (setf (choice-selected-item taxonomy-depth-panel)
                taxonomy-max-depth)

          (setf (choice-selected-item abox-graph-depth-panel)
                abox-graph-max-depth)

          ;;;
          ;;;
          ;;;

          (setf (choice-selected-item concepts-radio-panel)
                (ecase concept-list-mode
                  (:all "All")
                  (:primitive "Primitive")
                  (:defined "Defined")
                  (:unsatisfiable "Unsatisfiable")))

          (setf (choice-selected-item roles-radio-panel)
                (ecase role-list-mode
                  (:roles "All")
                  (:transitive-roles "Transitive")
                  (:unsatisfiable "Unsatisfiable")
                  (:features "Features")
                  (:cd-attributes "Attributes")
                  (:datatype-properties "DT Properties")
                  (:annotation-properties "AN Properties")))

          (setf (choice-selected-item assertions-radio-panel)
                (ecase assertion-list-mode
                  (:concept-assertions "Concept A")
                  (:role-assertions "Role A")
                  (:same-as-assertions "Same As A")
                  (:different-from-assertions "Different From A")
                  (:attribute-assertions "Attribute A")
                  (:constraint-assertions "Constraint A")
                  (:annotation-concept-assertions "Annotation CA")
                  (:annotation-role-assertions "Annotation RA")))

          (setf (choice-selected-item queries-radio-panel)
                (ecase query-list-mode
                  (:all "All")
                  (:ready "Ready") 
                  (:running "Running")
                  (:waiting "Waiting")
                  (:cheap-waiting "Cheap W.")
                  (:expensive-waiting "Expensive W.")
                  (:processed "Processed")
                  (:accurate "Acc.") 
                  (:inaccurate "Inacc."))))))))
