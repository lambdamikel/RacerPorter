;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defconstant +sirius-input-model+ 
  `((:GESTURE-SPEC sirius-gesture-spec-input)
    ((:BUTTON-1 :PRESS) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-START-REGION)
    ((:BUTTON-1 :PRESS :SHIFT) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-BUTTON-1-SHIFT-ACTION)
    ((:BUTTON-1 :PRESS :SHIFT :CONTROL) CAPI::MOUSE-EDITOR-CALL EDITOR::MOUSE-SAVE-KILL)
    ((:MOTION :BUTTON-1) CAPI::MOUSE-EDITOR-CALL EDITOR::MOUSE-SET-POINT-AND-HIGHLIGHT)
    ((:BUTTON-1 :RELEASE) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-FINISH-REGION)
    ((:BUTTON-1 :RELEASE :SHIFT) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-CURSOR-RELEASE)
    ((:BUTTON-1 :SECOND-PRESS) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-START-MARK-FORM)
    ((:BUTTON-2 :PRESS) CAPI::MOUSE-EDITOR-CALL CAPI::EDITOR-PANE-PASTE-SELECTION)
    ((:BUTTON-3 :PRESS :SHIFT) CAPI::MOUSE-EDITOR-CALL EDITOR::MOUSE-COPY-AND-PASTE)
                           
    ((:gesture-spec "control-p") previous-command)
    ((:gesture-spec "meta-p")    previous-command)

    ((:gesture-spec "control-n") next-command)
    ((:gesture-spec "meta-n")    next-command)

    ((:gesture-spec "meta-z") tilde-command)

    ((:gesture-spec #\tab) ,#'(lambda (&rest args) 
                                (declare (ignorable args))
                                (complete-racer-command)))
    ((:gesture-spec "control-a") ,#'(lambda (&rest args) 
                                      (declare (ignorable args))
                                      (complete-racer-command)))

    ((:gesture-spec "control-8") ,#'(lambda (&rest args) 
                                      (declare (ignorable args))
                                      (with-sirius-slots 
                                       (command-pane)
                                       (call-editor command-pane "Dynamic Completion"))))

    ((:gesture-spec "control-7") ,#'(lambda (&rest args) 
                                      (declare (ignorable args))
                                      (with-sirius-slots 
                                       (command-pane)
                                       (call-editor command-pane "Complete Racer Filename"))))

    ((:gesture-spec "control-c") abort-command)
    ((:gesture-spec "meta-c")    abort-command)
    ((:gesture-spec "control-z") abort-command)
    ((:gesture-spec "meta-z")    abort-command)
                           
    ((:gesture-spec #\return) sirius-enter-command-from-shell)
    ((:gesture-spec #\space) sirius-enter-space-from-shell)
    ((:gesture-spec #\backspace) sirius-enter-backspace-from-shell)
    ((:gesture-spec #\() sirius-enter-opening-par-from-shell)
    ((:gesture-spec #\)) sirius-enter-closing-par-from-shell)

    ((:gesture-spec "control-s") sirius-incr-search-forward)
    ((:gesture-spec "control-r") sirius-incr-search-backward)
                                
    ;;; f. SPARQL MultiLine-Queries
    ((:gesture-spec #\return :control) sirius-enter-newline-from-shell)))

;;;
;;;
;;;

(defun sirius-gesture-spec-input (&rest args) 
  (if (member (fourth args)
              (list #S(SYSTEM::GESTURE-SPEC :DATA 120 :MODIFIERS 2))
              :test #'equalp)
      (beep-pane)
    (apply #'CAPI::GESTURE-SPEC-INPUT args)))

;;;
;;;
;;;


(defmacro query-or-rule (var query rule cont &rest args)
  `(when ,var
     (cond ((not 
             (member 
              (enter-mouse-select-command
               (list 'describe-query-status ,var))
              '(:not-found :|not-found|)))
            (background-result-command-nc
             ,(if args
                  (let ((args (remove nil args)))
                    `(list ',query ,var ,@args))
                `(list ',query ,var  :abox sirius-current-abox))
             (lambda (res) 
               (declare (ignorable res)) 
               (display-result-if-needed res)
               ,cont)))
           ((not 
             (member 
              (enter-mouse-select-command
               (list 'describe-rule-status ,var))
              '(:not-found :|not-found|)))
            (background-result-command-nc
             ,(if args
                  (let ((args (remove nil args)))
                    `(list ',rule ,var ,@args))
                `(list ',rule ,var  :abox sirius-current-abox))
             (lambda (res) 
               (declare (ignorable res)) 
               (display-result-if-needed res)
               ,cont)))
           (t 
            (setf (profile-current-query-or-rule (active-profile)) nil)))))
            
(defmacro query-or-rule2 (var query rule cont &rest args)
  `(when ,var
     (cond ((not 
             (member 
              (enter-mouse-select-command
               (list 'describe-query-status ,var))
              '(:not-found :|not-found|)))
            (background-result-command-nc
             ,(if args
                  `(list ',query ,var ,@args)
                `(list ',query ,var  :abox sirius-current-abox))
             (lambda (res) 
               (declare (ignorable res)) 
               (display-result-if-needed res)
               ,cont)))
           ((not 
             (member 
              (enter-mouse-select-command
               (list 'describe-rule-status ,var))
              '(:not-found :|not-found|)))
            (background-result-command-nc
             ,(if args
                  `(list ',rule ,var ,@args)
                `(list ',rule ,var  :abox sirius-current-abox))
             (lambda (res) 
               (declare (ignorable res)) 
               (display-result-if-needed res)
               ,cont)))
           (t
            (setf (profile-current-query-or-rule (active-profile)) nil)))))

;;;
;;;
;;; 

(defun display-result-if-needed (res)
  (unless (profile-show-info-pane (active-profile))
    (unless (or (eq res :void)
                (eq res :|void|)
                (eq res :okay)
                (eq res :|okay|)
                (eq res :okay-full-reset)
                (eq res :|okay-full-reset|)
                (eq res :okay-rule-deleted)
                (eq res :|okay-rule-deleted|)
                (eq res :okay-query-deleted)
                (eq res :|okay-query-deleted|)
                (eq res :okay-deleted)
                (eq res :|okay-deleted|))
      
      (sirius-message
       (with-pretty-printing () 
          (format nil "~S" res))))))

;;;
;;;
;;; 

(defun get-visible-min-width (string)
  (declare (ignorable string))
  #+(or (not :lispworks6)
        :win32
        :mac)
  `(:character ,(1+ (length string)))
  #-(or (not :lispworks6)
        :win32
        :mac)
  nil)

(defun get-visible-max-width (string)
  `(:character ,(1+ (length string))))

;;;
;;;
;;;

(define-interface sirius ()
  ((image :accessor image :initform nil)
   (ext-image :accessor ext-image :initform nil)
   (button-status :accessor button-status :initform nil)
   (last-button-status :accessor last-button-status :initform t)
   (log-length :accessor log-length :initform 0)

   (interface-displayed-p :accessor interface-displayed-p :initform nil)
   (load-file-on-display :accessor load-file-on-display :initform nil))
  
  (:panes

   ;;;
   ;;; Main Panes
   ;;; 

   #-:native-progress-bar
   (progress-bar
    capi:output-pane
    :visible-min-height `(:character 1)
    :visible-max-height `(:character 1)
    :title-position :frame)

   #+:native-progress-bar
   (progress-bar
    capi:progress-bar
    :visible-min-height `(:character 1)
    :visible-max-height `(:character 1)
    :start 0
    :end 100
    :title-position :frame)

   ;;;
   ;;;
   ;;;

   (racer-request-title-pane title-pane
                             :title 
                             #+:racer-with-sirius
                             "RacerPlus is processing"
                             #-:racer-with-sirius
                             "RacerPro is processing"
                             :title-font *title-font*
                             :background +state-bc+
                             :foreground +state-fc+
                             :horizontal-scroll nil
                             :vertical-scroll nil)

   (racer-request-pane title-pane
                       :visible-border t
                       :font *info-font*
                       :max-height `(:character 1)
                       :visible-min-width *sirius-field-width*
                       :background +state-bc+
                       :foreground +state-fc+)

   (cancel-racer-request-button push-button :text "Abort Request"
                                :font *racer-button-font*
                                :background +button-bc+
                                :foreground +button-fc+
                                :callback-type :none
                                :callback
                                #'(lambda ()
                                    (with-profile-access
                                      (setf abort-confirmed-p nil
                                            abort-requested-p t))))
   
   ;;;
   ;;;
   ;;; 
   
   (server-pane multi-column-list-panel 
                
                :font *list-font*
                :title-font *title-font*

                :background +sirius-bc+
                :foreground +sirius-fc+

                :test-function #'item-equal
                
                :columns

                `((:title "Profile Name" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Profile Name"))
                  (:title "Default?" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Default?"))
                  (:title "Comm Status" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Comm Status"))
                  (:title "Server Case" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Server Case"))
                  (:title "Host" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Host"))
                  (:title "Port" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Port"))
                  (:title "Version" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Version"))
                  (:title "Auto Connect?" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Auto Connect?"))
                  (:title "Unsafe?" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Unsafe?")))

                :item-print-functions 
                (loop as i from 1 to 9 collect #'line-item-printer-no-doublequotes-no-keywords)

                :callback-type :data

                :selection-callback 
                #'(lambda (data)
                    (activate-profile data))
                
                :column-function #'(lambda (profile)
                                     (list (profile-name profile)
                                           (yes-or-no (profile-default profile))
                                           (if (eq (profile-status profile) :ready)
                                               :ready
                                             (or (profile-current-communication-status profile)
                                                 :not-connected))
                                           (profile-case profile)
                                           (profile-ip profile)
                                           (profile-port profile)
                                           (profile-racer-version profile)
                                           (yes-or-no (profile-auto-connect profile))
                                           (yes-or-no-or-unknown 
                                            (if (connected-and-socket-alive-p profile)
                                                (profile-unsafe profile)
                                              :dont-know)))))

   (command-pane editor-pane 
                 :title-font *title-font*
                 :font  
                 *shell-font*
                 :echo-area t
                 :enabled t
                 :buffer-name "Command"
                 :background +sirius-bc+
                 :foreground +sirius-fc+
                 :text ""
                 :input-model +sirius-input-model+

                 :visible-height `(:character ,+sirius-listener-height+)

                 :buffer-modes '("Lisp"))

   (modeline title-pane :font *shell-font*)

   ;;;
   ;;; TBox, ABox Panes: Single Selection List Panels 
   ;;; 
   
   (tbox-pane multi-column-list-panel

              :font *list-font*
              :title-font *title-font*

              :background +sirius-bc+
              :foreground +sirius-fc+
              
              :test-function #'(lambda (x y) (item-equal x (first (first y))))

              :columns 

              `((:title "TBox Name" :adjust :left
                 :visible-min-width ,(get-visible-min-width "TBox Name"))
                (:title "Classified?" :adjust :left
                 :visible-min-width ,(get-visible-min-width "Classified?"))
                (:title "Meta Constraint?" :adjust :left
                 :visible-min-width ,(get-visible-min-width "Meta Constraint?"))
                (:title "Cyclic?" :adjust :left
                 :visible-min-width ,(get-visible-min-width "Cyclic?"))
                (:title "Language" :adjust :left
                 :visible-min-width ,(get-visible-min-width "Language"))
                (:title "# Concepts" :adjust :left
                 :visible-min-width ,(get-visible-min-width "# Concepts"))
                (:title "# Roles" :adjust :left
                 :visible-min-width ,(get-visible-min-width "# Roles"))
                (:title "Associated ABoxes" :adjust :left
                 :visible-min-width ,(get-visible-min-width "Associated ABoxes")))
              
              :item-print-functions 
              (cons (lambda (x) 
                      (line-item-printer (first x) nil (second x)))
                    (loop as i from 1 to 7 collect #'line-item-printer))

              :callback-type :data
              :selection-callback 
              #'(lambda (data)
                  (let ((tbox (first (first data))))
                    (with-profile-access
                      (let ((res 
                             (enter-mouse-select-command
                              `(find-tbox ,tbox))))

                        (unless (error-res-p res)
                          (setf sirius-current-tbox tbox)
                          (reset-tbox-specific-pane-contents)))))))
   
   (abox-pane multi-column-list-panel 
              
              :title-font *title-font*
              :font *list-font*              

              :background +sirius-bc+
              :foreground +sirius-fc+
              
              :test-function #'(lambda (x y) (item-equal x (first (first y))))

              :columns
              
              `((:title "ABox Name" :adjust :left
                 :visible-min-width ,(get-visible-min-width "ABox Name"))
                (:title "Associated TBox" :adjust :left
                 :visible-min-width ,(get-visible-min-width "Associated TBox"))
                (:title "Realized?" :adjust :left
                 :visible-min-width ,(get-visible-min-width "Realized?"))
                (:title "Language" :adjust :left
                 :visible-min-width ,(get-visible-min-width "Language"))
                (:title "# Individuals" :adjust :left
                 :visible-min-width ,(get-visible-min-width "# Individuals"))
                (:title "# Concept Assertions" :adjust :left
                 :visible-min-width ,(get-visible-min-width "# Concept Assertions"))
                (:title "# Role Assertions" :adjust :left
                 :visible-min-width ,(get-visible-min-width "# Role Assertions")))

              :item-print-functions 
              (cons (lambda (x) 
                      (line-item-printer (first x) nil (second x)))
                    (loop as i from 1 to 6 collect #'line-item-printer))

              :callback-type :data

              :selection-callback 
              #'(lambda (data)
                  (let ((abox (first (first data))))
                    (with-profile-access
                      (let ((res 
                             (enter-mouse-select-command
                              `(find-abox ,abox))))
                        
                        (unless (error-res-p res)
                          (setf sirius-current-abox abox)
                          (reset-abox-specific-pane-contents)))))))
   
   ;;;
   ;;; Multiple Selection List Panels: Concepts, Roles, Individuals 
   ;;; 
   
   (concepts-pane list-panel

                  :title-font *title-font*
                  :font *list-font*              

                  :interaction :multiple-selection
                  :background +sirius-bc+
                  :foreground +sirius-fc+
                  :callback-type :data
                  :print-function #'(lambda (item) 
                                      (line-item-printer item nil
                                                         (eq item 
                                                             (with-profile-access
                                                               current-concept))))
                  :test-function #'item-equal

                  :selection-callback 
                  #'(lambda (concept)
                      (with-profile-access
                        (let* ((res
                                (enter-mouse-select-command
                                 `(concept? ,concept ,sirius-current-tbox))))
                          
                          (unless (error-res-p res)
                            (toggle-concepts (list concept))
                            (schedule-update)))))
                  
                  :retract-callback
                  #'(lambda (concept)
                      (with-profile-access
                        (toggle-concepts (list concept))
                        (schedule-update))))


   (roles-pane multi-column-list-panel
               
               :title-font *title-font*
               :font *list-font*
               
               :background +sirius-bc+
               :foreground +sirius-fc+
               
               :test-function #'item-equal 

               :columns 
               `((:title "Role" :adjust :left
                  :visible-min-width ,(get-visible-min-width "Role"))
                 (:title "Inverse Role" :adjust :left
                  :visible-min-width ,(get-visible-min-width "Inverse Role"))
                 (:title "Domain" :adjust :left
                  :visible-min-width ,(get-visible-min-width "Domain"))
                 (:title "Range" :adjust :left
                  :visible-min-width ,(get-visible-min-width "Range"))
                 (:title "Parents" :adjust :left
                  :visible-min-width ,(get-visible-min-width "Parents")))
              
               :interaction :multiple-selection
               :background +sirius-bc+
               :foreground +sirius-fc+
               :callback-type :data

               :item-print-functions 
               (list #'(lambda (item) 
                         (line-item-printer item nil
                                            (eq item 
                                                (with-profile-access
                                                  current-role))))
                     #'(lambda (item) 
                         (line-item-printer-permit-empty-content item nil
                                                                 (eq item 
                                                                     (with-profile-access
                                                                       current-role))))

                     #'line-item-printer-max-width-permit-empty-content
                     #'line-item-printer-max-width-permit-empty-content
                     #'line-item-printer-max-width-permit-empty-content)
                     
               :selection-callback 
               #'(lambda (data)
                   (with-profile-access
                     (let* ((role (first data))
                            (res 
                             (enter-mouse-select-command
                              `(role? ,role ,sirius-current-tbox))))
                       
                       (unless (error-res-p res)
                         (toggle-roles (list role)))
                       (schedule-update))))
               
               :retract-callback
               #'(lambda (data)
                   (let ((role (first data)))
                     (toggle-roles (list role))
                     (schedule-update))))

   (individuals-pane list-panel 
                     
                     :title-font *title-font*
                     :font *list-font*
                     
                     :interaction :multiple-selection
                     :background +sirius-bc+
                     :foreground +sirius-fc+
                     :callback-type :data
                     
                     :print-function #'(lambda (item)
                                         (line-item-printer item nil 
                                                            (eq item 
                                                                (with-profile-access
                                                                  current-individual))))
                     
                     :test-function #'item-equal

                     :selection-callback 
                     #'(lambda (ind)
                         (with-profile-access
                           (let ((res 
                                  (enter-mouse-select-command
                                   `(individual? ,ind ,sirius-current-abox))))
                             
                             (unless (error-res-p res)
                               (toggle-individuals (list ind)))
                             
                             (schedule-update))))

                     :retract-callback 
                     #'(lambda (ind)
                         (with-profile-access
                           (toggle-individuals (list ind))
                           (schedule-update))))

   ;;;
   ;;;
   ;;; 

   (assertions-pane list-panel 

                    :title-font *title-font*
                    :font *list-font*
                     
                    :test-function #'item-equal
                    :print-function #'line-item-printer
                    :background +sirius-bc+
                    :foreground +sirius-fc+

                    :interaction :multiple-selection
                    :callback-type :data

                    :selection-callback 
                    #'(lambda (data)
                        (with-profile-access
                          (toggle-assertions 
                           (list (list assertion-list-mode data))))

                        (schedule-update))
                  
                    :retract-callback
                    #'(lambda (data)
                        (with-profile-access
                          (toggle-assertions
                           (list (list assertion-list-mode data))))

                        (schedule-update)))

   ;;;
   ;;;
   ;;; 

   (axioms-pane multi-column-list-panel
                
                :title-font *title-font*
                :font *list-font*
                
                :interaction :multiple-selection

                :background +sirius-bc+
                :foreground +sirius-fc+
                
                :columns 
                `((:title "ID" :adjust :left
                   :visible-min-width ,(get-visible-min-width "ID"))
                  (:title "Axiom Type & Attribute Type" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Axiom Type & Attribute Type"))
                  (:title "Arguments" :adjust :left
                   :visible-min-width ,(get-visible-min-width "Arguments")))

                ;;; :separator-item :separator
                ;;; gibt es nicht

                :item-print-functions 
                (list #'(lambda (item &optional stream)
                          (line-item-printer-permit-empty-content 
                           item stream
                           (with-profile-access
                             (and current-axiom
                                  (eql item
                                       (first current-axiom))))))

                      #'line-item-printer-permit-empty-content  
                      #'line-item-printer-permit-empty-content  
                      #'(lambda (x &optional stream)
                          (declare (ignorable x))
                          (format stream "")))

                :test-function #'item-equal

                :callback-type :data
               
                :selection-callback 
                #'(lambda (data)
                    (with-profile-access
                      (let* ((axiom-id (first data))
                             (res (when axiom-id
                                    (enter-mouse-select-command
                                     `(owlapi-id-to-axiom ,axiom-id)))))

                        (when (and (not (error-res-p res)) res)
                          (toggle-axioms (list data)))
                        
                        (schedule-update))))

                :retract-callback 
                #'(lambda (data)
                    (with-profile-access
                      (let* ((axiom-id (first data))
                             (res (when axiom-id
                                    (enter-mouse-select-command
                                     `(owlapi-id-to-axiom ,axiom-id)))))

                        (when (and (not (error-res-p res)) res)
                          (toggle-axioms (list data)))

                        (schedule-update)))))

   (axioms-selector list-panel
                    :title "Display (& Create New) Axioms of Type"
                    
                    :title-font *title-font*
                    :font *list-font*
                    
                    :background +sirius-bc+
                    :foreground +sirius-fc+
                    
                    :test-function #'(lambda (x y) 
                                       (string-equal x (second y)))

                    :print-function #'(lambda (x &optional stream)
                                        (with-profile-access
                                          (let* ((highlight-p 
                                                  (string-equal (second x) 
                                                                (symbol-name new-axiom-type)))

                                                 (item (if highlight-p 
                                                           (string-upcase (second x))
                                                         (second x)))
                                                 (abstract-p (not (third x)))
                                                 (c (if abstract-p
                                                        " (Abstract)"
                                                      (if highlight-p 
                                                          " (NEW AXIOM)"
                                                        ""))))

                                            (ecase (first x)
                                              (0 (format stream "~A~A" item c))
                                              (1 (format stream "|_~A~A" item c))
                                              (2 (format stream "|__~A~A" item c))
                                              (3 (format stream "|___~A~A" item c))
                                              (4 (format stream "|____~A~A" item c))
                                              (5 (format stream "|_____~A~A" item c))))))
                    
                    :items '((0 "Axiom")

                             (1 "Loaded Axiom")
                             (1 "Unloaded Axiom")
                            
                             (1 "ImportsDeclarationAxiom"               "OWLAPI-getOWLImportsDeclarationAxiom")
                             (1 "PrefixDeclarationAxiom"                "OWLAPI-getOWLPrefixDeclarationAxiom")
                             (1 "OntologyVersionDeclarationAxiom"       "OWLAPI-getOWLOntologyVersionDeclarationAxiom")
                              
                             (1 "DatatypeDefinitionAxiom"               "OWLAPI-getOWLDatatypeDefinitionAxiom")
                             
                             (1 "LogicalAxiom")

                             (2 "HasKeyAxiom"                           "OWLAPI-getOWLHasKeyAxiom")

                             (2 "ClassAxiom")

                             (3 "SubClassAxiom"                         "OWLAPI-getOWLSubClassAxiom")
                             (3 "EquivalentClassesAxiom"                "OWLAPI-getOWLEquivalentClassesAxiom")
                             (3 "DisjointUnionAxiom"                    "OWLAPI-getOWLDisjointUnionAxiom")
                             (3 "DisjointClassesAxiom"                  "OWLAPI-getOWLDisjointClassesAxiom")

                             (2 "IndividualAxiom")
                             (3 "ClassAssertionAxiom"                   "OWLAPI-getOWLClassAssertionAxiom")
                             (3 "SameIndividualsAxiom"                  "OWLAPI-getOWLSameIndividualsAxiom")
                             (3 "DifferentIndividualsAxiom"             "OWLAPI-getOWLDifferentIndividualsAxiom")

                             (3 "IndividualRelationshipAxiom")
                             
                             (4 "ObjectPropertyAssertionAxiom"          "OWLAPI-getOWLObjectPropertyAssertionAxiom")
                             (4 "NegativeObjectPropertyAssertionAxiom"  "OWLAPI-getOWLNegativeObjectPropertyAssertionAxiom")
                             (4 "DataPropertyAssertionAxiom"            "OWLAPI-getOWLDataPropertyAssertionAxiom")
                             (4 "NegativeDataPropertyAssertionAxiom"    "OWLAPI-getOWLNegativeDataPropertyAssertionAxiom")
                             
                
                             (2 "PropertyAxiom")
                             
                             (3 "DataPropertyAxiom")
                             (4 "EquivalentDataPropertiesAxiom"         "OWLAPI-getOWLEquivalentDataPropertiesAxiom")
                             (4 "DisjointDataPropertiesAxiom"           "OWLAPI-getOWLDisjointDataPropertiesAxiom")
                             (4 "DataPropertyRangeAxiom"                "OWLAPI-getOWLDataPropertyRangeAxiom")
                             (4 "DataPropertyDomainAxiom"               "OWLAPI-getOWLDataPropertyDomainAxiom")
                             (4 "DataSubPropertyAxiom"                  "OWLAPI-getOWLDataSubPropertyAxiom")
                             (4 "FunctionalDataPropertyAxiom"           "OWLAPI-getOWLFunctionalDataPropertyAxiom")

                             (3 "ObjectPropertyAxiom")
                             (4 "TransitiveObjectPropertyAxiom"         "OWLAPI-getOWLTransitiveObjectPropertyAxiom")
                             (4 "FunctionalObjectPropertyAxiom"         "OWLAPI-getOWLFunctionalObjectPropertyAxiom")
                             (4 "ReflexiveObjectPropertyAxiom"          "OWLAPI-getOWLReflexiveObjectPropertyAxiom")
                             (4 "IrreflexiveObjectPropertyAxiom"        "OWLAPI-getOWLIrreflexiveObjectPropertyAxiom")
                             (4 "InverseFunctionalObjectPropertyAxiom"  "OWLAPI-getOWLInverseFunctionalObjectPropertyAxiom")
                             (4 "AsymmetricObjectPropertyAxiom"         "OWLAPI-getOWLAsymmetricObjectPropertyAxiom")
                             (4 "AntiSymmetricObjectPropertyAxiom"      "OWLAPI-getOWLAntiSymmetricObjectPropertyAxiom")
                             (4 "ObjectPropertyChainSubPropertyAxiom"   "OWLAPI-getOWLObjectPropertyChainSubPropertyAxiom")
                             
                             (4 "ObjectSubPropertyAxiom"                "OWLAPI-getOWLObjectSubPropertyAxiom")
                             (4 "ObjectPropertyDomainAxiom"             "OWLAPI-getOWLObjectPropertyDomainAxiom")
                             (4 "ObjectPropertyRangeAxiom"              "OWLAPI-getOWLObjectPropertyRangeAxiom")
                             
                             (4 "InverseObjectPropertiesAxiom"          "OWLAPI-getOWLInverseObjectPropertiesAxiom")
                             (4 "EquivalentObjectPropertiesAxiom"       "OWLAPI-getOWLEquivalentObjectPropertiesAxiom")
                             (4 "DisjointObjectPropertiesAxiom"         "OWLAPI-getOWLDisjointObjectPropertiesAxiom")
                             (4 "SymmetricObjectPropertyAxiom"          "OWLAPI-getOWLSymmetricObjectPropertyAxiom")
                               

                             (2 "DeclarationAxiom"                      "OWLAPI-getOWLDeclarationAxiom")
                             (3 "ImplicitDeclarationAxiom"              "OWLAPI-getOWLImplicitDeclarationAxiom")

                             (2 "AnnotationAxiom") 
                              
                             (3 "OntologyAnnotationAxiom"               "OWLAPI-getOWLOntologyAnnotationAxiom")
                             (3 "EntityAnnotationAxiom"                 "OWLAPI-getOWLEntityAnnotationAxiom")
                             (3 "AxiomAnnotationAxiom"                  "OWLAPI-getOWLAxiomAnnotationAxiom")
                             (3 "AnnotationAssertionAxiom"              "OWLAPI-getOWLAnnotationAssertionAxiom")
                           
                             (3 "AnnotationPropertyDomainAxiom"         "OWLAPI-getOWLAnnotationPropertyDomainAxiom")
                             (3 "AnnotationPropertyRangeAxiom"          "OWLAPI-getOWLAnnotationPropertyRangeAxiom")
                             (3 "SubAnnotationPropertyOfAxiom"          "OWLAPI-getOWLSubAnnotationPropertyOfAxiom"))

                    :interaction :multiple-selection
                
                    :callback-type :data
                    :selection-callback 
                    #'(lambda (data)
                        (with-profile-access
                          (let* ((item (second data))
                                 (iitem (intern item)))

                            (pushnew iitem current-axiom-types)
                            (when (third data) (setf new-axiom-type iitem))
                            
                            (schedule-update))))

                    :retract-callback 
                    #'(lambda (data)
                        (with-profile-access
                          (let* ((item (second data))
                                 (iitem (intern item)))

                            (unless (eq iitem new-axiom-type)
                              (setf current-axiom-types
                                    (delete iitem current-axiom-types)))

                            (when (third data)
                              (setf new-axiom-type nil))
                                
                            (schedule-update)))))
   

   ;;;
   ;;; Graph Panes 
   ;;; 

   (taxonomy-pane editor-pane
                 :font  
                 *shell-font*
                  :title ""
                  :buffer-name "TAX"
                  :accepts-focus-p t
                  :background +sirius-bc+
                  :foreground +sirius-fc+
                  :vertical-scroll t
                  :horizontal-scroll nil
                  :enabled nil)

   (role-hierarchy-pane editor-pane
                        :font  
                        *shell-font*
                        :title ""
                        :buffer-name "RHI"
                        :accepts-focus-p t
                        :background +sirius-bc+
                        :foreground +sirius-fc+
                        :vertical-scroll t
                        :horizontal-scroll nil
                        :enabled nil)
 
   (abox-graph-pane editor-pane
                    :font  
                    *shell-font*
                    :title ""
                    :buffer-name "NET"
                    :accepts-focus-p t
                    :background +sirius-bc+
                    :foreground +sirius-fc+
                    :vertical-scroll t
                    :horizontal-scroll nil
                    :enabled nil)

   ;;;
   ;;; Queries Pane: List Panel (Single Selection)
   ;;; 

   (queries-pane  multi-column-list-panel 
                  
                  :title-font *title-font*
                  :font *list-font*

                  :background +sirius-bc+
                  :foreground +sirius-fc+

                  :test-function #'(lambda (x y) (item-equal x (first y)))

                  :columns 
                  
                  `((:title "Id" :adjust :left
                     :visible-min-width ,(get-visible-min-width "Id"))
                    (:title "State" :adjust :left
                     :visible-min-width ,(get-visible-min-width "State"))
                    (:title "Cheap?" :adjust :left
                     :visible-min-width ,(get-visible-min-width "Cheap?"))
                    (:title "Next?" :adjust :left
                     :visible-min-width ,(get-visible-min-width "Next?"))
                    (:title "Query / Rule" :adjust :left
                     :visible-min-width ,(get-visible-min-width "Query / Rule")))
                  
                  :item-print-functions 
                  (loop as i from 1 to 6 collect #'line-item-printer)

                  :callback-type :data

                  :selection-callback 
                  #'(lambda (data)
                      (with-profile-access
                        (let* ((query (first data))
                               (res
                                (cond ((member 
                                        (enter-mouse-select-command
                                         `(describe-query-status ,query))
                                        '(:not-found :|not-found|))

                                       (enter-mouse-select-command
                                        `(describe-rule-status ,query)))
                                      (t 
                                       (enter-mouse-select-command
                                        `(describe-query-status ,query))))))

                          (unless (error-res-p res)
                            (setf current-query-or-rule query)
                            (schedule-update)))))

                  :retract-callback
                  #'(lambda (data)
                      (declare (ignorable data))
                      (with-profile-access
                        (setf current-query-or-rule nil)
                        (schedule-update))))
   
   (defined-queries-pane list-panel 
                         
                         :title-font *title-font*
                         :font *list-font*
                         
                         :interaction :single-selection
                         :background +sirius-bc+
                         :foreground +sirius-fc+
                         :callback-type :data 
                         :print-function #'line-item-printer
                         :test-function #'item-equal

                         :selection-callback 
                         #'(lambda (data)
                             (declare (ignorable data))
                             (with-profile-access
                               (when sirius-current-tbox
                                 (let* ((name (second data))
                                        (vars (third data)))
                                   (background-result-command 
                                    `(describe-definition ,name 
                                                          :arity ,(length vars)
                                                          :tbox ,sirius-current-tbox)
                                    (lambda (res)
                                      (declare (ignorable res))
                                      (setf current-definition (list name vars))
                                      (schedule-update)))))))
                         
                         :retract-callback 
                         #'(lambda (data)
                             (declare (ignorable data))
                             (with-profile-access
                               (setf current-definition nil)
                               (schedule-update))))

   ;;;
   ;;;
   ;;; 

   (query-result-pane  multi-column-list-panel 
                       
                       :title-font *title-font*
                       :font *list-font*
                       
                       :background +sirius-bc+
                       :foreground +sirius-fc+
                       :interaction :multiple-selection
                     
                       :test-function #'(lambda (x y) 
                                          (item-equal x (third y)))
                      
                       :columns `((:title "No." :adjust :left
                                   :visible-min-width ,(get-visible-min-width "No."))
                                  (:title "Variable" :adjust :left
                                   :visible-min-width ,(get-visible-min-width "Variable"))
                                  (:title "Binding" :adjust :left
                                   :visible-min-width ,(get-visible-min-width "Binding")))
                      
                       :item-print-functions 
                       (list #'line-item-printer
                             #'line-item-printer 
                             #'(lambda (item) 
                                 (with-profile-access
                                   (line-item-printer item nil 
                                                      (or (eq item current-individual)
                                                          (eq item current-concept))))))
                                                          
                
                       :callback-type :data 
                      
                       :selection-callback 
                       #'(lambda (data)
                           (let ((data (third data)))
                             (cond ((symbolp data)
                                    (with-profile-access
                                      (let ((res 
                                             (enter-mouse-select-command
                                              `(individual? ,data ,sirius-current-abox))))
                                        (when (and (not (error-res-p res))
                                                   res)
                                          (toggle-individuals (list data))))
                                      (let ((res 
                                             (enter-mouse-select-command
                                              `(concept? ,data ,sirius-current-abox))))
                                        (when (and (not (error-res-p res))
                                                   res)
                                          (toggle-concepts (list data)))))
                                    (schedule-update))
                                   (t (beep-pane)))))

                       :retract-callback 
                       #'(lambda (data)
                           (let ((data (third data)))
                             (cond ((symbolp data)
                                    (cond ((symbolp data)
                                           (with-profile-access
                                             (let ((res 
                                                    (enter-mouse-select-command
                                                     `(individual? ,data ,sirius-current-abox))))
                                               (when (and (not (error-res-p res))
                                                          res)
                                                 (toggle-individuals (list data))))
                                             (let ((res 
                                                    (enter-mouse-select-command
                                                     `(concept? ,data ,sirius-current-abox))))
                                               (when (and (not (error-res-p res))
                                                          res)
                                                 (toggle-concepts (list data)))))
                                           (schedule-update))
                                          (t (beep-pane))))))))
   
   ;;;
   ;;; Log Pane
   ;;; 

   (log-pane editor-pane
             :title ""

             :title-font *title-font*
             
             :font  
             *shell-font*

             :buffer-name "Log"

             :background +sirius-bc+
             :foreground +sirius-fc+

             :vertical-scroll t
             :horizontal-scroll t
             
             :accepts-focus-p t
             ;;; accepts focus notwendig wegen CAPI warnings

             :enabled t)

   ;;;
   ;;; Logo Pane
   ;;; 

   (logo-pane output-pane
              ;simple-pinboard-layout
              :enabled nil
              :accepts-focus-p nil
              :display-callback 'display-logo-pane
              :background +logo-bc+
              :horizontal-scroll nil
              :vertical-scroll nil)

   (version-info-pane title-pane
                      :text +version-info+)

   ;;;
   ;;;
   ;;;
   
   (info-pane editor-pane
              :title "Info"
              :title-font *title-font*

                :font  
                *shell-font*

                :buffer-name "Info"
              :accepts-focus-p nil
              ;;; ACHTUNG! T IST WICHTIG, LT. LISPWORKS SUPPORT
              ;;; ERSCHEINEN SONST DIE CAPI WARNINGS, WENN NICHT
              ;;; EINE PANE PRO TAB DEN INITIALEN FOCUS AUTOMATISCH
              ;;; ANNEHMEN KANN! DAS IST HIER DIE INFO PANE
              ;;; Update: Ist mit LW5 wieder obsolet (Warnings gibt's sowieso... :-))
              ;;; -> wieder auf nil
              :background +sirius-bc+
              :foreground +sirius-fc+
              :buffer-name "Info"

              :vertical-scroll t
              :horizontal-scroll t
             
              :enabled t

              :max-height `(:character ,+sirius-info-max-height+)
              :visible-height `(:character ,+sirius-info-height+))

   ;;;
   ;;; Radio Panels 
   ;;;

   (concepts-radio-panel radio-button-panel
                         :font *radiobox-font*
                         :background +button-bc+
                         :foreground +button-fc+
                         :items '("All" 
                                  "Primitive"  
                                  "Defined"
                                  "Unsatisfiable")
                         :test-function #'item-equal
                         :callback-type :data
                         :selection-callback
                         #'(lambda (data)
                             (with-profile-access
                               (setf concept-list-mode
                                     (cond ((string= data "All")
                                            :all)
                                           ((string= data "Primitive")
                                            :primitive)
                                           ((string= data "Defined")
                                            :defined)
                                           ((string= data "Unsatisfiable")
                                            :unsatisfiable)))

                               ;;; notwendiges Update
                               (schedule-update))))

   
   (roles-radio-panel radio-button-panel
                      :font *radiobox-font*
                      :background +button-bc+
                      :foreground +button-fc+
                      :test-function #'item-equal
                   
                      :items '("All" 
                               "Unsatisfiable"
                               "Transitive" 
                               "Features"  
                               "Attributes" 
                               "DT Properties"
                               "AN Properties")

                      :callback-type :data
                      :selection-callback
                      #'(lambda (data)
                          (with-profile-access
                            (setf role-list-mode
                                  (cond ((string= data "All")
                                         :roles)
                                        ((string= data "Unsatisfiable")
                                         :unsatisfiable)
                                        ((string= data "Transitive")
                                         :transitive-roles)
                                        ((string= data "Features")
                                         :features)
                                        ((string= data "Attributes")
                                         :cd-attributes)
                                        ((string= data "DT Properties")
                                         :datatype-properties)
                                        ((string= data "AN Properties")
                                         :annotation-properties)))

                            (schedule-update))))

   
   (assertions-radio-panel radio-button-panel
                           :font *radiobox-font*
                           :background +button-bc+
                           :foreground +button-fc+
                           :test-function #'item-equal
                   
                           :items '("Concept A" 
                                    "Role A"  
                                    "Same As A"
                                    "Different From A"
                                    "Attribute A" 
                                    "Constraint A" 
                                    "Annotation CA"
                                    "Annotation RA")

                           :callback-type :data
                           :selection-callback
                           #'(lambda (data)
                               (with-profile-access
                                 (setf assertion-list-mode
                                       (cond ((string= data "Concept A")
                                              :concept-assertions)
                                             ((string= data "Role A")
                                              :role-assertions)
                                             ((string= data "Same As A")
                                              :same-as-assertions)
                                             ((string= data "Different From A")
                                              :different-from-assertions)
                                             ((string= data "Attribute A")
                                              :attribute-assertions)
                                             ((string= data "Constraint A")
                                              :constraint-assertions)
                                             ((string= data "Annotation CA")
                                              :annotation-concept-assertions)
                                             ((string= data "Annotation RA")
                                              :annotation-role-assertions))))

                               (schedule-update)))
   
   
   (queries-radio-panel radio-button-panel
                        :font *radiobox-font*
                        :background +button-bc+
                        :foreground +button-fc+
                        :test-function #'item-equal
                        :items '("All" "Ready" "Running" "Waiting"
                                       "Cheap W." "Expensive W."
                                       "Processed" "Acc." "Inacc.")
                        :callback-type :data
                        :selection-callback #'(lambda (data)
                                                (declare (ignorable args))
                                                (with-profile-access
                                                  (setf current-query-or-rule nil
                                                        query-list-mode 
                                                        (cond ((string= data "All") :all)
                                                              ((string= data "Ready") :ready)
                                                              ((string= data "Running") :running)
                                                              ((string= data "Waiting") :waiting)
                                                              ((string= data "Cheap W.") :cheap-waiting)
                                                              ((string= data "Expensive W.") :expensive-waiting)
                                                              ((string= data "Processed") :processed)
                                                              ((string= data "Acc.") :accurate)
                                                              ((string= data "Inacc.") :inaccurate)))

                                                  (schedule-update))))

 
   ;;;
   ;;; State Display 
   ;;;

   (tbox-title-pane title-pane
                    :title "TBox (*t*)"
                    :title-font *title-font*
                    :background +state-bc+
                    :foreground +state-fc+
                    :horizontal-scroll nil
                    :vertical-scroll nil)

   (abox-title-pane title-pane
                    :title "ABox (*a*)"
                    :title-font *title-font*
                    :background +state-bc+
                    :foreground +state-fc+
                    :horizontal-scroll nil
                    :vertical-scroll nil)
                    
   (query-or-rule-title-pane title-pane
                             :title "Query / Rule (*qor*)"
                             :title-font *title-font*
                             :background +state-bc+
                             :foreground +state-fc+
                             :horizontal-scroll nil
                             :vertical-scroll nil)

   (definition-title-pane title-pane
                          :title "Definition (*def* = Name)"
                          :title-font *title-font*
                          :background +state-bc+
                          :foreground +state-fc+
                          :horizontal-scroll nil
                          :vertical-scroll nil)
   
                  
   (role-title-pane title-pane
                    :title "Role (*r*)"
                    :title-font *title-font*
                    :background +state-bc+
                    :foreground +state-fc+
                    :horizontal-scroll nil
                    :vertical-scroll nil)
   
   
   (concept-title-pane title-pane
                       :title "Concept (*c*)"
                       :title-font *title-font*
                       :background +state-bc+
                       :foreground +state-fc+
                       :horizontal-scroll nil
                       :vertical-scroll nil)
                  
   (individual-title-pane title-pane
                          :title "Individual (*i*)"
                          :title-font *title-font*
                          :background +state-bc+
                          :foreground +state-fc+
                          :horizontal-scroll nil
                          :vertical-scroll nil)


   (axiom-title-pane title-pane
                     :title "Axiom (*ax*)"
                     :title-font *title-font*
                     :background +state-bc+
                     :foreground +state-fc+
                     :horizontal-scroll nil
                     :vertical-scroll nil)

   (namespace-title-pane title-pane
                         :title "Namespace (#!:, *n*)"
                         :title-font *title-font*
                         :background +state-bc+
                         :foreground +state-fc+
                         :horizontal-scroll nil
                         :vertical-scroll nil)
   
   (profile-name-pane title-pane
                      :title "Active Profile"
                      :title-font *title-font*
                      :background +state-bc+
                      :foreground +state-fc+
                      :horizontal-scroll nil
                      :vertical-scroll nil)
   

   #+:pulse-pane
   (server-request-pulse-pane image-pinboard-object 
                              :accepts-focus-p nil
                              :image (get-pulse-image :not-connected 
                                                      :not-connected))

   (server-request-title-pane title-pane
                              :title "Request"
                              :title-font *title-font*
                              :background +state-bc+
                              :foreground +state-fc+
                              :horizontal-scroll nil
                              :vertical-scroll nil)

   (server-request-pane title-pane
                        :visible-border t
                        :font *info-font*
                        :max-height `(:character 1)
                        :visible-min-width *sirius-field-width*
                        :background +state-bc+
                        :foreground +state-fc+)
   
   (server-response-title-pane title-pane

                               :title "Response"
                               :title-font *title-font*
                               :background +state-bc+
                               :foreground +state-fc+
                               :horizontal-scroll nil
                               :vertical-scroll nil)

   (server-response-pane title-pane
                         :visible-border t
                         :font *info-font*
                         :max-height `(:character 1)
                         :visible-min-width *sirius-field-width*
                         :background +state-bc+
                         :foreground +state-fc+)

   ;;;
   ;;;
   ;;; 

   (lambda-list-pane title-pane
                     :title ""
                     :visible-border nil
                     :font *shell-font*
                     :background +state-bc+
                     :foreground +state-fc+
                     :max-width (* 2 *sirius-field-width*)
                     :min-width (* 2 *sirius-field-width*)
                     :width (* 2 *sirius-field-width*)
                     :max-height `(:character 2))

   #|
   (lambda-info-button push-button :text "Function Doc"
                       :font *button-font*
                       :background +button-bc+
                       :foreground +button-fc+
                       :callback-type :none
                       :callback #'show-lambda-doc) |# 

   (complete-button push-button :text "Complete Input"
                    :font *button-font*
                    :background +button-bc+
                    :foreground +button-fc+
                    :callback-type :none
                    :callback #'(lambda () 
                                  (complete-racer-command command-pane)))

   ;;;
   ;;;
   ;;;

   (cur-tbox-pane title-pane
                  :font *info-font*
                  :visible-border t
                  :background +state-bc+
                  :foreground +state-fc+
                  :horizontal-scroll nil
                  :vertical-scroll nil
                  :visible-min-width *sirius-field-width*)
                  
   (cur-abox-pane title-pane
                  :font *info-font*
                  :visible-border t
                  :background +state-bc+
                  :foreground +state-fc+
                  :horizontal-scroll nil
                  :vertical-scroll nil
                  :visible-min-width *sirius-field-width*)

   (cur-query-or-rule-pane title-pane
                           :font *info-font*
                           :visible-border t
                           :background +state-bc+
                           :foreground +state-fc+
                           :horizontal-scroll nil
                           :vertical-scroll nil
                           :visible-min-width *sirius-field-width*)

   (cur-definition-pane title-pane
                        :font *info-font*
                        :visible-border t
                        :background +state-bc+
                        :foreground +state-fc+
                        :horizontal-scroll nil
                        :vertical-scroll nil
                        :visible-min-width *sirius-field-width*)

   (cur-role-pane title-pane
                  :font *info-font*
                  :visible-border t
                  :background +state-bc+
                  :foreground +state-fc+
                  :horizontal-scroll nil
                  :vertical-scroll nil
                  :external-min-width (- *sirius-field-width* 50))

   (cur-no-selected-roles-pane title-pane
                               :font *info-font*
                               :visible-border t
                               :background +state-bc+
                               :foreground +state-fc+
                               :horizontal-scroll nil
                               :vertical-scroll nil
                               :external-min-width 50)
   
   (cur-individual-pane title-pane
                        :font *info-font*
                        :visible-border t
                        :background +state-bc+
                        :foreground +state-fc+
                        :horizontal-scroll nil
                        :vertical-scroll nil
                        :external-min-width (- *sirius-field-width* 50))


   
   (cur-no-selected-individuals-pane title-pane
                                     :font *info-font*
                                     :visible-border t
                                     :background +state-bc+
                                     :foreground +state-fc+
                                     :horizontal-scroll nil
                                     :vertical-scroll nil
                                     :external-min-width 50)
   
   (cur-concept-pane title-pane
                     :font *info-font*
                     :visible-border t
                     :background +state-bc+
                     :foreground +state-fc+
                     :horizontal-scroll nil
                     :vertical-scroll nil
                     :external-min-width (- *sirius-field-width* 50))
   
   (cur-no-selected-concepts-pane title-pane
                                  :font *info-font*
                                  :visible-border t
                                  :background +state-bc+
                                  :foreground +state-fc+
                                  :horizontal-scroll nil
                                  :vertical-scroll nil
                                  :external-min-width 50)

   (cur-namespace-pane title-pane
                       :font *info-font*
                       :visible-border t
                       :background +state-bc+
                       :foreground +state-fc+
                       :horizontal-scroll nil
                       :vertical-scroll nil
                       :visible-min-width *sirius-field-width*)

   (cur-profile-pane title-pane
                     :font *info-font*
                     :visible-border t
                     :background +state-bc+
                     :foreground +state-fc+
                     :horizontal-scroll nil
                     :vertical-scroll nil
                     :visible-min-width *sirius-field-width*)


   (cur-axiom-pane title-pane
                   :font *info-font*
                   :visible-border t
                   :background +state-bc+
                   :foreground +state-fc+
                   :horizontal-scroll nil
                   :vertical-scroll nil
                   :external-min-width (- *sirius-field-width* 50))

   (cur-no-selected-axioms-pane title-pane
                                :font *info-font*
                                :visible-border t
                                :background +state-bc+
                                :foreground +state-fc+
                                :horizontal-scroll nil
                                :vertical-scroll nil
                                :external-min-width 50)

   ;;;
   ;;;
   ;;; 

   (remove-urls-checkbox check-button
                         :font *checkbox-font*
                         :text "Simplify"
                         :callback-type :none
                         :selected (with-profile-access remove-urls)
                         :selection-callback 
                         #'(lambda ()
                             (with-profile-access
                               (setf remove-urls t)
                               (resync-state)
                               (case active-tab 
                                 (:role-hierarchy 
                                  (next-update-layout-role-hierarchy))
                                 (:abox-graph
                                  (next-update-layout-abox-graph))
                                 (:taxonomy
                                  (next-update-layout-taxonomy)))
                               
                               ;;; notwendiges Update
                               (when t
                                 (schedule-update))))

                         :retract-callback 
                         #'(lambda ()
                             (with-profile-access
                               (setf remove-urls nil)

                               (case active-tab 
                                 (:role-hierarchy 
                                  (next-update-layout-role-hierarchy))
                                 (:abox-graph
                                  (next-update-layout-abox-graph))
                                 (:taxonomy
                                  (next-update-layout-taxonomy)))

                               ;;; notwendiges Update
                               (when t
                                 (schedule-update))))

                         :background +state-bc+
                         :foreground +state-fc+)



   (maintain-arguments-checkbox check-button
                                :font *checkbox-font*
                                :text "Arg. Comp."
                                :callback-type :none
                                :selected (with-profile-access maintain-arguments)
                                :selection-callback 
                                #'(lambda ()
                                    (with-profile-access
                                      (setf maintain-arguments t)))                           

                                :retract-callback 
                                #'(lambda ()
                                    (with-profile-access
                                      (setf maintain-arguments nil)))

                                :background +state-bc+
                                :foreground +state-fc+)


   (autofocus-checkbox check-button
                       :font *checkbox-font*
                       :text "Auto Update"
                       :callback-type :none
                       :selected (with-profile-access autofocus)
                       :selection-callback 
                       #'(lambda ()
                           (with-profile-access
                             (setf autofocus t)
                             
                             (case active-tab 
                               (:role-hierarchy 
                                (next-update-layout-role-hierarchy))
                               (:abox-graph
                                (next-update-layout-abox-graph))
                               (:taxonomy
                                (next-update-layout-taxonomy)))
                             
                             (schedule-update)))

                       :retract-callback 
                       #'(lambda ()
                           (with-profile-access
                             (setf autofocus nil)

                             (case active-tab 
                               (:role-hierarchy 
                                (next-update-layout-role-hierarchy))
                               (:abox-graph
                                (next-update-layout-abox-graph))
                               (:taxonomy
                                (next-update-layout-taxonomy)))
                             
                             (schedule-update)))
                         
                       :background +state-bc+
                       :foreground +state-fc+)
   

   (freeze-graph-checkbox check-button
                          :font *checkbox-font*
                          :text "Freeze Graph"
                          :callback-type :none
                          :selected (with-profile-access autofocus)
                          :selection-callback 
                          #'(lambda ()
                              (with-profile-access
                                (setf freeze-graph t)
                                (case active-tab 
                                  (:role-hierarchy 
                                   (next-update-compute-and-layout-role-hierarchy))
                                  (:abox-graph
                                   (next-update-compute-and-layout-abox-graph))
                                  (:taxonomy
                                   (next-update-compute-and-layout-taxonomy)))

                                (schedule-update)))
                          
                          :retract-callback 
                          #'(lambda ()
                              (with-profile-access
                                (setf freeze-graph nil)
                                (case active-tab 
                                  (:role-hierarchy 
                                   (next-update-compute-and-layout-role-hierarchy))
                                  (:abox-graph
                                   (next-update-compute-and-layout-abox-graph))
                                  (:taxonomy
                                   (next-update-compute-and-layout-taxonomy)))
                                   
                                (schedule-update)))
                         
                          :background +state-bc+
                          :foreground +state-fc+)
   

   (selected-first-checkbox check-button
                            :font *checkbox-font*
                            :text "Sel. First"
                            :callback-type :none
                            :selected (with-profile-access selected-first)
                            :selection-callback 
                            #'(lambda ()
                                (with-profile-access
                                  (setf selected-first t)

                                  (case active-tab 
                                    (:role-hierarchy 
                                     (next-update-layout-role-hierarchy))
                                    (:abox-graph
                                     (next-update-layout-abox-graph))
                                    (:taxonomy
                                     (next-update-layout-taxonomy)))
                                  
                                  (when t
                                    (schedule-update))))
                           
                            :retract-callback 
                            #'(lambda ()
                                (with-profile-access
                                  (setf selected-first nil)

                                  (case active-tab 
                                    (:role-hierarchy 
                                     (next-update-layout-role-hierarchy))
                                    (:abox-graph
                                     (next-update-layout-abox-graph))
                                    (:taxonomy
                                     (next-update-layout-taxonomy)))
                                     
                                  (when t 
                                    (schedule-update))))

                            :background +state-bc+
                            :foreground +state-fc+)



   (selected-only-checkbox check-button
                           :font *checkbox-font*
                           :text "Sel. Only"
                           :callback-type :none
                           :selected (with-profile-access selected-only)
                           :selection-callback 
                           #'(lambda ()
                               (with-profile-access
                                 (setf selected-only t)
                                 (schedule-update)))
                           
                           :retract-callback 
                           #'(lambda ()
                               (with-profile-access
                                 (setf selected-only nil)
                                 (schedule-update)))

                           :background +state-bc+
                           :foreground +state-fc+)

   (search-item text-input-pane
                :title "Search & Select"
                :title-font *title-font*
                :font *shell-font*
                :title-position :left
                :background +state-bc+
                :foreground +state-fc+
                :horizontal-scroll nil
                :max-width '(:character 20)
                :callback-type :data
                :callback #'(lambda (data)
                              (with-profile-access
                                (with-update-display-status
                                  (case active-tab
                                    (:concepts 
                                     (let ((items
                                            (remove-if-not 
                                             #'(lambda (item)
                                                 (search data (line-item-printer item nil)
                                                         :test #'equalp))
                                             (tflatten
                                              (ensure-list
                                               (racer-function1 
                                                (all-atomic-concepts
                                                 sirius-current-tbox)))))))

                                       (cond (items
                                              (select-concepts items)
                                              (schedule-update))
                                             (t (beep-pane)))))
                                  
                                    (:taxonomy 
                                     (let ((items
                                            (remove-if-not 
                                             #'(lambda (item)
                                                 (search data (line-item-printer item nil)
                                                         :test #'equalp))
                                             (tflatten
                                              (ensure-list
                                               (racer-function1 
                                                (all-atomic-concepts
                                                 sirius-current-tbox)))))))

                                       (if (equal items '(:error))
                                           (beep-pane)
                                         (cond (items
                                                (select-concepts items)
                                                (when t
                                                  (schedule-update)))
                                               (t (beep-pane))))))
                                  
                                    (:roles
                                     (let ((items
                                            (remove-if-not 
                                             #'(lambda (item)
                                                 (search data (line-item-printer item nil)
                                                         :test #'equalp))
                                             (remove-if #'consp
                                                        (ensure-list
                                                         (racer-function1
                                                          (all-roles 
                                                           sirius-current-tbox)))))))
                                       (cond (items
                                              (select-roles items)
                                              (schedule-update))
                                             (t (beep-pane)))))

                                    (:role-hierarchy
                                     (let ((items
                                            (remove-if-not 
                                             #'(lambda (item)
                                                 (search data (line-item-printer item nil)
                                                         :test #'equalp))
                                             (remove-if #'consp
                                                        (ensure-list
                                                         (racer-function1
                                                          (all-roles 
                                                           sirius-current-tbox)))))))

                                       (if (eq items '(:error))
                                           (beep-pane)
                                         (cond (items
                                                (select-roles items)
                                                (when t
                                                  (schedule-update)))
                                               (t (beep-pane))))))
                                  
                                    (:individuals
                                     (let ((items
                                            (remove-if-not 
                                             #'(lambda (item)
                                                 (search data (line-item-printer item nil)
                                                         :test #'equalp))
                                             (ensure-list
                                              (racer-function1
                                               (all-individuals 
                                                sirius-current-abox))))))

                                       (cond (items
                                              (select-individuals items)
                                              (schedule-update))
                                             (t (beep-pane)))))

                                    (:query-input
                                     (let ((concept-items
                                            (remove-if-not 
                                             #'(lambda (item)
                                                 (search data (line-item-printer item nil)
                                                         :test #'equalp))
                                             (ensure-list
                                              (racer-function1
                                               (all-atomic-concepts 
                                                sirius-current-tbox)))))

                                           (ind-items
                                            (remove-if-not 
                                             #'(lambda (item)
                                                 (search data (line-item-printer item nil)
                                                         :test #'equalp))
                                             (ensure-list
                                              (racer-function1
                                               (all-individuals 
                                                sirius-current-abox))))))

                                       (if (or (equal concept-items '(:error))
                                               (equal ind-items '(:error)))
                                           (beep-pane)
                                         (cond ((or concept-items
                                                    ind-items)

                                                (select-concepts concept-items)
                                                (select-individuals ind-items)
                                            
                                                (when t
                                                  (schedule-update)))
                                           
                                               (t (beep-pane))))))
                                    
                                    (:abox-graph
                                     (let ((role-items
                                            (remove-if-not 
                                             #'(lambda (item)
                                                 (search data (line-item-printer item nil)
                                                         :test #'equalp))

                                             (remove-if #'consp
                                                        (ensure-list
                                                         (racer-function1
                                                          (all-roles 
                                                           sirius-current-tbox))))))

                                           (ind-items
                                            (remove-if-not 
                                             #'(lambda (item)
                                                 (search data (line-item-printer item nil)
                                                         :test #'equalp))
                                             (ensure-list
                                              (racer-function1
                                               (all-individuals 
                                                sirius-current-abox))))))

                                       (if (or (equal role-items '(:error))
                                               (equal ind-items '(:error)))
                                           (beep-pane)
                                         (cond ((or role-items
                                                    ind-items)

                                                (select-roles role-items)
                                                (select-individuals ind-items)
                                            
                                                (when t
                                                  (schedule-update)))
                                           
                                               (t (beep-pane))))))

                                    (:axioms
                                     (with-profile-access
                                       (dolist (ax (coerce (collection-items axioms-pane) 'list))
                                         (unless (selected-axiom? ax)
                                           (let ((item (ensure-list (third ax)))
                                                 (found nil))
                                             (block exit
                                               (maptree item
                                                        #'(lambda (x)
                                                            (when (search data 
                                                                          (ensure-string x)
                                                                          :test #'string-equal)
                                                              (setf found t)
                                                              (return-from exit nil)))))
                                             (when found 
                                               (select-axioms (list ax))))))

                                       (setf current-axiom nil)
                                       (schedule-update))))))))

   ;;;
   ;;; Navigator 
   ;;; 

   (fold-or-unfold-pane option-pane
                        :background +button-bc+
                        :foreground +button-fc+
                        :test-function #'item-equal
                        :items (list "Classic Layout" "No Status" "No Info" "Lean Layout")
                        :font *title-font*
                        :visible-min-width '(:character 15)
                        :visible-max-width '(:character 15)
                        :callback-type :data
                        :selection-callback
                        #'(lambda (data)
                            (with-profile-access
                              (cond ((string= data "Classic Layout")
                                     (setf show-info-pane t
                                           show-status-pane t))
                                    ((string= data "No Status")
                                     (setf show-info-pane t
                                           show-status-pane nil))
                                    ((string= data "No Info")
                                     (setf show-info-pane nil
                                           show-status-pane t))
                                    ((string= data "Lean Layout")
                                     (setf show-info-pane nil
                                           show-status-pane nil)))
                              (schedule-update))))


   (cur-history-title-pane title-pane
                           :title "History"
                           :title-font *title-font*
                           :background +state-bc+
                           :foreground +state-fc+
                           :horizontal-scroll nil
                           :vertical-scroll nil)

   (cur-history-pane title-pane
                     :visible-border t
                     :font *info-font*
                     :background +state-bc+
                     :foreground +state-fc+
                     :horizontal-scroll nil
                     :vertical-scroll nil
                     :visible-min-width *sirius-history-field-width*
                     :visible-max-width *sirius-history-field-width*)

   (focus-prev push-button :text "  <  "
               :background +button-bc+
               :foreground +button-fc+
               :enabled nil
               :font *button-font*
               :callback-type :none
               :callback #'focus-prev)

   (focus-next push-button :text "  >  "
               :background +button-bc+
               :foreground +button-fc+
               :enabled nil
               :font *button-font*
               :callback-type :none
               :callback #'focus-next)

   
   (focus-start push-button :text "  |<  "
                :font *button-font*
                :background +button-bc+
                :foreground +button-fc+
                :enabled nil
                :callback-type :none
                :callback #'focus-start)

   (focus-end push-button :text "  >|  "
              :font *button-font*
              :background +button-bc+
              :foreground +button-fc+
              :enabled nil
              :callback-type :none
              :callback #'focus-end)

   (focus-delete push-button :text "Delete"
                 :font *button-font*
                 :background +button-bc+
                 :foreground +button-fc+
                 :enabled nil
                 :callback-type :none
                 :callback #'delete-focus)
   
   (reset-sirius-state-button push-button :text "Delete All"
                              :font *button-font*
                              :background +button-bc+
                              :foreground +button-fc+
                              :enabled nil
                              :callback-type :none
                              :callback #'clear-history)
   
   (resync-sirius-state-button push-button :text "Recover"
                               :font *button-font*
                               :background +button-bc+
                               :foreground +button-fc+
                               :callback-type :none
                               :callback #'clear-and-resync)

   ;;;
   ;;; Taxonomy Tab
   ;;;


   (taxonomy-roots-panel radio-button-panel
                         :font *radiobox-font*
                         :visible-border t
                         :foreground +button-fc+
                         :background +button-bc+                     
                         :test-function #'item-equal
                         :items '("All Concepts" "Cur. Concept" "Sel. Concepts")
                         :callback-type :data
                         :selection-callback
                         #'(lambda (data)
                             (with-profile-access
                               (setf taxonomy-root-kind
                                     (cond ((string= data "All Concepts")
                                            :all)
                                           ((string= data "Cur. Concept")
                                            :current)
                                           ((string= data "Sel. Concepts")
                                            :selected)))
                                            
                               (clear-taxonomy-roots)
                           
                               (next-update-compute-and-layout-taxonomy)
                               
                               (schedule-update))))
   
   (taxonomy-orientation-panel radio-button-panel
                               :font *radiobox-font*
                               :visible-border t
                               :foreground +button-fc+
                               :background +button-bc+
                               :test-function #'item-equal
                               :items '("Hor." "Ver.")
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (with-profile-access
                                     (setf taxonomy-orientation
                                           (cond ((string= data "Hor.")
                                                  :left-right)
                                                 ((string= data "Ver.")
                                                  :top-down)
                                                 (t (error "bad string"))))
                                 
                                     (next-update-layout-taxonomy)
                                     
                                     (schedule-update))))


   (taxonomy-kind-panel radio-button-panel
                        :font *radiobox-font*
                        :visible-border t
                        :background +button-bc+
                        :foreground +button-fc+
                        :items '("Tree" "Graph")
                        :test-function #'item-equal
                        :callback-type :data
                        :selection-callback
                        #'(lambda (data)
                            (with-profile-access
                              (setf taxonomy-kind 
                                    (cond ((string= data "Tree")
                                           :tree)
                                          ((string= data "Graph")
                                           :graph)))
                           
                              (next-update-layout-taxonomy)
                               
                              (schedule-update))))


   (taxonomy-depth-panel option-pane
                         :background +button-bc+
                         :foreground +button-fc+
                         :items (cons 'unbounded (loop as i from 0 to 10 collect i))
                         :font *title-font*
                         :visible-min-width (get-visible-min-width "Unbounded")
                         :visible-max-width (get-visible-max-width "Unbounded")
                         :callback-type :data
                         :selection-callback
                         #'(lambda (data)
                             (with-profile-access
                               (setf taxonomy-max-depth data)

                               (next-update-compute-and-layout-taxonomy)
                               
                               (schedule-update))))


   (taxonomy-cancel-request-button push-button 
                                   :font *button-font*
                                   :text "Cancel Request"
                                   :enabled nil
                                   :background +button-bc+
                                   :foreground +button-fc+
                                   :callback-type :none
                                   :selection-callback
                                   #'(lambda ()
                                       (with-profile-access
                                         (when (typep taxonomy-graph 'pooled-process)

                                           (setf autofocus nil)
                                           (abort-process-please taxonomy-graph)

                                           #-:mac
                                           (please-wait-for-racer taxonomy-graph)

                                           (next-update-compute-and-layout-taxonomy)

                                           (reset-active-profile :cancel-request-p t)
                                           
                                           (sleep +wait-until-canceled-sleep-time+) 

                                           (schedule-update)))))


   (taxonomy-reset-button push-button 
                          :font *button-font*
                          :text "Reset Graph"
                          :background +button-bc+
                          :foreground +button-fc+
                          :callback-type :none
                          :selection-callback
                          #'(lambda ()
                              (next-update-compute-and-layout-taxonomy)
                                            
                              (schedule-update)))

   
   (taxonomy-request-button push-button 
                            :text "Request Graph"
                            :background +button-bc+
                            :foreground +button-fc+
                            :callback-type :none
                            :font *racer-button-font*
                            :selection-callback
                            #'request-taxonomy)

                                     
   (taxonomy-display-button push-button 
                            :font *button-font*
                            :text "Display Graph"
                            :background +button-bc+
                            :foreground +button-fc+
                            :callback-type :none
                            :selection-callback
                            #'(lambda ()
                                (with-profile-access
                                  (setf graph-drawing-aborted nil)
                                  (establish-taxonomy-pane))))

   (taxonomy-print-button push-button 
                          :font *button-font*
                          :text "Print Graph"
                          :background +button-bc+
                          :foreground +button-fc+
                          :callback-type :none
                          :selection-callback
                          #'(lambda ()
                              (with-suspended-server-pane-update
                                (with-profile-access
                                  (let ((*printing* t))
                                    (handler-case
                                        (simple-print-port taxonomy-graph-pane 
                                                           :scale :scale-to-fit
                                                           :interactive t)
                                      (error (error)
                                        (error-message "Sorry, I am unable to print.~%The error \"~A\" occured :-("
                                                         error))))))))
   
   ;;;
   ;;;
   ;;; 

   (show-bottom-checkbox check-button 
                         :font *checkbox-font*
                         :text "Show Bottom"
                         :callback-type :none
                         :selected (with-profile-access show-bottom)
                         :selection-callback 
                         #'(lambda ()
                             (with-profile-access
                               (setf show-bottom t)

                               (next-update-compute-and-layout-taxonomy)

                               (schedule-update)))

                         :retract-callback 
                         #'(lambda ()
                             (with-profile-access
                               (setf show-bottom nil)

                               (next-update-compute-and-layout-taxonomy)

                               (schedule-update))))

   
   (show-top-checkbox check-button 
                      :font *checkbox-font*
                      :text "Show Top"
                      :callback-type :none
                      :selected (with-profile-access show-top)
                      :selection-callback 
                      #'(lambda ()
                          (with-profile-access
                            (setf show-top t)

                            (next-update-compute-and-layout-taxonomy)
                            
                            (schedule-update)))

                      :retract-callback 
                      #'(lambda ()
                          (with-profile-access
                            (setf show-top nil)

                            (next-update-compute-and-layout-taxonomy)

                            (schedule-update))))

   ;;;
   ;;; Role Hierarchy Tab
   ;;;

   (role-roots-panel radio-button-panel
                     :font *radiobox-font*
                     :visible-border t
                     ;:title "Graph Root(s)"
                     ;:title-position :bottom
                     ;:title-adjust :center
                     :foreground +button-fc+
                     :background +button-bc+                     
                     :test-function #'item-equal
                     :items '("All Roles" "Cur. Role" "Sel. Roles")
                     :callback-type :data
                     :selection-callback
                     #'(lambda (data)
                         (with-profile-access
                           (setf role-hierarchy-root-kind
                                 (cond ((string= data "All Roles")
                                        :all)
                                       ((string= data "Cur. Role")
                                        :current)
                                       ((string= data "Sel. Roles")
                                        :selected)))

                           (clear-role-hierarchy-roots)
                                             
                           (case active-tab 
                             (:role-hierarchy 
                              (next-update-compute-and-layout-role-hierarchy))

                             (:abox-graph
                              (next-update-compute-and-layout-abox-graph)))
                           
                           (schedule-update))))


   (role-orientation-panel radio-button-panel
                           :font *radiobox-font*
                           :visible-border t
                           ;:title "Orientation"
                           ;:title-position :bottom
                           ;:title-adjust :center
                           :foreground +button-fc+
                           :background +button-bc+
                           :test-function #'item-equal
                           :items '("Hor." "Ver.")
                           :callback-type :data
                           :selection-callback
                           #'(lambda (data)
                               (with-profile-access
                                 (setf role-hierarchy-orientation
                                       (cond ((string= data "Hor.")
                                              :left-right)
                                             ((string= data "Ver.")
                                              :top-down)
                                             (t (error "bad string"))))
                                 
                                 (next-update-layout-role-hierarchy)
                                 
                                 (schedule-update))))


   (role-hierarchy-kind-panel radio-button-panel
                              :font *radiobox-font*
                              ;:title "Graph Type"
                              :visible-border t
                              ;:title-position :bottom
                              ;:title-adjust :center
                              :background +button-bc+
                              :foreground +button-fc+
                              :items '("Tree" "Graph")
                              :test-function #'item-equal
                              :callback-type :data
                              :selection-callback
                              #'(lambda (data)
                                  (with-profile-access
                                    (setf role-hierarchy-kind 
                                          (cond ((string= data "Tree")
                                                 :tree)
                                                ((string= data "Graph")
                                                 :graph)))
                           
                                    (next-update-layout-role-hierarchy)
                                    
                                    (schedule-update))))
   

   (role-hierarchy-depth-panel option-pane
                               ;:title "Tree Depth"
                               ;:title-position :bottom
                               ;:title-adjust :center
                               :background +button-bc+
                               :foreground +button-fc+
                               :items (cons 'unbounded (loop as i from 0 to 10 collect i))
                               :font *title-font*
                               :visible-min-width (get-visible-min-width "Unbounded")
                               :visible-max-width (get-visible-max-width "Unbounded")
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (with-profile-access

                                     (setf role-hierarchy-max-depth data)

                                     (next-update-compute-and-layout-role-hierarchy)
                              
                                     (schedule-update))))


   (role-hierarchy-cancel-request-button push-button 
                                         :font *button-font*
                                         :text "Cancel Request"
                                         :enabled nil
                                         :background +button-bc+
                                         :foreground +button-fc+
                                         :callback-type :none
                                         :selection-callback
                                         #'(lambda ()
                                             (with-profile-access
                                               (when (typep role-hierarchy-graph 'pooled-process)
                                                 
                                                 (setf autofocus nil)
                                                 (abort-process-please role-hierarchy-graph)

                                                 #-:mac
                                                 (please-wait-for-racer role-hierarchy-graph)

                                                 (next-update-compute-and-layout-role-hierarchy)

                                                 (reset-active-profile :cancel-request-p t)

                                                 (sleep +wait-until-canceled-sleep-time+) 
                                                 
                                                 (schedule-update)))))


   (role-hierarchy-reset-button push-button 
                                :font *button-font*
                                :text "Reset Graph"
                                :background +button-bc+
                                :foreground +button-fc+
                                :callback-type :none
                                :selection-callback
                                #'(lambda ()
                                    (with-profile-access
                                      (next-update-compute-and-layout-role-hierarchy)
                                            
                                      (schedule-update))))

   
   (role-hierarchy-request-button push-button 
                                  :text "Request Graph"
                                  :background +button-bc+
                                  :foreground +button-fc+
                                  :font *racer-button-font*
                                  :callback-type :none
                                  :selection-callback
                                  #'request-role-hierarchy)
                                     
   (role-hierarchy-display-button push-button 
                                  :font *button-font*
                                  :text "Display Graph"
                                  :background +button-bc+
                                  :foreground +button-fc+
                                  :callback-type :none
                                  :selection-callback
                                  #'(lambda ()
                                      (with-profile-access
                                        (setf graph-drawing-aborted nil)
                                        (establish-role-hierarchy-pane))))
                                      
   (role-hierarchy-print-button push-button 
                                :font *button-font*
                                :text "Print Graph"
                                :background +button-bc+
                                :foreground +button-fc+
                                :callback-type :none
                                :selection-callback
                                #'(lambda ()
                                    (with-profile-access
                                      (let ((*printing* t))
                                        (handler-case
                                            (simple-print-port role-hierarchy-graph-pane
                                                               :scale :scale-to-fit
                                                               :interactive t)
                                          (error (error)
                                            (error-message "Sorry, I am unable to print.~%The error \"~A\" occured :-("
                                                             error)))))))

   ;;;
   ;;; ABox Graph Tab
   ;;;

   (abox-graph-roots-panel radio-button-panel
                           :font *radiobox-font*
                           :visible-border t
                           :foreground +button-fc+
                           :background +button-bc+                     
                           :test-function #'item-equal
                           :items '("All Inds." "Cur. Ind." "Sel. Inds.")
                           :callback-type :data
                           :selection-callback
                           #'(lambda (data)
                               (with-profile-access
                           
                                 (setf abox-graph-root-kind
                                       (cond ((string= data "All Inds.")
                                              :all)
                                             ((string= data "Cur. Ind.")
                                              :current)
                                             ((string= data "Sel. Inds.")
                                              :selected)))

                                 (clear-abox-graph-roots)

                                 (next-update-compute-and-layout-abox-graph)
                               
                                 (schedule-update))))


   (abox-graph-orientation-panel radio-button-panel
                                 :font *radiobox-font*
                                 :visible-border t
                                 :foreground +button-fc+
                                 :background +button-bc+
                                 :test-function #'item-equal
                                 :items '("Hor." "Ver.")
                                 :callback-type :data
                                 :selection-callback
                                 #'(lambda (data)
                                     (with-profile-access
                                       (setf abox-graph-orientation
                                             (cond ((string= data "Hor.")
                                                    :left-right)
                                                   ((string= data "Ver.")
                                                    :top-down)
                                                   (t (error "bad string"))))
                                 
                                       (next-update-layout-abox-graph)
                                 
                                       (schedule-update))))


   (abox-graph-kind-panel radio-button-panel
                          :font *radiobox-font*
                          :visible-border t
                          :background +button-bc+
                          :foreground +button-fc+
                          :items '("Tree" "Graph")
                          :test-function #'item-equal
                          :callback-type :data
                          :selection-callback
                          #'(lambda (data)
                              (with-profile-access
                                (setf abox-graph-kind 
                                      (cond ((string= data "Tree")
                                             :tree)
                                            ((string= data "Graph")
                                             :graph)))
                           
                                (next-update-layout-abox-graph)
                                    
                                (schedule-update))))


   (abox-graph-depth-panel option-pane
                           :background +button-bc+
                           :foreground +button-fc+
                           :items (cons 'unbounded (loop as i from 0 to 10 collect i))
                           :font *title-font*
                           :visible-min-width (get-visible-min-width "Unbounded")
                           :visible-max-width (get-visible-max-width "Unbounded")
                           :callback-type :data
                           :selection-callback
                           #'(lambda (data)
                               (with-profile-access

                                 (setf abox-graph-max-depth data)
                              
                                 (next-update-compute-and-layout-abox-graph)
                              
                                 (schedule-update))))


   (abox-graph-cancel-request-button push-button 
                                     :font *button-font*
                                     :text "Cancel Request"
                                     :enabled nil
                                     :background +button-bc+
                                     :foreground +button-fc+
                                     :callback-type :none
                                     :selection-callback
                                     #'(lambda ()
                                         (with-profile-access
                                           (when (typep abox-graph 'pooled-process)

                                             (setf autofocus nil)
                                             (abort-process-please abox-graph)

                                             #-:mac
                                             (please-wait-for-racer abox-graph)

                                             (next-update-compute-and-layout-abox-graph)

                                             (reset-active-profile :cancel-request-p t)

                                             (sleep +wait-until-canceled-sleep-time+) 
                                             
                                             (schedule-update)))))


   (abox-graph-reset-button push-button 
                            :font *button-font*
                            :text "Reset Graph"
                            :background +button-bc+
                            :foreground +button-fc+
                            :callback-type :none
                            :selection-callback
                            #'(lambda ()
                                (with-profile-access

                                  (next-update-compute-and-layout-abox-graph)
                                            
                                  (schedule-update))))

   
   (abox-graph-request-button push-button 
                              :text "Request Graph"
                              :background +button-bc+
                              :foreground +button-fc+
                              :font *racer-button-font*
                              :callback-type :none
                              :selection-callback
                              #'request-abox-graph)
                                     
   (abox-graph-display-button push-button 
                              :font *button-font*
                              :text "Display Graph"
                              :background +button-bc+
                              :foreground +button-fc+
                              :callback-type :none
                              :selection-callback
                              #'(lambda ()
                                  (with-profile-access
                                    (setf graph-drawing-aborted nil)
                                    (establish-abox-graph-pane))))
   
   (abox-graph-print-button push-button 
                            :font *button-font*
                            :text "Print Graph"
                            :background +button-bc+
                            :foreground +button-fc+
                            :callback-type :none
                            :selection-callback
                            #'(lambda ()
                                (with-profile-access
                                  (let ((*printing* t))
                                    (handler-case
                                        (simple-print-port abox-graph-graph-pane
                                                           :scale :scale-to-fit
                                                           :interactive t)
                                      (error (error)
                                        (error-message "Sorry, I am unable to print.~%The error \"~A\" occured :-("
                                                         error)))))))
   
   ;;;
   ;;;
   ;;; 
 
   (include-transitive-roles-checkbox check-button 
                                      :font *checkbox-font*
                                      :text "Trans. Roles"
                                      :callback-type :none
                                      :selected (with-profile-access include-transitive-roles)
                                      :selection-callback 
                                      #'(lambda ()
                                          (with-profile-access
                                            (setf include-transitive-roles t)
                                            
                                            (next-update-compute-and-layout-abox-graph)
                                            
                                            (when t                                 
                                              (schedule-update))))

                                      :retract-callback 
                                      #'(lambda ()
                                          (with-profile-access
                                            (setf include-transitive-roles nil)

                                            (next-update-compute-and-layout-abox-graph)

                                            (schedule-update))))
  
   (only-selected-successors-checkbox check-button 
                                      :font *checkbox-font*
                                      :text "Only Sel. Succs"
                                      :callback-type :none
                                      :selected (with-profile-access only-selected-successors)
                                      :selection-callback 
                                      #'(lambda ()
                                          (with-profile-access
                                            (setf only-selected-successors t)
                                            
                                            (next-update-compute-and-layout-abox-graph)
                                            
                                            (when t                                 
                                              (schedule-update))))

                                      :retract-callback 
                                      #'(lambda ()
                                          (with-profile-access
                                            (setf only-selected-successors nil)

                                            (next-update-compute-and-layout-abox-graph)

                                            (schedule-update))))
  
   (show-top-role-checkbox check-button 
                      :font *checkbox-font*
                      :text "Show Top Role"
                      :callback-type :none
                      :selected (with-profile-access show-top-role)
                      :selection-callback 
                      #'(lambda ()
                          (with-profile-access
                            (setf show-top-role t)

                            (next-update-compute-and-layout-abox-graph)
                            
                            (schedule-update)))

                      :retract-callback 
                      #'(lambda ()
                          (with-profile-access
                            (setf show-top-role nil)

                            (next-update-compute-and-layout-abox-graph)

                            (schedule-update))))


 
   (told-only-checkbox check-button 
                       :font *checkbox-font*
                       :text "Told Only"
                       :callback-type :none
                       :selected (with-profile-access told-only)
                       :selection-callback 
                       #'(lambda ()
                           (with-profile-access
                             (setf told-only t)

                             (next-update-compute-and-layout-abox-graph)

                             (schedule-update)))

                       :retract-callback 
                       #'(lambda ()
                           (with-profile-access
                             (setf told-only nil)

                             (next-update-compute-and-layout-abox-graph)

                             (schedule-update))))
  
   (node-labels-checkbox check-button 
                         :font *checkbox-font*
                         :text "Labels"
                         :callback-type :none
                         :selected (with-profile-access node-labels)
                         :selection-callback 
                         #'(lambda ()
                             (with-profile-access
                               (setf node-labels t)

                               (next-update-compute-and-layout-abox-graph)

                               (schedule-update)))

                         :retract-callback 
                         #'(lambda ()
                             (with-profile-access
                               (setf node-labels nil)

                               (next-update-compute-and-layout-abox-graph)

                               (schedule-update))))
  
   (datatype-fillers-checkbox check-button 
                              :font *checkbox-font*
                              :text "Data Fillers"
                              :callback-type :none
                              :selected (with-profile-access datatype-fillers)
                              :selection-callback 
                              #'(lambda ()
                                  (with-profile-access
                                    (setf datatype-fillers t)

                                    (next-update-compute-and-layout-abox-graph)

                                    (schedule-update)))

                              :retract-callback 
                              #'(lambda ()
                                  (with-profile-access
                                    (setf datatype-fillers nil)

                                    (next-update-compute-and-layout-abox-graph)

                                    (schedule-update))))
  
   ;;;
   ;;; Buttons 
   ;;;

   (clear-button push-button :text "Clear Shell"
                 :font *button-font*
                 :background +button-bc+
                 :foreground +button-fc+
                 :callback-type :none
                 :callback #'(lambda () 
                               (clear-command)
                               (input-prompt t)))

   (clear-log-button push-button :text "Clear Log"
                     :font *button-font*
                     :background +button-bc+
                     :foreground +button-fc+
                     :callback-type :none
                     :callback #'clear-log)

   (save-shell-log-button push-button :text "Save Shell..."
                          :font *button-font*
                          :background +button-bc+
                          :foreground +button-fc+
                          :callback-type :none
                          :callback #'save-shell-log)

   (show-info-button push-button :text "Show Manual"
                     :font *button-font*
                     :background +button-bc+
                     :foreground +button-fc+
                     :callback-type :none
                     :callback #'show-info)


   (logo-show-info-button push-button :text "Show Manual"
                          :visible-min-width 400
                          :font *button-font*
                          :background +logo-bc+
                          :callback-type :none
                          :callback #'(lambda (&rest args)
                                        (declare (ignorable args))
                                        (with-profile-access
                                          (setf active-tab :shell)
                                          (switch-to-active-tab)
                                          (show-info))))

   (license-button push-button :text "License Details"
                   :font *button-font*
                   :background +button-bc+
                   :foreground +button-fc+
                   :callback-type :none
                   :callback #'show-license-details)

   (quit-button push-button :text "Quit"
                :font *button-font*
                :background +button-bc+
                :foreground +button-fc+
                :callback-type :none
                :callback #'quit-sirius-and-ask-for-shutdown-of-server-started-by-this-racerporter)
   

   (quit-and-shutdown-button push-button :text "Shutdown RacerPro & Quit"
                             :font *button-font*
                             :background +button-bc+
                             :foreground +button-fc+
                             :callback-type :none
                             :callback #'quit-sirius-and-shutdown)

   (full-reset-button push-button :text "Full Reset"
                      :background +button-bc+
                      :foreground +button-fc+
                      :font *racer-button-font*
                      :callback-type :none
                      :callback #'(lambda ()
                                    (with-profile-access
                                      (background-state-changing-result-command
                                       `(full-reset)
                                       (lambda (res)
                                         (declare (ignorable res))
                                        ;(clear-history)
                                         (with-promoted-lock
                                           (sirius-use-current-abox-tbox))
                                         (schedule-update))))))

   ;;;
   ;;;
   ;;; 

   (new-profile-button push-button :text "Copy Profile..."
                       :font *button-font*
                       :background +button-bc+
                       :foreground +button-fc+
                       :callback-type :none
                       :callback #'(lambda ()
                                     (with-suspended-server-pane-update
                                       (new-profile))
                                     (update-server-pane t)
                                     (schedule-update)))
                          
   (edit-profile-button push-button :text "Edit Profile..."
                        :font *button-font*
                        :background +button-bc+
                        :foreground +button-fc+
                        :callback-type :none
                        :callback #'(lambda ()
                                      (with-suspended-server-pane-update
                                        (edit-profile))
                                      (update-server-pane t)
                                      (schedule-update)))
   
   (delete-profile-button push-button :text "Delete Profile..."
                          :font *button-font*
                          :background +button-bc+
                          :foreground +button-fc+
                          :callback-type :none
                          :callback #'(lambda ()
                                        (with-suspended-server-pane-update
                                          (delete-profile))
                                        (update-server-pane t)
                                        (schedule-update)))


   ;;;
   ;;;
   ;;; 

   (connect-button push-button 
                   :text "Connect"
                   :background +button-bc+
                   :foreground +button-fc+
                   :font *racer-button-font*
                   :callback-type :none
                   :callback #'connect-or-disconnect)

   (start-server-button push-button :text "Start RacerPro"
                        :font *button-font*
                        :background +button-bc+
                        :foreground +button-fc+
                        :callback-type :none
                        :callback #'(lambda ()
                                      (start-server) 
                                      (when (connected-and-socket-alive-p)
                                        (input-prompt))))
     
   (kill-server-button push-button :text "Shutdown RacerPro"
                       :background +button-bc+
                       :foreground +button-fc+
                       :font *racer-button-font*
                       :callback-type :none
                       :callback #'(lambda ()
                                     (kill-server)
                                     (schedule-update)))
     
   (dump-server-button push-button :text "Store Image..."
                       :background +button-bc+
                       :foreground +button-fc+
                       :font *racer-button-font*
                       :callback-type :none
                       :callback #'(lambda ()
                                     (store-image)
                                     (schedule-update)))
   
   (reload-server-button push-button :text "Restore Image..."
                         :background +button-bc+
                         :foreground +button-fc+
                         :font *racer-button-font*
                         :callback-type :none
                         :callback #'(lambda ()
                                       (restore-image)
                                       (schedule-update)))
   
   #+:sirius-editor
   (open-kb-button push-button :text "Open in Editor..."
                   :font *button-font*
                   :background +button-bc+
                   :foreground +button-fc+
                   :callback-type :none
                   :callback #'open-kb)

   #+:sirius-editor
   (editor-button push-button :text "New Editor"
                  :font *button-font*
                  :background +button-bc+
                  :foreground +button-fc+
                  :callback-type :none
                  :callback #'sirius-editor)

   (load-kb-button push-button :text "Load..."
                   :font *racer-button-font*
                   :background +button-bc+
                   :foreground +button-fc+
                   :font *racer-button-font*
                   :callback-type :none
                   :callback #'load-kb)

   (abox-consistent-button push-button :text "Consistent?"
                           :background +button-bc+
                           :foreground +button-fc+
                           :font *racer-button-font*
                           :callback-type :none
                           :callback #'(lambda ()
                                         (with-profile-access
                                           (background-result-command
                                            `(abox-consistent? ,sirius-current-abox)))))


   (forget-abox-button push-button :text "Forget"
                       :background +button-bc+
                       :foreground +button-fc+
                         :font *racer-button-font*
                       :callback-type :none
                       :callback #'(lambda ()
                                     (with-profile-access
                                       (background-state-changing-result-command
                                        `(forget-abox ,sirius-current-abox)
                                        (lambda (res) 
                                          (declare (ignorable res))
                                          (with-promoted-lock
                                            (sirius-use-current-abox-tbox))
                                          (schedule-update))))))
                                 
   (realize-abox-button push-button :text "Realize"
                        :background +button-bc+
                        :foreground +button-fc+
                        :font *racer-button-font*
                        :callback-type :none 
                        :callback #'(lambda ()
                                      (with-profile-access
                                        (background-result-command
                                         `(realize-abox ,sirius-current-abox)
                                         (lambda (res) 
                                           (display-result-if-needed res))))))
                                          

   (tbox-coherent-button push-button :text "Coherent?"
                         :background +button-bc+
                         :foreground +button-fc+
                         :font *racer-button-font*
                         :callback-type :none
                         :callback #'(lambda ()
                                       (with-profile-access
                                         (background-result-command
                                          `(tbox-coherent? ,sirius-current-tbox)
                                          (lambda (res) 
                                            (display-result-if-needed res))))))

   (forget-tbox-button push-button :text "Forget"
                       :background +button-bc+
                       :foreground +button-fc+
                         :font *racer-button-font*
                       :callback-type :none
                       :callback #'(lambda ()
                                     (with-profile-access
                                       (background-state-changing-result-command
                                        `(forget-tbox ,sirius-current-tbox)
                                        (lambda (res) 
                                          (declare (ignorable res))
                                          (with-promoted-lock
                                            (sirius-use-current-abox-tbox))
                                          (schedule-update))))))

   (tbox-classify-button push-button :text "Classify"
                         :background +button-bc+
                         :foreground +button-fc+
                         :font *racer-button-font*
                         :callback-type :none
                         :callback #'(lambda ()
                                       (with-profile-access
                                         (background-result-command
                                          `(classify-tbox ,sirius-current-tbox)
                                          (lambda (res) 
                                            (display-result-if-needed res))))))


   (describe-concept-button push-button :text "Descr. Concept"
                            :background +button-bc+
                            :foreground +button-fc+
                            :font *racer-button-font*
                            :callback-type :none
                            :callback #'(lambda ()
                                          (with-profile-access
                                            (background-result-command
                                             `(describe-concept
                                               ,current-concept ,sirius-current-tbox)
                                             (lambda (res) 
                                               (display-result-if-needed res))))))


   (describe-role-button push-button :text "Descr. Role"
                         :background +button-bc+
                         :foreground +button-fc+
                         :font *racer-button-font*
                         :callback-type :none
                         :callback #'(lambda ()
                                       (with-profile-access
                                         (background-result-command
                                          `(describe-role
                                            ,current-role ,sirius-current-tbox)
                                          (lambda (res) 
                                            (display-result-if-needed res))))))


   (role-domain-button push-button :text "Domain"
                       :background +button-bc+
                       :foreground +button-fc+
                         :font *racer-button-font*
                       :callback-type :none
                       :callback #'(lambda ()
                                     (with-profile-access
                                       (background-result-command
                                        `(atomic-role-domain
                                          ,current-role ,sirius-current-tbox)
                                        (lambda (res) 
                                          (display-result-if-needed res))))))


   (role-range-button push-button :text "Range"
                      :background +button-bc+
                      :foreground +button-fc+
                         :font *racer-button-font*
                      :callback-type :none
                      :callback #'(lambda ()
                                    (with-profile-access
                                      (background-result-command
                                       `(atomic-role-range
                                         ,current-role ,sirius-current-tbox)
                                       (lambda (res) 
                                         (display-result-if-needed res))))))

   (role-synonyms-button push-button :text "Synonyms"
                         :background +button-bc+
                         :foreground +button-fc+
                         :font *racer-button-font*
                         :callback-type :none
                         :callback #'(lambda ()
                                       (with-profile-access
                                         (background-result-command
                                          `(atomic-role-synonyms
                                            ,current-role ,sirius-current-tbox)
                                          (lambda (res) 
                                            (display-result-if-needed res))))))

   (role-parents-button push-button :text "Parents"
                        :background +button-bc+
                        :foreground +button-fc+
                         :font *racer-button-font*
                        :callback-type :none
                        :callback #'(lambda ()
                                      (with-profile-access
                                        (background-result-command
                                         `(atomic-role-parents
                                           ,current-role ,sirius-current-tbox)
                                         (lambda (res) 
                                           (display-result-if-needed res))))))

   (role-children-button push-button :text "Children"
                         :background +button-bc+
                         :foreground +button-fc+
                         :font *racer-button-font*
                         :callback-type :none
                         :callback #'(lambda ()
                                       (with-profile-access
                                         (background-result-command
                                          `(atomic-role-children
                                            ,current-role ,sirius-current-tbox)
                                          (lambda (res) 
                                            (display-result-if-needed res))))))

   (describe-individual-button push-button :text "Descr. Ind."
                               :background +button-bc+
                               :foreground +button-fc+
                               :font *racer-button-font*
                               :callback-type :none
                               :callback #'(lambda ()
                                             (with-profile-access
                                               (background-result-command
                                                `(describe-individual1
                                                  ,current-individual
                                                  ,sirius-current-abox)
                                                (lambda (res) 
                                                  (display-result-if-needed res))))))

   ;;;
   ;;;
   ;;;

   (delete-assertions-button push-button :text "Delete Selected"
                             :font *racer-button-font*
                             :background +button-bc+
                             :foreground +button-fc+
                             :callback-type :none
                             :callback #'(lambda () 
                                           (with-profile-access
                                             (dolist (assertion (profile-all-selected-assertions))
                                               (let ((type (first assertion))
                                                     (assertion (second assertion)))

                                                 ;;; nicht enter-state-changing-result-command, wegen *display-error*-Bindung! 
                                                 ;;; schlecht in einer Loop, wenn Errors reported werden... 
                                                   
                                                 (enter-state-changing-result-command 
                                                    
                                                  (case type
                                                    ((:concept-assertions :annotation-concept-assertions)
                                                     `(forget-concept-assertion ,sirius-current-abox
                                                                                ,@assertion))

                                                    ((:role-assertions :annotation-role-assertions)
                                                     `(forget-role-assertion ,sirius-current-abox
                                                                             ,@(first assertion)
                                                                             ,(second assertion)))

                                                    (:attribute-assertions
                                                     `(forget-constrained-assertion ,sirius-current-abox
                                                                                    ,@(rest assertion)))

                                                    (:constraint-assertions
                                                     `(forget-constraint ,sirius-current-abox
                                                                         ,assertion))
                                                    (:different-from-assertions
                                                     `(forget-all-different-assertion ,sirius-current-abox
                                                                                      ,(rest assertion)))
                                                    (:same-as-assertions
                                                     `(forget-same-individual-as-assertion ,sirius-current-abox
                                                                                           ,@(rest assertion)))
                                                    (otherwise
                                                     `(current-abox))))))
                                                    
                                             (unselect-assertions (profile-all-selected-assertions))
                                               
                                             (update-assertions-pane))))


   (clear-assertion-selection-button push-button :text "Clear Sel. Assertions"
                                     :font *button-font*
                                     :background +button-bc+
                                     :foreground +button-fc+
                                     :callback-type :none
                                     :callback #'(lambda () 
                                                   (unselect-all-assertions)
                                                   (update-assertions-pane)))

   
   (delete-individuals-button push-button :text "Delete Selected"
                              :font *racer-button-font*
                              :background +button-bc+
                              :foreground +button-fc+
                              :callback-type :none
                              :callback #'(lambda () 
                                            (with-profile-access
                                              (dolist (ind (profile-all-selected-individuals))
                                                (enter-state-changing-result-command
                                                 `(forget-individual ,ind ,sirius-current-abox)))

                                              (unselect-individuals (profile-all-selected-individuals))
                                              
                                              (case active-tab
                                                ((:individuals 
                                                  :abox-graph)
                                                 (schedule-update))))))
                                                    
   ;;;
   ;;;
   ;;; 
                   
   (ontology-title-pane title-pane
                        :title "Ontology Container (*oo*)"
                        :title-font *title-font*
                        :background +state-bc+
                        :foreground +state-fc+
                        :horizontal-scroll nil
                        :vertical-scroll nil)

   (reasoner-title-pane title-pane
                        :title "Reasoner Container (*or*)"
                        :title-font *title-font*
                        :background +state-bc+
                        :foreground +state-fc+
                        :horizontal-scroll nil
                        :vertical-scroll nil)

   (cur-ontology-pane title-pane
                      :font *info-font*
                      :visible-border t
                      :background +state-bc+
                      :foreground +state-fc+
                      :horizontal-scroll nil
                      :vertical-scroll nil
                      :visible-min-width *sirius-field-width*)

   (cur-reasoner-pane title-pane
                      :font *info-font*
                      :visible-border t
                      :background +state-bc+
                      :foreground +state-fc+
                      :horizontal-scroll nil
                      :vertical-scroll nil
                      :visible-min-width *sirius-field-width*)

   ;;;
   ;;;
   ;;;

   (reasoner-selection-panel option-pane
                             :title "Reasoner Container"
                             :title-font *title-font*
                             :font *title-font*
                             :background +button-bc+
                             :foreground +button-fc+
                             :items nil 
                             :print-function #'line-item-printer
                             :visible-min-width '(:character 40)
                             :visible-max-width '(:character 40)
                             :callback-type :data
                             :selection-callback
                             #'(lambda (data)
                                 (with-profile-access
                                   (enter-state-changing-result-command 
                                    `(owlapi-set-current-reasoner ,data))
                                   (sirius-use-current-abox-tbox)
                                   (schedule-update))))
   
   (ontology-selection-panel option-pane
                             :title "Ontology Container"
                             :title-font *title-font*
                             :font *title-font*
                             :background +button-bc+
                             :foreground +button-fc+
                             :items nil 
                             :test-function #'(lambda (x y) 
                                                (equal (first (ensure-list x))
                                                       (first (ensure-list y))))
                             :print-function #'(lambda (x &optional stream)
                                                 (format stream "~A, ~A" 
                                                         (line-item-printer (first x) nil)
                                                         (second x)))
                             :visible-min-width '(:character 40)
                             :visible-max-width '(:character 40)
                             :callback-type :data
                             :selection-callback
                             #'(lambda (data)
                                 (with-profile-access
                                   (setf current-ontology (first data))
                                   (schedule-update))))

   (delete-axioms-button push-button :text "Dispose Axioms"
                         :font *racer-button-font*
                         :background +button-bc+
                         :foreground +button-fc+
                         :callback-type :none
                         :callback #'(lambda () 
                                       (with-profile-access
                                         (enter-state-changing-result-command 
                                          `(owlapi-dispose-axioms 
                                            ,(profile-all-selected-axioms)
                                            ,current-reasoner))
                                         
                                         (unselect-all-axioms)
                                         (schedule-update))))

   
   (new-axiom-button push-button :text "New Axiom..."
                     :font *racer-button-font*
                     :background +button-bc+
                     :foreground +button-fc+
                     :callback-type :none
                     :callback #'new-axiom)

   (select-axioms-button push-button :text "Select Axioms using *c*, *i*, *r*"
                         :font *button-font*
                         :background +button-bc+
                         :foreground +button-fc+
                         :callback-type :none
                         :callback #'(lambda () 
                                       (with-profile-access
                                         
                                         (let ((axioms nil))
                                           (dolist (ax (coerce (collection-items axioms-pane) 'list))
                                             (unless (selected-axiom? ax)
                                               (let ((item (ensure-list (third ax)))
                                                     (found nil))
                                                 (block exit
                                                   (maptree item
                                                            #'(lambda (x)
                                                                (when 
                                                                    (or (selected-concept? x)
                                                                        (selected-role? x)
                                                                        (selected-individual? x))
                                                                  (setf found t)
                                                                  (return-from exit nil)))))
                                                 (when found 
                                                   (push ax axioms)))))

                                           (select-axioms axioms))

                                         ;;;(setf current-axiom nil)
                                         (schedule-update))))


   (clear-axiom-selection-button push-button :text "Clear Sel. Axioms"
                                 :font *button-font*
                                 :background +button-bc+
                                 :foreground +button-fc+
                                 :callback-type :none
                                 :callback #'(lambda () 
                                               (unselect-all-axioms)
                                               (schedule-update)))
   
   (edit-axiom-button push-button :text "Edit Axiom..."
                      :font *racer-button-font*
                      :background +button-bc+
                      :foreground +button-fc+
                      :callback-type :none
                      :callback #'edit-current-axiom)

   
   (new-reasoner-button push-button :text "New Reasoner..."
                        :font *racer-button-font*
                        :background +button-bc+
                        :foreground +button-fc+
                        :callback-type :none
                        :callback #'(lambda () 
                                      (with-profile-access
                                        (let ((new (prompt-for-string "Enter Name of new Reasoner")))
                                          (when new
                                            (enter-state-changing-result-command
                                             `(owlapi-new-reasoner ,(intern new :racer-user)))
                                            
                                            (sirius-use-current-abox-tbox)
                                            (schedule-update))))))
   
   
   (save-reasoner-button push-button :text "Export All Axioms..."
                         :font *racer-button-font*
                         :background +button-bc+
                         :foreground +button-fc+
                         :callback-type :none
                         :callback #'(lambda () 
                                       (with-profile-access
                                         (let ((file
                                                (save-file-prompter 
                                                 (format nil "Store Ontologies in Reasoner Container ~A" 
                                                         current-reasoner)
                                                 "*.racer" nil (user-directory))))
                                           
                                           (when file     
                                             (background-result-command 
                                              `(owlapi-export-reasoner ,current-reasoner
                                                                       ,file)
                                              (lambda (res) 
                                                (display-result-if-needed res)
                                                (schedule-update))))))))


   (delete-reasoner-button push-button :text "Dispose Reasoner"
                           :font *racer-button-font*
                           :background +button-bc+
                           :foreground +button-fc+
                           :callback-type :none
                           :callback #'(lambda () 
                                         (with-profile-access
                                           (enter-state-changing-result-command
                                            `(owlapi-dispose-reasoner ,current-reasoner))
                                           
                                           (sirius-use-current-abox-tbox)
                                           (schedule-update))))

   (new-ontology-button push-button :text "New Ont..."
                        :font *racer-button-font*
                        :background +button-bc+
                        :foreground +button-fc+
                        :callback-type :none
                        :callback #'(lambda () 
                                      (with-profile-access
                                        (let ((new (prompt-for-string 
                                                    (format nil "Enter Name of new Ontology in ~A"
                                                            current-reasoner))))
                                          (when new
                                            (enter-state-changing-result-command
                                             `(owlapi-new-ontology ,(intern new :racer-user)
                                                                   ,current-reasoner))

                                            (sirius-use-current-abox-tbox)
                                            (schedule-update))))))


   (load-ontology-button push-button :text "Load Ont."
                         :font *racer-button-font*
                         :background +button-bc+
                         :foreground +button-fc+
                         :callback-type :none
                         :callback #'(lambda () 
                                       (with-profile-access
                                         (background-state-changing-result-command
                                          `(owlapi-load-ontology ,current-ontology ,current-reasoner)
                                          (lambda (res)
                                            (declare (ignorable res))
                                            (sirius-use-current-abox-tbox)
                                            (schedule-update))))))
   
   (unload-ontology-button push-button :text "Unload Ont."
                           :font *racer-button-font*
                           :background +button-bc+
                           :foreground +button-fc+
                           :callback-type :none
                           :callback #'(lambda () 
                                         (with-profile-access
                                           (background-state-changing-result-command
                                            `(owlapi-unload-ontology ,current-ontology ,current-reasoner)
                                            (lambda (res)
                                              (declare (ignorable res))
                                              (sirius-use-current-abox-tbox)
                                              (schedule-update))))))


   (load-axioms-button push-button :text "Load Axioms"
                       :font *racer-button-font*
                       :background +button-bc+
                       :foreground +button-fc+
                       :callback-type :none
                       :callback #'(lambda () 
                                     (with-profile-access
                                       (background-state-changing-result-command
                                        `(owlapi-load-axioms ,current-ontology 
                                                             ,(profile-all-selected-axioms))
                                        (lambda (res)
                                          (declare (ignorable res))
                                          (sirius-use-current-abox-tbox)
                                          (schedule-update))))))
   
   (unload-axioms-button push-button :text "Unload Axioms"
                         :font *racer-button-font*
                         :background +button-bc+
                         :foreground +button-fc+
                         :callback-type :none
                         :callback #'(lambda () 
                                       (with-profile-access
                                         (background-state-changing-result-command
                                          `(owlapi-unload-axioms ,current-ontology 
                                                                 ,(profile-all-selected-axioms))
                                          (lambda (res)
                                            (declare (ignorable res))
                                            (sirius-use-current-abox-tbox)
                                            (schedule-update))))))


   (save-ontology-button push-button :text "Export Ont. Axioms..."
                         :font *racer-button-font*
                         :background +button-bc+
                         :foreground +button-fc+
                         :callback-type :none
                         :callback #'(lambda () 
                                       (with-profile-access
                                         (let* ((file
                                                 (save-file-prompter 
                                                  (format nil "Store Ontology Container ~A" 
                                                          current-reasoner)
                                                  "*.*" nil (user-directory))))


                                           (when file

                                             (let* ((type1
                                                     (pathname-type (pathname file)))

                                                    (type
                                                     (cond ((member type1
                                                                    '("owl" "rdf" "rdfs")
                                                                    :test #'equal)
                                                            :owl)
                                                           ((member type1
                                                                    '("owx" "xml")
                                                                    :test #'equal)
                                                            :owx)
                                                           ((member type1
                                                                    '("owf" "ofn" "funct" "fun" "functional")
                                                                    :test #'equal)
                                                            :ofn)
                                                           ((member type1
                                                                    '("racer" "lisp")
                                                                    :test #'equal)
                                                            :racer))))

                                               (if (not type)
                                                   
                                                   (error-message "Unknown Ontology Format '.~A'. 
But I know how to save '.racer', '.owl', '.ofn', and '.owx'!" type1)

                                                 (case type 

                                                   (:racer
                                                    (background-state-changing-result-command
                                                     `(owlapi-export-ontology ,current-ontology
                                                                              ,file
                                                                              :reasoner 
                                                                              ,current-reasoner)
                                                     (lambda (res)
                                                       (display-result-if-needed res)
                                                       (schedule-update))))

                                                  (otherwise 
                                                 
                                                   (background-state-changing-result-command
                                                    `(owlapi-save-ontology ,current-ontology
                                                                           ,file
                                                                            :reasoner 
                                                                            ,current-reasoner
                                                                            :syntax ,(to-keyword type))
                                                     (lambda (res)
                                                       (display-result-if-needed res)
                                                       (schedule-update))))))))))))

   (delete-ontology-button push-button :text "Dispose Ont."
                           :font *racer-button-font*
                           :background +button-bc+
                           :foreground +button-fc+
                           :callback-type :none
                           :callback #'(lambda () 
                                         (with-profile-access
                                           (background-state-changing-result-command
                                            `(owlapi-dispose-ontology ,current-ontology
                                                                      ,current-reasoner 
                                                                      t)
                                            
                                            (lambda (res) 
                                              (declare (ignorable res))
                                              (sirius-use-current-abox-tbox)
                                              (schedule-update))))))

   ;;;
   ;;;
   ;;; 

   (select-all-concepts-button push-button :text "Select All"
                               :font *button-font*
                               :background +button-bc+
                               :foreground +button-fc+
                               :callback-type :none
                               :callback #'(lambda ()
                                             (with-profile-access
                                               (select-concepts
                                                (ensure-list
                                                 (racer-function1 
                                                  (all-atomic-concepts
                                                   sirius-current-tbox)))))
                                             (schedule-update)))   

   (select-all-roles-button push-button :text "Select All"
                            :font *button-font*
                            :background +button-bc+
                            :foreground +button-fc+
                            :callback-type :none
                            :callback #'(lambda ()
                                          (with-profile-access
                                            (select-roles
                                             (ensure-list
                                              (racer-function1 
                                               (all-roles
                                                sirius-current-tbox)))))
                                          (schedule-update)))   

   (select-all-individuals-button push-button :text "Select All"
                                  :font *button-font*
                                  :background +button-bc+
                                  :foreground +button-fc+
                                  :callback-type :none
                                  :callback #'(lambda ()
                                                (with-profile-access
                                                  (select-individuals
                                                   (ensure-list
                                                    (racer-function1 
                                                     (all-individuals
                                                      sirius-current-abox)))))
                                                (schedule-update)))

   (select-all-assertions-button push-button :text "Select All"
                                 :font *button-font*
                                 :background +button-bc+
                                 :foreground +button-fc+
                                 :callback-type :none
                                 :callback #'(lambda ()
                                               (with-profile-access
                                                 (select-assertions
                                                  (mapcar #'(lambda (x) 
                                                              (list assertion-list-mode x))
                                                          (get-all-assertions))))
                                               (schedule-update)))

   ;;;
   ;;;
   ;;;

   (clear-concept-selection-button push-button :text "Clear Sel. Concepts"
                                   :font *button-font*
                                   :background +button-bc+
                                   :foreground +button-fc+
                                   :callback-type :none
                                   :callback #'(lambda ()
                                                 (unselect-all-concepts)
                                                 (schedule-update)))
   
   (select-parent-concepts-button push-button :text "Select Parents"
                                  :font *button-font*
                                  :background +button-bc+
                                  :foreground +button-fc+
                                  :callback-type :none
                                  :callback #'(lambda ()
                                                (select-parents)
                                                (schedule-update)))

   (select-children-concepts-button push-button :text "Select Children"
                                    :font *button-font*
                                    :background +button-bc+
                                    :foreground +button-fc+
                                    :callback-type :none
                                    :callback #'(lambda ()
                                                  (select-children)
                                                  (schedule-update)))

   (clear-role-selection-button push-button :text "Clear Sel. Roles"
                                :font *button-font*
                                :background +button-bc+
                                :foreground +button-fc+
                                :callback-type :none
                                :callback #'(lambda ()
                                              (unselect-all-roles)
                                              (schedule-update)))

   (clear-individual-selection-button push-button :text "Clear Sel. Inds."
                                      :font *button-font*
                                      :background +button-bc+
                                      :foreground +button-fc+
                                      :callback-type :none
                                      :callback #'(lambda ()
                                                    (unselect-all-individuals)
                                                    (schedule-update)))
   ;;;
   ;;;
   ;;;
   
   (assign-selected-concepts-from-result-button push-button :text "Sel. Concepts := Last Result"
                                                :font *button-font*
                                                :background +button-bc+
                                                :foreground +button-fc+
                                                :callback-type :none
                                                :callback #'(lambda ()
                                                              (with-profile-access
                                                                (select-concepts
                                                                 (remove-if-not
                                                                  #'(lambda (x)
                                                                      (racer-function 
                                                                       (concept? x sirius-current-tbox)))
                                                                  (tflatten sirius-star)))
                                                                (schedule-update))))

   (assign-selected-roles-from-result-button push-button :text "Sel. Roles := Last Result"
                                             :font *button-font*
                                             :background +button-bc+
                                             :foreground +button-fc+
                                             :callback-type :none
                                             :callback #'(lambda ()
                                                           (with-profile-access
                                                             (select-roles
                                                              (remove-if-not
                                                               #'(lambda (x)
                                                                   (racer-function 
                                                                    (role? x sirius-current-tbox)))
                                                               (tflatten sirius-star)))
                                                             (schedule-update))))

   (assign-selected-individuals-from-result-button push-button :text "Sel. Individuals := Last Result"
                                                   :font *button-font*
                                                   :background +button-bc+
                                                   :foreground +button-fc+
                                                   :callback-type :none
                                                   :callback #'(lambda ()
                                                                 (with-profile-access
                                                                   (select-individuals
                                                                    (remove-if-not
                                                                     #'(lambda (x)
                                                                         (racer-function 
                                                                          (individual? x sirius-current-abox)))
                                                                     (tflatten sirius-star)))
                                                                   (schedule-update))))

   ;;;
   ;;;
   ;;;

   (individual-synonyms-button push-button :text "Synonyms"
                               :background +button-bc+
                               :foreground +button-fc+
                               :font *racer-button-font*
                               :callback-type :none
                               :callback #'(lambda ()
                                             (with-profile-access
                                               (background-result-command 
                                                `(individual-synonyms
                                                  ,current-individual
                                                  ,sirius-current-abox)
                                                (lambda (res)
                                                  (display-result-if-needed res)
                                                  (select-individuals
                                                   (apply #'append
                                                          (mapcar #'ensure-list res))))))))

   (individual-antonyms-button push-button :text "Antonyms"
                               :background +button-bc+
                               :foreground +button-fc+
                               :font *racer-button-font*
                               :callback-type :none
                               :callback #'(lambda ()
                                             (with-profile-access
                                               (background-result-command 
                                                `(individual-antonyms
                                                  ,current-individual
                                                  ,sirius-current-abox)
                                                (lambda (res)
                                                  (display-result-if-needed res)
                                                  (select-individuals
                                                   (apply #'append
                                                          (mapcar #'ensure-list res))))))))


   (direct-types-button push-button :text "Direct Types"
                        :background +button-bc+
                        :foreground +button-fc+
                        :font *racer-button-font*
                        :callback-type :none
                        :callback #'(lambda ()
                                      (with-profile-access
                                        (background-result-command 
                                         `(individual-direct-types 
                                           ,current-individual
                                           ,sirius-current-abox)
                                         (lambda (res)
                                           (display-result-if-needed res)
                                           (select-concepts
                                            (apply #'append
                                                   (mapcar #'ensure-list res))))))))

   (all-types-button push-button :text "All Types"
                     :background +button-bc+
                     :foreground +button-fc+
                     :font *racer-button-font*
                     :callback-type :none
                     :callback #'(lambda ()
                                   (with-profile-access
                                     (background-result-command
                                      `(individual-types ,current-individual
                                                         ,sirius-current-abox)
                                      (lambda (res) 
                                        (display-result-if-needed res)
                                        (select-concepts 
                                         (apply #'append
                                                (mapcar #'ensure-list res))))))))
                     
   (change-current-tbox-button push-button :text "Set Server TBox"
                               :background +button-bc+
                               :foreground +button-fc+
                               :font *racer-button-font*
                               :callback-type :none
                               :callback #'(lambda ()
                                             (with-profile-access
                                               (enter-state-changing-result-command
                                                `(set-current-tbox ,sirius-current-tbox))
                                               (schedule-update))))

   (describe-tbox-button push-button :text "Describe"
                         :background +button-bc+
                         :foreground +button-fc+
                         :callback-type :none
                         :font *racer-button-font*
                         :callback #'(lambda ()
                                       (with-profile-access
                                         (background-result-command
                                          `(describe-tbox ,sirius-current-tbox))
                                         (lambda (res) 
                                           (display-result-if-needed res)))))
                                      

   (change-current-abox-button push-button :text "Set Server ABox"
                               :background +button-bc+
                               :foreground +button-fc+
                               :font *racer-button-font*
                               :callback-type :none
                               :callback #'(lambda () 
                                             (with-profile-access
                                               (enter-state-changing-result-command
                                                `(set-current-abox ,sirius-current-abox))
                                               (schedule-update))))

   (describe-abox-button push-button :text "Describe"
                         :background +button-bc+
                         :foreground +button-fc+
                         :font *racer-button-font*
                         :callback-type :none
                         :callback #'(lambda ()
                                       (with-profile-access
                                         (background-result-command
                                          `(describe-abox ,sirius-current-abox)
                                          (lambda (res) 
                                            (display-result-if-needed res))))))
 
   (instance-query-button push-button :text "Concept Query"
                          :background +button-bc+
                          :foreground +button-fc+
                          :font *racer-button-font*
                          :callback-type :none
                          :callback
                          #'(lambda ()
                              (with-profile-access
                                (background-result-command 
                                 `(retrieve (racer-user::?x)
                                            (racer-user::?x ,current-concept)
                                            :abox ,sirius-current-abox)
                                 (lambda (res) 
                                   (display-result-if-needed res) 
                                   (select-individuals (mapcar #'cadar res)))))))

   (concept-synonyms-button push-button :text "Synonyms"
                               :background +button-bc+
                               :foreground +button-fc+
                               :font *racer-button-font*
                            :callback-type :none
                            :callback #'(lambda ()
                                          (with-profile-access
                                            (background-result-command 
                                             `(atomic-concept-synonyms
                                               ,current-concept
                                               ,sirius-current-tbox)
                                             (lambda (res)
                                               (display-result-if-needed res)
                                               (select-concepts
                                                (apply #'append
                                                       (mapcar #'ensure-list res))))))))
   
   ;;;
   ;;;
   ;;;

   #+:sonic 
   (sonic-lcs push-button :text "LCS"
              :background +button-bc+
              :foreground +button-fc+
              :font *racer-button-font*
              :callback-type :none
              :callback #'(lambda ()
                            (with-profile-access
                              (background-result-command 
                               `(sonic-lcs
                                 ,(profile-all-selected-concepts))
                               (lambda (res) 
                                 (new-concept-definition-editor res)
                                 (schedule-update))))))
   
   #+:sonic 
   (sonic-scs push-button :text "SCS"
              :background +button-bc+
              :foreground +button-fc+
              :font *racer-button-font*
              :callback-type :none
              :callback #'(lambda ()
                            (with-profile-access
                              (background-result-command 
                               `(sonic-scs
                                 ,(profile-all-selected-concepts))
                               (lambda (res) 
                                 (new-concept-definition-editor res)
                                 (schedule-update))))))

      
   #+:sonic 
   (sonic-approx push-button :text "Approximate"
                 :background +button-bc+
                 :foreground +button-fc+
                 :font *racer-button-font*
                 :callback-type :none
                 :callback #'(lambda ()
                               (with-profile-access
                                 (background-result-command
                                  `(sonic-approximate
                                    ,current-concept)
                                  (lambda (res)
                                    (new-concept-definition-editor res)
                                    (schedule-update))))))

   #+:sonic 
   (sonic-msc push-button :text "MSC"
              :background +button-bc+
              :foreground +button-fc+
              :font *racer-button-font*
              :callback-type :none
              :callback #'(lambda ()
                            (with-profile-access
                              (background-result-command 
                               `(sonic-msc
                                 ,current-individual)
                               (lambda (res) 
                                 (new-concept-definition-editor res)
                                 (schedule-update))))))

   #+:sonic 
   (sonic-generalize push-button :text "Generalize"
                     :background +button-bc+
                     :foreground +button-fc+
                     :font *racer-button-font*
                     :callback-type :none
                     :callback #'(lambda ()
                                   (with-profile-access
                                     (background-result-command 
                                      `(sonic-generalize
                                        ,(profile-all-selected-individuals))
                                      (lambda (res) 
                                        (new-concept-definition-editor res)
                                        (schedule-update))))))
   
   ;;;
   ;;;
   ;;;

   #-:sonic
   (msc-k-button push-button :text "MSC k..."
                 :background +button-bc+
                 :foreground +button-fc+
                 :font *racer-button-font*
                 :callback-type :none
                 :callback #'(lambda ()
                               (with-profile-access
                                 (let ((k 
                                        (prompt-for-integer "Enter k Value")))
                                   (when (and (integerp k)
                                              (plusp k))
                                     (let ((name 
                                            (intern
                                             (prompt-for-string "Enter Concept Name")
                                             :racer-user)))
                                       (when name
                                         (background-state-changing-result-command
                                          `(msc-k 
                                            ,current-individual
                                            ,k
                                            :abox ,sirius-current-abox
                                            :include-direct-types t
                                            :name ,name)
                                          (lambda (res)
                                            (declare (ignorable res))
                                            (schedule-update))))))))))


   ;;;
   ;;; Query Commands
   ;;;

   (describe-query-button push-button :text "Describe Rewritten"
                          :background +button-bc+
                          :foreground +button-fc+
                          :font *racer-button-font*
                          :callback-type :none
                          :callback #'(lambda ()
                                        (with-profile-access
                                          (query-or-rule current-query-or-rule
                                                         describe-query
                                                         describe-rule 
                                                         (progn)
                                                         nil))))
   
   (describe-original-query-button push-button :text "Describe Original"
                                   :background +button-bc+
                                   :foreground +button-fc+
                                   :font *racer-button-font*
                                   :callback-type :none
                                   :callback #'(lambda ()
                                                 (with-profile-access
                                                   (query-or-rule2 current-query-or-rule
                                                                   describe-query
                                                                   describe-rule
                                                                   (progn)
                                                                   nil))))

   (delete-query-button push-button :text "Delete"
                        :background +button-bc+
                        :foreground +button-fc+
                        :font *racer-button-font*
                        :callback-type :none
                        :callback #'(lambda ()
                                      (with-profile-access
                                        (query-or-rule current-query-or-rule
                                                       delete-query
                                                       delete-rule 
                                                       (progn
                                                         (setf current-query-or-rule nil)
                                                         (schedule-update))
                                                       nil))))
                                        

   (abort-query-button push-button :text "Abort"
                       :background +button-bc+
                       :foreground +button-fc+
                       :font *racer-button-font*
                       :callback-type :none
                       :callback #'(lambda ()
                                     (with-profile-access
                                       (query-or-rule current-query-or-rule
                                                      abort-query
                                                      abort-rule 
                                                      (progn)
                                                      nil))))

   (answer-query-button push-button :text "Get Answer"
                        :background +button-bc+
                        :foreground +button-fc+
                        :font *racer-button-font*
                        :callback-type :none
                        :callback #'(lambda ()
                                      (with-profile-access
                                        (with-profile-access
                                          (query-or-rule current-query-or-rule
                                                         get-answer
                                                         get-answer
                                                         (progn)
                                                         nil)))))

   (execute-query-button push-button :text "Execute"
                         :background +button-bc+
                         :foreground +button-fc+
                         :font *racer-button-font*
                         :callback-type :none
                         :callback #'(lambda ()
                                       (with-profile-access
                                         (query-or-rule current-query-or-rule
                                                        execute-query
                                                        execute-rule
                                                        (progn)
                                                        nil))))

   (reexecute-query-button push-button :text "Re-Execute"
                           :background +button-bc+
                           :foreground +button-fc+
                           :font *racer-button-font*
                           :callback-type :none
                           :callback #'(lambda ()
                                         (with-profile-access
                                           (query-or-rule current-query-or-rule
                                                          reexecute-query
                                                          reexecute-rule
                                                          (progn)
                                                          nil))))

   (reprepare-query-button push-button :text "Re-Prepare"
                           :background +button-bc+
                           :foreground +button-fc+
                           :font *racer-button-font*
                           :callback-type :none
                           :callback #'(lambda ()
                                         (with-profile-access
                                           (query-or-rule current-query-or-rule
                                                          reprepare-query
                                                          reprepare-rule
                                                          (progn)
                                                          nil))))

   (get-next-tuple-button push-button :text "Next Tuple"
                          :background +button-bc+
                          :foreground +button-fc+
                          :font *racer-button-font*
                          :callback-type :none
                          :callback #'(lambda ()
                                        (with-profile-access
                                          (query-or-rule current-query-or-rule
                                                         get-next-tuple
                                                         get-next-set-of-rule-consequences
                                                         (progn)
                                                         nil))))
   
   (nrql-status-button push-button :text "Status"
                       :background +button-bc+
                       :foreground +button-fc+
                       :font *racer-button-font*
                       :callback-type :none
                       :callback #'(lambda ()
                                     (with-profile-access
                                       (background-result-command-nc
                                        `(describe-query-processing-mode)
                                        (lambda (res)
                                          (display-result-if-needed res))))))

   ;;;
   ;;;
   ;;; 

   (delete-definition-button push-button :text "Undefine"
                             :background +button-bc+
                             :foreground +button-fc+
                             :font *racer-button-font*
                             :callback-type :none
                             :callback #'(lambda ()
                                           (with-profile-access
                                             (enter-state-changing-result-command
                                              `(undefquery ,(first current-definition)
                                                           :arity ,(length (second current-definition))
                                                           :tbox ,sirius-current-tbox))
                                             (setf current-definition nil)
                                             (schedule-update))))


   ;;;
   ;;; Tab Pane 
   ;;; 

   (tab-pane tab-layout
             :font *tabs-font*

             :visible-child-function nil

             :description
             (list server-tab
 
                   shell-tab 

                   tbox-tab
                   abox-tab
                                
                   concepts-tab
                   roles-tab
                   individuals-tab
                   assertions-tab
                   axioms-tab

                   taxonomy-tab
                   role-hierarchy-tab
                   abox-graph-tab
                                
                   query-input-tab
                   queries-tab
                   defined-queries-tab
                                
                   log-tab
                                
                   #-:mac logo-tab)

             :test-function #'equalp 

             :items
             (list (list *tab-title-profiles* server-tab)
                   
                   (list *tab-title-shell* shell-tab)
                   
                   (list *tab-title-tboxes* tbox-tab)
                   (list *tab-title-aboxes* abox-tab)
                   
                   (list *tab-title-concepts* concepts-tab)
                   (list *tab-title-roles* roles-tab)
                   (list *tab-title-individuals* individuals-tab)
                   (list *tab-title-assertions* assertions-tab)
                   (list *tab-title-owlapi* axioms-tab)
                   
                   (list *tab-title-taxonomy* taxonomy-tab)
                   (list *tab-title-role-hierarchy* role-hierarchy-tab)
                   (list *tab-title-abox-graph* abox-graph-tab)
                   
                   (list *tab-title-query-io* query-input-tab)
                   (list *tab-title-queries* queries-tab)
                   (list *tab-title-def-queries* defined-queries-tab)
                   
                   (list *tab-title-log* log-tab)
                   
                   #-:mac (list *tab-title-about* logo-tab))
             
             :background +sirius-bc+ 
             :foreground +sirius-fc+ 
             
             :callback-type :data
             
             :selection-callback #'(lambda (data)
                                     (with-profile-access
                                       (cond ((eq (second data) server-tab)
                                              (setf active-tab :server))

                                             ((eq (second data) abox-tab)
                                              (setf active-tab :abox))

                                             ((eq (second data) tbox-tab)
                                              (setf active-tab :tbox))
                                           
                                             ((eq (second data) assertions-tab)
                                              (setf active-tab :assertions))
                                           
                                             ((eq (second data) axioms-tab)
                                              (setf active-tab :axioms))
                                           
                                             ((eq (second data) taxonomy-tab)
                                              (setf active-tab :taxonomy))
                                           
                                             ((eq (second data) role-hierarchy-tab)
                                              (setf active-tab :role-hierarchy))
                                           
                                             ((eq (second data) abox-graph-tab)
                                              (setf active-tab :abox-graph))
                                           
                                             ((eq (second data) individuals-tab)
                                              (setf active-tab :individuals))
                                             
                                             ((eq (second data) query-input-tab)
                                              (setf active-tab :query-input))
                                             
                                             ((eq (second data) queries-tab)
                                              (setf active-tab :queries))

                                             ((eq (second data) defined-queries-tab)
                                              (setf active-tab :defined-queries))
                                           
                                             ((eq (second data) shell-tab)
                                              (setf active-tab :shell))

                                             ((eq (second data) concepts-tab)
                                              (setf active-tab :concepts))
                                           
                                             ((eq (second data) roles-tab)
                                              (setf active-tab :roles))
                                           
                                             ((eq (second data) logo-tab)
                                              (setf active-tab :logo))
                                           
                                             ((eq (second data) log-tab)
                                              (setf active-tab :log))

                                             )
                                       
                                       (schedule-update)
                                       ))

             :print-function 'car))

  (:layouts
   
   (main-layout column-layout 
                '(tab-pane)
                :horizontal-scroll *horizontal-scroll*
                :vertical-scroll *vertical-scroll*
                :background +sirius-bc+ 
                :foreground +sirius-fc+ 
                :adjust :center)

   ;;;
   ;;;
   ;;; 
   
   #+:ignore
   (lambda-args-pane 
    row-layout 
    ;;;'(lambda-list-pane lambda-info-button complete-button)
    '(modeline lambda-info-button complete-button)
    :adjust :center
    :gap 20)

   ;;;
   ;;;
   ;;; 

   (server-tab column-layout 
               '(state-pane1
                 navi-pane
                 
                 server-pane
                 server-buttons1
                 server-buttons2
                 info-pane)
               :adjust :center)

   (shell-tab column-layout 
              '(state-pane1
                navi-pane
                
                command-pane
                modeline
                ;;lambda-args-pane
                shell-buttons1
                shell-buttons2
                shell-buttons3)
              :adjust :center)

   (tbox-tab column-layout 
             '(state-pane1
               navi-pane
               
               tbox-pane
               tbox-buttons1
               info-pane)
             :adjust :center)

   (abox-tab column-layout 
             '(state-pane1
               navi-pane
               
               abox-pane
               abox-buttons1
               info-pane)
             :adjust :center)


   (concepts-tab column-layout 
                 '(state-pane1
                   navi-pane
                   
                   concepts-pane
                   concepts-buttons1
                   concepts-buttons2
                   info-pane)
                 :adjust :center)

   (roles-tab column-layout 
              '(state-pane1
                navi-pane
                
                roles-pane
                roles-buttons1
                roles-buttons2
                info-pane)
              :adjust :center)

   (individuals-tab column-layout 
                    '(state-pane1
                      navi-pane
                      
                      individuals-pane
                      individuals-buttons1
                      info-pane)
                    :adjust :center)

   (assertions-tab column-layout 
                   '(state-pane1
                     navi-pane
                     
                     assertions-pane
                     assertions-buttons1
                     assertions-buttons3
                     assertions-buttons2
                     info-pane)
                   :adjust :center)

   (axioms-tab column-layout 
               '(state-pane1
                 navi-pane
                 
                 axioms-pane-and-selector
                 axioms-buttons3
                 axioms-buttons2
                 axioms-buttons1
                 info-pane)
               :adjust :center)

  
   (taxonomy-tab column-layout 
                 '(state-pane1
                   navi-pane
                   
                   taxonomy-pane
                   taxonomy-buttons1
                   taxonomy-buttons2
                   concepts-buttons2
                   info-pane)
                 :adjust :center)

   (role-hierarchy-tab column-layout 
                       '(state-pane1
                         navi-pane
                         
                         role-hierarchy-pane
                         role-hierarchy-buttons1
                         role-hierarchy-buttons2
                         roles-buttons2
                         info-pane)
                       :adjust :center)

   (abox-graph-tab column-layout 
                   '(state-pane1
                     navi-pane
                     
                     abox-graph-pane
                     abox-graph-buttons1
                     abox-graph-buttons2
                     abox-graph-buttons3
                     abox-graph-buttons4
                     info-pane)
                   :adjust :center) 

   (query-input-tab column-layout 
                    '(state-pane1
                      navi-pane
                      
                      command-pane
                      query-input-buttons1
                      query-input-buttons2                      
                      query-result-pane)
                    :adjust :center)

   (queries-tab column-layout 
                '(state-pane1
                  navi-pane
                  
                  queries-pane
                  queries-buttons1
                  queries-buttons2 
                  info-pane)
                :adjust :center)
 
   (defined-queries-tab column-layout 
                        '(state-pane1
                          navi-pane
                          
                          defined-queries-pane
                          defined-queries-buttons1
                          info-pane)
                        :adjust :center)
 

   (logo-tab column-layout 
             '(logo-pane
               version-info-pane
               logo-show-info-button)

             :ratios '(0.7 0.3)
             ;:background +logo-bc+
             :adjust :center)

   (log-tab column-layout 
            '(;state-pane1
              ;navi-pane
              log-pane
              clear-log-button)
            :adjust :center)

   ;;;
   ;;;
   ;;; 

   (server-request-title-pane1
    row-layout 
    '(server-request-title-pane)
    :adjust :center)

   (server-response-title-pane1
    row-layout 
    '(server-response-title-pane)
    :adjust :center)

   #+:pulse-pane
   (server-request-pulse-pane1 simple-pinboard-layout 
                               '(server-request-pulse-pane)
                               :accepts-focus-p nil
                               :enabled nil
                               :width 22
                               :height 22
                               :min-width 22
                               :min-height 22
                               :max-width 22
                               :max-height 22)

   (server-request-status-and-title-pane 
    row-layout 
    '(server-request-title-pane1
      #+:pulse-pane server-request-pulse-pane1
      )
    :adjust :center)

   (cur-concept-pane1 row-layout 
                      '(cur-concept-pane
                        cur-no-selected-concepts-pane)
                      :adjust :center)

   (cur-individual-pane1 row-layout 
                         '(cur-individual-pane
                           cur-no-selected-individuals-pane)
                         :adjust :center)

   (cur-role-pane1 row-layout 
                   '(cur-role-pane
                     cur-no-selected-roles-pane)
                   :adjust :center)


   (cur-axiom-pane1 row-layout 
                    '(cur-axiom-pane
                      cur-no-selected-axioms-pane)
                    :adjust :center)


   (state-pane1 grid-layout
                '(
                  profile-name-pane 
                  cur-profile-pane
                  namespace-title-pane 
                  cur-namespace-pane
                  
                  
                  tbox-title-pane 
                  cur-tbox-pane
                  abox-title-pane
                  cur-abox-pane
                    
                  concept-title-pane
                  cur-concept-pane1 
                  role-title-pane
                  cur-role-pane1 
  
                  individual-title-pane
                  cur-individual-pane1 
                  axiom-title-pane
                  cur-axiom-pane1 
                  
                  ;; query-or-rule-title-pane
                  ;; cur-query-or-rule-pane
                  ;; definition-title-pane
                  ;; cur-definition-pane

                  ;; reasoner-title-pane 
                  ;; cur-reasoner-pane
                  ;; ontology-title-pane
                  ;; cur-ontology-pane
                  

                  #+:pulse-pane
                  server-request-status-and-title-pane
                  #-:pulse-pane
                  server-request-title-pane1
                  
                  server-request-pane
                  server-response-title-pane1
                  
                  server-response-pane)

                :columns 4
                :rows 5

                :x-uniform-size-p nil
                :y-uniform-size-p nil)


   #| (state-pane column-layout
               '(state-pane1 
                 navi-pane)
               :adjust :center) |# 
                    
   (navi1-pane row-layout 
              '(fold-or-unfold-pane
                ;cur-history-title-pane
                focus-start
                focus-prev
                cur-history-pane
                focus-next
                focus-end
                ;focus-delete
                reset-sirius-state-button 
                resync-sirius-state-button                
                full-reset-button
                
                remove-urls-checkbox
                maintain-arguments-checkbox 
                ;;; selected-first-checkbox

                ;timeout-range
                )
              :adjust :center)

   #+:progress-bar-pane
   (server-controll-pane row-layout
                         '(racer-request-title-pane
                           racer-request-pane 
                           progress-bar
                           cancel-racer-request-button)
                         :adjust :center)


   (navi-pane column-layout 
              '(navi1-pane
                #+:progress-bar-pane 
                server-controll-pane)
              :adjust :center)

   ;;;
   ;;;
   ;;;

   (server-buttons1 row-layout 
                    '(show-info-button
                      edit-profile-button
                      new-profile-button
                      delete-profile-button
                      ;; load-profiles-button
                      ;; save-profiles2-button
                      ;: save-profiles-button
                      ;; save-profiles3-button
                      
                      #+:sirius-editor editor-button
                      #+:sirius-editor open-kb-button
                      license-button))

   (server-buttons2 row-layout 
                    '(connect-button
                      ;;full-reset-button
                      start-server-button
                      kill-server-button
                      load-kb-button
                      reload-server-button
                      dump-server-button
                     
                      #-:mac quit-button
                      #-:mac quit-and-shutdown-button 
                      ))

   (shell-buttons1 row-layout 
                   '(assign-selected-concepts-from-result-button
                     assign-selected-roles-from-result-button
                     assign-selected-individuals-from-result-button))
   

   (shell-buttons2 row-layout 
                   '(clear-concept-selection-button
                     clear-role-selection-button
                     clear-individual-selection-button))

   (shell-buttons3 row-layout 
                   '(show-info-button
                     ;license-button
                     save-shell-log-button
                     clear-button
                     ;;full-reset-button
                     #+:sirius-editor editor-button
                     #+:sirius-editor open-kb-button
                     load-kb-button
                     #-:mac quit-button
                     #-:mac quit-and-shutdown-button))

   (tbox-buttons1 row-layout
                  '(forget-tbox-button
                    describe-tbox-button
                    change-current-tbox-button
                    tbox-coherent-button
                    tbox-classify-button
                    ;; #-:mac quit-button
                    ))

   (abox-buttons1 row-layout
                  '(forget-abox-button
                    describe-abox-button
                    change-current-abox-button
                    abox-consistent-button
                    realize-abox-button))

   (concepts-buttons1 row-layout
                      '(selected-only-checkbox
                        concepts-radio-panel))

   (concepts-buttons2 row-layout
                      '(search-item 
                        selected-first-checkbox
                        select-all-concepts-button
                        clear-concept-selection-button
                        select-children-concepts-button
                        select-parent-concepts-button
                        describe-concept-button
                        instance-query-button
                        concept-synonyms-button
                        #+:sonic sonic-approx
                        #+:sonic sonic-lcs
                        #+:sonic sonic-scs))

   (roles-buttons1 row-layout
                   '(selected-only-checkbox
                     roles-radio-panel))

   (roles-buttons2 row-layout
                   '(search-item 
                     selected-first-checkbox
                     select-all-roles-button
                     clear-role-selection-button
                     describe-role-button
                     role-domain-button
                     role-range-button
                     role-parents-button
                     role-children-button
                     role-synonyms-button))

   (individuals-buttons1 row-layout
                         '(search-item 
                           selected-first-checkbox
                           selected-only-checkbox
                           select-all-individuals-button
                           clear-individual-selection-button 
                           describe-individual-button
                           individual-synonyms-button
                           individual-antonyms-button
                           direct-types-button
                           all-types-button
                           delete-individuals-button
                           ;;; #-:sonic msc-k-button
                           #+:sonic sonic-generalize
                           #+:sonic sonic-msc))

   (assertions-buttons1 row-layout
                        '(assertions-radio-panel))
   
   (assertions-buttons2 row-layout
                        '(select-all-assertions-button clear-assertion-selection-button delete-assertions-button))
   

   (assertions-buttons3 row-layout
                        '(selected-first-checkbox
                          selected-only-checkbox
                          taxonomy-roots-panel
                          abox-graph-roots-panel
                          role-roots-panel))


   (axioms-buttons1 row-layout
                    '(reasoner-selection-panel
                      delete-reasoner-button
                      new-reasoner-button
                      save-reasoner-button
                      ))

   (axioms-buttons2 row-layout
                    '(ontology-selection-panel 
                      delete-ontology-button
                      new-ontology-button
                      load-ontology-button
                      unload-ontology-button
                      save-ontology-button))


   (axioms-buttons3 row-layout
                    '(search-item
                      selected-first-checkbox
                      select-axioms-button
                      clear-axiom-selection-button
                      new-axiom-button
                      edit-axiom-button
                      delete-axioms-button 
                      load-axioms-button
                      unload-axioms-button
                      ))

   (axioms-pane-and-selector row-layout
                             '(axioms-selector
                               axioms-pane
                               )
                             :ratios '(0.3 0.7))
   
   (taxonomy-buttons1 row-layout
                      '(freeze-graph-checkbox
                        autofocus-checkbox
                        show-top-checkbox 
                        show-bottom-checkbox 
                        
                        taxonomy-request-button
                        ;taxonomy-cancel-request-button
                        taxonomy-display-button
                        taxonomy-reset-button
                        taxonomy-print-button))
   
   (taxonomy-buttons2 row-layout
                      '(taxonomy-roots-panel
                        taxonomy-orientation-panel
                        taxonomy-kind-panel
                        taxonomy-depth-panel))
   

   (role-hierarchy-buttons1 row-layout
                            '(freeze-graph-checkbox
                              autofocus-checkbox
                              
                              role-hierarchy-request-button
                              ;role-hierarchy-cancel-request-button
                              role-hierarchy-display-button
                              role-hierarchy-reset-button
                              role-hierarchy-print-button))

   (role-hierarchy-buttons2 row-layout
                            '(role-roots-panel
                              role-orientation-panel
                              role-hierarchy-kind-panel
                              role-hierarchy-depth-panel))


   (abox-graph-buttons1 row-layout
                        '(node-labels-checkbox
                          freeze-graph-checkbox
                          autofocus-checkbox
                          show-top-role-checkbox
                          only-selected-successors-checkbox
                          include-transitive-roles-checkbox
                          datatype-fillers-checkbox
                          told-only-checkbox 
                          
                          abox-graph-request-button
                          ;abox-graph-cancel-request-button
                          abox-graph-display-button
                          abox-graph-reset-button
                          abox-graph-print-button))

   (abox-graph-buttons2 row-layout
                        '(abox-graph-roots-panel
                          role-roots-panel))
   
   (abox-graph-buttons3 row-layout
                        '(abox-graph-orientation-panel
                          ;;; abox-graph-kind-panel                          
                          abox-graph-depth-panel))  

   (abox-graph-buttons4 row-layout
                        '(search-item
                          selected-first-checkbox
                          clear-individual-selection-button 
                          clear-role-selection-button 
                          describe-individual-button
                          direct-types-button
                          all-types-button
                          delete-individuals-button
                          ;;; #-:sonic msc-k-button
                          #+:sonic sonic-generalize
                          #+:sonic sonic-msc))
   
   (query-input-buttons1 row-layout 
                         '(search-item 
                           selected-first-checkbox
                           assign-selected-individuals-from-result-button
                           clear-individual-selection-button 
                           describe-individual-button
                           direct-types-button
                           all-types-button
                           delete-individuals-button
                           ;;; #-:sonic msc-k-button
                           #+:sonic sonic-generalize
                           #+:sonic sonic-msc))

   (query-input-buttons2 row-layout 
                         '(assign-selected-concepts-from-result-button
                           clear-concept-selection-button
                           select-children-concepts-button
                           select-parent-concepts-button
                           describe-concept-button
                           instance-query-button))

   (queries-buttons1 row-layout
                     '(queries-radio-panel))

  
   (queries-buttons2 row-layout
                     '(nrql-status-button
                       describe-query-button
                       describe-original-query-button
                       get-next-tuple-button
                       answer-query-button
                       delete-query-button
                       abort-query-button
                       execute-query-button
                       reexecute-query-button
                       reprepare-query-button
                       ;; #-:mac quit-button
                       ))

   (defined-queries-buttons1 row-layout
                             '(delete-definition-button))
   )

  (:default-initargs 
   
   :title +sirius-name+
   :title-font *title-font*
   
   ;; :min-width 1200
   ;; :best-height 730
   
   :message-area nil 
   :auto-menus nil

   ;;;
   ;;; Hooks setzen 
   ;;; 

   #|

   #+(and :macosx :sirius-editor)
   :create-callback

   #+(and :macosx :sirius-editor)
   #'(lambda (interface)
       (declare (ignorable interface))

       (setf *debugger-hook* 
             #'(lambda (error me-or-my-encapsulation)
                 (declare (ignore me-or-my-encapsulation))
                 (break "here")
                 #-:sirius-dev
                 (beep-pane)
                 
                 #+:sirius-dev
                 (error error))))
   |# 

   :top-level-hook
   #'(lambda (func interface)
       (declare (ignore interface))

       (handler-case
         
           (funcall func)
         
         (editor::editor-error (error)
           (sirius-show-modeline (format nil "Editor problem: ~A" error)))
         
         (error (error)

           #-:sirius-dev
           (beep-pane)
           (let ((message 
                  (format nil 
                          "~%*** Runtime Error:~%*** ~A.~%*** Condition Type: ~A~%" error (type-of error))))
             (format t message)
             (when (search "libpng" message)
               (format t 
                       "~%*** PLEASE ENSURE THAT /usr/lib/libpng12.so IS INSTALLED~%" error (type-of error))))
                 
           #+:sirius-dev
           (error error))))

   :confirm-destroy-function #'terminate-sirius

   :destroy-callback #'sirius-destroy))

;;;
;;;
;;; 

(defun terminate-sirius (sirius &rest args &key (shutdown-racerpro nil) from-editor-process-p)
  (declare (ignorable sirius args from-editor-process-p))

  (let ((profile (active-profile)))

    (cond ((or (not (profile-confirm-quit profile))
               (confirm-yes-or-no +quit-message+))
           (kill-processes)
           (kill-all-processes)
           (process-wait (not *communication-status-updater*))
    
           (setf (symbol-function 'get-pulse-image)
                 #'(lambda (status comm-status)  
                     (declare (ignore status comm-status))
                     nil))

           (when (profile-autosave-on-quit profile)
             (save-profiles nil nil))

           (let ((*no-logging* t))
             (dolist (profile *sirius-profiles*)
               (when (profile-log-stream profile)
               
                 (close (profile-log-stream profile))
                 (setf (profile-log-stream profile) nil)))
      
             #+:sirius-editor 
             (when *sirius-editor-app*
               (editor::window-save-all-files)

               (execute-with-interface #+(or :win32 :linux) *sirius-editor-app*
                                       #-(or :win32 :linux)
                                       (if from-editor-process-p 
                                           *sirius-editor-app*
                                         sirius)
                                       #'(lambda ()
                                           ;;; :force-p t: dont call confirm destroy, 
                                           ;;; dont call destroy-callback
                                           (quit-interface *sirius-editor-app* :force t)
                                           (setf *sirius-editor-app* nil))))

             ;; (profile-kill-started-server-on-exit profile) this slot is probably no longer used!

          
             (when (and (connected-and-socket-alive-p)
                        (or shutdown-racerpro
                            (and (profile-server-started profile)
                                 (confirm-yes-or-no "Shutdown started ~A?"
                                                    (get-server-description)))))

               (kill-server t t)))

           (close-connection t)

           #+:racer-with-sirius
           (when (confirm-yes-or-no "Also Shutdown RacerPro?")
             (lispworks:quit))

           (sleep 1)

           t)
        
          (t nil))))

(defun sirius-destroy (sirius)
  (declare (ignorable sirius))
  t)

(defun quit-sirius-and-ask-for-shutdown-of-server-started-by-this-racerporter (&optional (sirius *sirius-app*))
  (when (terminate-sirius sirius)
    (destroy sirius)))

(defun quit-sirius-and-shutdown (&optional (sirius *sirius-app*))
  (when (terminate-sirius sirius :shutdown-racerpro t) 
    (destroy sirius)))

;;;
;;;
;;;


(defun sirius-running-p ()
  (with-sirius-app (app)
    (and app
         (slot-value app 'interface-displayed-p))))

(defmethod interface-display :after ((sirius sirius))

  (handler-case 
      
      (progn 

        (init-process-pool)
        (kill-processes)
        (disable-push-buttons)
        (setf *dialog-displayed* nil)

        ;; (display-message "1")

        (with-slots (interface-displayed-p #+(and :mac :sirius-editor)
                                           load-file-on-display
                                           role-hierarchy-pane
                                           taxonomy-pane
                                           abox-graph-pane
                                           racer-request-pane) sirius


          #+(and (not :sirius-dev) (not :mac) :sirius-editor)
          (let ((file (and (null (rest (rest system:*line-arguments-list*)))
                           (second system:*line-arguments-list*))))
            (when (and file (probe-file file))
              (open-connection t)
              (sirius-editor file)))

          #+(and :mac :sirius-editor)
          (when load-file-on-display
            (open-connection t)
            (sirius-editor load-file-on-display)
            (setf load-file-on-display nil))

          (setup-shell-pane t)
          (setf interface-displayed-p t)

          (setf *role-hierarchy-info-pane* role-hierarchy-pane
                *taxonomy-info-pane* taxonomy-pane
                *abox-graph-info-pane* abox-graph-pane)

          ;; (display-message "2")
          
          (setup-fonts)
    
          ;; (sleep +wait-for-interface-to-settle-before-start-and-connect-sleep-time+)
    
          ;; (display-message "3")

          (schedule-update)
    
          ;; (sleep +wait-for-interface-to-settle-before-start-and-connect-sleep-time+)

          ;; (sleep 3)
          ;; (sleep 1)
    
          
          (setf *background-color*
                (simple-pane-background racer-request-pane))

          (with-profile-access

            (unless (connected-and-socket-alive-p)

              ;; (display-message "4")

              (let ((button nil)
                    (profile (active-profile)))

                (force-update-all-screens)
          
                #+(and (not :mac) (not :racer-with-sirius))
                (when (or (profile-auto-start-if-default profile)
                          (profile-auto-connect profile))
                  (setf button (non-modal-message "Please Wait...")))
          
                (cond ((profile-auto-start-if-default profile)
                 
                       ;; (display-message "5")
                       
                       (clear-current-prompt-and-input)

                       (let ((server (start-server nil nil)))

                         (when (and (profile-auto-connect profile)
                                    (not (connected-and-socket-alive-p)))
                           (clear-current-prompt-and-input)

                           (cond ((open-connection t)
                                  (enter-dummy-command
                                   (format nil "Automatically connected to ~A" 
                                           (get-server-description profile t))                  
                                   (list :okay (get-server-description profile t))))

                                 (t

                                  (when (eq server :no-server)
                                    #-:mac
                                    (apply-in-pane-process button
                                                           #'(lambda ()
                                                               (quit-interface button)))

                                    (error-message "Error: Cannot find RacerPro Executable!~%Please specify path to RacerPro using \"Edit Profile -> RacerPro Executable\"!"))
              
                                  (enter-dummy-command
                                   (format nil "Couldn't connect to ~A" 
                                           (get-server-description profile))                  
                                   (list :error (get-server-description  profile))))))))
              
                      ((profile-auto-connect profile)
                 
                       ;; (display-message "a")

                       (clear-current-prompt-and-input)
                       
                       ;; (display-message "b")

                       (if (open-connection t)
                           (enter-dummy-command
                            (format nil "Automatically connected to ~A" 
                                    (get-server-description profile t))                  
                            (list :okay (get-server-description profile t)))
                         (enter-dummy-command
                          (format nil "Couldn't connect to ~A" 
                                  (get-server-description profile))                  
                          (list :error (get-server-description  profile))))))

                (when button
                  (apply-in-pane-process button
                                         #'(lambda ()
                                             (quit-interface button)))))))

          (start-processes)))

    (error (error) 
           #+:sirius-dev (error error)
           #-:sirius-dev 
           (progn
             (cannot-startup (format nil "~A" error))
             (sleep 2)
             (lispworks::quit)))))


(defun setup-fonts ()
  (with-sirius-app (sirius)
    (let* ((button (slot-value sirius 'connect-button))
           (font 
            (gp:convert-to-font-description 
             button 
             (simple-pane-font button))))
      (declare (ignorable font))

      #+:ignore
      (dolist (button-and-condition +racer-command-buttons+)
        (let ((button (slot-value sirius (first button-and-condition))))
          (setf (simple-pane-font button)
                (apply #'gp:make-font-description
                       (append '(:weight :bold) (profile-button-font (active-profile))))))))))
      

(defun show-info ()
  (with-sirius-app (sirius)
    (with-slots (command-pane) sirius
      (let ((text (get-manual)))
      
        (dolist (pane '(info-pane command-pane))
          (let ((pane (slot-value sirius pane)))
            ;(setf (simple-pane-enabled pane) t)

            (setf (editor-pane-fixed-fill pane) 80
                  (editor-pane-text pane) text)

            (setf (editor-pane-fixed-fill command-pane) nil))))))

  (setup-shell-pane))
      
;;;
;;;
;;;

(defmethod interface-keys-style ((self sirius))
  :emacs)

;;;
;;;
;;;


(defun reset-tbox-specific-pane-contents ()
  (with-sirius-app (app)
    (with-slots (role-hierarchy-tab
                 taxonomy-tab) app
        
      (dolist (pane '(concepts-pane
                      roles-pane))
        (setf (collection-items (slot-value app pane)) nil))
      
      (with-profile-access

        (apply-in-pane-process app
                               #'(lambda ()
                                   (setf (layout-description role-hierarchy-tab)
                                         (remove nil 
                                                 (list (when show-status-pane 
                                                         'state-pane1)
                                                       'navi-pane
                                                       *role-hierarchy-info-pane*
                                                       'role-hierarchy-buttons1
                                                       'role-hierarchy-buttons2
                                                       'roles-buttons2
                                                       (when show-info-pane
                                                         'info-pane))))

                                   (setf (editor-pane-text  *role-hierarchy-info-pane*)
                                         "")
                                   
                                   (setf (layout-description taxonomy-tab)
                                         (remove nil 
                                                 (list (when show-status-pane  
                                                         'state-pane1)
                                                       'navi-pane
                                                       *taxonomy-info-pane*
                                                       'taxonomy-buttons1
                                                       'taxonomy-buttons2
                                                       'concepts-buttons2
                                                       (when show-info-pane
                                                         'info-pane))))

                                   (setf (editor-pane-text *taxonomy-info-pane*)
                                         "")))
                   
        (next-update-compute-and-layout-taxonomy)
        
        (next-update-compute-and-layout-role-hierarchy)

        (unselect-all-concepts)
        (unselect-all-roles)))))


(defun reset-abox-specific-pane-contents ()
  (with-sirius-app (app)
    (with-slots (abox-graph-tab) app
      
      (dolist (pane '(individuals-pane assertions-pane))
        (setf (collection-items (slot-value app pane)) nil))
      
      (with-profile-access
        
        (apply-in-pane-process app
                               #'(lambda ()
                                   (setf (layout-description abox-graph-tab)
                                         (remove nil 
                                                 (list (when show-status-pane
                                                         'state-pane1)
                                                       'navi-pane
                                                       *abox-graph-info-pane*
                                                       'abox-graph-buttons1
                                                       'abox-graph-buttons2
                                                       'abox-graph-buttons3
                                                       'abox-graph-buttons4
                                                       (when show-info-pane
                                                         'info-pane))))
      
                                   (setf (editor-pane-text *abox-graph-info-pane*)
                                         "")))
      
        (next-update-compute-and-layout-abox-graph)

        (unselect-all-individuals)))))

;;;
;;;
;;;       
 

(defun switch-to-active-tab ()
  (with-update-display-status
    (with-sirius-app (app) 
      (with-profile-access
        (with-slots (tab-pane
                     command-pane

                     abox-pane
                     tbox-pane
                     concepts-pane
                     individuals-pane
                     roles-pane
                     assertions-pane
                     axioms-pane

                     log-pane
                     queries-pane
                     defined-queries-pane
                   
                     taxonomy-pane
                     role-hierarchy-pane

                     server-pane

                     server-tab
                     shell-tab
                     tbox-tab
                     abox-tab
                     taxonomy-tab
                     role-hierarchy-tab
                     abox-graph-tab
                     concepts-tab 
                     roles-tab
                     individuals-tab
                     assertions-tab
                     axioms-tab
                     query-input-tab
                     queries-tab
                     defined-queries-tab
                     logo-tab 
                     log-tab) app

                    
          (capi:apply-in-pane-process 
           app #'(lambda ()

                   (refresh-layout-description)


                   (setf (capi:tab-layout-visible-child tab-pane)
                         (ecase active-tab
                           (:axioms axioms-tab)
                           (:server server-tab)
                           (:shell shell-tab)
                           (:abox abox-tab)
                           (:tbox tbox-tab)
                           (:assertions assertions-tab)
                           (:taxonomy taxonomy-tab)
                           (:role-hierarchy role-hierarchy-tab)
                           (:abox-graph abox-graph-tab)
                           (:individuals individuals-tab)
                           (:query-input query-input-tab)
                           (:queries queries-tab)
                           (:defined-queries defined-queries-tab)
                           (:concepts concepts-tab)
                           (:roles roles-tab)
                           (:log log-tab)
                           (:logo logo-tab)))

                   (setf (choice-selected-item tab-pane)
                         (ecase active-tab
                           (:server (list *tab-title-profiles* server-tab))
                           (:shell (list *tab-title-shell* shell-tab))
                           (:tbox (list *tab-title-tboxes* tbox-tab))
                           (:abox (list *tab-title-aboxes* abox-tab))
                           (:taxonomy (list *tab-title-taxonomy* taxonomy-tab))
                           (:role-hierarchy (list *tab-title-role-hierarchy* role-hierarchy-tab))
                           (:abox-graph (list *tab-title-abox-graph* abox-graph-tab))
                           (:concepts (list *tab-title-concepts* concepts-tab))
                           (:roles (list *tab-title-roles* roles-tab))
                           (:individuals (list *tab-title-individuals* individuals-tab))
                           (:assertions (list *tab-title-assertions* assertions-tab))
                           (:query-input (list *tab-title-query-io* query-input-tab))
                           (:queries (list *tab-title-queries* queries-tab))
                           (:defined-queries (list *tab-title-def-queries* defined-queries-tab))
                           (:log (list *tab-title-log* log-tab))
                           (:axioms (list *tab-title-owlapi* axioms-tab))
                           (:logo (list *tab-title-about* logo-tab))))

                   (case active-tab 
                     (:axioms 
                      (set-pane-focus axioms-pane))
                     (:server 
                      (set-pane-focus server-pane))
                     (:shell
                      (set-pane-focus command-pane))
                     (:tbox
                      (set-pane-focus tbox-pane))
                     (:tbox
                      (set-pane-focus abox-pane))
                     (:taxonomy
                      (set-pane-focus taxonomy-pane))
                     (:role-hierarchy
                      (set-pane-focus role-hierarchy-pane))
                     (:abox-graph
                      (set-pane-focus abox-pane))
                     (:concepts
                      (set-pane-focus concepts-pane))
                     (:individuals
                      (set-pane-focus individuals-pane))
                     (:roles
                      (set-pane-focus roles-pane))
                     (:assertions
                      (set-pane-focus assertions-pane))
                     (:queries
                      (set-pane-focus queries-pane))
                     (:defined-queries
                      (set-pane-focus defined-queries-pane))
                     (:log
                      (set-pane-focus log-pane))))))))))
        
(defun get-active-tab ()
  (with-profile-access 
    (with-sirius-slots (server-tab
                      shell-tab
                      abox-tab
                      tbox-tab
                      taxonomy-tab
                      role-hierarchy-tab
                      abox-graph-tab
                      concepts-tab
                      roles-tab
                      individuals-tab
                      assertions-tab
                      query-input-tab
                      queries-tab
                      defined-queries-tab
                      log-tab
                      axioms-tab
                      logo-tab) 
                     (ecase active-tab
                       (:server server-tab)
                       (:shell shell-tab)
                       (:tbox tbox-tab)
                       (:abox abox-tab)
                       (:taxonomy taxonomy-tab)
                       (:role-hierarchy role-hierarchy-tab)
                       (:abox-graph abox-graph-tab)
                       (:concepts concepts-tab)
                       (:roles roles-tab)
                       (:individuals individuals-tab)
                       (:assertions assertions-tab)
                       (:query-input query-input-tab)
                       (:queries queries-tab)
                       (:defined-queries defined-queries-tab)
                       (:log log-tab)
                       (:axioms axioms-tab)
                       (:logo logo-tab)))))

(defun refresh-layout-description ()
  (let* ((tab (get-active-tab))
         (descr (layout-description tab)))

    (with-profile-access

      (cond (show-status-pane
             (when (and (not (eq (first descr) 'state-pane1))
                        (not (member active-tab '(:logo :log))))
               (setf descr (cons 'state-pane1 descr))))

            ((eq (first descr) 'state-pane1)
             (setf descr (rest descr))))
      
      (cond (show-info-pane
             (when (and (not (eq (first (last descr)) 'info-pane))
                        (not (member active-tab '(:shell :query-input :logo :log))))
               (setf descr (append descr (list 'info-pane)))))

            ((eq (first (last descr)) 'info-pane)
             (setf descr (butlast descr))))

      (setf (layout-description tab) descr)

      #+:progress-bar-pane 
      (with-sirius-slots (navi-pane)
                         (setf (layout-description navi-pane)
                               (if show-progress-bar
                                   '(navi1-pane server-controll-pane)
                                 '(navi1-pane)))))))


;;;
;;;
;;;


(defun sirius-use-current-abox-tbox () 
  (with-profile-access
    
    (without-caching
      (setf sirius-current-tbox nil
            sirius-current-abox nil
            current-reasoner nil
            current-ontology nil)))
  
  ;;; neu setzen: 
  (resync-state))

(defun clear-and-resync ()

  (reset-active-profile 
   :clear-and-resync-p t)
                                              
  (when (connected-and-socket-alive-p)
    (resync-state))
  
  (schedule-update))


(defun resync-state ()

  ;;; muss nach (potentiell) 
  ;;; zustandveraendernden Aufrufen
  ;;; gerufen werden

  (with-sirius-app (app)
    (let ((profile (active-profile)))
      (with-synchronous-request
        (without-caching
                  
          (with-slots (abox-pane
                       tbox-pane
                       queries-pane
                       concepts-pane
                       individuals-pane
                       reasoner-selection-panel
                       ontology-selection-panel) app

            (with-profile-access                     

              (when (or (not sirius-current-tbox)
                        (not (racer-function1 (find-tbox sirius-current-tbox) nil)))
                (setf sirius-current-tbox (racer-function1 (current-tbox) nil)))

              (when (or (not sirius-current-abox)
                        (not (racer-function1 (find-abox sirius-current-abox) nil)))
                (setf sirius-current-abox (racer-function1 (current-abox) nil)))

              (when (or (not current-individual)
                        (not sirius-current-abox)
                        (not (racer-function1 (individual-p current-individual 
                                                            sirius-current-abox)
                                              nil)))
                (unselect-all-individuals))

              (when (or (not current-role)
                        (not sirius-current-tbox)
                        (not (racer-function1 (role-p current-role 
                                                      sirius-current-tbox)
                                              nil)))
                (unselect-all-roles))

              (when (or (not current-concept)
                        (not sirius-current-tbox)
                        (not (racer-function1 (concept-p (if (consp current-concept)
                                                             (first current-concept)
                                                           current-concept)
                                                         sirius-current-tbox)
                                              nil)))
                (unselect-all-concepts))

              ;;;
              ;;;
              ;;;

              (when (or (not current-role)
                        (not sirius-current-tbox)
                        (not (racer-function1 (role-p current-role 
                                                      sirius-current-tbox)
                                              nil)))
                (unselect-all-roles))

              ;;;
              ;;;
              ;;;


              (when (or (not current-reasoner)
                        (not (member current-reasoner 
                                     (racer-function1 (owlapi-get-reasoners) nil))))
                (setf current-reasoner (racer-function1 (owlapi-get-current-reasoner))))

              (when (and current-reasoner
                         (or (not current-ontology)
                             (not (member current-ontology
                                          (racer-function1 (owlapi-get-ontologies current-reasoner) nil)))))
                (setf current-ontology (or 
                                        (let ((res (racer-function1 
                                                    (owlapi-get-auto-ontology current-reasoner)
                                                    nil)))
                                          (unless (member res '(:|void| :void))
                                            res))
                                        (first (racer-function1 (owlapi-get-ontologies current-reasoner) nil)))))

              #| (when current-reasoner 
                   (setf simplified-protocol
                         (owlapi-uses-simplified-protocol current-reasoner))) |# 

              ;;;
              ;;;
              ;;;

              (when sirius-current-tbox
                (setf (choice-selected-item tbox-pane)
                      sirius-current-tbox))

              (when sirius-current-abox
                (setf (choice-selected-item abox-pane)
                      sirius-current-abox))
             
              (when current-individual
                (setf (choice-selected-item individuals-pane)
                      current-individual))

              (when current-concept
                (setf (choice-selected-item concepts-pane)
                      (first (ensure-list current-concept))))
        
              (when current-query-or-rule
                (setf (choice-selected-item queries-pane)
                      current-query-or-rule))

              ;;;
              ;;;
              ;;; 

              (when current-reasoner
                (setf (choice-selected-item reasoner-selection-panel) current-reasoner))

              (when current-ontology
                (setf (choice-selected-item ontology-selection-panel) current-ontology))

              ;;;
              ;;;
              ;;; 
            
              (setf sirius-current-namespace 
                    (if (racer-function1 (find-tbox sirius-current-tbox) nil)
                        (let ((prefix
                               (ensure-string
                                (racer-function1 
                                 (get-namespace-prefix sirius-current-tbox)
                                 nil))))

                          #+:ignore 
                          (unless (string-equal prefix "nil")
                            (remove #\# prefix))

                          prefix)

                      nil))

              (setf sirius-current-namespace-prefixes 
                    (if (or (string= (profile-racer-version profile) "1.9.0")
                            (string= (profile-racer-version profile) "1.9.1"))
                        nil
                      (mapcar #'(lambda (entry)
                                  (list (when (first entry)
                                        ; sonst nil = default namespace!
                                          (ensure-string (first entry)))
                                        ;;(remove #\# (ensure-string (second entry)))
                                        (ensure-string (second entry))))
                              (racer-function1 (get-namespace-prefixes) nil))))

              (clrhash namespace-2-prefix-hash)
              (clrhash prefix-2-namespace-hash)

              (when sirius-current-namespace
                (setf (gethash sirius-current-namespace namespace-2-prefix-hash) "")
                (setf (gethash "" prefix-2-namespace-hash) sirius-current-namespace))

              (loop as (from to) in sirius-current-namespace-prefixes do
                    (setf (gethash to namespace-2-prefix-hash) from)
                    (setf (gethash from prefix-2-namespace-hash) to)))))))))
                  
;;;
;;;
;;;

(defun raise-racerporter ()
  (with-sirius-app (sirius)
    (raise-interface sirius)))

#+:sirius-editor
(defun raise-racereditor ()
  (when *sirius-editor-app*
    (raise-interface *sirius-editor-app*)))

;;;
;;;
;;;

(defun sirius ()

  (handler-case 
    
      (progn 
        #+(and :linux (not :gtk))
        (setf CAPI-MOTIF-LIBRARY::*WARN-ABOUT-INVALID-WINDOW*  nil)
  
        (when *sirius-app*
          (quit-interface *sirius-app* :force t))

        (when *sirius-editor-app*
          (quit-interface *sirius-editor-app* :force t))

        (setf *sirius-editor-app* nil)

        (setf *running* nil)

        (setf *sirius-profiles* nil)

        #+:sirius-dev 
        (setf *process-pool* nil)
        ;;; sonst nervig 

        (init-process-pool)
        (close-all-connections)

        ;; (display-message (format nil "~S" system:*line-arguments-list*))

        (let* ((width 
                (second (member "-width" system:*line-arguments-list* :test #'string=)))
               
               (height
                (second (member "-height" system:*line-arguments-list* :test #'string=)))
               
               (vertical-scroll 
                (member "-vertical-scrollbar"  system:*line-arguments-list* :test #'string=))
               
               (horizontal-scroll 
                (member "-horizontal-scrollbar"  system:*line-arguments-list* :test #'string=))
               
               (overwrite-font-size-to 
                (member "-overwrite-font-size"  system:*line-arguments-list* :test #'string=))
              
               (help 
                (member "-help"  system:*line-arguments-list* :test #'string=))

               (width (when width 
                        (read-from-string width)))

               (height (when height 
                         (read-from-string height))))
          
          (when help
            (let ((string "~%Help - recognized arguments: 
  -overwrite-font-size <n> 
  -width <n>
  -height <n>
  -vertical-scrollbar 
  -horizontal-scrollbar
  -help~%~%"))
              #+:win32
              (display-message (format nil string))
              #-:win32
              (format t string)))
          
          #+:mac
          (capi:set-application-interface
           (make-instance 'sirius-application 
                          :max-width width 
                          :width width 
                          :height height
                          :max-height height))

          (load-sirius-profiles)

          (setf (profile-busy-counter (active-profile)) 0)

          (setf *horizontal-scroll* 
                horizontal-scroll
                *vertical-scroll* 
                vertical-scroll)

          (setf *sirius-field-width* 
                (if width 
                    (/ width 3)
                  (/ (profile-width (active-profile)) 3)))

          ;;;
          ;;; Fonts
          ;;;

          (handler-case 
          
              (progn 
                
                (when overwrite-font-size-to 
                  (setf *global-font-size* 
                        (read-from-string (second overwrite-font-size-to))))

                (when (and (not overwrite-font-size-to)
                           (profile-overwrite-font-size (active-profile)))
                  (setf overwrite-font-size-to
                        (list t (profile-overwrite-font-size (active-profile)))
                        *global-font-size* 
                        (profile-overwrite-font-size (active-profile))))
          
                (when overwrite-font-size-to
                  (dolist (slot '(tabs-font 
                                  button-font
                                  racer-button-font
                                  checkbox-font
                                  radiobox-font
                                  title-font
                                  graph-font
                                  info-font
                                  list-font
                                  shell-font))

                    (let* ((font 
                            (slot-value (active-profile) slot))
                           (pos (member :size font)))
                
                      (if pos
                          (setf (second pos) *global-font-size*)
                        (setf (slot-value (active-profile) slot)
                              (append font
                                      `(:size ,*global-font-size*)))))))

                (with-profile-access
            
                  (setf *tabs-font* 
                        (when tabs-font (apply #'gp:make-font-description tabs-font))
                  
                        *button-font*
                        (when button-font (apply #'gp:make-font-description button-font))
                  
                        *racer-button-font*
                        (when racer-button-font (apply #'gp:make-font-description racer-button-font))
                  
                        *checkbox-font*
                        (when checkbox-font (apply #'gp:make-font-description checkbox-font))
                  
                        *radiobox-font*
                        (when radiobox-font (apply #'gp:make-font-description radiobox-font))
                  
                        *title-font*
                        (when title-font (apply #'gp:make-font-description title-font))
                  
                        *graph-font*
                        (when graph-font (apply #'gp:make-font-description graph-font))
                  
                        *info-font*
                        (when info-font (apply #'gp:make-font-description info-font))

                        *list-font*
                        (when list-font (apply #'gp:make-font-description list-font))

                        *shell-font*
                        (when shell-font (apply #'gp:make-font-description shell-font)))))

            (error (error) 
              (declare (ignorable error))
              
              (let ((profile
                     (first (get-default-profiles))))


                (with-access-to-profile profile
            
                  (setf *tabs-font* 
                        (when tabs-font (apply #'gp:make-font-description tabs-font))
                  
                        *button-font*
                        (when button-font (apply #'gp:make-font-description button-font))
                  
                        *racer-button-font*
                        (when racer-button-font (apply #'gp:make-font-description racer-button-font))
                  
                        *checkbox-font*
                        (when checkbox-font (apply #'gp:make-font-description checkbox-font))
                  
                        *radiobox-font*
                        (when radiobox-font (apply #'gp:make-font-description radiobox-font))
                  
                        *title-font*
                        (when title-font (apply #'gp:make-font-description title-font))
                  
                        *graph-font*
                        (when graph-font (apply #'gp:make-font-description graph-font))
                  
                        *info-font*
                        (when info-font (apply #'gp:make-font-description info-font))

                        *list-font*
                        (when list-font (apply #'gp:make-font-description list-font))

                        *shell-font*
                        (when shell-font (apply #'gp:make-font-description shell-font)))))))
                
                       
          
          (setf *pe-font* (pe-font)
                *pe-title-font* (pe-title-font)
                *pe-title-bold-font* (pe-title-bold-font))


          ;;;
          ;;;
          ;;;

          (mapcar #'(lambda (name title) 
                      (setf (symbol-value name)
                            title))
            
                  '(*TAB-TITLE-OWLAPI*
                    *TAB-TITLE-SHELL*
                    *TAB-TITLE-ABOUT*
                    *TAB-TITLE-QUERIES*
                    *TAB-TITLE-ROLE-HIERARCHY*
                    *TAB-TITLE-INDIVIDUALS*
                    *TAB-TITLE-PROFILES*
                    *TAB-TITLE-LOG*
                    *TAB-TITLE-QUERY-IO*
                    *TAB-TITLE-TAXONOMY*
                    *TAB-TITLE-ROLES*
                    *TAB-TITLE-TBOXES*
                    *TAB-TITLE-ABOXES*
                    *TAB-TITLE-DEF-QUERIES*
                    *TAB-TITLE-ABOX-GRAPH*
                    *TAB-TITLE-ASSERTIONS*
                    *TAB-TITLE-CONCEPTS*)

                  '("Axioms"
                    "Shell"
                    "About"
                    "Queries + Rules"
                    "Role Hierarchy"
                    "Individuals"
                    "Profiles"
                    "Log"
                    "Query IO"
                    "Taxonomy"
                    "Roles"
                    "TBoxes"
                    "ABoxes"
                    "Def. Queries"
                    "ABox Graph"
                    "Assertions"
                    "Concepts"))

          ;;;
          ;;;
          ;;;
    
          (if #+:lispworks6 
              nil
            #-:lispworks6 
            (and (not *sirius-logo-image*)
                 (not *inline-logo-image*))
        
            (cannot-startup)

            (progn
              (let ((*package* (find-package :racer-user)))
                (display
                 (setf *sirius-app* 
                       (make-instance 'sirius
                                      :width (or width 
                                                 (profile-width (active-profile)))
                                      :height (or height
                                                  (profile-height (active-profile)))))
                 #-:mac :process #-:mac t))))))
      
    (error (error) 
      #+:sirius-dev (error error)
      #-:sirius-dev 
      (progn
        (cannot-startup (format nil "~A" error))
        (sleep 2)
        (lispworks::quit)))))


;;;
;;;
;;;

(defun test-loop-monitor ()

  (loop

   (sleep 3)
   (princ (list (profile-processing-command (active-profile))
                (profile-current-command-status (active-profile))
                (profile-current-communication-status (active-profile))
                (profile-status (active-profile))))
   (terpri)
   (pprint (first (profile-history (active-profile))))
   (terpri)))

