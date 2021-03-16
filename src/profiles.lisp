;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(defpersistentclass profile () 
  ((name :accessor profile-name :initform "Unnamed" :initarg :name)
   
   (sirius-temp-directory :accessor profile-sirius-temp-directory :initform nil :initarg :sirius-temp-directory)
   
   (confirm-quit :accessor profile-confirm-quit :initform #+:sirius-dev t #-:sirius-dev t)

   (autosave-on-quit :accessor profile-autosave-on-quit :initform t)                     

   (pretty-print-input :accessor profile-pretty-print-input :initform nil)

   (popup-message-box :accessor profile-popup-message-box :initform nil)
   
   (load-into-default :accessor profile-load-into-default :initform nil)

   (put-porter-commands-in-history :accessor profile-put-porter-commands-in-history :initform t)

   (show-mouse-select-commands :accessor profile-show-mouse-select-commands :initform nil)

   (ask-before-transmitting-large-files :accessor profile-ask-before-transmitting-large-files :initform nil)

   (editor-eval-maintain-axioms  :accessor profile-editor-eval-maintain-axioms :initform nil)

   (time-all-operations  :accessor profile-time-all-operations :initform nil)

   ;;;
   ;;;
   ;;;

   (width :accessor profile-width :initform 1100)

   (height :accessor profile-height :initform 900)

   ;;;
   ;;;
   ;;;   
   
   (last-load-directory :accessor profile-last-load-directory :initform (user-directory))

   (last-editor-directory :accessor profile-last-editor-directory :initform (user-directory))

   (last-image-directory :accessor profile-last-image-directory :initform (user-directory))


   ;;;
   ;;;
   ;;;   

   (overwrite-font-size :accessor profile-overwrite-font-size :initform nil)

   (tabs-font :accessor profile-tabs-font :initform (get-helvetica-bold-font))
   
   (button-font :accessor profile-button-font :initform (get-helvetica-font))

   (racer-button-font :accessor profile-racer-button-font :initform (get-helvetica-bold-font))
   
   (checkbox-font :accessor profile-checkbox-font :initform (get-helvetica-font))

   (radiobox-font :accessor profile-radiobox-font :initform (get-helvetica-font))

   (title-font :accessor profile-title-font :initform (get-helvetica-font))

   (graph-font :accessor profile-graph-font :initform (get-helvetica-font))

   (info-font :accessor profile-info-font :initform (get-helvetica-bold-font))

   (list-font :accessor profile-list-font :initform (get-system-font))

   (shell-font :accessor profile-shell-font :initform 
               #+:lispworks6
               nil
               #-:lispworks6
               (get-system-large-font))
   
   ;;;
   ;;;
   ;;;


   (show-progress-bar  :accessor profile-show-progress-bar :initform t)

   (show-status-led :accessor profile-show-status-led :initform nil)
   
   (show-status-pane :accessor profile-show-status-pane :initform t)
   
   (show-info-pane :accessor profile-show-info-pane :initform t)
   
   ;;;
   ;;;
   ;;;

   (use-logfile :accessor profile-use-logfile :initform nil)

   (use-logbuffer :accessor profile-use-logbuffer :initform t)

   (sparql-pattern :accessor profile-sparql-pattern :initform "(sparql-retrieve ~S)")

   (porter-logfile :accessor profile-porter-logfile :initform  (sirius-logfile-file))

   (editor-configfile :accessor profile-editor-configfile :initform (sirius-editor-configfile))

   ;;;
   ;;; Hilfsvariablen
   ;;; 

   (taxonomy-pane-command :initform nil)

   (role-hierarchy-pane-command :initform nil)

   (abox-graph-pane-command :initform nil)

   (graph-drawing-aborted :accessor profile-graph-drawing-aborted :initform nil)

   ;;;
   ;;;
   ;;;

   (busy-counter :accessor profile-busy-counter :initform 0)

   (updating-counter :accessor profile-updating-counter :initform 0)

   (dont-update-server-pane :accessor dont-update-server-pane :initform nil)

   (update-queue :accessor profile-update-queue :initform nil)

   (update-queue-last-pointer :accessor profile-update-queue-last-pointer :initform nil)

   ;;;
   ;;; Profile Characteristics 
   ;;; 
   
   (profile-uses-internal-racer-p :accessor profile-uses-internal-racer-p :initform nil :initarg :uses-internal-racer-p)
   
   (default :accessor profile-default :initarg :default :initform nil)

   (auto-connect :accessor profile-auto-connect :initarg :auto-connect :initform t)
   
   (host :accessor profile-host :initform "localhost" :initarg :host)

   (port :accessor profile-port :initform "8088" :initarg :port)

   (control-port :accessor profile-control-port :initform "8089" :initarg :control-port)

   (dig-port :accessor profile-dig-port :initform "8080" :initarg :dig-port)

   (open-connection-timeout :accessor profile-open-connection-timeout :initform +open-connection-timeout+)

   (close-connection-timeout :accessor profile-close-connection-timeout :initform +close-connection-timeout+)

   (acquire-server-info-timeout :accessor profile-acquire-server-info-timeout :initform +acquire-server-info-timeout+)

   (wait-for-started-server-before-connect-time  :accessor profile-wait-for-started-server-before-connect-time :initform 
                                                 +wait-for-started-server-before-connect-timeout+)


   ;;;
   ;;; Connection Specific 
   ;;;

   (log-stream :accessor profile-log-stream :initform nil 
               :not-persistent)
  
   (socket :accessor profile-socket :initform nil  
           :not-persistent)

   (socket-lock :accessor profile-socket-lock :initform (make-lock)
                :not-persistent)

   (progress-socket :accessor profile-progress-socket :initform nil  
                    :not-persistent)

   (last-progress-value :accessor profile-last-progress-value :initform nil  
                        :not-persistent)

   (abort-requested-p :accessor profile-abort-requested-p :initform nil  
                      :not-persistent)
   
   (abort-confirmed-p :accessor profile-abort-confirmed-p :initform nil  
                      :not-persistent)

   (evaluation-process :accessor profile-evaluation-process :initform nil  
                      :not-persistent)   

   (case :accessor profile-case :initform :unknown 
     :not-persistent)

   (alisp-racer :accessor profile-alisp-racer :initform :unknown 
                :not-persistent)

   (server-description :accessor profile-server-description :initform nil 
                       :not-persistent)
   
   (verbose-server-description :accessor profile-verbose-server-description :initform nil 
                               :not-persistent)

   (racer-version :accessor profile-racer-version :initform :unknown
                  :not-persistent)

   (connection-died :accessor profile-connection-died :initform nil 
                    :not-persistent)

   (request-counter :accessor profile-request-counter :initform 0  
                    :not-persistent)

   (processing-command :accessor profile-processing-command :initform nil 
                       :not-persistent)

   (current-command-status :accessor profile-current-command-status :initform :ready
                           :not-persistent)

   (current-communication-status :accessor profile-current-communication-status :initform nil 
                                 :not-persistent)
   
   (current-answer :accessor profile-current-answer :initform "" 
                   :not-persistent)

   (current-cache-hit :accessor profile-current-cache-hit :initform nil
                      :not-persistent)

   
   ;;;
   ;;; Startup Options for RacerPro 
   ;;; 

   (auto-start-if-default :accessor profile-auto-start-if-default 
                          :initform #+:racer-with-sirius nil 
                          #-:racer-with-sirius t) 
   ;; (connect-after-start :accessor profile-connect-after-start :initform t)
   (unsafe :accessor profile-unsafe :initform t)
   (debug :accessor profile-debug :initform nil)
   (no-http-console-log :accessor profile-no-http-console-log :initform nil)
   (verbose :accessor profile-verbose :initform t)
   (dig-1-1 :accessor profile-dig-1-1 :initform nil)
   (owllink :accessor profile-owllink :initform nil)
  
   (racerpro-executable :accessor profile-racerpro-executable 
                        :initform (find-racerpro-executable))

   (start-in-terminal :accessor profile-start-in-terminal :initform t)

   (logging-file :accessor profile-logging-file :initform "") ; file! 
   (temp-directory :accessor profile-temp-directory :initform "")
   (license-file :accessor profile-license-file :initform "")

   (startup-arguments :accessor profile-startup-arguments :initform "")

   (kill-started-server-on-exit :accessor profile-kill-started-server-on-exit :initform t) 

   (server-started :accessor profile-server-started :initform nil)

   ;;; 
   ;;; Session Context 
   ;;;

   (history :accessor profile-history :initform nil
            :not-persistent)

   (history-position :accessor profile-history-position :initform 1
                     :not-persistent)

   (commands :accessor profile-commands :initform nil
             :not-persistent)

   (n :accessor profile-n :initform 0
      :not-persistent)
   
   (counter :accessor profile-counter :initform 0
            :not-persistent)
   
   (current-prompt :accessor profile-current-prompt :initform nil
                   :not-persistent)

   (prompt-ready-p :accessor profile-prompt-ready-p :initform t
                   :not-persistent)
   
   (current-prompt-point :accessor profile-current-prompt-point :initform nil
                         :not-persistent)

   (shell-content :accessor shell-content :initform ""
                  :not-persistent)
      
   ;;;
   ;;; Status Display 
   ;;; 

   (active-tab :accessor profile-active-tab :initform :server
               :not-persistent)


   ;;;
   ;;;
   ;;;
   
   (current-concept :accessor profile-current-concept :initform nil
                    :not-persistent)

   (all-selected-concepts-hash :accessor profile-all-selected-concepts-hash 
                               :initform (make-hash-table :size 1000 :rehash-size 2.0)
                               :not-persistent)      
   
   ;;;
   ;;;
   ;;;
   
   (current-role :accessor profile-current-role :initform nil
                 :not-persistent)

   (all-selected-roles-hash :accessor profile-all-selected-roles-hash 
                            :initform  (make-hash-table :size 1000 :rehash-size 2.0 :test #'equal)
                            :not-persistent)

   ;;;
   ;;;
   ;;;

   (current-individual :accessor profile-current-individual :initform nil
                       :not-persistent)
   
   (all-selected-individuals-hash :accessor profile-all-selected-individuals-hash 
                                  :initform (make-hash-table :size 1000 :rehash-size 2.0)
                                  :not-persistent)
   
   ;;;
   ;;;
   ;;; 


   (current-assertion :accessor profile-current-assertion :initform nil
                      :not-persistent)
   
   (all-selected-assertions-hash :accessor profile-all-selected-assertions-hash 
                                 :initform (make-hash-table :size 1000 :rehash-size 2.0 :test #'equal)
                                 :not-persistent)
   
   ;;;
   ;;;
   ;;; 

   (current-query-result :accessor profile-current-query-result :initform nil
                         :not-persistent)
   
   ;;;
   ;;;
   ;;;

   (sirius-current-tbox :accessor profile-sirius-current-tbox 
                        :initform 
                        #+:racer-server (current-tbox)
                        #-:racer-server nil
                        :not-persistent)

   (sirius-current-abox :accessor profile-sirius-current-abox
                        :initform 
                        #+:racer-server (current-tbox) 
                        #-:racer-server nil
                        :not-persistent)

   (sirius-current-namespace :accessor profile-sirius-current-namespace 
                             :initform 
                             #+:racer-server (get-namespace-prefix (current-tbox))
                             #-:racer-server nil
                             :not-persistent)

   (sirius-current-namespace-prefixes :accessor profile-sirius-current-namespace-prefixes
                                      :initform  nil
                                      :not-persistent)

   (namespace-2-prefix-hash :accessor profile-namespace-2-prefix-hash 
                            :initform (make-hash-table :test #'equal)
                            :not-persistent)

   (prefix-2-namespace-hash :accessor profile-prefix-2-namespace-hash 
                            :initform (make-hash-table :test #'equal)
                            :not-persistent)
  
   ;;;
   ;;; Hashes 
   ;;; 
   
   (answer-hash :accessor profile-answer-hash 
                :initform (make-hash-table :size 10000 :test 'equal :rehash-size 2.0)
                :not-persistent)

   ;;;
   ;;; Options 
   ;;;
   
   (autofocus :accessor profile-autofocus :initform nil 
              :initarg :autofocus)

   (freeze-graph :accessor profile-freeze-graph :initform t 
                 :initarg :profile-freeze-graph)

   (remove-urls :accessor profile-remove-urls :initform t
                :initarg :remove-urls)

   (selected-first :accessor selected-first :initform nil
                   :initarg :selected-first)

   (selected-only :accessor selected-only :initform nil
                  :initarg :selected-only)

   (maintain-arguments :accessor profile-maintain-arguments :initform nil
                       :initarg :maintain-arguments)

   (show-bottom :accessor profile-show-bottom :initform nil
                :initarg :show-bottom)

   (show-top :accessor profile-show-top :initform t
             :initarg :show-top)

   (include-transitive-roles :accessor profile-include-transitive-roles 
                             :initform nil
                             :initarg :include-transitive-roles)

   (show-top-role :accessor profile-show-top-role
                  :initform nil
                  :initarg :show-top-role)


   (only-selected-successors :accessor profile-only-selected-successors
                             :initform nil
                             :initarg :only-selected-successors)
   
   (told-only :accessor profile-told-only
              :initform nil
              :initarg :told-only)

   (node-labels :accessor profile-node-labels
                :initform nil
                :initarg :node-labels)

   (datatype-fillers :accessor profile-datatype-fillers
                     :initform t
                     :initarg :datatype-fillers)
   
   ;;;
   ;;;
   ;;; 

   (current-query-or-rule :accessor profile-current-query-or-rule :initform nil
                          :not-persistent)

   (current-definition :accessor profile-current-definition :initform nil
                       :not-persistent)

   ;;;
   ;;;
   ;;; 

   (new-axiom-type :accessor new-axiom-type :initform nil
                   :not-persistent)

   (current-axiom :accessor profile-current-axiom :initform nil
                  :not-persistent)
   
   (all-selected-axioms-hash :accessor profile-all-selected-axioms-hash 
                             :initform (make-hash-table :size 1000 :rehash-size 2.0 :test #'equal)
                             :not-persistent) 

   (current-reasoner :accessor profile-current-reasoner :initform nil
                     :not-persistent)

   (current-ontology :accessor profile-current-ontology :initform nil
                     :not-persistent)

   #| (simplified-protocol :accessor profile-simplified-protocol :initform nil
                           :not-persistent) |# 

   (current-axiom-types :accessor profile-current-axiom-types :initform '(racer-user::|LogicalAxiom|)
                        :not-persistent)

   ;;;
   ;;; 
   ;;; 

   (query-list-mode :accessor profile-query-list-mode :initform :all
                    :initarg :query-list-mode)

   (rule-list-mode :accessor profile-rule-list-mode :initform :all
                   :initarg :rule-list-mode)
   
   (role-list-mode :accessor profile-role-list-mode :initform :roles
                   :initarg :role-list-mode)

   (concept-list-mode :accessor profile-concept-list-mode :initform :all
                      :initarg :concept-list-mode)

   (assertion-list-mode :accessor profile-assertion-list-mode :initform :concept-assertions
                        :initarg :assertion-list-mode)

   ;;;
   ;;;
   ;;; 

   (taxonomy-kind :accessor profile-taxonomy-kind :initform :graph ;;; wichtig!!!
                  :initarg :taxonomy-kind)

   (abox-graph-kind :accessor profile-abox-graph-kind :initform :tree
                    :initarg :abox-graph-kind)

   (role-hierarchy-kind :accessor profile-role-hierarchy-kind :initform :tree
                        :initarg :role-hierarchy-kind)

   ;;;
   ;;;
   ;;;
   
   (taxonomy-scroll :accessor profile-taxonomy-scroll :initform nil)

   (abox-graph-scroll :accessor profile-abox-graph-scroll :initform nil)

   (role-hierarchy-scroll :accessor profile-role-hierarchy-scroll :initform :nil)

   ;;;
   ;;;
   ;;;

   (taxonomy-root-kind :accessor profile-taxonomy-root-kind :initform :all
                       :initarg :taxonomy-root-kind)

   (abox-graph-root-kind :accessor profile-abox-graph-root-kind :initform :current
                         :initarg :abox-graph-root-kind)

   (role-hierarchy-root-kind :accessor profile-role-hierarchy-root-kind :initform :all
                             :initarg :role-hierarchy-root-kind)

   ;;;
   ;;;
   ;;; 

   (abox-graph-max-depth :accessor profile-abox-graph-max-depth :initform 1
                         :initarg :abox-graph-max-depth)

   (taxonomy-max-depth :accessor profile-taxonomy-max-depth :initform 'unbounded
                       :initarg :taxonomy-max-depth)

   (role-hierarchy-max-depth :accessor profile-role-hierarchy-max-depth :initform 'unbounded
                             :initarg :role-hierarchy-max-depth)
   
   ;;;
   ;;; 
   ;;;

   (role-hierarchy-graph :accessor profile-role-hierarchy-graph :initform nil
                         :not-persistent)

   (role-hierarchy-graph-pane :accessor profile-role-hierarchy-graph-pane :initform nil
                              :not-persistent)

   (role-hierarchy-graph-pane-roots :accessor profile-role-hierarchy-graph-pane-roots :initform nil
                                    :not-persistent)

   (saved-role-hierarchy-graph-pane :accessor saved-profile-role-hierarchy-graph-pane :initform nil
                                    :not-persistent)



   (taxonomy-graph :accessor profile-taxonomy-graph :initform nil
                   :not-persistent)

   (taxonomy-graph-pane :accessor profile-taxonomy-graph-pane :initform nil
                        :not-persistent)

   (taxonomy-graph-pane-roots :accessor profile-taxonomy-graph-pane-roots :initform nil
                              :not-persistent)

   (saved-taxonomy-graph-pane :accessor profile-saved-taxonomy-graph-pane :initform nil
                              :not-persistent)



   (abox-graph :accessor abox-graph :initform nil
               :not-persistent)

   (abox-graph-graph-pane :accessor profile-abox-graph-graph-pane :initform nil
                          :not-persistent)

   (abox-graph-graph-pane-roots :accessor profile-abox-graph-graph-pane-roots :initform nil
                                :not-persistent)

   (saved-abox-graph-graph-pane :accessor profile-saved-abox-graph-graph-pane :initform nil
                                :not-persistent)

   ;;;
   ;;;
   ;;; 
   
   (role-hierarchy-orientation :accessor profile-role-hierarchy-orientation :initform :left-right
                               :initarg :role-hierarchy-orientation)

   (taxonomy-orientation :accessor profile-taxonomy-orientation :initform :left-right
                         :initarg :taxonomy-orientation)

   (abox-graph-orientation :accessor profile-abox-graph-orientation :initform :left-right
                           :initarg :abox-graph-orientation)   
   
   ;;;
   ;;;
   ;;;

   (potential-args :accessor potential-args
                   :initform (make-hash-table :size 1000 :rehash-size 2.0)
                   :not-persistent)
   
   (sirius-star-star-star :accessor sirius-star-star-star :initform nil
                          :not-persistent)

   (sirius-star-star :accessor sirius-star-star :initform nil
                     :not-persistent)

   (sirius-star :accessor sirius-star :initform nil
                :not-persistent)
   
   (sirius-plus-plus-plus :accessor sirius-plus-plus-plus :initform nil
                          :not-persistent)
   
   (sirius-plus-plus :accessor sirius-plus-plus :initform nil
                     :not-persistent)

   (sirius-plus :accessor sirius-plus :initform nil
                :not-persistent)))

;;;
;;;
;;;

(defvar *message-shown* nil)

(defun message-restart-required ()
  (unless *message-shown*
    (setf *message-shown* t)
    (sirius-message "Please use \"OK & Save Profile\" and then restart RacerPorter.")))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +slots+
    ;;; (name <copy-or-nor) 
    ;;; f. copy-profile! 
    '((NAME NIL)

      (WIDTH T)
      (HEIGHT T)

      (LAST-LOAD-DIRECTORY T)
      (LAST-EDITOR-DIRECTORY T)
      (LAST-IMAGE-DIRECTORY T)

      (OVERWRITE-FONT-SIZE T)
      (TABS-FONT T)
      (BUTTON-FONT T)
      (RACER-BUTTON-FONT T)
      (CHECKBOX-FONT T)
      (RADIOBOX-FONT T)
      (TITLE-FONT T)
      (GRAPH-FONT T)
      (INFO-FONT T)
      (LIST-FONT T)
      (SHELL-FONT T)

      (SIRIUS-TEMP-DIRECTORY T)
      (CONFIRM-QUIT T)
      (AUTOSAVE-ON-QUIT T)
      (PRETTY-PRINT-INPUT T)
      (POPUP-MESSAGE-BOX T)
      (LOAD-INTO-DEFAULT T)
      (PUT-PORTER-COMMANDS-IN-HISTORY T)
      (SHOW-MOUSE-SELECT-COMMANDS T)
      (ASK-BEFORE-TRANSMITTING-LARGE-FILES T)
      (EDITOR-EVAL-MAINTAIN-AXIOMS T)
      (TIME-ALL-OPERATIONS T)
      (SHOW-STATUS-LED T)
      (SHOW-PROGRESS-BAR T)
      (LAST-PROGRESS-VALUE NIL)
      (SHOW-INFO-PANE T)
      (SHOW-STATUS-PANE T)      
      (USE-LOGFILE T)
      (USE-LOGBUFFER T)
      (SPARQL-PATTERN T)
      (PORTER-LOGFILE T)
      (EDITOR-CONFIGFILE T)
      (TAXONOMY-PANE-COMMAND NIL)
      (ROLE-HIERARCHY-PANE-COMMAND NIL)
      (ABOX-GRAPH-PANE-COMMAND NIL)
      (GRAPH-DRAWING-ABORTED NIL)
      (PROFILE-USES-INTERNAL-RACER-P T)
      (DEFAULT T)
      (AUTO-CONNECT T)
      (HOST T)
      (PORT T)
      (DIG-PORT T)
      (OPEN-CONNECTION-TIMEOUT T)
      (CLOSE-CONNECTION-TIMEOUT T)
      (ACQUIRE-SERVER-INFO-TIMEOUT T)
      (WAIT-FOR-STARTED-SERVER-BEFORE-CONNECT-TIME T)
      (SOCKET NIL)
      (PROGRESS-SOCKET NIL)
      (ABORT-REQUESTED-P NIL)
      (ABORT-CONFIRMED-P NIL)
      (EVALUATION-PROCESS NIL)
      (SOCKET-LOCK NIL)
      (LOG-STREAM NIL)
      (CASE NIL)
      (ALISP-RACER NIL)
      (SERVER-DESCRIPTION NIL)
      (VERBOSE-SERVER-DESCRIPTION NIL)
      (RACER-VERSION NIL)
      (CONNECTION-DIED NIL)
      (REQUEST-COUNTER NIL)
      (PROCESSING-COMMAND NIL)
      (CURRENT-COMMAND-STATUS NIL)
      (CURRENT-COMMUNICATION-STATUS NIL)
      (CURRENT-ANSWER NIL)
      (CURRENT-CACHE-HIT NIL)
      (AUTO-START-IF-DEFAULT T)
      ;; (CONNECT-AFTER-START T)
      (UNSAFE T)
      (DEBUG T)
      (NO-HTTP-CONSOLE-LOG T)
      (VERBOSE T)
      (DIG-1-1 T)
      (OWLLINK T)
      (RACERPRO-EXECUTABLE T)
      (START-IN-TERMINAL T)
      (LOGGING-FILE T)
      (TEMP-DIRECTORY T)
      (LICENSE-FILE T)
      (STARTUP-ARGUMENTS T)
      (KILL-STARTED-SERVER-ON-EXIT T)
      (SERVER-STARTED NIL)
      (HISTORY NIL)
      (HISTORY-POSITION NIL)
      (COMMANDS NIL)
      (N NIL)
      (COUNTER NIL)
      (CURRENT-PROMPT NIL)
      (CURRENT-PROMPT-POINT NIL)
      (PROMPT-READY-P NIL)
      (SHELL-CONTENT NIL)
      (ACTIVE-TAB NIL)
      (CURRENT-CONCEPT NIL)
      (ALL-SELECTED-CONCEPTS-HASH NIL)
      (CURRENT-ROLE NIL)
      (ALL-SELECTED-ROLES-HASH NIL)
      (CURRENT-INDIVIDUAL NIL)
      (ALL-SELECTED-INDIVIDUALS-HASH NIL)
      (CURRENT-ASSERTION NIL)
      (ALL-SELECTED-ASSERTIONS-HASH NIL)
      (CURRENT-QUERY-RESULT NIL)
      (SIRIUS-CURRENT-TBOX NIL)
      (SIRIUS-CURRENT-ABOX NIL)
      (SIRIUS-CURRENT-NAMESPACE NIL)
      (SIRIUS-CURRENT-NAMESPACE-PREFIXES NIL)
      (NAMESPACE-2-PREFIX-HASH NIL)
      (PREFIX-2-NAMESPACE-HASH NIL)      
      (CURRENT-AXIOM NIL)
      (ALL-SELECTED-AXIOMS-HASH NIL)
      (NEW-AXIOM-TYPE NIL)
      (CURRENT-REASONER NIL)
      (CURRENT-ONTOLOGY NIL)
      ;;(SIMPLIFIED-PROTOCOL NIL)
      (CURRENT-AXIOM-TYPES NIL)
      (ANSWER-HASH NIL)
      (AUTOFOCUS T)
      (FREEZE-GRAPH T)
      (REMOVE-URLS T)
      (SELECTED-FIRST T)
      (SELECTED-ONLY T)
      (MAINTAIN-ARGUMENTS T)
      (SHOW-BOTTOM T)
      (SHOW-TOP T)
      (INCLUDE-TRANSITIVE-ROLES T)
      (SHOW-TOP-ROLE T)
      (ONLY-SELECTED-SUCCESSORS T)
      (TOLD-ONLY T)
      (NODE-LABELS T)
      (DATATYPE-FILLERS T)
      (CURRENT-QUERY-OR-RULE NIL)
      (CURRENT-DEFINITION NIL)
      (QUERY-LIST-MODE T)
      (RULE-LIST-MODE T)
      (ROLE-LIST-MODE T)
      (CONCEPT-LIST-MODE T)
      (ASSERTION-LIST-MODE T)
      (TAXONOMY-KIND T)
      (ABOX-GRAPH-KIND T)
      (ROLE-HIERARCHY-KIND T)
      (TAXONOMY-SCROLL NIL)
      (ABOX-GRAPH-SCROLL NIL)
      (ROLE-HIERARCHY-SCROLL NIL)
      (TAXONOMY-ROOT-KIND T)
      (ABOX-GRAPH-ROOT-KIND T)
      (ROLE-HIERARCHY-ROOT-KIND T)
      (ABOX-GRAPH-MAX-DEPTH T)
      (TAXONOMY-MAX-DEPTH T)
      (ROLE-HIERARCHY-MAX-DEPTH T)
      (ROLE-HIERARCHY-GRAPH NIL)
      (ROLE-HIERARCHY-GRAPH-PANE NIL)
      (ROLE-HIERARCHY-GRAPH-PANE-ROOTS NIL)
      (SAVED-ROLE-HIERARCHY-GRAPH-PANE NIL)
      (TAXONOMY-GRAPH NIL)
      (TAXONOMY-GRAPH-PANE NIL)
      (TAXONOMY-GRAPH-PANE-ROOTS NIL)
      (SAVED-TAXONOMY-GRAPH-PANE NIL)
      (ABOX-GRAPH NIL)
      (ABOX-GRAPH-GRAPH-PANE NIL)
      (ABOX-GRAPH-GRAPH-PANE-ROOTS NIL)
      (SAVED-ABOX-GRAPH-GRAPH-PANE NIL)
      (ROLE-HIERARCHY-ORIENTATION T)
      (TAXONOMY-ORIENTATION T)
      (ABOX-GRAPH-ORIENTATION T)
      (POTENTIAL-ARGS NIL)
      (SIRIUS-STAR-STAR-STAR NIL)
      (SIRIUS-STAR-STAR NIL)
      (SIRIUS-STAR NIL)
      (SIRIUS-PLUS-PLUS-PLUS NIL)
      (SIRIUS-PLUS-PLUS NIL)
      (SIRIUS-PLUS NIL)
      (BUSY-COUNTER NIL)
      (DONT-UPDATE-SERVER-PANE NIL)
      (UPDATING-COUNTER NIL)
      (UPDATE-QUEUE NIL)
      (UPDATE-QUEUE-LAST-POINTER NIL))))


(defmacro with-access-to-profile (profile &body body)
  `(WITH-SLOTS ,(mapcar #'first +slots+) ,profile

     (declare (ignorable ,@(mapcar #'first +slots+)))

     ,@body))

(defmacro with-profile-access (&body body)
  `(with-access-to-profile (active-profile)
     ,@body))


(defmacro with-profile-request (&body body)
  (let ((prof (gensym)))
    `(let ((,prof (active-profile)))
       (unwind-protect
           (without-history-entries
             (with-sirius-app (sirius)
               (setf (button-status sirius) nil)
               (incf (profile-busy-counter ,prof)))
             ,@body)
         (decf (profile-busy-counter ,prof))))))

(defmacro with-socket-lock (&body body)
  `(mp:with-lock ((profile-socket-lock (active-profile)))
     ,@body))

(defmacro with-promoted-lock (&body body)
  `(progn 
     (unlock-lock (profile-socket-lock (active-profile)))
     ,@body))

(defun unlock-socket-lock (profile)
  (unlock-lock (profile-socket-lock profile)))

(defmethod create-new-profile-based-on ((profile profile))
  (let ((new 
         (make-instance 'profile
                        :name (format nil "Copy of ~A" 
                                      (profile-name profile)))))
      
    (dolist (slot +slots+)
      (let ((slot (first slot))
            (copy-p (second slot)))
        (when copy-p
          (setf (slot-value new slot)
                (let ((slot (slot-value profile slot)))
                  (typecase slot
                    (symbol slot)
                    (list (copy-tree slot))
                    (otherwise slot)))))))

    new))


(defmethod copy-profile ((profile profile))
  (let ((new  
         (make-instance 'profile
                        :name (profile-name profile))))
      
    (dolist (slot +slots+)
      (let ((slot (first slot))
            (copy-p (second slot)))
        (when copy-p
          (setf (slot-value new slot)
                (let ((slot (slot-value profile slot)))
                  (typecase slot
                    (symbol slot)
                    (list (copy-tree slot))
                    (otherwise slot)))))))

    new))

(defun copy-all-profiles ()
  (setf *profiles-to-save* 
        (mapcar #'copy-profile 
                *sirius-profiles*)))


;;;
;;;
;;;


(defun reset-profile (profile &key
                              clear-history-p  ; Clear History Button
                              clear-and-resync-p  ; Clear Caches Button 
                              
                              before-state-changing-shell-command-p 
                              
                              before-open-server-connection-p 
                              after-close-server-connection-p 
                              
                              cancel-request-p ; Cancle ABox / TBox / Role Hierarchy Graph 

                              )
  
  (let ((connection-specific-p (or after-close-server-connection-p before-open-server-connection-p))
        (request-specific-p (or after-close-server-connection-p cancel-request-p before-open-server-connection-p))
        (history-specific-p (or clear-history-p
                                ;;; before-open-server-connection-p
                                ;;; nein - sonst funktioniert reaktivierung von profiles nicht mehr!
                                ))

        ;;; wird fast nie ausgefuehrt, denn Clear & Resync deselektiert nur wenn notwendig (mehr "Intelligenz") 
        ;;; guckt, ob Element noch existiert etc.  

        (state-display-specific-p (or ;;; before-open-server-connection-p
                                      ;;; falsch - sonst funktioniert reaktivierung von profiles nicht mehr!
                                      ))
       
        (cache-specific-p (or clear-and-resync-p before-open-server-connection-p
                              before-state-changing-shell-command-p
                              cancel-request-p)))

    (with-access-to-profile profile

      #| 

 ? (TAXONOMY-PANE-COMMAND NIL)
 ? (ROLE-HIERARCHY-PANE-COMMAND NIL)
 ? (ABOX-GRAPH-PANE-COMMAND NIL)
 ? (GRAPH-DRAWING-ABORTED NIL)
 c (SOCKET NIL)
 c (CASE NIL)
 c (ALISP-RACER NIL)
 c (SERVER-DESCRIPTION NIL)
 c (VERBOSE-SERVER-DESCRIPTION NIL)
 c (RACER-VERSION NIL)
 r (CONNECTION-DIED NIL)
 r (REQUEST-COUNTER NIL)
 r (PROCESSING-COMMAND NIL)
 r (CURRENT-COMMAND-STATUS NIL)
 r (CURRENT-COMMUNICATION-STATUS NIL)
 r (CURRENT-ANSWER NIL)
 r (CURRENT-CACHE-HIT NIL)
 c (SERVER-STARTED NIL)
 h (HISTORY NIL)
 h (HISTORY-POSITION NIL)
 h (COMMANDS NIL)
 h (N NIL)
 h (COUNTER NIL)
 h (CURRENT-PROMPT NIL)
 h (CURRENT-PROMPT-POINT NIL)
 h (SHELL-CONTENT NIL)
 ? (ACTIVE-TAB NIL)
 s (CURRENT-CONCEPT NIL)
 s (ALL-SELECTED-CONCEPTS-HASH NIL)
 S (CURRENT-ROLE NIL)
 S (ALL-SELECTED-ROLES-HASH NIL)
 S (CURRENT-INDIVIDUAL NIL)
 S (ALL-SELECTED-INDIVIDUALS-HASH NIL)
 S (CURRENT-ASSERTION NIL)
 S (ALL-SELECTED-ASSERTIONS-HASH NIL)
 S (CURRENT-QUERY-RESULT NIL)
 S (SIRIUS-CURRENT-TBOX NIL)
 S (SIRIUS-CURRENT-ABOX NIL)
 S (SIRIUS-CURRENT-NAMESPACE NIL)
 S (SIRIUS-CURRENT-NAMESPACE-PREFIXES NIL)
 ca (ANSWER-HASH NIL)
 S (CURRENT-QUERY-OR-RULE NIL)
 S (CURRENT-DEFINITION NIL)
 s (TAXONOMY-SCROLL NIL)
 s (ABOX-GRAPH-SCROLL NIL)
 s (ROLE-HIERARCHY-SCROLL NIL)
 ca (ROLE-HIERARCHY-GRAPH NIL)
 ca (ROLE-HIERARCHY-GRAPH-PANE NIL)
 ca (TAXONOMY-GRAPH NIL)
 ca (TAXONOMY-GRAPH-PANE NIL)
 ca (ABOX-GRAPH NIL)
 ca (ABOX-GRAPH-GRAPH-PANE NIL)
 h (POTENTIAL-ARGS NIL)
 h (SIRIUS-STAR-STAR-STAR NIL)
 h (SIRIUS-STAR-STAR NIL)
 h (SIRIUS-STAR NIL)
 h (SIRIUS-PLUS-PLUS-PLUS NIL)
 h (SIRIUS-PLUS-PLUS NIL)
 h (SIRIUS-PLUS NIL))

|# 
      ;;;
      ;;;

      (when connection-specific-p 
        (dolist (slot '(SOCKET
                        PROGRESS-SOCKET
                        ABORT-REQUESTED-P
                        ABORT-CONFIRMED-P
                        EVALUATION-PROCESS
                        CASE
                        ALISP-RACER
                        SERVER-DESCRIPTION
                        VERBOSE-SERVER-DESCRIPTION
                        RACER-VERSION
                        ;SERVER-STARTED
                        ;;SIMPLIFIED-PROTOCOL
                        ))
          (slot-makunbound profile slot)))
    
      ;;;
      ;;;
      ;;;

      (when request-specific-p 
        (dolist (slot '(CONNECTION-DIED
                        REQUEST-COUNTER
                        PROCESSING-COMMAND
                        CURRENT-COMMAND-STATUS
                        CURRENT-COMMUNICATION-STATUS
                        CURRENT-ANSWER
                        CURRENT-CACHE-HIT))
          (slot-makunbound profile slot)))

      ;;;
      ;;;
      ;;;

      (when history-specific-p

        (reset-potential-args)

        (dolist (slot '(HISTORY
                        HISTORY-POSITION
                        COMMANDS
                        N
                        COUNTER
                        CURRENT-PROMPT
                        CURRENT-PROMPT-POINT
                        SHELL-CONTENT
                        SIRIUS-STAR-STAR-STAR
                        SIRIUS-STAR-STAR
                        SIRIUS-STAR
                        SIRIUS-PLUS-PLUS-PLUS
                        SIRIUS-PLUS-PLUS
                        SIRIUS-PLUS))
          (slot-makunbound profile slot)))

      ;;;
      ;;;
      ;;;

      ;;; wird nicht benoetigt!
      (when state-display-specific-p
        (dolist (slot '(CURRENT-CONCEPT
                        ALL-SELECTED-CONCEPTS-HASH
                        CURRENT-ROLE
                        ALL-SELECTED-ROLES-HASH
                        CURRENT-INDIVIDUAL
                        ALL-SELECTED-INDIVIDUALS-HASH
                        CURRENT-ASSERTION
                        ALL-SELECTED-ASSERTIONS-HASH
                        CURRENT-AXIOM
                        ALL-SELECTED-AXIOMS-HASH
                        CURRENT-QUERY-RESULT
                        SIRIUS-CURRENT-TBOX
                        SIRIUS-CURRENT-ABOX
                        SIRIUS-CURRENT-NAMESPACE
                        SIRIUS-CURRENT-NAMESPACE-PREFIXES
                        CURRENT-QUERY-OR-RULE
                        CURRENT-DEFINITION
                        ;TAXONOMY-SCROLL
                        ;ABOX-GRAPH-SCROLL
                        ;ROLE-HIERARCHY-SCROLL
                        ;TAXONOMY-PANE-COMMAND
                        ;ROLE-HIERARCHY-PANE-COMMAND
                        ;ABOX-GRAPH-PANE-COMMAND
                        ;GRAPH-DRAWING-ABORTED
                        ))
          (slot-makunbound profile slot)))
    
      ;;;
      ;;;
      ;;;

      (when cache-specific-p 

        (dolist (slot '(ANSWER-HASH 

                        ROLE-HIERARCHY-GRAPH
                        ROLE-HIERARCHY-GRAPH-PANE
                        ROLE-HIERARCHY-GRAPH-PANE-ROOTS
                        SAVED-ROLE-HIERARCHY-GRAPH-PANE

                        TAXONOMY-GRAPH
                        TAXONOMY-GRAPH-PANE
                        TAXONOMY-GRAPH-PANE-ROOTS
                        SAVED-TAXONOMY-GRAPH-PANE 
                        
                        ABOX-GRAPH
                        ABOX-GRAPH-GRAPH-PANE
                        ABOX-GRAPH-GRAPH-PANE-ROOTS
                        SAVED-ABOX-GRAPH-GRAPH-PANE
      
                        busy-counter
                        dont-update-server-pane

                        ))
        
          (slot-makunbound profile slot)))

      ;;;
      ;;;
      ;;;

      (initialize-instance profile))))

(defun reset-active-profile (&rest args)
  (apply #'reset-profile (active-profile) args))
                  

;;;
;;;
;;; 
  
(defun profile-ip (profile)
  (if (uses-internal-racer-p profile)
      "localhost"
    (if (socket-alive-p (profile-socket profile))
        (comm::ip-address-string 
         (comm:get-socket-address
          (comm:socket-stream-socket (sirius::profile-socket profile))))
      (profile-host profile))))

;;;
;;;
;;;
        
(defun get-default-profiles ()
  (let ((profiles 
         (reverse 
          (list 
           (make-instance 'profile
                          :name 

                          #+:racer-with-sirius
                          "4: Internal Racer Engine / Small TBoxes, Small ABoxes" 
                          #-:racer-with-sirius
                          "4: Localhost / Small TBoxes, Small ABoxes" 

                          :sirius-temp-directory (sirius-temp-directory)
                          :default nil
                          :auto-connect t
                          :host "localhost"
                          :port 
                          #+:racer-with-sirius 
                          ""
                          #-:racer-with-sirius 
                          "8088"
                              
                          :uses-internal-racer-p 
                          #+:racer-with-sirius t
                          #-:racer-with-sirius nil
                              
                          :autofocus t
                          :show-bottom nil
                          :show-top t

                          :include-transitive-roles t
                          :told-only nil
                          :node-labels nil

                          :query-list-mode :all
                          :rule-list-mode :all
                          :role-list-mode :roles

                          :concept-list-mode :all
                          :assertion-list-mode :concept-assertions
                          :taxonomy-kind :graph
                          :abox-graph-kind :tree
                          :role-hierarchy-kind :tree
                          :taxonomy-root-kind :all
                          :abox-graph-root-kind :all
                          :role-hierarchy-root-kind :all
                          :abox-graph-max-depth 'unbounded
                          :taxonomy-max-depth 'unbounded
                          :role-hierarchy-max-depth 'unbounded
                          )

           (make-instance 'profile
                              
                          :name
                          #+:racer-with-sirius
                          "3: Internal Racer Engine / Small TBoxes, Big ABoxes" 
                          #-:racer-with-sirius
                          "3: Localhost / Small TBoxes, Big ABoxes" 
                               
                          :sirius-temp-directory (sirius-temp-directory)
                          :default nil
                          :auto-connect t
                          :host "localhost"
                          :port
                          #+:racer-with-sirius
                          ""
                          #-:racer-with-sirius
                          "8088"

                          :uses-internal-racer-p 
                          #+:racer-with-sirius t
                          #-:racer-with-sirius nil


                          :autofocus t
                          :show-bottom nil
                          :show-top t

                          :include-transitive-roles t
                          :told-only nil
                          :node-labels nil

                          :query-list-mode :all
                          :rule-list-mode :all
                          :role-list-mode :roles

                          :concept-list-mode :all
                          :assertion-list-mode :concept-assertions
                          :taxonomy-kind :graph
                          :abox-graph-kind :tree
                          :role-hierarchy-kind :tree
                          :taxonomy-root-kind :all
                          :abox-graph-root-kind :current
                          :role-hierarchy-root-kind :all
                          :abox-graph-max-depth 0
                          :taxonomy-max-depth 'unbounded
                          :role-hierarchy-max-depth 'unbounded
                          )

           (make-instance 'profile

                          :name
                          #+:racer-with-sirius
                          "2: Internal Racer Engine / Big TBoxes, Small ABoxes" 
                          #-:racer-with-sirius
                          "2: Localhost / Big TBoxes, Small ABoxes" 


                          :sirius-temp-directory (sirius-temp-directory)
                          :default nil
                          :auto-connect t
                          :host "localhost"
                          :port
                          #+:racer-with-sirius
                          ""
                          #-:racer-with-sirius
                          "8088"

                          :uses-internal-racer-p 
                          #+:racer-with-sirius t
                          #-:racer-with-sirius nil

                          :autofocus t
                          :show-bottom nil
                          :show-top t

                          :include-transitive-roles t
                          :told-only nil
                          :node-labels nil

                          :query-list-mode :all
                          :rule-list-mode :all
                          :role-list-mode :roles

                          :concept-list-mode :all
                          :assertion-list-mode :concept-assertions
                          :taxonomy-kind :tree
                          :abox-graph-kind :tree
                          :role-hierarchy-kind :tree
                          :taxonomy-root-kind :current
                          :abox-graph-root-kind :all
                          :role-hierarchy-root-kind :all
                          :abox-graph-max-depth 'unbounded
                          :taxonomy-max-depth 0
                          :role-hierarchy-max-depth 0
                          )

           (make-instance 'profile
                          :name
                          #+:racer-with-sirius
                          "1: Internal Racer Engine / Big TBoxes, Big ABoxes" 
                          #-:racer-with-sirius
                          "1: Localhost / Big TBoxes, Big ABoxes" 


                          :sirius-temp-directory (sirius-temp-directory)
                          :default t
                          :auto-connect t
                          :host "localhost"
                              
                          :port 
                          #+:racer-with-sirius
                          ""
                          #-:racer-with-sirius
                          "8088"

                          :uses-internal-racer-p 
                          #+:racer-with-sirius t
                          #-:racer-with-sirius nil

                          :autofocus t
                          :show-bottom nil
                          :show-top t

                          :include-transitive-roles t
                          :told-only nil
                          :node-labels nil

                          :query-list-mode :all
                          :rule-list-mode :all
                          :role-list-mode :roles

                          :concept-list-mode :all
                          :assertion-list-mode :concept-assertions
                          :taxonomy-kind :tree
                          :abox-graph-kind :tree
                          :role-hierarchy-kind :tree
                          :taxonomy-root-kind :current
                          :abox-graph-root-kind :current
                          :role-hierarchy-root-kind :all
                          :abox-graph-max-depth 0
                          :taxonomy-max-depth 0
                          :role-hierarchy-max-depth 0
                          )))))

    profiles))


;;;
;;;
;;;

(defun local-connection-p (&optional (profile *active-profile*))
  (or (uses-internal-racer-p profile)
      (member (profile-host profile)
              '("localhost" "127.0.0.1")
              :test #'string-equal)
      (and (profile-socket profile)
           (= (comm:get-socket-peer-address
               (comm:socket-stream-socket (profile-socket profile)))
              (comm:get-socket-address
               (comm:socket-stream-socket (profile-socket profile)))))))

(defun uses-internal-racer-p (&optional (profile *active-profile*))
  (profile-uses-internal-racer-p profile))

(defun open-logfile-if-needed (profile)
  (with-access-to-profile profile
    (when (and (not log-stream)
               use-logfile
               (not (blank-line-p porter-logfile)))
      (setf log-stream 
            (open porter-logfile 
                  :direction :output
                  :if-exists :append
                  :if-does-not-exist :create)))))

(defun make-active-profile (profile)
  (setf *active-profile* profile
        *interface-active-profile* profile)
  (open-logfile-if-needed profile)

  (read-editor-config)

  (with-profile-access
    (setf active-tab :server)))

(defun active-profile ()
  *active-profile*)

(defmacro with-profile ((profile) &body body)
  `(let* ((*active-profile* ,profile))

     (cond ((not *active-profile*)
            (report-error "No active profile found!"))

           (t ,@body))))

;;;
;;;
;;; 

(defun get-server-description (&optional (profile *active-profile*) verbose-p)
  (labels ((do-it () 
             (concatenate 'string
                          (if (uses-internal-racer-p)
                              (format nil "Internal ")
                            "")

                          (if verbose-p 

                              (if (not (string-equal (profile-racer-version profile)
                                                     "unknown"))
                                  (format nil "RacerPro ~A running on ~A:~A (case: ~A)"
                                          (profile-racer-version profile)
                                          (profile-host profile)
                                          (profile-port profile)
                                          (profile-case profile))
                                (format nil "RacerPro running on ~A:~A"
                                        (profile-host profile)
                                        (profile-port profile)))

                            (if (not (string-equal (profile-racer-version profile)
                                                   "unknown"))
                                (format nil "RacerPro V. ~A @ ~A:~A" 
                                        (profile-racer-version profile)
                                        (profile-host profile)
                                        (profile-port profile))
                              (format nil "RacerPro @ ~A:~A" 
                                      (profile-host profile)
                                      (profile-port profile)))))))             

    (if verbose-p 
        (or (profile-verbose-server-description profile)
            (setf (profile-verbose-server-description profile) (do-it)))
      (or (profile-server-description profile)
          (setf (profile-server-description profile) (do-it))))))

;;;
;;;
;;;

(defun load-sirius-profiles (&optional (fn (sirius-profiles-file)))
  (cond ((and fn 
              (probe-file fn)
              (file-is-readable-p fn))
         
         (setf *sirius-profiles*
               (or (load-persistent-object fn)
                   (progn 
                     (report-error
		      (format nil "Sorry, profile file 
~A 
is probably for another version of RacerPorter. 
Will use my default profiles instead." fn))
                     (setf *profiles-loaded-from* nil
                           *warn-before-overwriting-profiles* t)

                     (get-default-profiles))))
                     
         (setf *profiles-loaded-from* fn))
        
        (t (setf *profiles-loaded-from* nil)
           (setf *sirius-profiles* 
                 (get-default-profiles))))

  #-:racer-with-sirius
  (setf *sirius-profiles*
        (delete-if #'uses-internal-racer-p *sirius-profiles*))
  
  (copy-all-profiles)

  (let ((default
         (or (find-if #'profile-default *sirius-profiles*)
             (first *sirius-profiles*)
             (first (get-default-profiles)))))

    (make-active-profile default)
  
    (when (or (not (profile-sirius-temp-directory default))
              (blank-line-p (profile-sirius-temp-directory default)))

      (sirius-message "Cannot find RacerPorter default temp directory ~A! 
Please press OK to continue, and specify the \"RacerPorter Temp Directory\"  
using the \"Edit Profile...\" dialog." 
                       (or (profile-sirius-temp-directory default)
                           (sirius-temp-directory))))))
      
(defun save-sirius-profiles (&optional (fn (sirius-profiles-file)))
  (let* ((profiles (copy-tree *profiles-to-save*))
         (dir (pathname-directory fn)))

    (when (consp dir)
      (let ((res nil)) 
        (mapl #'(lambda (x) 
                  (when (cdr x)
                    (push (reverse x) res)))
              (reverse dir))

        (dolist (dir res)
          (let ((dir (make-pathname
                      :host (pathname-host fn)
                      :directory dir)))
            (unless (probe-file dir)
              (system:make-directory dir))))))
    
    (make-object-persistent profiles fn)))

;;;
;;; 
;;;

(defvar *editor-profile* nil)

(defvar *edit* nil)

(defvar *canceled?* nil)

(defvar *save?* nil)

;;;
;;;
;;;

(defmethod executable-specified-p ((profile profile))
  (and (profile-racerpro-executable profile)
       (not (blank-line-p (profile-racerpro-executable profile)))
       (probe-file (profile-racerpro-executable profile))))

;;;
;;;
;;; 

(defun pe-title-font (&optional (n *global-font-size*))
  (apply #'gp:make-font-description (get-helvetica-font n)))

(defun pe-title-bold-font (&optional (n *global-font-size*))
  (apply #'gp:make-font-description (get-helvetica-bold-font n)))

(defun pe-font (&optional (n *global-font-size*))
  (apply #'gp:make-font-description (get-helvetica-font n)))

;;;
;;;
;;;

(define-interface profile-manager-dialog ()
  ()

  (:panes 

   (item text-input-pane
         :title "Profile Name"
         :title-position :top
         
         :title-font *pe-title-font*
         :font *pe-font*
         
         :width '(:character 30)
         :text (format nil "~A" (profile-name *editor-profile*)))   

   (confirm-quit check-button
                 :title-font *pe-title-font*
                 :font *pe-font*
                 :text "Confirm Quit"
                 :selected (profile-confirm-quit *editor-profile*))

   (autosave-on-quit check-button
                        :text "Auto-Save Profiles on Quit"
                        :title-font *pe-title-font*
                        :font *pe-font*             
                        :selected (profile-autosave-on-quit *editor-profile*))

   (pretty-print check-button
                 :text "Don't Modify User Input"
                 :title-font *pe-title-font*
                 :font *pe-font*
                 :selected (not (profile-pretty-print-input *editor-profile*)))

   (popup-message-box check-button
                      :text "Popup Message Box for Large Results"
                      :title-font *pe-title-font*
                      :font *pe-font*
                      :selected (profile-popup-message-box *editor-profile*))
   
   (load-into-default check-button
                      :text "Load Into DEFAULT KB"
                      :title-font *pe-title-font*
                      :font *pe-font*
                      :selected (profile-load-into-default *editor-profile*))
   
   (porter-commands-in-history check-button
                               :text "Put Button Commands in Shell History"
                               :title-font *pe-title-font*
                               :font *pe-font*
                               :selected (profile-put-porter-commands-in-history *editor-profile*))

   (show-mouse-select-commands check-button
                               :text "Show Mouse Select Commands"
                               :title-font *pe-title-font*
                               :font *pe-font*
                               :selected (profile-show-mouse-select-commands *editor-profile*))


   (ask-before-transmit check-button
                        :text "Ask Before Transmitting Large Files"
                        :title-font *pe-title-font*
                        :font *pe-font*
                        :selected (profile-ask-before-transmitting-large-files *editor-profile*))

   (editor-eval-maintain-axioms check-button
                                :text "Create Axioms by Default"
                                :title-font *pe-title-font*
                                :font *pe-font*
                                :selected (profile-editor-eval-maintain-axioms *editor-profile*))
      
   (time-all check-button
             :text "Time all Operations"
             :title-font *pe-title-font*
             :font *pe-font*
             :selected (profile-time-all-operations *editor-profile*))

   (show-status-led check-button
                    :text "Show Status LED"
                    :title-font *pe-title-font*
                    :font *pe-font*
                    :selected (profile-show-status-led *editor-profile*))

   (show-progress-bar check-button
                      :text "Show Progress Bar"
                      :title-font *pe-title-font*
                      :font *pe-font*
                      :selected (profile-show-progress-bar *editor-profile*))

   (use-logfile check-button
                :text "Use Porter Logfile"
                :title-font *pe-title-font*
                :font *pe-font*
                :selected (profile-use-logfile *editor-profile*))

   (use-logbuffer check-button
                  :text "Use Log Tab"
                  :title-font *pe-title-font*
                  :font *pe-font*
                  :selected (profile-use-logbuffer *editor-profile*))
   
   
   (sparql-pattern text-input-pane
                   :title "SPARQL Pattern"
                   :title-font *pe-title-font*
                   :font *pe-font*
                    
                   :title-position :top
                   :width '(:character 30)
                   :text (profile-sparql-pattern *editor-profile*))

   (porter-logfile text-input-pane
                   :title "RacerPorter Logfile"
                   :title-font *pe-title-font*
                   :font *pe-font*
                   :title-position :top
                   :width '(:character 30)
                   :text (if (profile-porter-logfile *editor-profile*)
                             (format nil "~A" (profile-porter-logfile *editor-profile*))
                           ""))

   (editor-configfile text-input-pane
                   :title "RacerEditor Config File"
                   :title-font *pe-title-font*
                   :font *pe-font*
                   :title-position :top
                   :width '(:character 30)
                   :text (if (profile-editor-configfile *editor-profile*)
                             (format nil "~A" (profile-editor-configfile *editor-profile*))
                           ""))


   (sirius-temp-dir text-input-pane
                    :title "RacerPorter Temp Directory"
                    :title-font *pe-title-font*
                    :font *pe-font*
                    :title-position :top
                    :width '(:character 30)
                    :text (if (profile-sirius-temp-directory *editor-profile*)
                              (format nil "~A" (profile-sirius-temp-directory *editor-profile*))
                            ""))

   (editor-logfile-button push-button 
                          :title " "
                          :title-position :top
                          :title-font *pe-title-font*
                          :font *pe-font*
                          :text "Browse..."
                          :background +button-bc+
                          :foreground +button-fc+
                          :callback-type :none
                          :callback
                          #'(lambda ()
                              (let ((temp 
                                     (save-file-prompter
                                      "Specify a Log File"
                                      "*.log"
                                      nil
                                      (or (profile-porter-logfile (active-profile))
                                          (user-directory)))))
                                (when temp 
                                  (setf (text-input-pane-text porter-logfile)
                                        (if (pathnamep temp)
                                            (namestring temp)
                                          temp))))))

  (editor-configfile-button push-button 
                            :title " "
                            :title-position :top
                            :title-font *pe-title-font*
                            :font *pe-font*
                            :text "Browse..."
                            :background +button-bc+
                            :foreground +button-fc+
                            :callback-type :none
                            :callback
                            #'(lambda ()
                                (let ((temp 
                                       (save-file-prompter
                                        "Specify an Editor Config File"
                                        (format nil "*.~A" (editor-config-extension))
                                        nil
                                        (or (profile-editor-configfile (active-profile))
                                            (user-directory)))))
                                  (when temp 
                                    (setf (text-input-pane-text editor-configfile)
                                          (if (pathnamep temp)
                                              (namestring temp)
                                            temp))))))


   (sirius-temp-dir-button push-button 
                           :title " "
                           :title-font *pe-title-font*
                           :font *pe-font*
                           :title-position :top
                           :text "Browse..."
                           :background +button-bc+
                           :foreground +button-fc+
                           :callback-type :none
                           :callback
                           #'(lambda ()
                               (let ((temp 
                                      (prompt-for-directory
                                       "Select or Specify a Temp Directory"
                                       :pathname (or (profile-sirius-temp-directory (active-profile))
                                                     (sirius-temp-directory)
                                                     (user-directory))
                                       :ok-check #'probe-file
                                       :if-does-not-exist :error)))
                                 (when temp 
                                   (setf (text-input-pane-text sirius-temp-dir)
                                         (if (pathnamep temp)
                                             (namestring temp)
                                           temp))))))

   (host text-input-pane
         :title "RacerPro TCP/IP Address"
         :title-position :top
         :title-font *pe-title-font*
         :font *pe-font*
         :width '(:character 30)
         :text (format nil "~A" (profile-host *editor-profile*)))

   (port text-input-pane
         :title "RacerPro Native Port (KRSS)"
         :title-position :top
         :title-font *pe-title-font*
         :font *pe-font*
         :width '(:character 30)
         :text (format nil "~A" (profile-port *editor-profile*)))



   ;;;
   ;;;
   ;;;

   (auto-connect check-button
                 :text "Auto Connect"
                 :title-font *pe-title-font*
                 :font *pe-font*
                 :enabled (profile-default *editor-profile*)
                 :selected (profile-auto-connect *editor-profile*))

   (make-default check-button
                 :text "Make Default Profile"
                 :title-font *pe-title-font*
                 :font *pe-font*
                 :selected (profile-default *editor-profile*))

   (kill-on-exit check-button
                 :text "Shutdown RacerPro on RacerPorter Exit"
                 :title-font *pe-title-font*
                 :font *pe-font*
                 :selected (profile-kill-started-server-on-exit *editor-profile*))

   ;;;
   ;;; Server Settings
   ;;;
   
   
   (dig-port text-input-pane
             :enabled (and (executable-specified-p *editor-profile*)                        
                           (profile-default *editor-profile*))
             :title "Start RacerPro with DIG Port"
             :title-font *pe-title-font*
             :font *pe-font*         
             :title-position :top
             :width '(:character 30)
             :text (format nil "~A" (profile-dig-port *editor-profile*)))

   (open-connection-timeout text-input-pane
                            :title "Open Connection Timeout (s)"
                            :title-font *pe-title-font*
                            :font *pe-font*
                            :title-position :top
                            :width '(:character 30)
                            :enabled t 
                            :text (format nil "~A" (profile-open-connection-timeout *editor-profile*)))

   (close-connection-timeout text-input-pane
                             :title "Close Connection Timeout (s)"
                             :title-font *pe-title-font*
                             :font *pe-font*
                             :title-position :top
                             :width '(:character 30)
                             :enabled t 
                             :text (format nil "~A" (profile-close-connection-timeout *editor-profile*)))

   (acquire-server-info-timeout text-input-pane
                                :title "Acquire Server Info Timeout (s)"
                                :title-font *pe-title-font*
                                :font *pe-font*
                                :title-position :top
                                :width '(:character 30)
                                :enabled t 
                                :text (format nil "~A" (profile-acquire-server-info-timeout *editor-profile*)))

   (wait-for-started-server-before-connect-time text-input-pane
                                                :title "Wait Before Auto Connect after Start (s)"
                                                :title-font *pe-title-font*
                                                :font *pe-font*
                                                :title-position :top
                                                :width '(:character 30)
                                                :enabled t 
                                                :text (format nil "~A" 
                                                              (profile-wait-for-started-server-before-connect-time 
                                                               *editor-profile*)))

   (racerpro text-input-pane
             :title "RacerPro Executable"
             :title-font *pe-title-font*
             :font *pe-font*
             :title-position :top
             :width '(:character 30)
             :enabled t ; (profile-default *editor-profile*)
             :callback #'(lambda (file app)
                           (enable-or-disable-buttons app file))

             :text (if (executable-specified-p *editor-profile*)
                       (format nil "~A" (profile-racerpro-executable *editor-profile*))
                     ""))
   
   #| (connect-after-start check-button
                           :enabled (executable-specified-p *editor-profile*)
                           :text "Auto Connect after RacerPro Start"
                           :title-font *pe-title-font*
                           :font *pe-font*
                           :selected (profile-connect-after-start *editor-profile*)) |# 
   
   (auto-start check-button
               :enabled (and (executable-specified-p *editor-profile*) 
                             (profile-default *editor-profile*))
               :text "Auto Start RacerPro if Default"
               :title-font *pe-title-font*
               :font *pe-font*             
               :selected (profile-auto-start-if-default *editor-profile*))

   
   (unsafe check-button
           :enabled (executable-specified-p *editor-profile*)                        
           :text "Start RacerPro in Unsafe Mode"
           :title-font *pe-title-font*
           :font *pe-font*             
           :selected (profile-unsafe *editor-profile*))

   (debug check-button
          :enabled (executable-specified-p *editor-profile*)                        
          :text "Start RacerPro with Debugging"
          :title-font *pe-title-font*
          :font *pe-font*             
          :selected (profile-debug *editor-profile*))

   (no-http-console-log check-button
                        :enabled (executable-specified-p *editor-profile*)                       
                        :text "Start RacerPro without DIG Logging"
                        :title-font *pe-title-font*
                        :font *pe-font*             
                        :selected (profile-no-http-console-log *editor-profile*))

   (verbose check-button
            :text "Start RacerPro Verbose"
            :enabled (executable-specified-p *editor-profile*)                        
            :title-font *pe-title-font*
            :font *pe-font*             
            :selected (profile-verbose *editor-profile*))

   (dig-1-1 check-button
            :text "Start RacerPro in DIG 1.1 Mode"
            :enabled (executable-specified-p *editor-profile*)
            :title-font *pe-title-font*
            :font *pe-font*             
            :selected (profile-dig-1-1 *editor-profile*))

   (owllink check-button
            :text "Start RacerPro in OWLLINK Mode"
            :enabled (executable-specified-p *editor-profile*)
            :title-font *pe-title-font*
            :font *pe-font*             
            :selected (profile-owllink *editor-profile*))

   #+:mac
   (start-in-terminal check-button
                      :text "Start RacerPro in Terminal Window"
                      :enabled (executable-specified-p *editor-profile*)
                      :title-font *pe-title-font*
                      :font *pe-font*             
                      :selected (profile-start-in-terminal *editor-profile*))
   
   ;;;
   ;;;
   ;;;

   (temp-directory text-input-pane
                   :enabled (and (executable-specified-p *editor-profile*)                                 
                                 (profile-default *editor-profile*))
                   :title "Start RacerPro with Temp Directory"
                   :title-font *pe-title-font*
                   :font *pe-font*             
                   :title-position :top
                   :width '(:character 30)
                   :text (format nil "~A" (profile-temp-directory *editor-profile*)))
   
   (license-file text-input-pane
                 :enabled (and (executable-specified-p *editor-profile*)                                 
                               (profile-default *editor-profile*))
                 :title "Start RacerPro with License File"
                 :title-position :top
                 :width '(:character 30)
                 :title-font *pe-title-font*
                 :font *pe-font*             
                 :text (format nil "~A" (profile-license-file *editor-profile*)))
   
   (logging-file text-input-pane
                 :enabled (and (executable-specified-p *editor-profile*)                                 
                               (profile-default *editor-profile*))
                 :title "Start RacerPro with Logging to File"
                 :title-position :top
                 :width '(:character 30)
                 :title-font *pe-title-font*
                 :font *pe-font*             
                 :text (profile-logging-file *editor-profile*))

   ;;;
   ;;;
   ;;; 

   (arguments editor-pane
              :enabled (and (executable-specified-p *editor-profile*)                                 
                            (profile-default *editor-profile*))
              :title "Start RacerPro with Additional Arguments"
              :title-position :top
              :width '(:character 30)
              ;:max-height '(:character 10)
              :buffer-name "Arguments"
              :vertical-scroll t
              :horizontal-scroll t
              :title-font *pe-title-font*
              :font *pe-font*             
              :text "")
   
   ;;;
   ;;;
   ;;;

   (title1 title-pane :title "Profile-Specific Connection Options" 
           :title-font *pe-title-bold-font*)
   
   (title2 title-pane :title "Profile-Specific Server Startup Options"
           :title-font *pe-title-bold-font*)

   (title3 title-pane :title "Profile-Specific General Options"
           :title-font *pe-title-bold-font*)

   (title4 title-pane :title "Profile-Specific Fonts"
           :title-font *pe-title-bold-font*)

   ;;;
   ;;;
   ;;;
   
   (ok push-button
       :callback-type :interface
       :callback #'(lambda (interface)
                     
                     (setf *canceled?* nil
                           *save?* nil)

                     (when (enter-current-selection interface)                                
                       (exit-dialog interface)))
       :text "OK")
   
   (cancel push-button
           :callback-type :interface
           :callback #'(lambda (interface)

                         (setf *canceled?* t
                               *save?* nil)

                         (exit-dialog interface))
           :text "Cancel")

   (save-ok push-button
            :callback-type :interface
            :callback #'(lambda (interface)
                          
                          (setf *canceled?* nil
                                *save?* t)

                          (when (enter-current-selection interface)
                            (exit-dialog interface)))

            :text
            (let ((namestring 
                   (namestring
                    (or *profiles-loaded-from*
                        (sirius-profiles-file)))))
              (declare (ignorable namestring))
              
              (format nil "OK & Save Profile")))

   (select-file-button push-button 
                       :title " "
                       :title-position :top
                       :title-font *pe-title-font*
                       :font *pe-font*             
                       :text "Browse..."
                       :background +button-bc+
                       :foreground +button-fc+
                       :callback #'(lambda (ignore app)
                                     (declare (ignorable ignore))
                                     (let ((file
                                            (open-file-prompter "Browse for RacerPro Executable"
                                                                "RacerPro*"
                                                                (program-directory))))
                                                                
                                       (when file
                                         (setf (text-input-pane-text racerpro) file)
                                         (enable-or-disable-buttons app file)))))

   (select-file-button1 push-button 
                        :title " "
                        :title-position :top
                        :title-font *pe-title-font*
                        :font *pe-font*             
                        :text "Browse..."
                        :enabled (and (executable-specified-p *editor-profile*)                        
                                      (profile-default *editor-profile*))
                        :background +button-bc+
                        :foreground +button-fc+
                        :callback-type :none
                        :callback #'(lambda ()
                                      (let ((file 
                                             (save-file-prompter "Select or Specify a Log File"
                                                                 "*.log"
                                                                 nil
                                                                 (user-directory))))
                                        (when file 
                                          (setf (text-input-pane-text logging-file)
                                                file)))))

   (select-file-button2 push-button 
                        :title " "
                        :title-position :top
                        :title-font *pe-title-font*
                        :font *pe-font*             
                        :text "Browse..."
                        :enabled (and (executable-specified-p *editor-profile*)                        
                                      (profile-default *editor-profile*))
                        :background +button-bc+
                        :foreground +button-fc+
                        :callback-type :none
                        :callback #'(lambda ()
                                      (let ((file
                                             (open-file-prompter "Select or Specify a License File"
                                                                 "*.racerlicense"
                                                                 (user-directory))))
                                        (when file
                                          (setf (text-input-pane-text license-file)
                                                file)))))
                                        

   (select-file-button3 push-button 
                        :title " "
                        :title-position :top
                        :text "Browse..."
                        :title-font *pe-title-font*
                        :font *pe-font*             
                        :enabled (and (executable-specified-p *editor-profile*)                        
                                      (profile-default *editor-profile*))
                        :background +button-bc+
                        :foreground +button-fc+
                        :callback-type :none
                        :callback #'(lambda ()
                                      (let ((temp 
                                             (prompt-for-directory
                                              "Select or Specify a Temp Directory"
                                              :pathname 
                                              (or (profile-sirius-temp-directory (active-profile))
                                                  (sirius-temp-directory)
                                                  (user-directory))
                                              :ok-check #'probe-file
                                              :if-does-not-exist :error)))
                                        (when temp 
                                          (setf (text-input-pane-text temp-directory)
                                                (if (pathnamep temp)
                                                    (namestring temp)
                                                  temp))))))

   ;;;
   ;;;
   ;;;
   
   (overwrite-font-size text-input-pane
                        :title "Use this global Font Size (in Points)"
                        :title-position :top
         
                        :title-font *pe-title-font*
                        :font *pe-font*
         
                        :width '(:character 30)
                        :text (format nil "~A" (profile-overwrite-font-size *editor-profile*)))

   (width-field text-input-range
                
                :start 600
                :end 2048
                :value (profile-width *editor-profile*)
                :callback #'(lambda (num)
                              (setf (profile-width *editor-profile*) num))
                :callback-type :data
                
                :title "Width"
                :title-position :top
         
                :title-font *pe-title-font*
                :font *pe-font*)

   (height-field text-input-range
                
                 :start 700
                 :end 2048
                 :value (profile-height *editor-profile*)
                 :callback #'(lambda (num)
                               (setf (profile-height *editor-profile*) num))
                 :callback-type :data
                
                 :title "Height"
                 :title-position :top
         
                 :title-font *pe-title-font*
                 :font *pe-font*)
   
   ;;;
   ;;;
   ;;;
   
   (select-tabs-font-button push-button 
                            :title " "
                            :title-position :top
                            :text "Tab Title Font..."
                            :title-font *pe-title-font*
                            :font *pe-font*             
                            :background +button-bc+
                            :foreground +button-fc+
                            :callback-type :none
                            :callback #'(lambda ()
                                          (with-profile-access
                                            (let ((font 
                                                   (prompt-for-font "Choose Font for Tab Titles"
                                                                    :font 
                                                                    (apply #'gp:make-font-description
                                                                           tabs-font))))
                                              (when font
                                                (message-restart-required)
                                                (setf tabs-font
                                                      (gp:font-description-attributes
                                                       (gp:font-description font))))))))

  
   (select-shell-font-button push-button 
                             :title " "
                             :title-position :top
                             :text "Shell Font..."
                             :title-font *pe-title-font*
                             :font *pe-font*             
                             :background +button-bc+
                             :foreground +button-fc+
                             :callback-type :none
                             :callback #'(lambda ()
                                           (with-profile-access
                                             (let ((font 
                                                    (prompt-for-font "Choose Font for Shell"
                                                                     :font 
                                                                     (if shell-font
                                                                         (apply #'gp:make-font-description
                                                                                shell-font)
                                                                       (gp:find-best-font arguments
                                                                                          (apply
                                                                                           #'gp::make-font-description 
                                                                                           '(:stock  
                                                                                             :system-fixed-font)))))))
                                                                       
                                               (when font
                                                 (message-restart-required)
                                                 (with-sirius-app (app)
                                                   (unless (gp:font-fixed-width-p app font)
                                                     (sirius-message "This is not a fixed-width font.
Pretty printing will not really work. 
Please consider selecting a fixed-width font.")))
                                                 (setf shell-font
                                                       (gp:font-description-attributes
                                                        (gp:font-description font))))))))
   
   (select-button-font-button push-button 
                              :title " "
                              :title-position :top
                              :text "Button Font..."
                              :title-font *pe-title-font*
                              :font *pe-font*             
                              :background +button-bc+
                              :foreground +button-fc+
                              :callback-type :none
                              :callback #'(lambda ()
                                            (with-profile-access
                                              (let ((font 
                                                     (prompt-for-font "Choose Font for Push Buttons"
                                                                      :font 
                                                                      (apply #'gp:make-font-description
                                                                             button-font))))
                                                (when font
                                                  (message-restart-required)
                                                  (setf button-font
                                                        (gp:font-description-attributes
                                                         (gp:font-description font))))))))
   
   (select-racer-button-font-button push-button 
                              :title " "
                              :title-position :top
                              :text "Racer Button Font..."
                              :title-font *pe-title-font*
                              :font *pe-font*             
                              :background +button-bc+
                              :foreground +button-fc+
                              :callback-type :none
                              :callback #'(lambda ()
                                            (with-profile-access
                                              (let ((font 
                                                     (prompt-for-font "Choose Font for Racer Push Buttons"
                                                                      :font 
                                                                      (apply #'gp:make-font-description
                                                                             racer-button-font))))
                                                (when font
                                                  (message-restart-required)
                                                  (setf racer-button-font
                                                        (gp:font-description-attributes
                                                         (gp:font-description font))))))))
   
   (select-checkbox-font-button push-button 
                                :title " "
                                :title-position :top
                                :text "Checkbox Font..."
                                :title-font *pe-title-font*
                                :font *pe-font*             
                                :background +button-bc+
                                :foreground +button-fc+
                                :callback-type :none
                                :callback #'(lambda ()
                                              (with-profile-access
                                                (let ((font
                                                       (prompt-for-font "Choose Font for Checkboxes"
                                                                        :font 
                                                                        (apply #'gp:make-font-description
                                                                               checkbox-font))))
                                                  (when font
                                                    (message-restart-required)
                                                    (setf checkbox-font
                                                          (gp:font-description-attributes
                                                           (gp:font-description font))))))))

   
   (select-radiobox-font-button push-button 
                                :title " "
                                :title-position :top
                                :text "Radiobox Font..."
                                :title-font *pe-title-font*
                                :font *pe-font*             
                                :background +button-bc+
                                :foreground +button-fc+
                                :callback-type :none
                                :callback #'(lambda ()
                                              (with-profile-access
                                                (let ((font 
                                                       (prompt-for-font "Choose Font for Radioboxes"
                                                                        :font 
                                                                        (apply #'gp:make-font-description
                                                                               radiobox-font))))
                                                  (when font
                                                    (message-restart-required)
                                                    (setf radiobox-font
                                                          (gp:font-description-attributes
                                                           (gp:font-description font))))))))
   
   (select-title-font-button push-button 
                             :title " "
                             :title-position :top
                             :text "Title Font..."
                             :title-font *pe-title-font*
                             :font *pe-font*             
                             :background +button-bc+
                             :foreground +button-fc+
                             :callback-type :none
                             :callback #'(lambda ()
                                           (with-profile-access
                                             (let ((font 
                                                    (prompt-for-font "Choose Font for Titles"
                                                                     :font 
                                                                     (apply #'gp:make-font-description
                                                                            title-font))))
                                               (when font
                                                 (message-restart-required)
                                                 (setf title-font
                                                       (gp:font-description-attributes
                                                        (gp:font-description font))))))))
                                                         
        
   (select-graph-font-button push-button 
                             :title " "
                             :title-position :top
                             :text "Graph Font..."
                             :title-font *pe-title-font*
                             :font *pe-font*             
                             :background +button-bc+
                             :foreground +button-fc+
                             :callback-type :none
                             :callback #'(lambda ()
                                           (with-profile-access
                                             (let ((font 
                                                    (prompt-for-font "Choose Font for Graphs"
                                                                     :font 
                                                                     (apply #'gp:make-font-description
                                                                            graph-font))))
                                               (when font
                                                 (message-restart-required)
                                                 (setf graph-font
                                                       (gp:font-description-attributes
                                                        (gp:font-description font))))))))
                                                     
     
   (select-info-font-button push-button 
                            :title " "
                            :title-position :top
                            :text "Status Display Font..."
                            :title-font *pe-title-font*
                            :font *pe-font*             
                            :background +button-bc+
                            :foreground +button-fc+
                            :callback-type :none
                            :callback #'(lambda ()
                                          (with-profile-access
                                            (let ((font 
                                                   (prompt-for-font "Choose Font for Status Display"
                                                                    :font 
                                                                    (apply #'gp:make-font-description
                                                                           info-font))))
                                              (when font
                                                (message-restart-required)
                                                (setf info-font
                                                      (gp:font-description-attributes
                                                       (gp:font-description font))))))))

   (select-list-font-button push-button 
                            :title " "
                            :title-position :top
                            :text "List Display Font..."
                            :title-font *pe-title-font*
                            :font *pe-font*             
                            :background +button-bc+
                            :foreground +button-fc+
                            :callback-type :none
                            :callback #'(lambda ()
                                          (with-profile-access
                                            (let ((font 
                                                   (prompt-for-font "Choose Font for List Displays"
                                                                    :font 
                                                                    (apply #'gp:make-font-description
                                                                           list-font))))
                                              (when font
                                                (message-restart-required)
                                                (setf list-font
                                                      (gp:font-description-attributes
                                                       (gp:font-description font)))))))))

  
  (:layouts 
   (main-layout column-layout 
                '(fields
                  #-:mac :divider
                  #-:mac buttons2
                  #-:mac save-ok 
                  )
                :adjust :center)

 
   (racerpro1 row-layout
              '(racerpro 
                select-file-button)
              :adjust :center)

   (logging1 row-layout
             '(logging-file 
               select-file-button1)
             :adjust :center)

   (license1 row-layout
             '(license-file 
               select-file-button2)
             :adjust :center)

   (temp1 row-layout
          '(temp-directory
            select-file-button3)
          :adjust :center)

   (width-and-height-row row-layout
                         '(width-field
                           height-field)
                         :adjust :center)
   
   (fields0 column-layout 
            '(title3
              
              #-:mac :divider 
              item
              ;;sparql-pattern             
              sirius-temp-dir-row
              porter-logfile-row
              editor-configfile-row
              
              make-default
              use-logfile
              use-logbuffer
              pretty-print
              popup-message-box
              #+:sonic load-into-default
              porter-commands-in-history
              show-mouse-select-commands
              ask-before-transmit
              editor-eval-maintain-axioms
              time-all
              #+:pulse-pane
              show-status-led             
              autosave-on-quit
              #+:progress-bar-pane
              show-progress-bar
              confirm-quit

              )
            :adjust :left)

   (fields0a column-layout 
             '(title4
               
              #-:mac :divider

              select-tabs-font-button
              select-shell-font-button
              select-button-font-button
              select-racer-button-font-button
              select-checkbox-font-button
              select-radiobox-font-button
              select-title-font-button
              select-graph-font-button
              select-info-font-button
              select-list-font-button

              #-:mac :divider
              
              width-and-height-row              
              overwrite-font-size
              
              )
             :adjust :center)
      
   (fields1 column-layout 
            '(title1

              #-:mac :divider 
             
              
              host 
              port
              open-connection-timeout
              close-connection-timeout
              acquire-server-info-timeout
              auto-connect
              )
            :adjust :left)

   (fields2 column-layout 
            '(title2
              #-:mac :divider 
              fields3)
            :adjust :left)

   (fields3 row-layout
            '(fields3a
              fields3b)
            :y-adjust :top)

   (sirius-temp-dir-row row-layout
                        '(sirius-temp-dir
                          sirius-temp-dir-button)
                        :y-adjust :top)

   (porter-logfile-row row-layout
                       '(porter-logfile
                         editor-logfile-button)
                       :y-adjust :top)

   (editor-configfile-row row-layout
                       '(editor-configfile
                         editor-configfile-button)
                       :y-adjust :top)

   (fields3a column-layout
             '(racerpro1               
               wait-for-started-server-before-connect-time
               dig-port
               auto-start
               ;; connect-after-start
               #+:mac start-in-terminal
               unsafe
               debug
               no-http-console-log
               verbose
               dig-1-1
               owllink
               ;;kill-on-exit
               )
             :adjust :left)

   (fields3b column-layout
             '(logging1
               license1
               temp1
               arguments)
             :adjust :left)

   (fields row-layout
           '(fields0 
             #-:mac :divider 
             fields0a
             #-:mac :divider 
             fields1 
             #-:mac :divider 
             fields2
             )
           :adjust :top)
           
   
   (buttons2 row-layout
             '(ok cancel)
             :adjust :center))  
  (:default-initargs 
   :title "Profile Editor"))


(defun enable-or-disable-buttons (app file)
  (let ((on (and file
                 (not (blank-line-p file))
                 (probe-file file)
                 ;(profile-default *editor-profile*)
                 )))

    (if on
        (enable-buttons app)
      (disable-buttons app))))

(defun enable-buttons (app)
  (dolist (slot '(auto-start
                  ;; connect-after-start
                  #+:mac start-in-terminal
                  unsafe
                  debug
                  no-http-console-log 
                  verbose
                  dig-1-1
                  owllink
                  dig-port
                  logging-file
                  license-file
                  temp-directory
                  arguments
                  select-file-button1
                  select-file-button2
                  select-file-button3
                  auto-start
                  ;auto-connect
                  ;;kill-on-exit
                  wait-for-started-server-before-connect-time
                  ))
    
    (setf (simple-pane-enabled (slot-value app slot)) t)))


(defun disable-buttons (app)
  (dolist (slot '(auto-start
                  ;; connect-after-start
                  #+:mac start-in-terminal
                  unsafe
                  debug
                  no-http-console-log 
                  verbose
                  dig-1-1
                  owllink
                  dig-port
                  logging-file
                  license-file
                  temp-directory
                  arguments
                  select-file-button1
                  select-file-button2
                  select-file-button3
                  auto-start
                  ;auto-connect
                  ;;kill-on-exit
                  wait-for-started-server-before-connect-time
                  ))

    (setf (simple-pane-enabled (slot-value app slot)) nil)))    
   
(defun enter-current-selection (app)
  (with-slots (item 
               confirm-quit
               autosave-on-quit
               porter-logfile
               pretty-print
               popup-message-box
               load-into-default
               porter-commands-in-history
               show-mouse-select-commands
               ask-before-transmit
               editor-eval-maintain-axioms 
               time-all
               #+:pulse-pane
               show-status-led
               #+:progress-bar-pane
               show-progress-bar
               use-logfile
               use-logbuffer
               sparql-pattern
               auto-start
               ;; connect-after-start
               #+:mac start-in-terminal
               host port make-default auto-connect
               racerpro unsafe debug no-http-console-log verbose
               dig-1-1
               owllink
               dig-port
               open-connection-timeout
               close-connection-timeout
               acquire-server-info-timeout
               wait-for-started-server-before-connect-time
               logging-file
               license-file
               temp-directory
               sirius-temp-dir
               editor-configfile
               arguments
               width-field
               height-field
               overwrite-font-size) app
    
    (let ((width (text-input-range-value width-field))
          (height (text-input-range-value height-field))
          (global-font-size (text-input-pane-text overwrite-font-size))          
          
          (item (text-input-pane-text item))
          (confirm-quit (button-selected confirm-quit))
          (autosave-on-quit (button-selected autosave-on-quit))
          (pretty-print-input (not (button-selected pretty-print)))
          (popup-message-box (button-selected popup-message-box))
          (load-into-default (button-selected load-into-default))          
          (porter-commands-in-history (button-selected porter-commands-in-history))
          (show-mouse-select-commands (button-selected show-mouse-select-commands))
          (ask-before-transmit (button-selected ask-before-transmit))
          (editor-eval-maintain-axioms (button-selected editor-eval-maintain-axioms))
          (time-all (button-selected time-all))
          #+:pulse-pane
          (show-status-led (button-selected show-status-led))
          #+:progress-bar-pane
          (show-progress-bar (button-selected show-progress-bar))
          (use-logfile (button-selected use-logfile))
          (use-logbuffer (button-selected use-logbuffer))
          (sparql-pattern (text-input-pane-text sparql-pattern))
          (porter-logfile (text-input-pane-text porter-logfile))
          (editor-configfile (text-input-pane-text editor-configfile))
          ;; (kill (button-selected kill-on-exit))
          #| (connect-start
              (button-selected connect-after-start)) |# 
          #+:mac 
          (start-in-terminal
           (button-selected start-in-terminal))
          (auto-start-if-default
           (button-selected auto-start))
          (host (text-input-pane-text host))
          (port 
           (text-input-pane-text port))
          (make-default 
           (button-selected make-default))
          (auto-connect
           (button-selected auto-connect))
          (racerpro 
           (text-input-pane-text racerpro))
          (unsafe 
           (button-selected unsafe))
          (debug 
           (button-selected debug))
          (no-http-console-log 
           (button-selected no-http-console-log))
          (verbose 
           (button-selected verbose))
          (dig-1-1 
           (button-selected dig-1-1))
          (owllink
           (button-selected owllink))
          (dig-port 
           (text-input-pane-text dig-port))
          (open-connection-timeout
           (text-input-pane-text open-connection-timeout))
          (close-connection-timeout
           (text-input-pane-text close-connection-timeout))
          (acquire-server-info-timeout
           (text-input-pane-text acquire-server-info-timeout))
          (wait-for-started-server-before-connect-time
           (text-input-pane-text wait-for-started-server-before-connect-time))
          (logging-file 
           (text-input-pane-text logging-file))
          (license-file 
           (text-input-pane-text license-file))
          (temp-directory 
           (text-input-pane-text temp-directory))
          (sirius-temp-dir 
           (text-input-pane-text sirius-temp-dir))
          (arguments (editor-pane-text arguments)))

      (when *editor-profile*
        (unless (uses-internal-racer-p *editor-profile*)
          (dolist (port (list port dig-port 
                              open-connection-timeout close-connection-timeout
                              wait-for-started-server-before-connect-time acquire-server-info-timeout))
            (let ((x 
                   (handler-case 
                       (parse-integer port)
                     (error ()
                       nil))))
              (unless (and x 
                           (integerp x)
                           (>= x 0))
                       
                (beep-pane)
                (return-from enter-current-selection nil)))))
        
        (when (and (not *edit*)
                   (find item *sirius-profiles* 
                         :key #'profile-name
                         :test #'string=))
          (beep-pane)
          (return-from enter-current-selection nil))

        (setf (profile-name *editor-profile*) item
              (profile-width *editor-profile*) width
              (profile-height *editor-profile*) height
              (profile-overwrite-font-size *editor-profile*) (handler-case (parse-integer global-font-size)
                                                               (error () nil))
              (profile-confirm-quit *editor-profile*) confirm-quit
              (profile-autosave-on-quit *editor-profile*) autosave-on-quit
              (profile-pretty-print-input *editor-profile*) pretty-print-input
              (profile-popup-message-box *editor-profile*) popup-message-box
              (profile-load-into-default *editor-profile*) load-into-default
              (profile-put-porter-commands-in-history *editor-profile*) porter-commands-in-history
              (profile-show-mouse-select-commands *editor-profile*) show-mouse-select-commands
              (profile-ask-before-transmitting-large-files *editor-profile*) ask-before-transmit
              (profile-editor-eval-maintain-axioms *editor-profile*) editor-eval-maintain-axioms
              (profile-time-all-operations *editor-profile*) time-all
              #+:pulse-pane
              (profile-show-status-led *editor-profile*) 
              #+:pulse-pane 
              show-status-led
              #+:progress-bar-pane
              (profile-show-progress-bar *editor-profile*) 
              #+:progress-bar-pane
              show-progress-bar
              (profile-use-logfile *editor-profile*) use-logfile
              (profile-use-logbuffer *editor-profile*) use-logbuffer
              (profile-sparql-pattern *editor-profile*) sparql-pattern
              (profile-host *editor-profile*) host
              (profile-port *editor-profile*) port
              (profile-default *editor-profile*) make-default
              (profile-auto-connect *editor-profile*) auto-connect
              
              ;; (profile-connect-after-start *editor-profile*) connect-start
              #+:mac (profile-start-in-terminal *editor-profile*)
	      #+:mac start-in-terminal
              (profile-auto-start-if-default *editor-profile*) auto-start-if-default

              (profile-racerpro-executable *editor-profile*)
              (when (and racerpro (probe-file racerpro)) 
                racerpro)
              
              (profile-unsafe *editor-profile*) unsafe
              ;; (profile-kill-started-server-on-exit *editor-profile*) kill
              (profile-debug *editor-profile*) debug
              (profile-no-http-console-log *editor-profile*) no-http-console-log
              (profile-verbose *editor-profile*) verbose
              (profile-dig-1-1 *editor-profile*) dig-1-1
              (profile-owllink *editor-profile*) owllink
              (profile-dig-port *editor-profile*) dig-port
          
              (profile-open-connection-timeout *editor-profile*) 
              (read-from-string open-connection-timeout)
              (profile-close-connection-timeout *editor-profile*) 
              (read-from-string close-connection-timeout)
              (profile-acquire-server-info-timeout *editor-profile*)
              (read-from-string acquire-server-info-timeout)
              (profile-wait-for-started-server-before-connect-time *editor-profile*)
              (read-from-string wait-for-started-server-before-connect-time)
          
              (profile-logging-file *editor-profile*) 
              (when (and logging-file)
                logging-file)

              (profile-porter-logfile *editor-profile*)
              porter-logfile

              (profile-editor-configfile *editor-profile*)
              editor-configfile

              (profile-license-file *editor-profile*) 
              (when (and license-file (probe-file license-file)) license-file)
              
              (profile-temp-directory *editor-profile*) 
              (when (and temp-directory (probe-file temp-directory)) temp-directory)
              
              (profile-sirius-temp-directory *editor-profile*) 
              (when (and sirius-temp-dir (probe-file sirius-temp-dir)) sirius-temp-dir)
              
              (profile-startup-arguments *editor-profile*) arguments)

        #+:progress-bar-pane
        (progn 
          (kill-pb-process)
          (when show-progress-bar
            (start-pb-process)))

        t))))

#-:mac
(defun profile-manager ()
  (setf *canceled?* nil)
  (handler-case 
      (let ((dialog (make-instance 'profile-manager-dialog)))
        
        (enable-or-disable-buttons dialog 
                                   (profile-racerpro-executable *editor-profile*))

        (setf (editor-pane-text (slot-value dialog 'arguments)) 
              (or (profile-startup-arguments *editor-profile*) ""))

        (display-dialog dialog :modal t))
    (error (error)
      (sirius-message "~A" error))))

#+:mac
(defun profile-manager ()
  (setf *canceled?* nil)
  (handler-case 
      (let ((dialog (make-instance 'profile-manager-dialog)))
        
        (setf (editor-pane-text (slot-value dialog 'arguments)) 
              (or (profile-startup-arguments *editor-profile*) ""))
            
        (enable-or-disable-buttons dialog 
                                   (profile-racerpro-executable *editor-profile*))
        (popup-confirmer dialog 
                         nil
                         
                         :modal t
                         :callback-type :none
                         
                         :ok-button
                         #| (let ((namestring 
                                   (namestring
                                    (or *profiles-loaded-from*
                                        (sirius-profiles-file)))))
                              (format nil "OK & Save Profile"
                                      (substitute-backslashes-with-slashes 
                                       namestring))) |#
                         "OK & Save Profile"
                         
                         :ok-function #'(lambda (interface)
                                          (declare (ignorable interface))
                                          
                                          (setf *canceled?* nil
                                                *save?* t)
                                          
                                          (when (enter-current-selection dialog)
                                            (exit-dialog dialog)))

                         :no-button "OK & Don't Save to Profiles File"
                         :no-function 
                         #'(lambda ()
                             
                             (setf *canceled?* nil
                                   *save?* nil)

                             (when (enter-current-selection dialog)                                
                               (exit-dialog dialog)))

                         :cancel-button "Cancel"
                         :cancel-function
                         #'(lambda (interface)
                             (declare (ignore interface))

                             (setf *canceled?* t
                                   *save?* nil)
                             
                             (exit-dialog dialog))))

    (error (error)
      (sirius-message "~A" error))))

;;;
;;;
;;;

(defun check-profiles ()
  (when (profile-default (active-profile))
    
    ;;; es kann nur eines geben 
    (mapc #'(lambda (x y)
              (cond ((eq x (active-profile))
                     (setf (profile-default x) t)
                     (setf (profile-default y) t))
                    (t
                     (setf (profile-default x) nil)
                     (setf (profile-default y) nil))))

          *sirius-profiles*
          *profiles-to-save*))

  (unless (some #'profile-default *sirius-profiles*)
    (setf (profile-default (first *sirius-profiles*)) t))

  (unless (some #'profile-default *profiles-to-save*) 
    (setf (profile-default (first *profiles-to-save*)) t)))
  
#|
(defun get-new-profile-name ()
  (let ((names 
         (loop as profile in *sirius-profiles*
               collect (profile-name profile)))
        (counter 0))
    (loop 
     (let ((new-name
            (format nil "Unnamed ~A" (incf counter))))
       (unless (find new-name names :test #'string-equal)
         (return-from get-new-profile-name new-name))))))
|#

(defun edit-profile ()
  (when (active-profile)
    
    (let* ((*editor-profile* (active-profile))          
           (*edit* t)
           (pos (position *editor-profile*
                          *sirius-profiles*)))

      (profile-manager)
      
      (unless *canceled?*

        (check-profiles)

        (cond (*save?*

               (setf *profiles-to-save*
                     (append (subseq *profiles-to-save* 0 pos)
                             (cons (copy-profile *editor-profile*)
                                   (subseq *profiles-to-save* (1+ pos)))))
           
               (save-profiles nil))

              (t 
             
               ))

        (when (connection-was-established-p)
          (close-server-connection))))))


(defun new-profile () ;; copy profile
  (let ((*editor-profile* 
         (create-new-profile-based-on (active-profile))))
    
    (profile-manager)
    
    (unless *canceled?*
      (push *editor-profile* *sirius-profiles*)
      
      (push (copy-profile *editor-profile*) *profiles-to-save*)
      
      (activate-profile *editor-profile*)

      (check-profiles)

      (when *save?*
        (save-profiles nil)))))

(defun delete-profile ()
  (when (active-profile)

    (when (confirm-yes-or-no (format nil "Really delete Profile ~A?"
                                     (profile-name (active-profile))))

      (let ((pos (position (active-profile) *sirius-profiles*)))
        (setf *sirius-profiles*
              (delete (active-profile) *sirius-profiles*))

        (setf *profiles-to-save*
              (append (subseq *profiles-to-save* 0 pos)
                      (subseq *profiles-to-save* (1+ pos)))))
      
      (close-server-connection))

    (unless *sirius-profiles*
      (setf *sirius-profiles*
            (get-default-profiles))
      (copy-all-profiles))

    (check-profiles)

    (activate-profile (first *sirius-profiles*))))
    

;;;
;;;
;;;

(defun save-profiles (&optional (dialog-p t) info-p)
  
  (unless dialog-p 
    (when *warn-before-overwriting-profiles*
      (setf *warn-before-overwriting-profiles* nil)
      (let ((fn 
             (namestring 
              (or *profiles-loaded-from*
                  (sirius-profiles-file)))))
        (when (prompt-for-confirmation 
               (format nil "Make a backup of old profile file
~S before overwriting?" fn))
          (sys::copy-file fn (format nil "~A.bak" fn))))))
                     

  (let* ((file 
          (make-pathname :name 
                         (pathname-name 
                          (or *profiles-loaded-from*
                              (sirius-profiles-file)))
                         :host 
                         (pathname-host 
                          (or *profiles-loaded-from*
                              (sirius-profiles-file)))
                         :type 
                         (pathname-type 
                          (or *profiles-loaded-from*
                              (sirius-profiles-file)))
                         :directory
                         (pathname-directory 
                          (or *profiles-loaded-from*
                              (sirius-profiles-file)))))
         
         (file 
          (if dialog-p 
              (save-file-prompter "Enter a Profile Filename" 
                                  (format nil "*.~A" (profile-extension))
                                  nil
                                  (namestring file))
            file)))

    (when file
      (when info-p 
        (unless (confirm-yes-or-no 
                 (format nil "Save Profiles to ~A?"
                         (substitute-backslashes-with-slashes 
                          (namestring file))))
          (return-from save-profiles nil)))

      (save-sirius-profiles file))))

(defun load-profiles ()
  (let ((file 
         (open-file-prompter "Enter a Profile Filename" 
                             (format nil "*.~A" (profile-extension))
                             (user-directory))))
                             
    (when file  
      (load-sirius-profiles file))))
