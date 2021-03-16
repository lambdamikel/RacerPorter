;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defconstant +newline-string+ (make-string 1 :initial-element #\Newline))

#+(not :racer-with-sirius)
(progn 
  (defparameter +welcome-text+ 
    (format nil "Welcome to ~A, the Graphical User Interface for RacerPro!" 
            +sirius-name+))
  
  (defparameter +version-info+ 
    (format nil "This is ~A Version ~A / Build ~A" 
            +sirius-name+
            +sirius-version+
            +build-version+)))
          
#+:racer-with-sirius
(progn 
  (defparameter +welcome-text+ 
    (format nil "Welcome to ~A!" +sirius-name+))

  (defparameter +version-info+ 
    (format nil "This is ~A Version ~A (RacerPorter Version ~A / Build ~A)" 
            +sirius-name+
            *dl-prover-version*
            +sirius-version+
            +build-version+)))

;;;
;;;
;;;

(defun get-concepts-pane-empty-text ()
  (let ((info nil))
    (concatenate 'string
                 "The list of"
                 (if (selected-only (active-profile))
                     " selected"
                   "")
                 (case (profile-concept-list-mode (active-profile))
                   (:all "")
                   (:defined " defined")
                   (:primitive " primitive")
                   (:unsatisfiable " unsatisfiable"))
                 " concepts in TBox \""
                 (symbol-name (profile-sirius-current-tbox (active-profile)))
                 "\" is empty (wrong or empty TBox?)."
               
                 (if (and (not (profile-current-concept (active-profile)))
                          (selected-only (active-profile)))
                     (setf info "
No current concept!")
                   "")
    
                 (if (and (not (profile-all-selected-concepts))
                          (selected-only (active-profile)))
                     (setf info "
No selected concepts!")
                   "")

                 (if info
                     " Possible solutions: either 
- disable the checkbox \"Sel. Only\", or 
- determine some current / selected concepts by name using \"Search \& Select\", or 
- determine some current / selected concepts using another concept-oriented tab (e.g., \"Taxonomy\"). 
More possible solutions: 
- select another TBox using the \"TBoxes\" tab, 
- enable the radion button \"All\", 
- reconnect to this Racer server using a \"Small TBoxes\" default profile and retry."
                   ""))))

(defun get-roles-pane-empty-text ()
  (let ((info nil))
    (concatenate 'string
                 "The list of"
                 (if (selected-only (active-profile))
                     " selected"
                   "")
                 (case (profile-role-list-mode (active-profile))
                   (:roles "")
                   (:unsatisfiable " unsatisfiable")
                   (:transitive-roles " transitive")
                   (:features " functional") 
                   (:cd-attributes " CD attribute")
                   (:datatype-properties " datatype")
                   (:annotation-properties " annotation"))                  
                 " roles in TBox \""
                 (symbol-name (profile-sirius-current-tbox (active-profile)))
                 "\" is empty (wrong or empty TBox?)."
               
                 (if (and (not (profile-current-role (active-profile)))
                          (selected-only (active-profile)))
                     (setf info "
No current role!")
                   "")

                 (if (and (not (profile-all-selected-roles))
                          (selected-only (active-profile)))
                     (setf info
                           "
No selected roles!")
                   "")

                 (if info
                     " Possible solutions: either 
- disable the checkbox \"Sel. Only\", or 
- determine some current / selected roles by name using \"Search \& Select\", or 
- determine some current / selected roles using another role-oriented tab (e.g., \"Role Hierarchy\"). 
More possible solutions: 
- select another TBox using the \"TBoxes\" tab, 
- enable the radion button \"All\", 
- reconnect to this Racer server using a \"Small TBoxes\" default profile and retry."
                   ""))))

(defun get-individuals-pane-empty-text ()
  (let ((info nil))
    (concatenate 'string 
                 "The list of"
                 (if (selected-only (active-profile))
                     " selected"
                   "")
                 " individuals in ABox \""
                 (symbol-name (profile-sirius-current-abox (active-profile)))
                 "\" is empty (wrong or empty ABox?)."

                 (if (and (not (profile-current-individual (active-profile)))
                          (selected-only (active-profile)))
                     (setf info "
No current individual!")
                   "")

                 (if (and (not (profile-all-selected-individuals))
                          (selected-only (active-profile)))
                     (setf info "
No selected individuals!")
                   "")

                 (if info
                     " Possible solutions: either 
- disable the checkbox \"Sel. Only\", or 
- determine some current / selected individuals by name using \"Search \& Select\", or 
- determine some current / selected indivdiuals using another individual-oriented tab (e.g., \"ABox Graph\"). 
More possible solutions: 
- select another ABox using the \"ABoxes\" tab, 
- reconnect to this Racer server using a \"Small ABoxes\" default profile and retry."
                   ""))))

(defun get-assertions-pane-empty-text ()
  (let ((info nil)
        (info2 nil))
    (concatenate 'string 
                 "The list of"

                 (if (selected-only (active-profile))
                     " selected"
                   "")
                 (case (profile-assertion-list-mode (active-profile))
                   (:concept-assertions " concept")
                   (:role-assertions " role")
                   (:same-as-assertions " same-as")
                   (:different-from-assertions " different-from")
                   (:attribute-assertions " attribute")
                   (:constraint-assertions " constraint")
                   (:annotation-concept-assertions " annotation concept")
                   (:annotation-role-assertions " annotation role"))
                 " assertions in ABox \""
                 (symbol-name (profile-sirius-current-abox (active-profile)))
                 "\" is empty (wrong or empty ABox?)."
               
                 (if (selected-only (active-profile))
                     " Possible solutions: 
- disable the checkbox \"Sel. Only\""
                   "")
                 
                 (case (profile-taxonomy-root-kind (active-profile))
                   (:current (if (not (profile-current-concept (active-profile)))
                                 (setf info "
No current concept!")
                               ""))
                   (:selected (if (not (profile-all-selected-concepts))
                                  (setf info "
No selected concepts!")
                                ""))
                   (otherwise ""))

                 (if info
                     " Possible solutions: either 
- enable radio button \"All Concepts\", or 
- determine some current / selected concepts using a concept-oriented tab (e.g., \"Concepts\", \"Taxonomy\"), or 
- reconnect to this Racer server using a \"Small TBoxes\" default profile and retry."
                   "")
               
                 ;;;
                 ;;;
                 ;;;

                 (setf info2 info
                       info nil)

                 (case (profile-abox-graph-root-kind (active-profile))
                   (:current (if (not (profile-current-individual (active-profile)))
                                 (setf info "
No current individual!")
                               ""))
                   (:selected (if (not (profile-all-selected-individuals))
                                  (setf info "
No selected individuals!")
                                ""))
                   (otherwise ""))

                 (if info
                     " Possible solutions: either
- enable radio button \"All Inds.\", or 
- determine some current / selected individuals using an individual-oriented tab (e.g., \"Individuals\", \"ABox Graph\"), or 
- reconnect to this Racer server using a \"Small ABoxes\" default profile and retry."
                   "")
               
                 ;;;
                 ;;;
                 ;;; 

                 (setf info2 (or info2 info)
                       info nil)

                 (case (profile-role-hierarchy-root-kind (active-profile))
                   (:current (if (not (profile-current-role (active-profile)))
                                 (setf info "
No current role!")
                               ""))
                   (:selected (if (not (profile-all-selected-roles))
                                  (setf info "
No selected roles!")
                                ""))
                   (otherwise ""))

               
                 (if info 
                     " Possible solutions: either
- enable radio button \"All Roles\", or 
- determine some current / selected roles using a role-oriented tab (e.g., \"Roles\", \"Role Hierarchy\"), or 
- reconnect to this Racer server using a \"Small TBoxes\" default profile and retry."
                   "")

                 (if (or info info2)
                     "
More possible solutions: 
- select another ABox using the \"ABoxes\" tab, 
- play with the radio buttons: \"Concept A\", \"Role A\", ...,  
- reconnect to this Racer server using a \"Small ABoxes\" default profile and retry."
                   ""))))

;;;
;;;
;;;

(defun get-role-hierarchy-empty-text ()
  (let ((info nil))

    (concatenate 'string

                 "The role hierarchy of TBox \""
                 (symbol-name (profile-sirius-current-tbox (active-profile)))
                 "\" is empty (wrong or empty TBox?)."
               
                 (case (profile-role-hierarchy-root-kind (active-profile))
                   (:current (if (not (profile-current-role (active-profile)))
                                 (setf info "
No current role!")
                               ""))
                   (:selected (if (not (profile-all-selected-roles))
                                  (setf info "
No selected roles!")
                                ""))
                   (otherwise ""))
                 
                 (if info
                     " Possible solutions: either 
- enable radio button \"All Roles\", or 
- determine some current / selected roles by name using \"Search \& Select\" 
  (but ensure that \"Freeze Graph\" is disabled, or push \"Recover\" after search), or 
- determine some current / selected roles using another role-oriented tab (e.g., \"Roles\")
  (but ensure that \"Freeze Graph\" is disabled, and \"Cur. Role\" or \"Sel. Roles\" are enabled).
More possible solutions: 
- select another TBox using the \"TBoxes\" tab, 
- reconnect to this Racer server using a \"Small TBoxes\" default profile and retry, and also
- ensure that the depth limit is set to a reasonable value (use \"UNBOUNDED\" only for small TBoxes)."
                   ""))))

(defconstant +role-hierarchy-error-text+
  "An error occurred during Role Hierarchy computation. Sorry.")

(defconstant +role-hierarchy-aborted-text+
  "Computation of Role Hierarchy was aborted.")

(defconstant +role-hierarchy-running-text+
  "Computing Role Hierarchy.
Please wait. 
You may cancel the request with the \"Cancel Request\" button.")

(defconstant +role-hierarchy-computed-text+
  "Role Hierarchy has been computed. 
Please press  the \"Display Graph\" button to display it.
Note that this might take some time. 
RacerPorter is not available during that time.")

(defconstant +role-hierarchy-request-text+
  "Please press the \"Request Graph\" button to request the Role Hierarchy from RacerPro.")

;;;
;;;
;;; 

(defun get-taxonomy-empty-text ()
  (let ((info nil))
    (concatenate 'string 

                 "The taxonomy of TBox \""
                 (symbol-name (profile-sirius-current-tbox (active-profile)))
                 "\" is empty (wrong or empty TBox?)."
               
                 (case (profile-taxonomy-root-kind (active-profile))
                   (:current (if (not (profile-current-concept (active-profile)))
                                 (setf info "
No current concept!")
                               ""))
                   (:selected (if (not (profile-all-selected-concepts))
                                  (setf info "
No selected concepts!")
                                ""))
                   (otherwise ""))

                 (if info
                     " Possible solutions: either 
- enable radio button \"All Concepts\", or 
- determine some current / selected concepts by name using \"Search \& Select\" 
  (but ensure that \"Freeze Graph\" is disabled, or push \"Recover\" after search), or 
- determine some current / selected concepts using another concept-oriented tab (e.g., \"Concepts\")
  (but ensure that \"Freeze Graph\" is disabled, and \"Cur. Concept\" or \"Sel. Concepts\" are enabled).
More possible solutions: 
- select another TBox using the \"TBoxes\" tab, 
- reconnect to this Racer server using a \"Small TBoxes\" default profile and retry, and also
- ensure that the depth limit is set to a reasonable value (use \"UNBOUNDED\" only for small TBoxes)."
                   ""))))


(defconstant +taxonomy-error-text+
  "An error occurred during Taxonomy computation. Sorry.")

(defconstant +taxonomy-aborted-text+
  "Computation of Taxonomy was aborted.")

(defconstant +taxonomy-running-text+
  "Computing Taxonomy.
Please wait. 
You may cancel the request with the \"Cancel Request\" button.")

(defconstant +taxonomy-computed-text+
  "Taxonomy has been computed.
Please press  the \"Display Graph\" button to display it.
Note that this might take some time.
RacerPorter is not available during that time.")

(defconstant +taxonomy-request-text+
  "Please press the \"Request Graph\" button to request the Taxonomy from RacerPro.")

;;;
;;;
;;; 

(defun get-abox-graph-empty-text ()
  (let ((info nil)
        (info2 nil))
    (concatenate 'string 
                 "The ABox Graph is empty."
               
                 (case (profile-abox-graph-root-kind (active-profile))
                   (:current (if (not (profile-current-individual (active-profile)))
                                 (setf info "
No current individual!")
                               ""))
                   (:selected (if (not (profile-all-selected-individuals))
                                  (setf info "
No selected individuals!")
                                ""))
                   (otherwise ""))

                 (if info
                     " Possible solutions: either 
- enable radio button \"All Inds.\", or 
- determine some current / selected individuals by name using \"Search \& Select\" 
  (but ensure that \"Freeze Graph\" is disabled, or push \"Recover\" after search), or
- determine some current / selected individuals using another individual-oriented tab (e.g., \"Individuals\")
  (but ensure that \"Freeze Graph\" is disabled, and \"Cur. Ind.\" or \"Sel. Inds.\" are enabled)."
                   "")

                 ;;;
                 ;;;
                 ;;;

                 (setf info2 info
                       info nil)
               
                 (case (profile-role-hierarchy-root-kind (active-profile))
                   (:current (if (not (profile-current-role (active-profile)))
                                 (setf info "
No current role!")
                               ""))
                   (:selected (if (not (profile-all-selected-roles))
                                  (setf info "
No selected roles!")
                                ""))
                   (otherwise ""))

                 (if info
                     " Possible solutions: either 
- enable radio button \"All Roles\", or 
- determine some current / selected roles by name using \"Search \& Select\" 
  (but ensure that \"Freeze Graph\" is disabled, or push \"Recover\" after search), or 
- determine some current / selected roles using another role-oriented tab (e.g., \"Roles\")
  (but ensure that \"Freeze Graph\" is disabled, and \"Cur. Role\" or \"Sel. Roles\" are enabled)."
                   "")

                 ;;;
                 ;;;
                 ;;;

                 (if (or info info2)
                     "
More possible solutions: 
- select another ABox using the \"ABoxes\" tab, 
- reconnect to this Racer server using a \"Small TBoxes / Small ABoxes\" default profile and retry,
- disable checkbox \"Only Sel. Successors\", 
- ensure that the depth limit is set to a reasonable value (use \"UNBOUNDED\" only for small ABoxes)."
                   ""))))

(defconstant +abox-graph-error-text+
  "An error occurred during ABox Graph computation (perhaps the ABox is inconsistent - please inspect the Log tab). Sorry.")

(defconstant +abox-graph-aborted-text+
  "Computation of ABox Graph was aborted.")

(defconstant +abox-graph-running-text+
  "Computing ABox Graph.
Please wait.
You may cancel the request with the \"Cancel Request\" button.")

(defconstant +abox-graph-computed-text+
  "ABox Graph has been computed. 
Please press  the \"Display Graph\" button to display it.
Note that this might take some time. 
RacerPorter is not available during that time.")

(defconstant +abox-graph-request-text+
  "Please press the \"Request Graph\" button to request the ABox Graph from RacerPro.")

;;;
;;;
;;;

(defvar *section-counter* 0)

(defconstant +editor-paragraph+ (make-string 2 :initial-element #\Newline))


(defun section (string &optional (char #\=) (counter-p t))
  (let ((string 
         (if counter-p 
             (format nil "  Section ~A: ~A  " (incf *section-counter*) string)
           (format nil "  ~A  " string))))
    (format nil "~%~%~A~%~%~A~%~%~A~%~%" (get-separator string char) string (get-separator string char))))

(defun get-manual ()
  (concatenate 'string
            
               (section +welcome-text+ #\* nil)
              
               +version-info+
               
               (get-info-text)
               
               +newline-string+
               
               ;;; (section "Your Interaction With RacerPro Starts Here" #\* nil)
               
               +newline-string+
               +newline-string+))

(defconstant +long-info-text+
    "/s What's New in this Version of RacerPorter?

+ \"Axioms\" tab, providing for interactive graphical OWL2 axiom browsing, axiom inspection, and axiom editing  

+ Automatic creation of editable OWL2 axioms for OWL KBs which are either loaded via the \"Load...\"  button 
(or use shell command \"(owl-read-file <file.owl> :maintain-owlapi-axioms t )\") or created during the evaluation 
of an OWL buffer with the RacerEditor

+ \"Query IO\" tab for interactive input of nRQL (and SPARQL) queries and interactive selection of query results  

+ Improved completion facility (now also works for filenames)

+ Improved ABox graph (also displays datatype fillers now)

+ Improved shell with new function parameter documentation feature

+ Improved RacerEditor with support for the new OWL2 functional syntax   

+ Various bug fixes and improvements

+ Profile handling is more intuitive 

+ True concurrent multi-session capabilities (e.g., work with a different
server while a taxonomy graph is computed in the background)  



/s General Overview of Features 

RacerPorter offers... 

... a powerful and convenient shell (with shell history, completion facility, pretty-printing and syntax coloring, ...) 

... an Emacs-compatible editor with buffer evaluation mechanism. Supports KRSS, OWL(2) RDF, OWL2 Functional, SPARQL. 

... transmission of KBs or editor buffers to remote RacerPro servers   

... ABox, TBox and role hierarchy visualization facilities

... a browser-like navigator and history facility 

... advanced focus and update control mechanisms 

... a clipboard mechanism for information interchange and focus control  

... management of different RacerPro servers 

... multiple session / multi-connection awareness   

... an asynchronous, multi-threaded, non-blocking GUI

... and much more! 


/s Quick Start Guide


If you have a running RacerPro server on localhost:8088, simply press the
\"Connect\" button on the \"Profiles\" tab to get connected to that
server. You can then load knowledge bases (KBs) using the \"Load KB...\"
button onto that server. The following file types / syntaxes for KBs are
recognized and supported: OWL RDF (*.owl, *.rdf), KRSS / native RacerPro
syntax (*.racer, *.lisp), and OWL2 functional (*.funct).

Please note that you can also start a RacerPro server on the local
machine. Push \"Edit Profile\" and specify the path to the RacerPro executable
on the local machine. Then, use the \"Start Server\" button the start that
server. If the server has started up successfully, you will be able to connect
to it with the \"Connect\" button. It is also possible to automatically
connect to a freshly started server (see below).


After a successful connection the RacerPorter GUI will become \"alive\" and
the functionality provided by the different tabs can be accessed.

/s Profiles and the Profiles Tab


Communication with (possibly different) RacerPro servers is managed using
so-called profiles. A profile has a name and specifies at least the parameters
of the connection (TCP/IP address and port) to a RacerPro server (see
\"Connection Options\" in the \"Edit Profile...\" dialog) so that RacerPorter
knows how to connect to that server.

Profiles are managed with the \"Profiles\" tab. Simply select a profile from
the list of profiles in the \"Profiles\" tab to activate it. There is always
one active profile at a time. The name of the active profile is always shown
in the status display (field \"Active Profile\").

There is one default profile. The default profile is the one which is
automatically activated during startup of RacerPorter. If auto connect is
enabled for this profile, the connection to the specified RacerPro server is
automatically established.

Moreover, a profile can also specify parameters required for starting up a
RacerPro server on the local machine (see \"Server Startup Options\" in the
\"Edit Profile...\" dialog). For the default profile, it is possible to
startup a fresh RacerPro server during startup of RacerPorter, with parameters
specified in the default profile. For non-default profiles, the \"Start
Server\" button can be used to startup a fresh RacerPro server. In any case,
in order to enable RacerPorter to start RacerPro servers, you'll have to
specify the \"RacerPro Executable\" in the \"Edit Profile...\" dialog.
Additional server command line arguments can be specified, as explained in the
\"RacerPro Users Guide\". Note that these additional arguments will apply to
freshly started RacerPro servers only and will not affect already running
RacerPro servers.

A profile contains more information than meets the eye - information which
cannot be edited with the profile editor. A profile also contains the session
context (e.g., the current shell content, the shell history and the navigator
history), as well as the state of the RacerPorter GUI (e.g., the status of the
check boxes and other GUI-gadgets controlling visualization options).  This
session-specific information is used to revert the state of the RacerPorter
GUI to a previous state if a previously used profile is reactivated
again. Note that, all though the session-specific information is not made
persistent (when the profile is saved from the profiles editor), the
visualization options specified in a profile are in fact made persistent. This
is how the four default profiles differ - they provide different default
settings for various visualization options, depending on the size of the
ABoxes / TBoxes under consideration.

The \"Connect\" button is used to connect / disconnect to / from the RacerPro
server specified in the (currently active) profile. The whole set of profiles
shown in the \"Profiles\" tab. The profiles can be saved into a profiles file
(made persistent) from the profile editor (button \"OK \& Save\"). On startup,
RacerPorter automatically looks for the profiles file
\"/fsirius-profiles-file-ns \" and loads it if it exists.

Once you have established a connection to a RacerPro server, the functionality
provided in the other tabs becomes available.  RacerPorter can manage multiple
connections to possibly different RacerPro servers at a time. For example, you
may send a time consuming request to one server. While that server is busy,
simply switch to a different profile to work with a different RacerPro
server. Once the time consuming request has been processed, you will be
notified and RacerPorter can automatically switch back to the original session
(and profile). You can switch between different profiles (and thus, servers
and corresponding sessions) at any time.

/bracer-with-sirius The special profile named \"Internal RacerPro\" accesses the built-in RacerPro reasoning engine. 
/bracer-with-sirius Note that this profile gives you maximum performance by speaking directly   
/bracer-with-sirius to the built-in RacerPro reasoning engine, without network latency etc. 

The \"Profiles\" tab offers some auxiliary functionality via command
buttons. It is possible to store / restore an image of a RacerPro server. This
image contains the complete state (data structures) of the server in binary
form. Since the server state contains the results of inferences (which might
have took a long time to be reached, e.g., a taxonomy computation can easily
last an hour or more on complex OWL KBs), a server state may be very valuable
and it should thus be possible to store and restore it quickly. This is the
purpose of the \"Store Image\" and \"Restore Image\" buttons. The \"Full
Reset\" not only sends the \"(full-reset)\" command to RacerPro (which then
disposes all its ABoxes, TBoxes, Queries, Rules, OWL Ontologies, etc.), but
also resets the RacerPorter GUI (clears the histories and session context,
etc.). However, \"Full Reset\" only affects the active profile and its
corresponding server.

A very important button is the \"Load...\" button. The load function can even
transmit a file to a remote RacerPro server (which does not have access to the
file system on which RacerPorter is running).  However, this will only work is
this remote server is running in the so-called \"unsafe mode\". You can see
from the profiles list whether the connected server is running in unsafe mode.
If the server has been started by RacerPorter, then you can specify in the
profile editor whether this server should be started in unsafe mode (see
checkbox \"Start RacerPro in Unsafe Mode\").  Load accepts files with the
following extensions: *.owl, *.rdf, *.funct, *.racer, *.krss, *.lisp. For OWL
files, you will be asked whether a set of auxiliary OWL axioms shall be
created from the OWL file. Answer with \"Yes\" if you wish to edit or browser
the axioms with the help of the \"Axioms\" tab. However, axiom objects need
some additional memory.


/s Overview of the RacerPorter GUI


The RacerPorter GUI is divided into five main areas - from top to bottom:

1. The different tabs. 

2. The status display. There is a general notion of current / selected objects
employed in RacerPorter. There is a notion of a current TBox, current ABox,
current concept, current individual, and so on. These current \"objects\" are
shown in the corresponding fields in the status display. The current objects
can be referred to in the shell via the indicated \"*...*\" variables as
arguments to shell commands (e.g., enter \"(describe-tbox *t*)\" in the shell
to get a description of the current TBox).

In general, the current / selected objects are employed for information flow
and for focus control (= \"what to display / visualize\") in RacerPorter. For
example, each TBox \"contains\" a set of concept names. Thus, the content of
the \"Concepts\" tab (see below) depends on the current TBox. RacerPro API
functions often require certain arguments. These arguments are supplied by the
current and selected objects (of the appropriate type). A \"postfix command
mode pattern\" is thus used as the main interaction pattern - first select the
arguments, then select and invoke the operation on the selected arguments. If
an operation requires only one argument, then the current object (of the
required type) is used, e.g., the current concept. If an operator requires
more than one argument, also the selected objects are passed as arguments to
the operation. In general, an operator can be invoked in various ways (e.g.,
via a push button or via the shell).  Multiple arguments (and thus,
multi-selection of objects) are currently only supported for objects of type:
concept, individual, role, and axiom.  The number of selected objects (of the
corresponding type) is shown in the little numeric box after the current
object (of the corresponding type). Moreover, the selected objects are
displayed in a highlighted style in the corresponding item lists, e.g., the
selected concepts are highlighted in the \"Concepts\" tab which simply lists
the concept names which are available in the current TBox.

3. The history-navigator. For each interaction, a so-called history entry is
created and maintained in a list, the so-called history.  Previous views /
states of the RacerPorter GUI can be reestablished easily with the help of the
navigator buttons. Thus, it is possible to go back to / reawake a previous
session state. Similar to a web browser, this navigation in interaction
history is supported with the help of navigator buttons (back and forth). The
current history position and length of the history is shown as well.  Single
history entries (or the whole history) can be deleted with the help of the
buttons \"Delete\" (resp. \"Delete All\"). Note that this does not affect the
RacerPro server.

Sometimes, in case of errors, the widgets of RacerPorter are disabled,
even though RacerPorter is still connected to a \"living\" RacerPro
server which accepts requests. In order to recover RacerPorter from
such errors and \"reawake\" its GUI, please use the \"Recover (After
Error)\" button. This button also invalidates the cache of
RacerPorter. So, if you suspect that RacerPorter displays incorrect
information, please press this button as well (the cache may have
become inaccurate if another application has modified the state of the
connected RacerPro server).

If the \"Simplify\" checkbox is enabled, then namespace prefixes in OWL URIs
(and long filename directory prefixes) are truncated and abbreviated with the
#!:-prefix syntax. Please note that #!: denotes the current namespace. You can
define additional prefixes with the help of the shell and the
\"define-prefix\" command (e.g., \"(define-prefix \"racer\"
\"http://www.racer-systems.com\")\").

By default, the items to be displayed (e.g., in the \"Concepts\" tab) are
sorted alphabetically. Selected objects are highlighted. If the \"Selected
First\" checkbox is enabled, then selected objects appear before the other
objects (e.g., selected concepts are listed before the other concepts in the
\"Concepts\" tab). Sometimes, one wants to focus only on the selected
objects. This is the purpose of the \"Sel. Only\" checkbox. If the
\"Arg. Completion\" checkbox is enabled, then RacerPorter will collect the
results of executed RacerPro commands in order to present them as input
arguments in shell interactions. In general, it the tab key is pressed in the
shell, a pop-up menu is presented, offering possible completions. This
includes operator names as well as arguments (provided argument completion was
enabled).

4. The tab-specific main area. This area changes according to the selected
tab. We describe the different tabs and their functionalities below. In
general, there is a set of tab-specific buttons below the tab-specific main
area.

5. With the exception of the \"Log\", \"About\", and \"Shell\" tab, there is
the \"Info\" pane (field). It is similar to the \"Shell\" tab, but does NOT
accept input. Often, results of commands invoked via buttons are presented
here (e.g., the result produced by pushing the \"Concept Query\" button in the
\"Concepts\" tab on a selected concept is shown there, as well as in the
shell, so it can be reused).

Please note that all this displayed information is session and thus profile
specific (depends on the active profile and thus changes if another profile /
session is activated).


/s The Information Flow and Focus Control Mechanism


The set of of current / selected objects is also called the clipboard (since
it provides the infrastructure for information exchange between different
tabs). Let us illustrate how this clipboard is used for advanced ontology
inspection tasks. Suppose that, in the taxonomy graph display, you exclusively
want to focus on the concepts of which a certain individual is an instance of
(the \"types\" of that individual). To achieve this, you would first select
the individual of interest from the \"Individuals\" tab and press the \"All
Types\" button. As a side-effect of the execution of the \"All Types\"
command, these concepts have become selected. This can be seen in the numeric
display after the current concept field in the status display which simply
shows the number of selected concepts (the cardinality of the set of selected
concept).  Moreover, in the \"Concepts\" tab, the now selected concepts are
highlighted.  Secondly, go to the \"Taxonomy\" tab. Here, the selected
concepts are highlighted as well.  If you now enable the \"Selected Concepts\"
checkbox and enable the \"Tree\" radio button, then the taxonomy display will
present one sub-taxonomy for each selected concept in the form of a tree.

Many tabs offer the \"Search & Select\" field which is used to select objects
(individuals, concepts, roles, axioms) based on their names (or structured
content in the case of axioms).  Simply enter a text string into the \"Search
\& Select\" field and the objects which contain the search string somehow in
their names (or in their structure, in the case of axioms) as a substring will
become selected (note that you need to press enter). Please note that \"Search
& Select\" always adds additional selected objects, but never deselects
already selected objects (and thus works in an accumulating way). If required,
the set of selected objects (individuals, concepts, roles, ...) should be
cleared manually by pushing the appropriate \"Clear ...  Selection\" button)
before a fresh \"Search & Select\" is performed.

Note that the selection (resp. clipboard contents) can always be cleared with
the appropriate buttons (e.g., \"Clear Selected Individuals\" unselects all
individuals).




/s The Shell Tab


The \"Shell\" tab offers textual, command-based interaction with
RacerPro for advanced users. The syntax of the native RacerPro
commands to be entered into the shell is documented in the RacerPro
Reference Manual. Moreover, there is a completion facility which can
also be used to learn about the available commands - simply enter
\"\(\" and press the tab key. This pops up the list of commands
accepted by RacerPro. Commands often require certain arguments. The
list of acceptable arguments - the so-called function lambda list is
shown in the line below the shell. You can also press the \"Function
Doc\" button to see the full lambda list in case the lambda list was
truncated. Completion also works for filenames - try \"(owl-read-file
\" and press tab key. You can get rid of these popup menus by
pressing Esc.

Commands always start with \"(\". When entering a complex command spanning
multiple lines, you can simply press enter to start a fresh line. RacerPorter
will not send the command to RacerPro until the last closing \")\" parenthesis
has been entered.

The shell also recognizes SPARQL queries which have a different syntax. For
entering SPARQL queries spanning multiple lines, use Ctrl-Enter. When the
SPARQL query is complete, press Enter.

As already mentioned, the shell has a completion facility for commands
as well as for arguments. Partial command names can be completed with
the help of the tab key (e.g., start typing \"(describe-con\" into the
shell and press tab). In case there is more than one possible
completion, a popup menu appears (use enter to confirm and escape to
exit this menu). Argument completion will only work if the \"Arg.
Completion\" checkbox is enabled. Only then RacerPorter will collect
and accumulate results returned by RacerPro to be presented as
possible arguments to the user in this popup menu. However, filenames
can always be completed.

Additionally, the shell maintains a command history. Simply use Alt-p or
Ctrl-p to get the previous command, and use Alt-n or Ctrl-n to get the next
command in the command history. It is also possible to position the cursor in
a previously entered command expression and simply press enter. The command is
then reexecuted.  Moreover, commands in the command history are numbered. To
reexecute command with number <n>, simply enter <n> and press enter.

The \"\~\" (needed for the specification of pathnames on Linux or Mac) is on
Alt-z (Apple-z).

As already explained, the current RacerPorter TBox, ABox, concept, etc., as
indicated in the status display, are available as bindings of special
variables: use \"*t*\", \"*a*\", \"*c*\", etc., as documented in the status
display. For example, use \"(describe-tbox *t*)\" to supply the current
RacerPorter TBox as an argument to the \"describe-tbox\" function.

Furthermore, the results of the last three RacerPro commands executed via the
shell are available as bindings of the three special variables \"*\", \"**\",
and \"***\". For example, if you have executed \"(current-tbox)\" and RacerPro
has returned \"default\", then \"*\" is bound to \"default\". Thus, you can
use \"(describe-tbox *)\" instead of \"(describe-tbox default)\".

If you are working with OWL, then concepts, roles, individuals will have
so-called namespace prefixes, e.g.
\"http://www.owl-ontologies.com/unnamed.owl#\" is the namespace prefix of the
concept \"http://www.owl-ontologies.com/unnamed.owl#cat\". It is often awkward
of having to type these rather long names. An abbreviated syntax is thus
offered.  The same abbreviation syntax which is used for display if the
\"Simplify\" checkbox is enabled is also accepted for input, e.g., \"#!:cat\"
refers to the default namespace. The default namespace is shown in the status
display. You can define additional prefixes with the help of the shell and the
\"define-prefix\" command (e.g., \"(define-prefix \"racer\"
\"http://www.racer-systems.com\")\").  There is also the \"#&:\" prefix which
is used for nRQL (mirror) data substrate queries (please consult the RacerPro
User Guide for more details). Thus, \"#&:cat\" expands into
\"*http://www.owl-ontologies.com/unnamed.owl#cat\" (please note the \"*\" at
the beginning).

As explained, certain special variables are bound to the current objects,
e.g., \"*t*\" is bound to the current TBox. The reverse direction of data flow
regarding the current objects is also supported, since it is possible to set
the current or selected objects to a result or result set returned by some
shell command. This is the purpose of the buttons \"Selected Concepts := Last
Result\", \"Selected Roles := Last Result\", and \"Selected Individuals :=
Last Result\". For example, one possibility to select the concept instances of
the concept \"C\" is to enter \"(concept-instances C)\" and push the
\"Selected Individuals := Last Result\" button. The returned individuals will
the become the selected individuals. The selection can be cleared using the
appropriate \"Clear Individual Selection\" button.

In some cases, the result of a RacerPro command can be very big (e.g., the
result of \"(all-individuals)\" with a KB containing thousands of
individuals). In such cases there is the option to put an abbreviated result
into the shell instead of the whole result (a popup confirmer will ask you).

RacerPro may need some time to execute a command. Whenever a command is send,
RacerPorter waits 3 seconds for its result from RacerPro. If the result is
still not available after 3 seconds, the execution of the command is performed
as a background request (in a background task).  Thus, the shell returns with
a notification \":busy-backgrounding\", and the RacerPorter GUI becomes
available again. Thus, it is not blocking further user requests and \"appears
alive\". However, it will accept no more commands for this session (RacerPro
server), and thus, RacerPorter disables its widgets (they will be reactivated
as soon as the request has been satisfied and the answer is available).
However, RacerPorter is still alive and you can indeed continue working - you
can either connect to another RacerPro server using a different profile,
continue editing with RacerEditor, or switch to the \"Log\" tab to see what's
going on, etc.  If you haven't changed the active profile, RacerPorter
automatically switches back to the original tab when then result is
available. If you have continued working with a different profile, RacerPorter
notifies you that the background request issued by the other sessions has now
returned a result and asks whether it should switch back to the original
session (profile) and tab to inspect the result.

The shell offers automatic pretty printing and reindentation of user
input. However, if the \"Don't modify user input\" is enabled in the the
active profile (see profile editor), then user input is not pretty printed,
since users might feel as being patronized.  By default, pretty printing of
user input is disabled.


/s A General Note about Push Buttons in RacerPro


Some of the push buttons send commands to a connected RacerPro server (e.g.,
the \"Concept Query\" button). These buttons are called command buttons in the
following. The titles of these command buttons are displayed somewhat
highlighted (italic font on Windows and Linux and bold font on the
Macintosh). The other push buttons do not produce requests for RacerPro. The
following applies to command buttons only.

Some of the RacerPro requests invoked by command buttons may take some time to
compute. These requests are executed in the same way as if they were invoked
from the shell. So, commands which take more than 3 seconds to finish also
produce \":busy-backgrounding\" messages, as described in the section above.

In the profile editor, you can specify whether the commands produced by
commands buttons will be put into the shell command history or not.

Please note that many command buttons automatically select certain objects
(i.e., concepts, individuals, roles).  For example, the \"Concept Query\"
button returns a (possibly empty) set of individuals. The returned individuals
become selected automatically.


/s The RacerEditor


The editor can be invoked from the \"Shell\" tab by pushing either the \"Open
KB...\" or the \"New Editor\" button. The editor uses standard
Emacs-compatible key bindings (thus, Ctrl-x Ctrl-w saves a file, Ctrl-x Ctrl-f
loads a file, etc.). The \"GNU Emacs Reference Card\" is a good reference.

The editor has three modes:

- Racer mode (for editing RacerPro KBs in RacerPro native syntax),

- OWL RDF (XML) mode, and 

- OWL Function mode. 

In the Racer mode, you can incrementally evaluate expressions using
Ctrl-Shift-e, or send a whole buffer to a RacerPro server using Ctrl-Shift-h
(see also \"Buffer\" Menu). In the OWL RDF mode, Ctrl-Shift-y can be used to
evaluate an OWL buffer. In the OWL Functional mode Ctrl-Shift-f is
used. Please also consult the editor menu. The OWL buffer evaluation will only
work if the connected RacerPro server is running in unsafe mode (consult the
\"Profiles\" tab for this information) since buffer evaluation requires
creation of a temporary file on the machine on which RacerPro is running.

Note that Ctrl-Shift-e also puts the evaluated command into the shell and in
the shell command history, unlike Ctrl-Shift-h.

If you want to evaluate SPARQL queries in native syntax, then it is important
to note that SPARQL queries are enclosed between blank lines (one before and
one after the query), since otherwise start and end of the SPARQL query cannot
be detected. To evaluate a single SPARQL query, put the cursor on the query
and press Ctrl-Shift-s.

In case you are working with OWL, you often want to work with an axiomatic,
auxiliary representation of your OWL KB. RacerPro can create so-called OWL
axiom objects from the content of an OWL file. The created axioms can then be
inspected end edited conveniently with the axioms editor provided in the
\"Axioms\" tab. OWL axiom objects can also be created automatically if an OWL
buffer is evaluated (Ctrl-Shift-y, Ctrl-Shift-f) from the editor. It is
determined by the active profile whether an OWL buffer evaluation request
creates auxiliary OWL axioms (see checkbox \"Create Axioms with Editor
Eval\"). Note that reasoning will also work without these auxiliary OWL
axioms. However, the presence of the OWL axioms is required if the OWL KB (or
OWL file) shall be edited in the \"Axioms\" tab on an axiomatic level. Please
note that axiom objects may require a lot of memory if the OWL KB is big (and
thus contains a lot of axioms).


/s The TBoxes Tab


The \"TBoxes\" tab lists the available TBoxes on the RacerPro server
(specified by the active profile) and provides some general information about
each TBox: the name of the TBox, whether the TBox is classified, the number of
role and concept names present in the TBox, whether the TBox has cyclic
definitions, whether it has a so-called \"Meta Constraint\" (indicating that
reasoning with the TBox may be hard), the employed description logic, the
number of roles, and the (names of the) ABoxes which reference this TBox.

The current RacerPorter TBox is highlighted in this list. The current RacerPro
TBox (returned by \"(current-tbox)\") is shown between \">>> ... <<<\"
markers. In general, the content of the TBox-specific tabs (i.e., the concepts
tab, the roles tab, the taxonomy tab, and the role hierarchy tab) depend on
the RacerPorter TBox, and NOT on the current RacerPro TBox. Thus, it is NOT
change the current TBox (and thus the state) of the RacerPro server in order
to inspect another TBox. Just select (click on) another TBox in the \"TBoxes\"
tab.  Use the main (left) mouse button to select the current RacerPorter
TBox. TBox-specific commands invoked by RacerPorter (e.g., via command buttons
such as \"Describe TBox\") are always applied to the current RacerPorter TBox,
not to the current RacerPro TBox. If you really want to change the current
RacerPro TBox, use the \"Set Racer TBox\" button. But in general, this will
not be necessary if you work with RacerPorter since the current RacerPorter
TBox (as shown in the status display) will always be supplies as an argument
to all TBox-specific commands invoked by RacerPorter.


/s The ABoxes Tab

The \"ABoxes\" tab lists the available ABoxes on the connected RacerPro server
and provides some general information about each ABox, such as the name of the
ABox, whether it is consistent and/or realized, which associated TBox it has,
the number of \"contained\" individuals, the utilized description logic, the
umber of contained concept and role assertions, etc.

The \"ABoxes\" tab works analogous to the \"TBoxes\" tab. Note that RacerPro also
has a current ABox (result of \"(current-abox)\"). The situation regarding the
differences between the current RacerPorter ABox (shown in the status display)
and the current RacerPro ABox is analogous to the \"TBoxes\" tab (and discussed
there, see above).

Some ABox-specific commands can be invoked with command buttons. 


/s The Concepts Tab


The \"Concepts\" tab lists the available concepts names (also: OWL classes,
atomic concepts) in the current RacerPorter TBox. The concepts are sorted
alphabetically. The tab can show either all, show only the defined, show only
the primitive, or show only the unsatisfiable concepts in the TBox. Selected
concepts are shown highlighted (the concepts in the \"clipboard\"). If the
\"Sel. First\" checkbox is turned on, then the selected concepts are at
the top of the list. If \"Sel. Only\" is on, then only the selected
concepts are shown (note that this might result in an empty list if no
concepts are selected).

Concepts can be selected and deselected as usual (use the main / left mouse
button). Multi-selection is supported here. The last selected concept always
becomes the current concept - note how the \"Concept\" field in the status
display changes when you (de)select concepts. The small number behind the
current concept in the status display shows the total number of selected
concepts (the cardinality of the set of selected concepts). Concepts can be
selected with the help of the \"Search \& Select\" field as well, as explained
above. The selection can be cleared with the \"Clear Concept Selection\"
button.

Some concept-specific commands can be invoked by command buttons and either
apply to the current RacerPorter concept (e.g., \"Concept Query\"), or to the
selected concepts (e.g., \"Select Parents\"). Note that the \"Concept Query\"
button allows you to retrieve the instances of the current concept. The
retrieved instances will automatically become selected individuals (see
\"Individuals\" tab).


/s The Roles Tab


The \"Roles\" tab lists the roles in the current RacerPorter TBox (and their
corresponding inverses). It works analogous to the \"Concepts\" tab. The list
either shows all roles, shows only the transitive roles, shows only the
functional roles, etc., depending on the selection of the provided radio
buttons. Multi-selection of roles is supported. The last selected role becomes
the current role, and selected roles are highlighted.


/s The Individuals Tab


The \"Individuals\" tab lists the individuals in the current RacerPorter ABox
and works analogous to the \"Concepts\" tab. Multi-selection is supported. The
last selected individual becomes the current individual, and selected
individuals are highlighted as usual.


/s The Assertions Tab


The \"Assertions\" presents the ABox assertions contained in the current
RacerPorter ABox. Depending on the selection of the radio buttons, different
kinds of assertions are shown (concept assertions, role assertions, attribute
assertions, constraint assertions, annotation concept assertions and
annotation role assertion in case of OWL ABoxes).

Assertions can be selected and deselected as usual. Multi-selection is
supported. However, there is no current Assertion, only a set of selected
assertions. The selected assertions can be deleted with the \"Delete Selected
Assertions\" button. However, in general this kind of editing should be
performed with the \"Axioms\" tab (in case an OWL KB is edited).

/s The Axioms Tab


The \"Axioms\" tab offers a convenient and powerful OWL axiom browser and
editor. Axioms are either created by hand with the \"New Axiom\" button, or
automatically during load of an OWL file (see \"Load...\" button in the
\"Profiles\" tab). The RacerPro-internal OWL parser which is invoked by
\"Load...\", the function \"owl-read-file\", can create OWL axioms
automatically during the parsing of an OWL document.

An OWL axiom object is simply a record-like data structure (an object) which
has an ID, a type, and various axiom type-specific attributes. With the axiom
type selector (on the left side) the types of axioms to be displayed (in the
list on the right side) is determined.  Note that axiom types are organized in
a class hierarchy. Some axiom types are abstract. Axioms of abstract axiom
classes cannot be created.

Axioms are contained in an ontology container. Each ontology container
contains a set of axioms. Moreover, an ontology container is contained in a
so-called reasoner container. Each reasoner container corresponds to a
RacerPro KB - a TBox/ABox pair (with same name). An axiom in an ontology
container can either be loaded or unloaded. A loaded axiom is considered for
reasoning with the TBox/ABox which corresponds to the reasoner
container. There is one default reasoner container which is called the
OWLAPI-KB reasoner. Please note that there is also a corresponding TBox/ABox
with that name. The reasoner and ontology containers can be selected from the
comboboxes showing the current reasoner and current ontology container. The
content of the ontology combobox depends on the current reasoner container.

The axioms tab always shows the axioms which are contained in the current
reasoner container. Single axioms can be selected and deselected. The last
selected axiom becomes the current axiom. All other selected axioms are shown
highlighted. Command buttons are provided for loading and unloading of axioms,
etc. They all apply to the current ontology (in the current reasoner). Please
note that such a reasoner container is NOT identical with a RacerPro
server. Each single RacerPro server instance can host an arbitrary number of
these reasoner containers. Reasoner Containers are simply OWLAPI-conformant
notions of KBs.

Attributes of axioms can be edited. In order to edit an attribute you'll have
to select the appropriate row of that axiom. The selected attribute of that
axiom can then be edited by pressing the \"Edit Axiom Attribute\" button.

In case you want to create a new axiom, make sure to select a non-abstract
axiom type from the axiom-type selector on the left. Select the axiom type
from the type selector until the word \"NEW AXIOM\" appears behind it, and the
\"New Axiom\" button will get enabled. If you push it, an interactive
graphical axiom editor will pop up. Basically, you'll have to supply an
attribute value for each attribute of an axiom.  You can either enter
expressions by hand using the supplied editor fields, or select \"building
blocks\" for interactive expression composition from the presented
comboboxes. An item which is selected from such a combobox list will be
inserted in the editor at the current cursor position. In case PLURAL is used
for the attribute description (e.g., DESCRIPTIONS instead of DESCRIPTION),
more than one valid (concept / description) expression may be entered into the
corresponding editor field. Simply separate the different expression by some
whitespace (e.g., space or newline).

Sometimes, only the axioms which mention a certain concept (role, individual)
shall be selected. For example, suppose you are looking for all
individual-specific axioms which mention a certain individual. Or suppose you
are looking for all class axioms referring to a certain class.  The current /
selected objects (concepts, roles, individual, ...)  can be exploited for this
purpose. This is what the button \"Select Axioms by Selection\" does ; e.g.,
suppose you are interested in the SubClass axioms which mention the concept
#!:cat. First, ensure that #!:cat is a current / selected concept. This can be
achieved, for example, with the help of the \"Concepts\" (or \"Taxonomy\")
tab. First press the button \"Clear Concepts Selection\" (and \"Clear Role
Selection\" as well). Then, select the #!:cat concept (it becomes the current
concept). Switch back to the \"Axioms\" tab. Deselect all axiom types in the
axiom type selector and select only the \"SubClassAxiom\" type. Next, push the
button \"Select Concepts by Selection\". If you turn on the \"Sel. Only\"
checkbox, then only the SubClass axioms which mention the #!:cat class are
displayed.

Some other axiom-, ontology container-, and reasoner container-specific
commands are provided by command buttons. Note that \"Dispose Axioms\"
disposes (unloads and then deletes) all selected axioms.


/s The Taxonomy Tab


The \"Taxonomy\" tab presents a graph display of the so-called concept
subsumption hierarchy (the so-called taxonomy). What is said in the following
also applies to the other graph displays in RacerPorter (the \"Role
Hierarchy\" and the \"ABox Graph\" tab). All graph displays can be configured
to either update their content automatically (thus, they behave like the
already discussed simple list displays, e.g., the \"Concepts\" tab), or
manually. Automatic updates are performed if the \"Auto Update\" checkbox is
enabled. In case the checkbox is disabled, updates must be performed by
hand. Please use the \"Request Graph\", \"Display Graph\" and \"Reset Graph\"
buttons for this purpose.

The display of a graph is a two-step process. First, the information to be
displayed has to be acquired from RacerPro. If the \"Request Graph\" button is
pushed, a background job is started which acquires the graph information.
This job can be canceled with the \"Cancel Request\" button. Once the graph is
available, the \"Display Graph\" button can be used - this is the second
step. A layout has to be computed for the graph. Please note that this can
even take more time than the first phase. Unfortunately, the RacerPorter GUI
is not available during layout computation (this job can't be processed in the
background); thus, the GUI blocks and disables its widgets. However, in the
Linux and Windows version of RacerPorter, the layout computation can be
canceled if it takes too long. Unfortunately, this is not the case for the
Macintosh version of RacerPorter.

In general, \"Auto Update\" can be problematic for big graphs. In case graph
computation or graph layout takes too long, you may cancel the operation, and
retry with some other layout and/or focus options, e.g., by focusing only on
the current or selected concepts and by specifying a max graph depth cut-off
limit.  This depth limit can be specified in the little combobox; default is
\"unbounded\". In some cases, also the tree display visualization option
results in unacceptable performance, due to a combinatorial explosion.

A node in the taxonomy graph represents a set of synonym concept (OWL class)
names, and an edge between two nodes represents a (direct) subsumption
relationship between the concepts (OWL classes) represented by these nodes.
Selected concepts will be shown in red, and the current concept will be
highlighted.

Either the top concept is taken as the root of the taxonomy graph, or the
current concept, or each selected concept is taken as a root. This is
controlled by the radio buttons labeled \"All Concept\", \"Cur. Concept\",
\"Sel. Concepts\".  The layout of the graph can be horizontal or
vertical. Moreover, the graph can be drawn as a tree or a DAG (directed
acyclic graph). The Top and Bottom concepts can be omitted. In some cases (if
the taxonomy is very big) it is helpful to limit the maximum depth of the
graph display. Use the depth-limit combobox for this. In the taxonomy, select
a \"?\" node in order to further expand this node. Obviously, these \"?\"
nodes only appear if max-depth is not set to \"unbounded\".

As with the \"Concepts\" tab, nodes can be selected and deselected with the
mouse. Multi-selection is supported.

If \"Freeze Graph\" is turned on, then selection of a node and thus, changing
the current concept, does not automatically change the graph layout. Note
that, if \"Freeze Graph\" is turned off, and \"Cur. Concept\" display mode is
used, then changing the current concept with a click of the left mouse button
sets the graph root to the current concept. This behavior is sometimes
undesirable.

Some concept-specific commands can be applied with the present command
buttons.  Note that, if \"Auto Update\" and \"Selected Concepts\" is turned
on, and \"Freeze Graph\" is turned off, then using the \"Search \& Select\"
field results in a very dynamic display.

Please note that the default profiles for \"Big TBoxes\" differ from the
default profiles for \"Small TBoxes\" only w.r.t. the default display options,
e.g., the big TBoxes profile specifies a max depth, whereas the small TBoxes
profile specifies unbounded depth, etc. The same distinction applies to the
\"Big ABoxes\" vs. \"Small ABoxes\" profiles (w.r.t. ABox visualization
options).


/s The Role Hierarchy Tab


The \"Role Hierarchy\" tab works analogous to the \"Taxonomy\" tab and should
not require any further explanation. Nodes in the graph represent synonym role
names, and edges direct role subsumption relationships. Please note that,
unlike in the Taxonomy, there is no \"Top\" and \"Bottom\" role. Roles can be
selected and deselected by clicking on the nodes. Multi-selection is supported
and works as expected.


/s The ABox Graph Tab


The \"ABox Graph\" tab is slightly more complicated than the \"Role
Hierarchy\" and \"Taxonomy\" tab, since it allows to focus on roles as well as
on individuals. Thus, focus options are present for roles as well as for
individuals.

In the ABox graph, nodes represent individuals, and edges represent told
and/or inferred (logically entailed) role assertions. So, an R-labeled edge
between node \"i\" and \"j\" represents a role assertion \"R(i,j)\" (or
\"(related i j R)\" assertion in RacerPro syntax). Individuals can be selected
and deselected by clicking on the nodes. Multi-selection is supported and
works as expected. Edges cannot be selected (although it would make sense in
order to select roles - this will be supported in a future version of
RacerPorter).

The display of inferred role assertions and the display of role assertions for
transitive roles can be enabled or disabled (checkboxes \"Told Information\"
and \"Transitive Roles\").

Selected roles can be used to determine which role assertions to display.  In
this mode, a role assertion is visualized if the corresponding role is
selected.  The set of selected individuals is exploited in the obvious way.

Each individual supplies a graph root. Since the exploited graph layout
algorithm currently cannot display cyclic graphs, a tree unraveling of the
graph, starting from the root individual, is used. Cycles are indicated with
the help of the \":back-to\" annotation. Please note that the \"Search \&
Select\" field can select individuals as well as roles by name here.

As expected, some individual-specific commands are present as command buttons
(as in the \"Individuals\" tab).


/s The Query IO Tab


The \"Query IO\" tab offers a tabular display for query results from which
query results can be selected. Whenever a nRQL or SPARQL query is entered into
the shell (or editor using Ctrl-Shift-e), its result set is shown in the list
below the shell (which is duplicated here from the \"Shell\" tab for
convenient query input) in this tab. From this result list, items in answer
tuples (individuals or concept names) can be selected.  Note that, in
principle, a nRQL query can return individuals or concept names (in case of
tbox-retrieve). Multi-selection of result items (individuals or concept names)
is supported.


/s The Queries (and Rules) Tab


This is a simple list display. Queries and rules are maintained as objects
(similar to TBoxes and ABoxes) by RacerPro. As such, queries and rules have a
complex life cycle and a state. This tab is used to manage queries and rules
as well as their states.  Queries and rules are ABox-specific. The list shows
basic information about the loaded queries for the current (RacerPorter)
ABox. Each query has an ID, a state, etc. Please consult the \"RacerPro User
Guide\" for more details. Moreover, the list can be advised to display only
queries / rules in certain states. It is thus possible to focus exclusively on
queries / rules which have terminated, which are currently running, which are
prepared and thus ready to run, etc.

The list supports only single selection. Thus, only one current query / rule
can be selected (see status field \"Query / Rule\" and variable *qor*).


Some query- / rule-specific commands are available via buttons. Please consult
the \"RacerPro Users Guide\" for details about the corresponding
commands. Note that the commands are applied to the current query / rule, as
indicated by the \"Query / Rule\" field in the status display.

/s The Defined Queries Tab


A defined query is a simple macro for a complex nRQL query body. Defined
queries must by acyclic. This tab simply lists all query definitions. Defined
queries are TBox-specific. Some commands to maintain the definitions are
provided as buttons as well.

The list supports only single selection. Thus, only one current definition can
be selected (see status display field \"Definition\" and variable *def*).


/s The Log Tab


Here, all communication with RacerPro servers is logged. Each request gets a
unique ID and can either be synchronous (S) or asynchronous (A). Moreover, it
is indicated whether a request is answered with a local cache lookup, whether
a request is performed in the background, etc. This log is cleared
automatically every 10000 lines. Please note that RacerPorter can also
maintain a log file. This depends on the active profile. Please consult the
profile editor for RacerPorter log file specific settings.


/s The About Tab (Windows and Linux Version Only)


Here you will find a very nice picture of RacerPorter and some background
information, such as the current build and version number, etc.

/s About this Manual 

This manual was last updated on October 6th, 2008, for RacerPorter 1.9.3 Beta.

/s Thank You for using RacerPro and RacerPorter! 

")


       
(defun racer-with-sirius () 
  #+:racer-with-sirius t
  #-:racer-with-sirius nil)


(defun sirius-profiles-file-ns ()
  (namestring (sirius-profiles-file)))

(defun get-info-text () 
  (let ((*section-counter* 0))
    (labels ((do-it (string)

               (let* ((s-start (search "/s" string))
                      (p-start (search "/p" string))
                      (f-start (search "/f" string))
                      (b-start (search "/b" string))
                      (min (min (or s-start 10000)
                                (or p-start 10000)
                                (or f-start 10000)
                                (or b-start 10000))))

                 (cond ((and p-start (= min p-start))
                        (concatenate 'string
                                     (subseq string 0 p-start)
                                     +editor-paragraph+
                                     (do-it (subseq string (+ 2 p-start)))))

                       ((and s-start (= min s-start))
                        (let ((s-end (position #\newline string :start s-start)))
                          (concatenate 'string
                                       (subseq string 0 s-start)
                                       (section (subseq string (+ s-start 2) s-end))
                                       (do-it (subseq string (+ 2 s-end))))))

                       ((and f-start (= min f-start))
                        (let ((f-end (position-if #'lw:whitespace-char-p string :start f-start)))
                          (concatenate 'string
                                       (subseq string 0 f-start)
                                       (let ((sym
                                              (intern (string-upcase
                                                       (subseq string (+ f-start 2) f-end))
                                                      (find-package :sirius))))
                                         (if (fboundp sym)
                                             (funcall sym)
                                           (symbol-value sym)))
                                       (do-it (subseq string f-end)))))

                       ((and b-start (= min b-start))
                        (let ((b-end (position-if #'lw:whitespace-char-p string :start b-start)))
                          (concatenate 'string
                                       (subseq string 0 b-start)
                                       (let ((sym
                                              (intern (string-upcase
                                                       (subseq string (+ b-start 2) b-end))
                                                      (find-package :sirius)))
                                             (end (position #\newline string :start b-end)))
                                         
                                         (if (if (fboundp sym)
                                                 (funcall sym)
                                               (symbol-value sym))

                                             (concatenate 'string
                                                          (subseq string (1+ b-end) end)
                                                          (do-it (subseq string (+ 1 end))))

                                           
                                           (do-it (subseq string (+ 1 end))))))))
                                                                 
                       (t string)))))

      (remove-redundant-newlines (do-it +long-info-text+)))))

(defun remove-redundant-newlines (string)
  (let ((list (coerce string 'list))
        (res nil)
        (count 0))
    
    (loop while list do
          (let ((char (pop list)))
            (if (eq char #\newline)
                (incf count)
              (progn 
                (unless (zerop count)
                  (push #\newline res)
                  (when (> count 1)
                    (push #\newline res)))
                (setf count 0)
                (push char res)))))

    (coerce (nreverse res) 'string)))
     
