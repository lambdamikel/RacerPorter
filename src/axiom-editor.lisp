;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defconstant +option-pane-width+ 
  '(:character 80))

(defconstant +long-option-pane-width+ 
  '(:character 120))

(defconstant +editor-height+
  '(:character 10))

(defconstant +editor-width+
  '(:character 80))

;;;
;;;
;;;

(defconstant +concept-constructors+
  '(|and| |or| |not| 
          
          |some| |all| 
          |at-least| |at-most| |exactly| 
          |one-of| |self-reference| |has-value|
          
          |d-all| |d-some| 
          |d-at-least| |d-at-most| |d-exactly|
          |d-filler| ; = DataHasValue (Konzept-Konstruktor)

          |a| |an| |no| |min| |max| |divisible| |not-divisible|
          |string=| |string<>| |boolean=| |boolean<>|
          |>| |>=| |<| |<=| |<>| |=| 
          |equal| |unequal|))

(defconstant +datarange-constructors+
  '(|d-facet| |d-base-type|
              |d-and| |d-or| |d-complement| 
              |d-possible-values| |d-restriction|))

(defconstant +facets+
  '(|minInclusive| |maxInclusive|
    |minExclusive| |maxExclusive|
    |minLength| |maxLength| |length|))

(defconstant +xsd-integer+ '|http://www.w3.org/2001/XMLSchema#integer|)
(defconstant +xsd-string+  '|http://www.w3.org/2001/XMLSchema#string|)
(defconstant +rdf-comment+ '|http://www.w3.org/2000/01/rdf-schema#comment|)

(defconstant +datatypes+
  '(|http://www.w3.org/2002/07/owl#real|
    |http://www.w3.org/2002/07/owl#rational|
    
    |http://www.w3.org/2001/XMLSchema#decimal|
    |http://www.w3.org/2001/XMLSchema#integer|
    |http://www.w3.org/2001/XMLSchema#nonNegativeInteger|
    |http://www.w3.org/2001/XMLSchema#nonPositiveInteger|
    |http://www.w3.org/2001/XMLSchema#positiveInteger|
    |http://www.w3.org/2001/XMLSchema#negativeInteger|
    |http://www.w3.org/2001/XMLSchema#long|
    |http://www.w3.org/2001/XMLSchema#int|
    |http://www.w3.org/2001/XMLSchema#short|
    |http://www.w3.org/2001/XMLSchema#byte|
    |http://www.w3.org/2001/XMLSchema#unsignedLong|
    |http://www.w3.org/2001/XMLSchema#unsignedInt|
    |http://www.w3.org/2001/XMLSchema#unsignedShort|
    |http://www.w3.org/2001/XMLSchema#unsignedByte|

    ;;; aus Racer known-datatype-p: 

    |http://www.w3.org/2001/XMLSchema#float|
    |http://www.w3.org/2001/XMLSchema#double|
    |http://www.w3.org/2001/XMLSchema#boolean|
    |http://www.w3.org/2001/XMLSchema#string|
    |http://www.w3.org/2001/XMLSchema#normalizedString|))

(defconstant +entities+
  '(|Datatype|
    |Class| 
    |ObjectProperty| 
    |DataProperty|
    |AnnotationProperty|
    |NamedIndividual|
    |Annotation|))

;;;
;;;
;;;

(defvar *ok* nil)

(defvar *text* nil)

;;;
;;;
;;;

(defun in-parens (x) 
  (concatenate 'string "(" x ")"))

(defun without-parens (x) 
  (concatenate 'string 
               " " 
               (subseq x 1 (1- (length x)))
               " "))

(defun pretty-name (attribute editor-type)
  (declare (ignorable editor-type))
  attribute)

(defun my-read-from-string1 (expr)
  (let ((*package* (find-package :sirius)))
    (my-read-from-string expr)))

;;;
;;;
;;; 

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun comp-fn (x y)
    (string= (ensure-string x)
             (ensure-string y)))
  
  (defun to-keyword (sym)
    (let ((sym
           (string-upcase (ensure-string sym))))
      (intern sym :keyword)))

  (defun rmember (item list)
    (member item list :test #'(lambda (x y)
                                (string-equal (ensure-string x)
                                              (ensure-string y)))))

  ;;;
  ;;;
  ;;;

  (defun parse-concept (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))

  (defun valid-concept-p (expr attrib)
    (ignore-errors 
      (let ((expr (parse-concept expr attrib)))
        (valid-concept1-p expr))))

  (defun valid-concept1-p (expr)
    (cond ((symbolp expr)
           t)

          ((consp expr)
               
           (case (first expr)
             ((|not| not)
              (valid-concept1-p (second expr)))
             ((|and| |or| and or)
              (every #'valid-concept1-p (rest expr)))
             ((|some| |all| some all)
              (and (valid-role1-p (second expr))
                   (valid-concept1-p (third expr))))
             ((|at-least| |at-most| |exactly| at-least at-most exactly)
              (and (integerp (second expr))
                   (not (minusp (second expr)))
                   (valid-role1-p (third expr))
                   (=> (fourth expr)
                       (valid-concept1-p (fourth expr)))))
             ((|one-of| one-of)
              (every #'symbolp (rest expr)))
             ((|has-value| has-value)
              (symbolp (second expr)))
             ((|self-reference| self-reference)
              (valid-role1-p (second expr)))
             ((|d-all| |d-some| d-all d-some)
              (and (valid-role1-p (second expr))
                   (valid-data-range1-p (third expr))))
             ((|d-at-least| |d-at-most| |d-exactly|
                            d-at-least d-at-most d-exactly)
              (and (integerp (second expr))
                   (not (minusp (second expr)))
                   (valid-role1-p (third expr))
                   (=> (fourth expr)
                       (valid-data-range1-p (fourth expr)))))
             ((|d-filler| d-filler)
              (and (valid-role1-p (second expr))
                   (valid-literal1-p (third expr))))
             ((|a| |an| |no| a an no)
              (second expr))
             ((|min| |max| |divisible| |not-divisible|
                     |string=|  |string<>| |boolean=| |boolean<>|
                     |>| |>=| |<| |<=| |<>| |=| 
                     |equal| |unequal|

                     min max divisible not-divisible
                     string=  string<> boolean= boolean<>
                     > >= < <= <> = 
                     equal unequal
                     )
              (and (second expr)
                   (third expr)))
             (otherwise nil)))))

  ;;;
  ;;;
  ;;;

  (defun parse-concepts (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))
    
  (defun valid-concepts-p (expr attrib)
    (ignore-errors 
      (let ((expr (parse-concepts expr attrib)))
        (valid-concepts1-p expr))))

  (defun valid-concepts1-p (expr)
    (every #'valid-concept1-p expr))
  
  ;;;
  ;;;
  ;;;

  (defun parse-data-range (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))

  (defun valid-data-range-p (expr attrib)
    (ignore-errors
      (let ((expr (parse-data-range expr attrib)))
        (valid-data-range1-p expr))))


  (defun valid-data-range1-p (expr)
    (cond ((symbolp expr)
           (or (rmember expr '(string integer boolean real))
               (rmember expr +datatypes+)
               (null expr)))

          ((consp expr)
         
           (case (first expr)
         
             ((|d-base-type| d-base-type)
              (rmember (second expr)
                       +datatypes+))

             ((|d-complement| d-complement)
              (valid-data-range1-p (second expr)))
           
             ((|d-and| |d-or| d-and d-or)
              (every #'valid-data-range1-p (rest expr)))

             ((|d-possible-values| d-possible-values)
              (every #'valid-data-range1-p (rest expr)))

             ((|d-restriction| d-restriction)
              (let* ((basetype (valid-data-range1-p (second expr)))
                     (facets-and-values (cddr expr))
                     (owl-facets-and-values
                      (loop as facet in facets-and-values by #'cddr 
                            as value in (cdr facets-and-values) by #'cddr 
                            collect (list facet value))))
                (and basetype 
                     (every #'(lambda (x) 
                                (apply #'valid-facet-restriction-p x))
                            owl-facets-and-values))))

             (otherwise nil)))
        
          (t nil)))

  (defun valid-facet-restriction-p (facet value)
    (declare (ignorable value))
    (rmember facet +facets+))
  
  ;;;
  ;;;
  ;;;

  (defun parse-role (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))
    
  (defun valid-role-p (expr attrib)
    (ignore-errors
      (let ((expr (parse-role expr attrib)))
        (valid-role1-p expr))))

  (defun valid-role1-p (expr)
    (or (symbolp expr)
        (and (consp expr)
             (or (eq (first expr) '|inv|)
                 (eq (first expr) 'inv))
             (valid-role1-p (second expr)))))

  ;;;
  ;;;
  ;;; 

  (defun parse-roles (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))
    
  (defun valid-roles-p (expr attrib)
    (ignore-errors
      (let ((expr (parse-roles expr attrib)))
        (valid-roles1-p expr))))

  (defun valid-roles1-p (expr)
    (and (consp expr)
         (every #'valid-role1-p expr)))

  ;;;
  ;;;
  ;;; 

  (defun parse-individual (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))
    
  (defun valid-individual-p (expr attrib)
    (ignore-errors
      (let ((expr (parse-individual expr attrib)))
        (valid-individual1-p expr))))

  (defun valid-individual1-p (expr)
    (symbolp expr))

  ;;;
  ;;;
  ;;; 

  (defun parse-individuals (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))
    
  (defun valid-individuals-p (expr attrib)
    (ignore-errors
      (let ((expr (parse-individuals expr attrib)))
        (valid-individuals1-p expr))))

  (defun valid-individuals1-p (expr)
    (and (consp expr)
         (every #'valid-individual1-p expr)))

  ;;;
  ;;;
  ;;; 

  (defun parse-literal (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))

  (defun valid-literal-p (expr attrib)
    (ignore-errors 
      (let ((expr (parse-literal expr attrib)))
        (valid-literal1-p expr))))

  (defun valid-literal1-p (expr)
    (and (consp expr)
         (or (eq (first expr) '|d-literal|)
             (eq (first expr) 'd-literal))
         (stringp (second expr))
         (=> (third expr)  ; sonst NIL (AUTO TYPE) 
             (valid-data-range1-p (third expr)))))

  ;;;
  ;;;
  ;;; 

  (defun parse-entity (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))
  
  (defun valid-entity-p (expr attrib)
    (ignore-errors
      (let ((expr (parse-entity expr attrib)))
        (valid-entity1-p expr))))

  (defun valid-entity1-p (expr)
    (and (consp expr)
         (not (cddr expr))
         (rmember (first expr) +entities+)))

  ;;;
  ;;;
  ;;;

  (defun parse-annotation (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))

  (defun valid-annotation-p (expr attrib)
    (ignore-errors
      (let ((expr (parse-annotation expr attrib)))
        (valid-annotation1-p expr))))

  (defun valid-annotation1-p (expr)
    (and (consp expr)
         (= (length expr) 3)
         (rmember (first expr) '(|Annotation|))))

  ;;;
  ;;;
  ;;;

  (defun parse-annotation-value (expr attrib)
    (declare (ignorable attrib))
    (my-read-from-string1 expr))

  (defun valid-annotation-value-p (expr attrib)
    (ignore-errors
      (let ((expr (parse-annotation-value expr attrib)))
        (valid-annotation-value1-p expr))))

  (defun valid-annotation-value1-p (expr)
    (consp expr))

  ;;;
  ;;;
  ;;; 

  (defun parse-axiom-id (expr attrib)
    (declare (ignorable attrib))
    (parse-integer expr))
  
  (defun valid-axiom-id-p (expr attrib)
    (ignore-errors
      (let ((expr (parse-axiom-id expr attrib)))
        (valid-axiom-id1-p expr))))

  (defun valid-axiom-id1-p (expr)
    (plusp expr))

  ;;;
  ;;;
  ;;; 

  (defun parse-uri (expr attrib)
    (declare (ignorable attrib))
    expr)
  
  (defun valid-uri-p (expr attrib)
    (ignore-errors
      (let ((expr
             (parse-uri expr attrib)))
        (valid-uri1-p expr))))

  (defun valid-uri1-p (expr)
    (symbolp expr))

  ;;;
  ;;;
  ;;; 

  (defun parse-prefix (expr attrib)
    (declare (ignorable attrib))
    expr)
  
  (defun valid-prefix-p (expr attrib)
    (ignore-errors
      (let ((expr
             (parse-prefix expr attrib)))
        (valid-prefix1-p expr))))

  (defun valid-prefix1-p (expr)
    (symbolp expr))

  ;;;
  ;;;
  ;;; 

  (defun parse-ontology (expr attrib) 
    (declare (ignorable attrib))
    (my-read-from-string1 expr))
  
  (defun valid-ontology-p (expr attrib)
    (ignore-errors
      (let ((expr 
             (parse-ontology expr attrib)))
        (valid-ontology1-p expr))))

  (defun valid-ontology1-p (expr)
    (with-profile-access
      (and (rmember expr
                   (racer-function1 (owlapi-get-ontologies current-reasoner) nil)))))

  ;;;
  ;;;
  ;;; 

)

;;;
;;;
;;;

(defun make-editor (value)
  (handler-case 
      (make-instance 'capi:multi-line-text-input-pane
                     :visible-min-height +editor-height+
                     :visible-max-height +editor-height+
                     :visible-min-width +editor-width+ 
                 ;:visible-max-width +editor-width+ 
                 
                     :vertical-scroll t
                     :horizontal-scroll t
                     :text value)
    (error ()
      (beep-pane))))


(defun insert-text (editor insert)
  (let* ((point (text-input-pane-caret-position editor))
         (text (text-input-pane-text editor))
         (n (length text))
         (insert 
          (apply #'concatenate 'string 
                 (remove nil
                         (list 
                          (when (and (not (zerop point))
                                     (not (or (char= (elt (text-input-pane-text editor) (1- point))
                                                     #\space)
                                              (char= (elt (text-input-pane-text editor) (1- point))
                                                     #\())))
                                                   
                            " ")
                          (line-item-printer insert nil)
                          (when (and (< point n)
                                     (not (or (char= (elt (text-input-pane-text editor) point)
                                                     #\space)
                                              (char= (elt (text-input-pane-text editor) point)
                                                     #\)))))
                                                                     
                            " "))))))
                           
    (setf (text-input-pane-text editor) 
          (concatenate 'string 
                       (subseq text 0 point)
                       insert
                       (subseq (text-input-pane-text editor) point)))

    (setf (text-input-pane-caret-position editor)
          (+ point (length insert)))))


(defun standard-layout (items)
  (make-instance 'capi:column-layout 
                 :description
                 items
                 :adjust :left))


(defconstant +ok-cancel-buttons+ 
  #'(lambda (value-fn)
      (let* ((ok-button 
              (make-instance 'push-button
                             :callback-type :interface
                             :callback #'(lambda (interface)
                                           (setf *ok* t
                                                 *text* 
                                                 (funcall value-fn))
                                           (quit-interface interface))
                             :text "OK"))
   
             (cancel-button
              (make-instance 'push-button
                             :callback-type :interface
                             :callback #'(lambda (interface)
                                           (setf *ok* nil)
                                           (quit-interface interface))
                             :text "Cancel"))

             (buttons
              (make-instance 'capi:row-layout 
                             :description (list ok-button cancel-button))))

        buttons)))

(defmacro standard-return (editor)
  `#'(lambda () 
       (let ((text (text-input-pane-text ,editor)))
         (if multi-p 
             (in-parens text)
           text))))

;;;
;;;
;;; 

(defconstant +concept-editor+
  #'(lambda (id attribute value &optional multi-p)
      (declare (ignorable multi-p))
      (with-profile-access
        (let* ((concept (when (and (valid-concept-p value attribute) 
                                   (symbolp value))
                          value))
               (title (if multi-p "Concept Description" "Concept Description"))
               (role nil)
               (operator (when (consp value) (first value)))

               (print-value (if value (block-item-printer value nil) ""))
               
               (editor
                (make-editor 
                 (if multi-p
                     (if (consp value)
                         (without-parens print-value)
                       print-value)
                   print-value)))

               (operator-selector 
                (make-instance 'option-pane
                               :title "Concept Constructor"
                               :title-position :center
                               :test-function #'comp-fn 
                               :selected-item operator
                               :items 
                               (let ((items (append +concept-constructors+
                                                    +datarange-constructors+
                                                    +facets+
                                                    +datatypes+)))
                                 (unless operator 
                                   (setf operator (first items)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf operator data)
                                   (insert-text editor `(,operator))
                                   (decf (text-input-pane-caret-position editor)))))
               
               (concept-selector 
                (make-instance 'option-pane
                               :title "Concept Name" 
                               :title-position :center
                               :test-function #'comp-fn
                               :print-function #'line-item-printer
                               :selected-item concept
                               :items 
                               (let ((items 
                                      (msort 
                                       (racer-function1 (all-atomic-concepts current-reasoner) nil)
                                       #'string<
                                       :key #'symbol-name)))
                                 (unless concept
                                   (setf concept (first items)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf concept data)
                                   (insert-text editor concept))))

               (role-selector 
                (make-instance 'option-pane
                               :title "Role"
                               :title-position :center
                               :test-function #'comp-fn
                               :print-function #'line-item-printer
                               :selected-item role
                               :items 
                               (let ((items
                                      (msort
                                       (remove-if #'consp 
                                                  (racer-function1 (all-roles current-reasoner) nil))
                                       #'string<
                                       :key #'symbol-name)))
                                 (unless role
                                   (setf role (first items)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf role data)
                                   (insert-text editor role))))


               (title 
                (make-instance 'title-pane 
                               :text (format nil "~A for Attribute ~A of Axiom ~A" 
                                             title 
                                             (pretty-name attribute :concept-editor)  
                                             id))))

          (list title editor 
                concept-selector
                role-selector 
                operator-selector
                (standard-return editor))))))
      
(defconstant +object-property-editor+
  #'(lambda (id attribute value &optional multi-p)  
      (with-profile-access
        (let* ((role (when (valid-role-p value attribute) value))
               (title  (if multi-p "Object Properties" "Object Property"))
                
               (print-value (if value (line-item-printer value nil) ""))
               
               (editor
                (make-editor 
                 (if multi-p
                     (if (consp value)
                         (without-parens print-value)
                       print-value)
                   print-value)))

               (role-selector 
                (make-instance 'option-pane
                               :title title
                               :title-position :center
                               :test-function #'comp-fn
                               :print-function #'line-item-printer 
                               :selected-item role
                               :items 
                               (let ((items 
                                      (msort 
                                       (remove-if #'consp
                                                  (racer-function1 (all-roles current-reasoner) nil))
                                       #'string<
                                       :key #'symbol-name)))
                                 (unless role
                                   (setf role (first items)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf role data)
                                   (unless multi-p 
                                     (setf (text-input-pane-text editor) ""))
                                   (insert-text editor role))))

               (title 
                (make-instance 'title-pane 
                               :text (format nil "~A for Attribute ~A of Axiom ~A" 
                                             title
                                             (pretty-name attribute :object-property-editor)
                                             id))))

              
          (list title editor role-selector 
                (standard-return editor))))))


(defconstant +data-property-editor+
  #'(lambda (id attribute value &optional multi-p) 
      (with-profile-access
        (let* ((role (when (valid-role-p value attribute) value))
               (title (if multi-p "Datatype Properties" "Datatype Property"))
               (print-value (if value (line-item-printer value nil) ""))
               
               (editor
                (make-editor 
                 (if multi-p
                     (if (consp value)
                         (without-parens print-value)
                       print-value)
                   print-value)))

               (role-selector 
                (make-instance 'option-pane
                               :title title 
                               :title-position :center
                               :test-function #'comp-fn
                               :print-function #'line-item-printer
                               :selected-item role
                               :items 
                               (let ((items 
                                      (msort 
                                       (remove-if-not #'(lambda (x) 
                                                          (racer-function1
                                                           (role-used-as-datatype-property-p x current-reasoner)
                                                           nil))
                                                      (remove-if #'consp
                                                                 (racer-function1 (all-roles current-reasoner) nil)))
                                       #'string<
                                       :key #'symbol-name)))
                                 (unless role
                                   (setf role (first items)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf role data)
                                   (unless multi-p (setf (text-input-pane-text editor) ""))
                                   (insert-text editor role))))

               (title 
                (make-instance 'title-pane 
                               :text (format nil "~A for Attribute ~A of Axiom ~A" 
                                             title
                                             (pretty-name attribute :data-property-editor)
                                             id))))

          (list title editor role-selector 
                (standard-return editor))))))


(defconstant +annotation-property-editor+
  #'(lambda (id attribute value &optional multi-p) 
      (with-profile-access
        (let* ((role (when (valid-role-p value attribute) value))
               (title (if multi-p "Annotation Properties" "Annotation Property"))
               (print-value (if value (line-item-printer value nil) ""))
               
               (editor
                (make-editor 
                 (if multi-p
                     (if (consp value)
                         (without-parens print-value)
                       print-value)
                   print-value)))

               (role-selector 
                (make-instance 'option-pane
                               :title title 
                               :title-position :center
                               :test-function #'comp-fn
                               :print-function #'line-item-printer
                               :selected-item role
                               :items 
                               (let ((items 
                                      (msort 
                                       (remove-if-not #'(lambda (x) 
                                                          (racer-function1
                                                           (role-used-as-annotation-property-p x current-reasoner)
                                                           nil))
                                                      (remove-if #'consp
                                                                 (racer-function1 (all-roles current-reasoner) nil)))
                                       #'string<
                                       :key #'symbol-name)))
                                 (unless role
                                   (setf role (first items)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf role data)
                                   (unless multi-p (setf (text-input-pane-text editor) ""))
                                   (insert-text editor role))))

               (title 
                (make-instance 'title-pane 
                               :text (format nil "~A for Attribute ~A of Axiom ~A" 
                                             title
                                             (pretty-name attribute :annotation-property-editor)
                                             id))))

          (list title editor role-selector 
                (standard-return editor))))))


(defconstant +individual-editor+
  #'(lambda (id attribute value &optional multi-p)
      (with-profile-access
        (let* ((ind (when (and (valid-individual-p value attribute)
                               (symbolp value))
                      value))
               (title (if multi-p "Individuals" "Individual"))
               (print-value (if value (line-item-printer value nil) ""))
               
               (editor
                (make-editor 
                 (if multi-p
                     (if (consp value)
                         (without-parens print-value)
                       print-value)
                   print-value)))

               (ind-selector 
                (make-instance 'option-pane
                               :title title 
                               :title-position :center
                               :test-function #'comp-fn
                               :selected-item ind
                               :items 
                               (let ((items 
                                      (msort 
                                       (racer-function1 (all-individuals current-reasoner) nil)
                                       #'string<
                                       :key #'symbol-name)))
                                 (unless ind
                                   (setf ind (first items)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :print-function #'line-item-printer
                               :selection-callback
                               #'(lambda (data)
                                   (setf ind data)
                                   (unless multi-p 
                                     (setf (text-input-pane-text editor) ""))
                                   (insert-text editor ind))))

               (title 
                (make-instance 'title-pane 
                               :text (format nil "~A for Attribute ~A of Axiom ~A" 
                                             title
                                             (pretty-name attribute :individual-editor)
                                             id))))

          (list title editor ind-selector 
                (standard-return editor))))))


(defconstant +base-type-selector+
  #'(lambda (&rest args)
      (apply #'make-instance 'option-pane
             :title "Base Type"
             :title-position :center
             :test-function #'comp-fn
             :print-function #'line-item-printer-also-print-nil
             :items (cons nil +datatypes+)
             :visible-min-width +option-pane-width+
             ;:visible-max-width +option-pane-width+
             :callback-type :data
             args)))


(defconstant +value-editor+
  #'(lambda (id attribute value) 
      (with-profile-access
        (let* ((lit-value (when (consp value) (second value)))
               (base-type (when (consp value) (second (third value))))

               (print-value (if lit-value (line-item-printer lit-value nil) ""))
               
               (value-editor
                (make-editor print-value))

               (base-type-selector 
                (funcall +base-type-selector+ 
                         :selected-item base-type 
                         :selection-callback 
                         #'(lambda (data) (setf base-type data))))                

               (editor 
                (make-instance 'capi:column-layout 
                               :description
                               (list value-editor base-type-selector)
                               :adjust :center))
                   
               (title 
                (make-instance 'title-pane 
                               :text (format nil "Data Literal for Attribute ~A of Axiom ~A"
                                             (pretty-name attribute :value-editor)
                                             id))))
                   
          (list title editor 
                #'(lambda ()
                    (format nil "(|d-literal| ~A (|d-base-type| |~A|))"
                            (text-input-pane-text value-editor)
                            base-type)))))))


(defconstant +annotation-value-editor+
  #'(lambda (id attribute value) 
      (with-profile-access
        (let* ((lit-value (when (consp value) (second value)))
               (base-type (when (consp value) (second (third value))))

               (print-value (if lit-value (line-item-printer lit-value nil) ""))
               
               (value-editor
                (make-editor print-value))

               (base-type-selector 
                (funcall +base-type-selector+ 
                         :selected-item base-type 
                         :selection-callback 
                         #'(lambda (data) (setf base-type data))))                

               (editor 
                (make-instance 'capi:column-layout 
                               :description
                               (list value-editor base-type-selector)
                               :adjust :center))
                   
               (title 
                (make-instance 'title-pane 
                               :text (format nil "Annotation Data Literal for Attribute ~A of Axiom ~A"
                                             (pretty-name attribute :value-editor)
                                             id))))
                   
          (list title editor 
                #'(lambda ()
                    (format nil "(|d-literal| ~A (|d-base-type| |~A|))"
                            (text-input-pane-text value-editor)
                            base-type)))))))


(defconstant +ontology-editor+
  #'(lambda (id attribute value) 
      (with-profile-access
        (let* ((ont value)
               (multi-p nil)

               (print-value (if value (line-item-printer value nil) ""))
               
               (editor
                (make-editor print-value))

               (ont-selector 
                (make-instance 'option-pane
                               :title "Ontology"
                               :title-position :center
                               :test-function #'comp-fn
                               :print-function #'line-item-printer
                               :selected-item ont
                               :items 
                               (let ((items 
                                      (msort 
                                       (racer-function1 (owlapi-get-ontologies current-reasoner) nil)
                                       #'string<
                                       :key #'symbol-name)))
                                 (unless ont
                                   (setf ont current-ontology))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf ont data)
                                   (setf (text-input-pane-text editor) "")
                                   (insert-text editor ont))))

               (title 
                (make-instance 'title-pane 
                               :text (format nil "Ontology for Attribute ~A of Axiom ~A"
                                             (pretty-name attribute :ontology-editor)
                                             id))))

          (list title editor ont-selector
                (standard-return editor))))))


(defconstant +uri-editor+
  #'(lambda (id attribute value) 
      (with-profile-access
        (let* ((print-value (if value (line-item-printer value nil) ""))
               (editor
                (make-editor print-value))
               
               (title 
                (make-instance 'title-pane 
                               :text (format nil "URI for Attribute ~A of Axiom ~A" 
                                             (pretty-name attribute :uri-editor)
                                             id))))

          
          (list title editor 
                #'(lambda () 
                    (my-read-from-string1
                     (text-input-pane-text editor))))))))


(defconstant +prefix-editor+
  #'(lambda (id attribute value) 
      (with-profile-access
        (let* ((print-value (if value (line-item-printer value nil) ""))
               (editor
                (make-editor print-value))
               
               (title 
                (make-instance 'title-pane 
                               :text (format nil "NAMESPACE-PREFIX for Attribute ~A of Axiom ~A" 
                                             (pretty-name attribute :prefix-editor)
                                             id))))

          
          (list title editor 
                #'(lambda () 
                    (my-read-from-string1
                     (text-input-pane-text editor))))))))


(defconstant +axiom-id-editor+
  #'(lambda (id attribute value) 
      (with-profile-access
        (let* ((ax-id (when (numberp value) value))

               (multi-p nil)
               (print-value (if value (line-item-printer value nil) ""))
               
               (editor
                (make-editor print-value))

               (id-selector 
                (make-instance 'option-pane
                               :title "Axiom ID"
                               :title-position :center
                               :test-function #'comp-fn
                               :print-function #'line-item-printer
                               :selected-item ax-id
                               :items 
                               (let ((items 
                                      (msort 
                                       (racer-function1 
                                        (owlapi-get-axioms-in current-ontology current-reasoner t))
                                       #'<
                                       :key #'first)))
                                 (when ax-id
                                   (setf ax-id (first items)))
                                 items)
                               :visible-min-width +long-option-pane-width+
                               ;:visible-max-width +long-option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf ax-id (first data))
                                   (setf (text-input-pane-text editor) "")
                                   (insert-text editor ax-id))))
                   
               (title 
                (make-instance 'title-pane 
                               :text (format nil "Axiom ID for Attribute ~A of Axiom ~A"
                                             (pretty-name attribute :axiom-id-editor)
                                             id))))

          (list title editor id-selector
                (standard-return editor))))))



(defconstant +data-range-editor+
  #'(lambda (id attribute value) 
      (with-profile-access
        (let* ((operator '|d-base-type|)
               (base-type +xsd-integer+)
                   
               (multi-p nil)
               (print-value (if value (line-item-printer value nil) ""))
               
               (editor
                (make-editor print-value))
                   
               (base-type-selector 
                (funcall +base-type-selector+
                         :selected-item base-type
                         :selection-callback
                         #'(lambda (data)
                             (setf base-type data)
                             (insert-text editor base-type))))

               (operator-selector 
                (make-instance 'option-pane
                               :title "Datarange Constructor"
                               :title-position :center
                               :test-function #'comp-fn
                               :print-function #'line-item-printer
                               :selected-item operator
                               :items 
                               (let ((items +facets+))
                                 (unless operator
                                   (setf operator (first items)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf operator data)
                                   (insert-text editor `(,operator))
                                   (decf (text-input-pane-caret-position editor)))))
                   
               (title 
                (make-instance 'title-pane 
                               :text (format nil "Data Range for Attribute ~A of Axiom ~A"
                                             (pretty-name attribute :data-range-editor)
                                             id))))

          (list title editor 
                operator-selector 
                base-type-selector
                (standard-return editor))))))


(defconstant +entity-editor+
  #'(lambda (id attribute value) 
      (with-profile-access
        (let* ((entity-type (when (consp value) (first value)))
               (entity-value (when (consp value) (second value)))
               (multi-p nil)
               (print-value (if value (line-item-printer value nil) ""))
               (editor
                (make-editor print-value)))

          (labels ((get-items ()
                     (msort 
                      (case (to-keyword entity-type)

                        ((:OWLClass :Class)
                         (racer-function1 (all-atomic-concepts current-reasoner) nil))
                        
                        (:ObjectProperty
                         (remove-if #'consp
                                    (racer-function1 (all-roles current-reasoner) nil)))
                        
                        (:DataProperty
                         (remove-if #'consp
                                    (racer-function1 (all-roles current-reasoner) nil)))
                        
                        (:AnnotationProperty
                         (remove-if #'consp
                                    (racer-function1 (all-roles current-reasoner) nil)))
                        
                        (:Individual
                         (racer-function1 (all-individuals current-reasoner) nil)))
                                     
                      #'string<
                      :key #'symbol-name)))
                   
            (let* ((entity-value-selector 
                    (make-instance 'option-pane
                                   :title "Entity Value"
                                   :title-position :center
                                   :test-function #'comp-fn
                                   :print-function #'line-item-printer
                                   :selected-item entity-value
                                   :items (get-items)
                                   :visible-min-width +option-pane-width+
                                   ;:visible-max-width +option-pane-width+
                                   :callback-type :data
                                   :selection-callback
                                   #'(lambda (data)
                                       (setf entity-value data)
                                       (setf (text-input-pane-text editor) "")
                                       (insert-text editor
                                                    `(,(or entity-type '?)
                                                      ,entity-value)))))
                   
                   (entity-type-selector 
                    (make-instance 'option-pane
                                   :title "Entity Type"
                                   :title-position :center
                                   :test-function #'comp-fn 
                                   :print-function #'line-item-printer
                                   :selected-item entity-type
                                   :items +entities+
                                   :visible-min-width +option-pane-width+
                                   ;:visible-max-width +option-pane-width+
                                   :callback-type :data
                                   :selection-callback
                                   #'(lambda (data)
                                       (setf entity-type data)
                                       (setf (text-input-pane-text editor) "")
                                       (insert-text editor
                                                    `(,entity-type
                                                      ,(or entity-value '?)))
                                       (setf (collection-items entity-value-selector)
                                             (get-items)))))
                       
                   (title 
                    (make-instance 'title-pane 
                                   :text (format nil "Entity for Attribute ~A of Axiom ~A" 
                                                 (pretty-name attribute :entity-editor)
                                                 id))))

              (list title editor 
                    entity-type-selector 
                    entity-value-selector
                    (standard-return editor))))))))


(defconstant +annotation-editor+
  #'(lambda (id attribute value) 
      (with-profile-access
        (let* ((annot (when (consp value) (third value)))
               (role (when (consp value) (second value)))

               (multi-p nil)
               (literal-p (consp annot)) ; (d-filler "42" (base-type integer))) ? 
                   
               (annot-value (if literal-p 
                                (second annot)
                              annot))

               (base-type (when literal-p
                            (second (third annot))))

               (print-value (if value (line-item-printer value nil) ""))

               (editor
                (make-editor print-value))

               (base-type-selector 
                (funcall +base-type-selector+ 
                         :selected-item base-type 
                         :selection-callback
                         #'(lambda (data)
                             (let ((current 
                                    (ignore-errors
                                      (my-read-from-string1 (text-input-pane-text editor)))))

                               (setf base-type data)

                               (setf (text-input-pane-text editor)
                                     (line-item-printer 
                                      `(|Annotation| ,(second current) 
                                                     (,@(butlast (third current))
                                                      (|d-base-type| ,base-type)))
                                      nil))))))

               (role-selector 
                (make-instance 'option-pane
                               :title "Role"
                               :title-position :center
                               :test-function #'comp-fn 
                               :print-function #'line-item-printer
                               :selected-item role 
                               :items 
                               (let ((items
                                      (msort
                                       (remove-if #'consp 
                                                  (racer-function1 (all-roles current-reasoner) nil))
                                       #'string<
                                       :key #'symbol-name)))
                                 (unless role
                                   (setf role (first items)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf role data)
                                   (let ((current 
                                          (ignore-errors
                                            (my-read-from-string1 (text-input-pane-text editor)))))

                                     (setf (text-input-pane-text editor)
                                           (line-item-printer 
                                            `(|Annotation| ,role ,(if (consp (third current))
                                                                      (third current)
                                                                    '?))
                                            nil))))))
                   
               (ind-selector 
                (make-instance 'option-pane
                               :title "Individual"
                               :title-position :center
                               :test-function #'comp-fn
                               :print-function #'line-item-printer
                               :selected-item annot-value
                               :items 
                               (let ((items 
                                      (msort 
                                       (racer-function1 (all-individuals current-reasoner) nil)
                                       #'string<
                                       :key #'symbol-name)))
                                 items)
                               :visible-min-width +option-pane-width+
                               ;:visible-max-width +option-pane-width+
                               :callback-type :data
                               :selection-callback
                               #'(lambda (data)
                                   (setf annot-value data)
                                   (let ((current 
                                          (ignore-errors
                                            (my-read-from-string1 (text-input-pane-text editor)))))

                                     (setf (text-input-pane-text editor)
                                           (line-item-printer 
                                            `(|Annotation| ,(second current)
                                                           ,(if (symbolp annot-value)
                                                                annot-value
                                                              '?))
                                            nil))))))
                   
               (title 
                (make-instance 'title-pane 
                               :text (format nil "Annotation for Attribute ~A of Axiom ~A" 
                                             (pretty-name attribute :annotation-editor)
                                             id))))

          (list title editor 
                base-type-selector role-selector ind-selector
                (standard-return editor))))))

;;;
;;;
;;;

(defun current-concept (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    current-concept))

(defun all-selected-concepts (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (loop as x being the hash-key of all-selected-concepts-hash collect x)))

(defun current-role (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    current-role))

(defun all-selected-roles (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (loop as x being the hash-key of all-selected-roles-hash collect x)))

(defun current-datatype-role (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (when (and current-role
               (racer-function1
                (role-used-as-datatype-property-p current-role current-reasoner)
                nil))
      current-role)))

(defun current-annotation-role (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (when (and current-role
               (racer-function1
                (role-used-as-annotation-property-p current-role current-reasoner)
                nil))
      current-role)))

(defun all-selected-datatype-roles (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (loop as x being the hash-key of all-selected-roles-hash
          when (racer-function1
                (role-used-as-datatype-property-p current-role current-reasoner)
                nil)
          collect x)))

(defun current-individual (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    current-individual))

(defun all-selected-individuals (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (loop as x being the hash-key of all-selected-individuals-hash collect x)))

(defun current-ontology (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    current-ontology))

(defun current-namespace (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    sirius-current-namespace))

(defun default-data-range (attribute)
  (declare (ignorable attribute))
  `(|d-base-type| ,+xsd-integer+))

(defun default-annotation-value (attribute)
  (declare (ignorable attribute))
  `(|d-literal| 
    "42" 
    (|d-base-type| ,+xsd-integer+)))

(defun default-literal-value (attribute)
  (declare (ignorable attribute))
  `(|d-literal| 
    "42" 
    (|d-base-type| ,+xsd-integer+)))

(defun default-ontology-import-uri (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (intern (ensure-string sirius-current-namespace))))

(defun default-ontology-version-uri (attribute)
  (declare (ignorable attribute))
  (with-profile-access
   "2.0"))

(defun default-namespace-uri (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (intern (ensure-string sirius-current-namespace))))

(defun default-namespace-prefix (attribute)
  (declare (ignorable attribute))
  '|test|)

(defun default-datatype-name-uri (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (intern
     (if sirius-current-namespace
         (concatenate 'string
                      sirius-current-namespace 
                      "#adult")
      "#!:adult"))))

(defun default-annotation-property-range-uri (attribute)
  (declare (ignorable attribute))
  +xsd-string+)

(defun default-annotation (attribute)
  (declare (ignorable attribute))
  `(|Annotation|
    ,+rdf-comment+
    (|d-literal|
     "Hi, how are you?" 
     (|d-base-type| 
      ,+xsd-string+))))

(defun default-entity (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (cond (current-concept 
           `(|Class| ,current-concept))
          (current-individual 
           `(|NamedIndividual| ,current-concept))
          (current-role 
           `(|ObjectProperty| ,current-concept)))))

(defun default-axiom-id (attribute)
  (declare (ignorable attribute))
  (with-profile-access
    (first current-axiom)))

;;;
;;;
;;; 

(defun get-attribute-editor (attribute axiom-type)
  (let ((a 
         (to-keyword attribute))
        (ty 
         (to-keyword axiom-type)))

    (declare (ignorable ty))
    
    ;;; 
    ;;; 3.8.2009 
    ;;; Constructor Parameters 
    ;;;

    '(OBJECT-PROPERTY-DOMAIN
      OBJECT-SUB-PROPERTY
      ANNOTATION-PROPERTY-RANGE
      ANNOTATION-SUBJECT
      ANNOTATION-VALUE
      SUB-CLASS
      SUPER-CLASS
      OBJECT-PROPERTY-RANGE
      AXIOM-ID
      ANNOTATION
      DATATYPE-NAME
      ONTOLOGY-IMPORT-URI
      DATA-PROPERTY-DOMAIN
      OBJECT-PROPERTY-CHAIN
      OBJECT-SUPER-PROPERTY
      ANNOTATION-PROPERTY
      ANNOTATION-PROPERTY-DOMAIN
      DATA-PROPERTY
      DATA-RANGE
      ONTOLOGY-VERSION-URI
      REL-DATA-PROPERTY
      VALUE
      DATA-PROPERTIES
      INDIVIDUAL
      DESCRIPTION
      NAMESPACE-PREFIX
      NAMESPACE
      DATA-SUB-PROPERTY
      DATA-SUPER-PROPERTY
      SUBJECT
      REL-OBJECT-PROPERTY
      OBJECT
      ENTITY
      INDIVIDUALS
      ANNOTATION-SUB-PROPERTY
      ANNOTATION-SUPER-PROPERTY
      DESCRIPTIONS
      KEY-CLASS
      KEY-OBJECT-PROPERTIES
      KEY-DATA-PROPERTIES
      OBJECT-PROPERTIES
      OBJECT-PROPERTY
      FIRST-OBJECT-PROPERTY
      SECOND-OBJECT-PROPERTY)
   
    ;;;
    ;;;
    ;;; 
        
    (case a
      
      ((:DESCRIPTION
        :SUB-CLASS
        :SUPER-CLASS
        :KEY-CLASS
        :OBJECT-PROPERTY-DOMAIN 
        :OBJECT-PROPERTY-RANGE
        :ANNOTATION-PROPERTY-DOMAIN
        :DATA-PROPERTY-DOMAIN)

       ;;; EDITOR CLOSURE, VALIDATOR CLOSURE, PARSER CLOSURE, DEFAULT CLOSURE (F. NEW AXIOM)

       (list +concept-editor+
             #'valid-concept-p 
             #'parse-concept
             #'current-concept))

      ((:DESCRIPTIONS)

       (list #'(lambda (&rest args) (apply +concept-editor+ (append args '(t))))
             #'valid-concepts-p
             #'parse-concepts
             #'all-selected-concepts))
      
      ((:INDIVIDUAL
        :SUBJECT
        :ANNOTATION-SUBJECT
        :OBJECT)

       (list +individual-editor+ 
             #'valid-individual-p 
             #'parse-individual
             #'current-individual))

      ((:INDIVIDUALS)
       
       (list #'(lambda (&rest args) (apply +individual-editor+ (append args '(t))))
             #'valid-individuals-p 
             #'parse-individuals 
             #'all-selected-individuals))

      ((:OBJECT-PROPERTY
        :REL-OBJECT-PROPERTY
        :OBJECT-SUB-PROPERTY
        :OBJECT-SUPER-PROPERTY
        :FIRST-OBJECT-PROPERTY
        :SECOND-OBJECT-PROPERTY)
       
       (list +object-property-editor+
             #'valid-role-p
             #'parse-role
             #'current-role))

      ((:OBJECT-PROPERTIES
        :KEY-OBJECT-PROPERTIES
        :OBJECT-PROPERTY-CHAIN)

       (list #'(lambda (&rest args) (apply +object-property-editor+ (append args '(t))))
             #'valid-roles-p
             #'parse-roles
             #'all-selected-roles))

      ((:DATA-PROPERTY
        :REL-DATA-PROPERTY
        :DATA-SUB-PROPERTY
        :DATA-SUPER-PROPERTY)

       (list +data-property-editor+ 
             #'valid-role-p
             #'parse-role
             #'current-datatype-role))

      ((:DATA-PROPERTIES 
        :KEY-DATA-PROPERTIES)

       (list 
        #'(lambda (&rest args) (apply +data-property-editor+ (append args '(t))))
        #'valid-roles-p 
        #'parse-roles
        #'all-selected-datatype-roles))

      ((:DATA-RANGE )
       
       (list +data-range-editor+
             #'valid-data-range-p 
             #'parse-data-range
             #'default-data-range))
      
      ((:ANNOTATION-VALUE)

       (list +annotation-value-editor+
             #'valid-annotation-value-p
             #'parse-annotation-value
             #'default-annotation-value))

      ((:VALUE)

       (list +value-editor+
             #'valid-literal-p
             #'parse-literal
             #'default-literal-value))

      ((:ANNOTATION-PROPERTY
        :ANNOTATION-SUB-PROPERTY 
        :ANNOTATION-SUPER-PROPERTY)
       
       (list +annotation-property-editor+ 
             #'valid-role-p
             #'parse-role
             #'current-annotation-role))

      ((:ANNOTATION)

       (list +annotation-editor+
             #'valid-annotation-p  
             #'parse-annotation
             #'default-annotation))
      
      ((:ENTITY)
       
       (list +entity-editor+
             #'valid-entity-p
             #'parse-entity
             #'default-entity))
      
      ((:ONTOLOGY-IMPORT-URI)

       (list +uri-editor+
             #'valid-uri-p
             #'parse-uri
             #'default-ontology-import-uri))

      ((:NAMESPACE)

       (list +uri-editor+
             #'valid-uri-p
             #'parse-uri
             #'default-namespace-uri))

      ((:NAMESPACE-PREFIX)

       (list +prefix-editor+
             #'valid-prefix-p
             #'parse-prefix
             #'default-namespace-prefix))

      ((:DATATYPE-NAME)

       (list +uri-editor+
             #'valid-uri-p
             #'parse-uri
             #'default-datatype-name-uri))

      ((:ONTOLOGY-VERSION-URI)

       (list +uri-editor+
             #'valid-uri-p
             #'parse-uri
             #'default-ontology-version-uri))

      ((:ANNOTATION-PROPERTY-RANGE)

       (list +uri-editor+
             #'valid-uri-p
             #'parse-uri
             #'default-annotation-property-range-uri))

      ((:AXIOM-ID)
       (list +axiom-id-editor+
             #'valid-axiom-id-p
             #'parse-axiom-id
             #'default-axiom-id)))))

;;;
;;;
;;; 

(defun edit-current-axiom ()
  (with-profile-access
    (when current-axiom
      (let* ((id (first current-axiom)))
        (new-axiom id)))))
         

(defun new-axiom (&optional axiom-id) ; wenn axiom-id = Edit axiom 
  (with-sirius-slots (axioms-pane)
    (with-profile-access

      (when (=> (not new-axiom-type)
              axiom-id)

      (setf *ok* nil)             

      (let* ((axiom 
              (when axiom-id ;; Edit Axiom? Werte holen
                (mapcar #'(lambda (x)
                            ;;; Attribut-Wert 
                            (list (second x) (third x)))
                        (remove-if-not #'(lambda (x) 
                                           (eq (first x) axiom-id))
                                       (coerce (collection-items axioms-pane) 'list)))))

             (new-axiom-type 
              (if axiom
                  (first (first axiom))
                new-axiom-type))

             (axiom (cdr axiom)) ; Typ-Zeile entfernen
             
             (constructor 
              (format nil "OWLAPI-getOWL~A" (symbol-name new-axiom-type)))

             (constructor-sym (intern constructor :racer-user))
             
             (lambda 
                     ;;; &optional reasoner  
                     (butlast
                      (butlast
                       (gethash constructor +lambda-hash+))))

             (id
              (or axiom-id
                  (1+ (racer-function1 (owlapi-get-axiom-counter current-reasoner) nil))))
             
             (forms
              (mapcar #'(lambda (attribute) 
                          (let ((ass
                                 (or (get-attribute-editor attribute new-axiom-type)
                                     (progn 
                                       (error-message 
                                        "Can't get axiom editor for attribute ~A" 
                                        attribute)
                                       (return-from new-axiom nil)))))
                            (list (funcall (first ass) ; gui
                                           id
                                           attribute 
                                           ;;; default closure: 
                                           (progn 
                                             (or (second (assoc attribute axiom  :test #'comp-fn)) ; Edit Axiom 
                                                 (when (fourth ass) 
                                                   (funcall (fourth ass) attribute)))))
                                  (second ass) ; validator
                                  (third ass) ; reader
                                  )))
                      lambda))

             (ok-setter (mapcar #'(lambda (x)
                                    (first (last (first x))))
                                forms))

             (readers  (mapcar #'(lambda (x)
                                   (third x))
                               forms))

             (validators  (mapcar #'(lambda (x)
                                      (second x))
                                  forms))             

             (answers nil)

             (layout
              (make-instance 'capi:column-layout 
                             :description
                             (append (apply #'append (mapcar #'(lambda (x) (butlast (first x))) forms))
                                     (list (funcall +ok-cancel-buttons+ 
                                                    #'(lambda () 
                                                        (setf answers 
                                                              (mapcar #'funcall 
                                                                      ok-setter))))))
                             :adjust :left)))
        
        (unless layout
          (error-message "Sorry, axiom editor for ~A not yet implemented."
                         new-axiom-type))
        
        
        (when layout
          (handler-case 
              (contain layout 
                       :x :center
                       :y :center
                       :auto-menus nil
                       :toolbar nil
                       :title (if axiom-id
                                  (format nil "Edit ~A (ID: ~A)" new-axiom-type id)
                                (format nil "New ~A (ID: ~A)" new-axiom-type id))
                       :confirm-destroy-function 
                       #'(lambda (&rest args)
                           (declare (ignorable args))
                           
                           (if *ok* 

                               (if (every #'(lambda (attribute x validator)
                                              (funcall validator x attribute))
                                          lambda answers validators)

                                   (progn 

                                     (handler-case 
                                         (let* ((arguments
                                                 (mapcar #'(lambda (attribute x reader) 
                                                             (funcall reader x attribute))
                                                         lambda answers readers))
                                                (command 

                                                 `(sequence 

                                                   ,@(when axiom-id
                                                       `((|OWLAPI-autoRemoveAxiomsFrom| ,current-ontology ,current-reasoner)
                                                         (|OWLAPI-removeAxiom| ,current-ontology 
                                                                                ,axiom-id
                                                                                ,current-reasoner)
                                                         (|OWLAPI-applyChanges|)
                                                         (|OWLAPI-disposeAxiom| 
                                                          ,axiom-id
                                                          ,current-reasoner)
                                                         (|OWLAPI-nextAxiomUseID| ,axiom-id)))

                                                   (|OWLAPI-autoAddAxiomsTo| ,current-ontology ,current-reasoner)
                                                   (,constructor-sym
                                                    ,@arguments 
                                                    ,current-reasoner)
                                                   (|OWLAPI-applyChanges|))))

                                           (enter-state-changing-result-command command)
                               
                                           (select-axioms (list (list id (intern (ensure-string new-axiom-type) :racer-user) nil t)))
                                           
                                           (update-active-tab))

                                       (error (error) (error-message "~A" error)))

                                     t)

                                 (let* ((validators1 validators)
                                        (lambda1 lambda)
                                        
                                       (attributes-ok? 
                                        (mapcar #'(lambda (x)
                                                    (let ((attrib (pop lambda1)))
                                                      (list attrib
                                                            (funcall (pop validators1) x attrib)
                                                            x)))
                                                answers))
                                       (bad
                                        (remove-if #'(lambda (x) 
                                                       (second x))
                                                   attributes-ok?)))

                                   (setf *ok* nil)

                                   (error-message "Bad Syntax: ~A"
                                                  (mapcar #'(lambda (x)
                                                              (list (first x)
                                                                    (third x)))
                                                          bad))
                                   
                                   nil))
                                                                          
                             t)))

            (error ()
              (beep-pane)))))))))


