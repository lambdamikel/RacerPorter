;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

(cl:defstruct (big-vector (:constructor make-big-vector-internal))
  n-elements 
  n-subarrays
  (arrays nil))

(defconstant +max-n-of-array-elements+ (- array-total-size-limit 1))

(defun make-big-vector (dimension &key (initial-element nil))
  (multiple-value-bind (n-subarrays-minus-1 size-of-rest-array)
      (floor dimension +max-n-of-array-elements+)
    (let ((big-vector (make-big-vector-internal)))
      (setf (big-vector-n-subarrays big-vector) (+ n-subarrays-minus-1 1))
      (setf (big-vector-n-elements big-vector) dimension)
      (setf (big-vector-arrays big-vector)
            (coerce (append (loop repeat n-subarrays-minus-1 
                                  collect (make-array +max-n-of-array-elements+
                                                      :initial-element initial-element))
                            (list (make-array size-of-rest-array)))
                    'vector))
      big-vector)))

(declaim (inline bvref))

(defun bvref (big-array index)
  (multiple-value-bind (subarray-index index-of-rest-array)
      (floor index +max-n-of-array-elements+)
    (svref (svref (big-vector-arrays big-array) subarray-index)
           index-of-rest-array)))

(declaim (inline (setf bvref)))

(defun (setf bvref) (new-value big-array index)
  (multiple-value-bind (subarray-index index-of-rest-array)
      (floor index +max-n-of-array-elements+)
    (setf (svref (svref (big-vector-arrays big-array) subarray-index)
                 index-of-rest-array)
          new-value)))
    
;;;
;;;
;;;

(defconstant +hash-table-size+ 1000)

(defconstant +rehash-size+ 1000)

(defvar *read-objects* nil)

(defvar *written-objects* 
  (make-hash-table :test #'equalp
                   :size +hash-table-size+
                   :rehash-size +rehash-size+))

;;;
;;;
;;;

(defvar *io-id-counter* 0)

(defvar *aux-counter* 0)

(defvar *ref-counter* 0)

(declaim (inline get-io-id-for get-io-id store retrieve-object 
                 write-coded-string
                 write-coded-string-1
                 read-coded-string
                 write-coded-symbol
                 read-coded-symbol
                 write-coded-integer
                 read-coded-integer
                 write-coded-number
                 read-coded-number
                 write-coded-marker
                 read-coded-marker
                 write-coded-list
                 read-coded-list
                 read-byte*
                 write-byte*
                 encode-char
                 decode-char
                 encode-byte
                 decode-byte))

;;;
;;;
;;;

(defun encode-char (char)
  (char-code char))

(defun decode-char (char-code)
  (code-char char-code))

(defun encode-byte (byte)
  byte)

(defun decode-byte (byte)
  byte)
  
;;;
;;;
;;;


(defun write-byte* (byte stream)
  (let ((byte (encode-byte byte)))
    (multiple-value-bind (c1 c2)
        (floor byte 16)    
      (write-char (code-char (+ 65 c1)) stream)
      (write-char (code-char (+ 65 c2)) stream))))


(defun read-byte* (stream)
  (let ((c1 (read-char stream nil 'eof))
        (c2 (read-char stream nil 'eof)))
    (if (or (eq c1 'eof) 
            (eq c2 'eof))
        'eof
      (let ((byte (+ (* 16 (- (char-code c1) 65))
                     (- (char-code c2) 65))))
        
        (decode-byte byte)))))

;;;
;;;
;;;


(defun read-coded-integer (stream)
  (let ((num 0)
        (index 1))
    (loop 
     (let ((byte (read-byte* stream)))
       (cond ((eq byte 'eof)
              (return-from read-coded-integer 'eof))
             ((= byte 255)
              (return-from read-coded-integer num))
             ((= byte 254)
              (return-from read-coded-integer (- num)))
             (t (incf num (* index byte))
                (setf index (* index 254))))))))

(defun write-coded-integer (stream num)
  (let ((orig-num num)
        (num (abs num)))
    (loop 
     (multiple-value-bind (div mod)
         (floor num 254)
       (write-byte* mod stream)
       (setf num div)
       (when (= 0 div) 
         (write-byte* (if (minusp orig-num)
                          254 255)
                      stream)
         (return-from write-coded-integer))))))


(defun write-coded-string (stream string)
  (write-coded-integer stream (length string))
  (loop as char across string do
        (write-byte* (encode-char char) stream)))

(defun write-coded-string-1 (stream string)
  (loop as char across string do
        (write-byte* (encode-char char) stream)))

(defun read-coded-string (stream)
  (let* ((n (read-coded-integer stream))
         (string (make-string n ;;:initial-element #\space
                              )))
    (loop as i from 1 to n do
          (setf (aref string (1- i))
                (decode-char (read-byte* stream))))
    string))

(defun write-coded-symbol (stream symbol)
  (let* ((package (symbol-package symbol))
         (name (symbol-name symbol))
         (length (+ 2
                    (if (null package)
                        (+ 2 (length name))
                      (+ (length (package-name package)) 2 (length name))))))
    (write-coded-integer stream length)
    (if (null package)
        (write-coded-string-1 stream "#:")
      (progn 
        (write-coded-string-1 stream (string-upcase (package-name package)))
        (write-coded-string-1 stream "::")))
    (write-coded-string-1 stream "|")
    (write-coded-string-1 stream name)
    (write-coded-string-1 stream "|")))


(defun read-coded-symbol (stream)
  (let ((string (read-coded-string stream)))
    (if (string= (subseq string 0 2) "#:")
	(read-from-string string)
      (let* ((pos  (position #\: string))
	     (packname
	      (read-from-string (subseq string 0 pos)))
	     (*package* (or (find-package packname)
			    (find-package (string-downcase packname)))))
	(intern (subseq string (+ 3 pos)
			(1- (length string)))
		*package*)))))

(defun write-coded-number (stream num)
  (write-coded-string stream (format nil "~S" num)))

(defun read-coded-number (stream)
  (read-from-string (read-coded-string stream)))

(defun write-coded-marker (stream marker)
  (write-byte* marker stream))

(defun read-coded-marker (stream)
  (read-byte* stream))


(defun write-coded-list (stream list)
  (write-coded-string stream (format nil "~S" list)))

(defun read-coded-list (stream)
  (read-from-string (read-coded-string stream)))



;;;
;;;
;;;

(defun get-io-id-for (object)
  (gethash object *written-objects*))

(defun get-io-id ()
  (incf *io-id-counter*))

(defun store (io-id obj)
  (setf *io-id-counter* (max io-id *io-id-counter*)) 
  (setf (bvref *read-objects* io-id) obj)
  obj)

(defun retrieve-object (io-id)
  (or (bvref *read-objects* io-id)
      (error "Object ~A not found! Dangling pointer!" io-id)))

;;;
;;;
;;;

(defun reset ()
  (setf *io-id-counter* 0
        *aux-counter* 0
        *ref-counter* 0)
  (clrhash *written-objects*))

(defun read-reset (n-of-objects-to-read)
  (setf *io-id-counter* 0
        *aux-counter* 0
        *ref-counter* 0)
  (setf *read-objects* (make-big-vector (+ n-of-objects-to-read 3)
                                        :initial-element nil)))

;;;
;;;
;;;

(cl:defclass persistent-object ()
  ())

(defmethod write-object-constructor ((obj persistent-object) stream)
  (declare (ignorable stream))
  nil)

(defmethod write-object-initializer ((obj persistent-object) stream)
  (declare (ignorable stream))
  nil)

(defmethod fill-persistent-object ((obj persistent-object) stream)
  (declare (ignorable stream))
  nil)

;;;
;;;
;;;

(defmethod read-slot-value ((obj persistent-object) (slot symbol) (stream stream))
  (read-value stream))

(defmethod write-slot-value ((obj persistent-object) (slot symbol) (stream stream))
  
  (write-referencer (slot-value obj slot) stream))

;;; verhindere, da"s initialize-instance fuer Subklassen aufgerufen wird!

(defmethod initialize-instance :around ((obj persistent-object) 
					&rest initargs
					&key (dont-initialize nil))
  (if (not dont-initialize)
      (progn
	(call-next-method))		; normale Initialisierung
    (apply #'shared-initialize obj t initargs)))
 
(defmethod initialize-loaded-persistent-object ((obj persistent-object))
  t)


;;;
;;;
;;;

(defconstant +already-present-marker+ 0)
(defconstant +string-marker+ 1)
(defconstant +array-marker+ 2)
(defconstant +hashtable-marker+ 3)
(defconstant +cons-marker+ 4)
(defconstant +nil-marker+ 5)
(defconstant +integer-marker+ 6)
(defconstant +number-marker+ 7)
(defconstant +symbol-marker+ 8)
(defconstant +object-marker+ 9)
(defconstant +unbound-marker+ 11)
(defconstant +section-marker+ 12)
(defconstant +pathname-marker+ 13)

(defvar *secret-hashtable-size-key* (gensym "*#?@secret-"))

;;;
;;;
;;;

(defmacro with-only-once-constructed-object ((object marker stream) &body body)
  `(unless (gethash ,object *written-objects*)
     (let ((io-id (get-io-id)))
       (setf (gethash ,object *written-objects*) io-id)
       (write-coded-marker ,stream ,marker)
       (write-coded-integer ,stream io-id)
       ,@body)))

(defmethod write-constructor :after ((object t) stream)
  (declare (ignorable stream))
  (incf *ref-counter*))

;;;
;;;
;;;

(defmethod write-constructor ((object t) stream)
  (declare (ignorable stream))
  nil)

(defmethod write-constructor ((object null) stream)
  (declare (ignorable stream))
  nil)

(defmethod write-constructor ((object cons) stream)
  (with-only-once-constructed-object (object +cons-marker+ stream)
                                     (write-constructor (car object) stream)
                                     (write-constructor (cdr object) stream)))


(defmethod write-constructor ((object string) stream)
  (with-only-once-constructed-object (object +string-marker+ stream)
                                     (write-coded-string stream object)))

(defmethod write-constructor ((object pathname) stream)
  (with-only-once-constructed-object (object +pathname-marker+ stream)
                                     (write-coded-string stream 
                                                         (namestring object))))

(defmethod write-constructor ((object symbol) stream)
  (with-only-once-constructed-object (object +symbol-marker+ stream)
                                     (write-coded-symbol stream object)))

(defmethod write-constructor ((object array) stream)
  (with-only-once-constructed-object (object +array-marker+ stream)
                                     (write-coded-list stream (array-dimensions object))
                                     (dotimes (i (array-total-size object))
                                       (write-constructor (row-major-aref object i) stream))))


(defmethod write-constructor ((object hash-table) stream)
  (with-only-once-constructed-object (object +hashtable-marker+ stream)
                                     (write-coded-integer stream (hash-table-count object))
                                     (write-coded-integer stream (hash-table-size object))
                                     (write-coded-number stream (hash-table-rehash-size object))
                                     (write-coded-number stream (hash-table-rehash-threshold object))
                                     (write-coded-symbol stream (hash-table-test object))
                                     (maphash #'(lambda (key value)
                                                  (write-constructor key stream)
                                                  (write-constructor value stream))
                                              object)))

(defmethod write-constructor ((object persistent-object) stream)
  (write-constructor (type-of object) stream)
  (with-only-once-constructed-object (object +object-marker+ stream)
                                     (write-referencer (type-of object) stream)
                                     (write-object-constructor object stream)))

;;;
;;;
;;;

(defmacro with-complex-object-header ((id stream) &body body)
  `(progn 
     (write-coded-integer ,stream ,id)
     ,@body))

(defun write-referencer (object stream)
  (typecase object
    (null 
     (write-coded-marker stream +nil-marker+))
    ((or cons array hash-table string pathname symbol persistent-object)
     (write-coded-marker stream +object-marker+)
     (write-coded-integer stream (get-io-id-for object)))
    (integer (write-coded-marker stream +integer-marker+)
             (write-coded-integer stream object))
    (number 
     (write-coded-marker stream +number-marker+)
     (write-coded-number stream object))
    (otherwise (error "Can't export objects of type ~A!"
                      (type-of object)))))


(defmethod write-initializer ((object string) id stream)
  (declare (ignorable object id stream))
  t)

(defmethod write-initializer ((object pathname) id stream)
  (declare (ignorable object id stream))
  t)

(defmethod write-initializer ((object symbol) id stream)
  (declare (ignorable object id stream))
  t)

(defmethod write-initializer ((object cons) id stream)
  (with-complex-object-header (id stream)    
                              (write-referencer (car object) stream)
                              (write-referencer (cdr object) stream)))


(defmethod write-initializer ((object array) id stream)
  (with-complex-object-header (id stream)
                              (dotimes (i (array-total-size object))
                                (write-referencer (row-major-aref object i)
                                                  stream))))


(defmethod write-initializer ((object hash-table) id stream)
  (with-complex-object-header (id stream)
                              (maphash #'(lambda (key value)
                                           (write-referencer key stream)
                                           (write-referencer value stream))
                                       object)))

(defmethod write-initializer ((object persistent-object) id stream)
  (with-complex-object-header (id stream)
                              (write-object-initializer object stream)))

;;;
;;;
;;;

(defmethod fill-object ((object symbol) stream id)  
  (declare (ignorable stream id))
  object)

(defmethod fill-object ((object string) stream id)  
  (declare (ignorable stream id))
  object)

(defmethod fill-object ((object pathname) stream id)  
  (declare (ignorable stream id))
  object)

(defmethod fill-object ((object cons) stream id)  
  (declare (ignorable id))
  (setf (car object)
        (read-value stream))
  (setf (cdr object)
        (read-value stream))
  object)

(defmethod fill-object ((object array) stream id)
  (declare (ignorable id))
  (dotimes (i (array-total-size object))
    (setf (row-major-aref object i)
          (read-value stream)))
  object)

(defmethod fill-object ((object hash-table) stream id)  
  (declare (ignorable id))
  (dotimes (i (gethash *secret-hashtable-size-key* object))
    (let ((key (read-value stream))
          (value (read-value stream)))
      (setf (gethash key object) value)))
  (remhash *secret-hashtable-size-key* object)
  object)

(defmethod fill-object ((object persistent-object) stream id)
  (declare (ignorable id))
  (fill-persistent-object object stream)
  object)

;;;
;;;
;;;

(defun read-value (stream)
  (let ((marker (read-coded-marker stream)))
    (cond ((or (= marker +object-marker+)
               (= marker +array-marker+)
               (= marker +hashtable-marker+))
	   (values 
	    (retrieve-object (read-coded-integer stream))))
          ((= marker +nil-marker+)
	   (values 
	    nil))
          ((= marker +integer-marker+)
	   (values 
	    (read-coded-integer stream)))	  
	  ((= marker +number-marker+)
	   (values 
	    (read-coded-number stream)))
	  (t (error "Bad marker ~A in read-value!" marker)))))

(defun construct-object (stream)
  (loop 
   (let ((marker (read-coded-marker stream)))
     
     (if (or (= marker +section-marker+)
             (eq marker 'eof))
         (return)
       (let ((id (read-coded-integer stream)))
         (store id
                (cond ((= marker +string-marker+)
                       (read-coded-string stream))
                      ((= marker +pathname-marker+)
                       (parse-namestring (read-coded-string stream)))
                      ((= marker +symbol-marker+)
                       (read-coded-symbol stream))
                      ((= marker +cons-marker+)
                       (cons (incf *aux-counter*) nil))
                      ((= marker +array-marker+)
                       (make-array (read-coded-list stream)))
                      ((= marker +hashtable-marker+)
                       (let* ((no-of-entries (read-coded-integer stream))
                              (size (read-coded-integer stream))
                              (rehash-size (read-coded-number stream))
                              (rehash-threshold (read-coded-number stream))
                              (test (read-coded-symbol stream))
                              (table (make-hash-table 
                                      :size size
                                      :rehash-size rehash-size
                                      :rehash-threshold rehash-threshold
                                      :test test)))
                         (setf (gethash *secret-hashtable-size-key* table)
                               no-of-entries)
                         table))
                      ((= marker +object-marker+)
                       (make-instance (read-value stream)
                                      :allow-other-keys t 
                                      :dont-initialize t))
                      (t (error "Unknown marker: ~A." marker)))))))))

(defun initialize-object (stream)
  (loop 
   (let ((io-id (read-coded-integer stream)))
     (if (eq io-id 'eof)
         (return)
       (fill-object (retrieve-object io-id) stream io-id)))))

;;;
;;;
;;;

(defmacro defpersistentclass (&rest rest)
  `(progn ,@(let* ((name (first rest))
		   (superclasses (append (second rest) '(persistent-object)))
		   (body (third rest))
		   (slotnames (mapcar #'first body))
                   (options (fourth rest)))
	      (list 
               `(defun ,(intern (format nil "is-~A-p" name)) (obj)
                  (typep obj ',name))
               
	       `(cl:defclass ,name ,superclasses 
		  ,(loop for slotspec in body collect
			 (remove :not-persistent slotspec))
                  .,(if (null options) nil (list options)))

	       `(defmethod write-object-constructor ((obj ,name) stream)
                  #+:allegro (declare (ignorable stream))
                  #+:lispworks (declare (ignorable stream))
                  #+:mcl (declare (ignorable stream))
                  (with-slots ,slotnames obj
                    (declare (ignorable ,.slotnames))
                    ,@(mapcan #'(lambda (slot)
                                  (let ((name (first slot)))
                                    (unless (member :not-persistent slot)
                                      `((when (slot-boundp obj ',name)
                                          (write-constructor ,name stream))))))
                              (reverse body)))
                  (call-next-method))

	       `(defmethod write-object-initializer ((obj ,name) stream)
                  #+:allegro (declare (ignorable stream))
                  #+:lispworks (declare (ignorable stream))
                  #+:mcl (declare (ignorable stream))
                  (progn ;;with-slots ,slotnames obj
                    ,@(mapcan #'(lambda (slot)
                                  (let ((name (first slot)))
                                    (unless (member :not-persistent slot)
                                      `((if (slot-boundp obj ',name)
                                            (write-slot-value obj ',name stream)
                                          (write-coded-marker stream +unbound-marker+))))))
                              (reverse body)))
                  (call-next-method))
	       
	       `(defmethod fill-persistent-object ((obj ,name) stream)
                  #+:allegro (declare (ignorable stream))
                  #+:lispworks (declare (ignorable stream))
                  #+:mcl (declare (ignorable stream))
		  (let (val unbound)
		    #+:allegro (declare (ignorable val unbound))
                    #+:lispworks (declare (ignorable val unbound))
		    #+:mcl (declare (ignorable val unbound))
		    (with-slots ,slotnames obj
                      (declare (ignorable ,.slotnames))
		      ,@(mapcan #'(lambda (slot)
				    (let ((name (first slot)))
				      (unless (member :not-persistent slot)
					`((if t ; (probe-bound-p stream)
                                              (setf ,name (read-slot-value obj ',name stream))
                                            (read-value stream))))))
				(reverse body)))
		    (call-next-method)))
               (list 'quote name)))))

;;;
;;;
;;;

(defun write-section-separator (stream)
  (write-coded-marker stream +section-marker+))

(defun make-object-persistent (obj fn &optional (package *package*))

  (with-open-file (stream fn 
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (let ((*package* package))
      (reset)    
      (write-constructor (list +build-version+ obj) stream)
      (write-section-separator stream)
      (let ((vals nil))
        (maphash #'(lambda (key value)
                     (push (list value key) vals))
                 *written-objects*)
        (dolist (pair (sort vals #'< :key #'first))
          (write-initializer (second pair) (first pair) stream))))))


(defun load-persistent-object (fn &key (initialization-protocol t))
  (with-open-file (stream fn :direction :input)
    (reset)
    (read-reset 10000)
    (handler-case 
        (progn
          (construct-object stream)
          (initialize-object stream)
          (when initialization-protocol
            (dotimes  (i (+ 2 *io-id-counter*))
              (let ((obj (bvref *read-objects* i)))
                (when (typep obj 'persistent-object)
            (initialize-loaded-persistent-object obj)))))
          (let ((result (retrieve-object 1)))
            (setf *read-objects* nil)
            (if (not (string-equal (first result) +build-version+))
                nil
              (second result))))
      (error (error) 
             nil))))

