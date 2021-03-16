;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: SIRIUS; Base: 10 -*-

(in-package sirius)

;;;
;;;
;;;

(defun get-keys-for (fn) 
  (let* ((fn (string-upcase (ensure-string fn)))
         (present (gethash fn +lambda-hash+))
         (key (when (consp present)
                (position "&KEY" present :test #'(lambda (x y)
                                                   (and (stringp x)
                                                        (stringp y) 
                                                        (string-equal x y))))))
         (keys 
          (set-difference
           (when key
             (mapcar #'first 
                     (mapcar #'ensure-list 
                             (subseq present (1+ key)))))
           '("&REST" "ARGS" "&BODY" "BODY" "&ALLOW-OTHER-KEYS")
           :test #'string-equal)))

    keys))

(defun compute-matching-commands (command &optional evaluate-p)
  (if (or (not command)
          (blank-line-p command))
      (if evaluate-p 
          +list-of-commands+
        +list-of-minilisp-commands+)
    (let* ((first (char-downcase (elt command 0)))
           (matches nil))
      (dolist (i (gethash first (if evaluate-p 
                                    +minilisp-commands+
                                  +commands+)))
        (when (let ((m (search command i :test #'string-equal)))
                (and m (zerop m)))
          (push i matches)))
      matches)))

(defun compute-matching-args (arg fn)
  (with-profile-access
    (if (or (not arg)
            (blank-line-p arg))
        
        (let ((res nil))

          (when (zerop (hash-table-count potential-args))
            (reset-potential-args))
          
          (maphash #'(lambda (key value)
                       (declare (ignorable key))
                       (dolist (item value)
                         (push item res)))
                   potential-args)

          (append 
           (mapcar #'(lambda (x) (format nil ":~A" (string-downcase x)))
                   (get-keys-for fn))
           (sort (remove-duplicates res) #'string<)))

      (if (char= #\" (elt arg 0))
          (handler-case 
              (let ((matches 
                     (remove-if-not #'(lambda (x) 
                                        (let ((x (pathname-name x)))
                                          (or (eq x :unspecific)
                                              (not 
                                               (member x
                                                       '(".owl" ".rdf" ".racer" ".txt" ".lisp" ".xml" ".funct"
                                                                ".OWL" ".RDF" ".RACER" ".TXT" ".LISP" ".XML" ".FUNCT")
                                                       :test #'(lambda (x y) (search y x)))))))
                                    (directory (concatenate 'string (subseq arg 1) "*")))))

                (values 
                 (sort
                  (mapcar #'(lambda (x)
                              (substitute-backslashes-with-slashes
                               (format nil "~A" (namestring x))))
                          matches)
                  #'string<)
                 t))
            (error ()
              nil))

        (let ((first (elt arg 0))
              (matches nil))

          (if (char= first #\:)
              (dolist (i (get-keys-for fn))
                (let ((i (format nil ":~A" (string-downcase i))))
                  (when (search arg i)
                    (push i matches))))
            (dolist (i (gethash first potential-args))
              (when (search arg i)
                (push i matches))))

          matches)))))

(defun reset-potential-args ()
  (with-profile-access

    (clrhash potential-args)

    (dolist (arg +list-of-args+)
      (let ((sym (elt arg 0)))
        (if (gethash sym potential-args)
            (push arg (gethash sym potential-args))
          (setf (gethash sym potential-args) (list arg)))))))
        

(defun register-potential-arg (arg &optional (key arg))
  (declare (ignorable key))
  (with-profile-access
    (let ((sym (elt key 0)))
      (if (gethash sym potential-args)
          (pushnew arg (gethash sym potential-args) :test #'equal)
        (setf (gethash sym potential-args) (list arg))))))


(defun complete-racer-command (&optional editor-p)
  (declare (ignorable pane args))
  
  (with-command-pane-or-editor-operation 
      editor-p

    (multiple-value-bind (command start-point end-point 
                                  command-start-point command-end-point
                                  complete-command-form-p
                                  missing-open-bracket-p)
        (get-function-name editor-p)
        
      (declare (ignorable start-point))

      ;; (display-message "~S ~S" complete-command-form-p missing-open-bracket-p)

      (let* ((evaluate-p (in-evaluate-context-p editor-p))
             (argument-p 
              (and command
                   (or (editor:point< (editor:buffer-point buffer) end-point)
                       (editor:point= (editor:buffer-point buffer) end-point))
                   (editor:point< command-end-point (editor:buffer-point buffer)))))

        (labels ((input-command (res)
                   (let ((can-close-p
                          (eq (get-lambda-for res) 'none)))
                          
                     (when command
                       (editor:move-point (editor:buffer-point buffer)
                                          command-start-point)

                       (editor:delete-next-character-command (length command)))
                       
                     (editor:insert-string
                      (editor:buffer-point buffer)
                      (if (and can-close-p (not complete-command-form-p))
                          (if missing-open-bracket-p 
                              (format nil "(~A)" res)
                            (format nil "~A)" res))
                        (if missing-open-bracket-p 
                            (format nil "(~A " res)
                          (format nil "~A " res))))))

                 (input-arg (orig res)

                   (ignore-errors
                     (editor:delete-next-character-command (length orig)))
                         
                   (editor:insert-string
                    (editor:buffer-point buffer)
                    (format nil "~A " res))))

          (cond (argument-p 

                 (let* ((point (editor:copy-point (editor:buffer-point buffer) :temporary))
                        (cpoint nil)
                        (arg nil)
                        (prev-char nil))
                                        
                   (handler-case 
                       (progn 
                         (editor:backward-character-command 1)
                     
                         (setf prev-char 
                               (elt
                                (editor:points-to-string           
                                 (editor:buffer-point buffer)
                                 point)
                                0))
                           
                         (editor:forward-character-command 1))
                     (error () (setf prev-char #\space)))

                   (cond ((whitespace-char-p prev-char)
                          (setf cpoint
                                (editor:copy-point (editor:buffer-point buffer) :temporary)))

                         ((editor:point> (editor:buffer-point buffer) command-end-point)

                          (editor:backward-form-command 1)

                          (setf cpoint
                                (editor:copy-point (editor:buffer-point buffer) :temporary))

                          (setf arg
                                (editor:points-to-string           
                                 (editor:buffer-point buffer)
                                 point)))

                         (t (return-from complete-racer-command nil)))

                   (multiple-value-bind (matches filename-p)
                       (compute-matching-args arg (get-function-name editor-p))

                     (if matches
                         (if (null (rest matches))
                               
                             (progn 
                               ;;; keine Ahnung, warum erforderlich: 
                               (editor:move-point (editor:buffer-point buffer) cpoint)
                               (input-arg arg
                                          (if filename-p 
                                              (if (not (pathname-name (first matches)))
                                                  (concatenate 'string "\"" (first matches))
                                                (concatenate 'string "\"" (first matches) "\""))
                                            (first matches))))

                           (let* ((completion 
                                   (make-instance
                                    'capi:list-panel
                                    :action-callback 
                                    'capi:exit-confirmer
                                    :title "Select Argument" 
                                    :items matches
                                    :interaction :single-selection))

                                  (res                                  
                                   (capi:popup-confirmer completion ""
                                                         :modal t
                                                         :title "Completion"
                                                         :value-function 'capi:choice-selected-item
                                                         :visible-min-width 
                                                         '(character 80)
                                                         :visible-min-height 
                                                         '(character 40)
                                                         :x 300
                                                         :y 300)))

                             (when res 
                               ;;; keine Ahnung, warum erforderlich: 
                               (editor:move-point (editor:buffer-point buffer) cpoint)
                               (input-arg arg res))))

                       (progn 
                         (editor:move-point (editor:buffer-point buffer) point)
                         (beep-pane))))))

                (t

                 (let* ((matches
                         (compute-matching-commands command evaluate-p))
                        (prev-char nil)
                        (point (editor:copy-point (editor:buffer-point buffer) :temporary)))

                   ;; (display-message (format nil "~A ~A ~A" command matches missing-open-bracket-p))

                   (if command
                       (handler-case 
                           (progn 
                             (editor:move-point (editor:buffer-point buffer) 
                                                command-start-point)
                             (editor:backward-character-command 1)
                             (setf prev-char 
                                   (elt
                                    (editor:points-to-string           
                                     (editor:buffer-point buffer)
                                     point)
                                    0))
                             (editor:forward-character-command 1)
                             (when (and prev-char
                                        (not (char= prev-char #\( )))
                               ;; (display-message (format nil "~A" prev-char))
                               (setf missing-open-bracket-p t)))
                         (error (error) 
                           (setf missing-open-bracket-p nil)
                           ;; (display-message (format nil "~A" error))
                           ))
                     (setf missing-open-bracket-p t))

                   (editor:move-point (editor:buffer-point buffer) point)

                   ;; (display-message (format nil "~S" missing-open-bracket-p))

                   (if matches

                       (progn

                         #+:ignore
                         (unless command
                           (editor:backward-form-command 1)
                           (editor:forward-kill-form-command 1))
                   
                         (if (null (rest matches))
                             (input-command (first matches))
                           (let* ((completion 
                                   (make-instance
                                    'capi:list-panel
                                    :action-callback 
                                    'capi:exit-confirmer
                                    :title "Select RacerPro Command" 
                                    :items matches
                                    :interaction :single-selection))
                                  (res 
                                   (capi:popup-confirmer completion ""
                                                         :modal t
                                                         :title "Completion"
                                                         :value-function 'capi:choice-selected-item
                                                         :visible-min-width 
                                                         '(character 80)
                                                         :visible-min-height 
                                                         '(character 40)
                                                         :x 300
                                                         :y 300)))

                             (when res
                               (input-command res)))))))))
                  
          (sirius-update-argument-display editor-p))))))


                     

