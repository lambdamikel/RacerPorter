(in-package editor)


(defparameter *xml-syntax-table*
  (create-syntax-table  ;;:string-escape #\\
			;; :d-escape #\#
			;;:escape #\\
			;; :comment #\/
			:nested nil
			;; :end-comment #\newline
			;;:double-comment #\/
			;;:second-comment #\*
			;;:first-close-comment #\*
			;;:second-close-comment #\/
			:string #\" 
			:close '(#\) #\} #\] #\>)
			:open '(#\( #\{ #\[ #\<)
			;; :special-char '(#\` #\')
			;; :string-within-symbol #\|
			:whitespace '(#\tab #\space
					    #\formfeed
					    #\newline
					    #\return)))

(defmode "XML" 
  :major-p t
  :vars '(("Paren Pause Period" . nil)
          ("Highlight Matching Parens" . t)
	  ("Comment Start" . "<!--")
	  ("Comment Begin" . "<!--")
	  ("Comment End" . "-->")
          ;;	  ("Auto Fill Space Indent" . t) 
	  ("Indent Function" . indent-for-c))
  :syntax-table *xml-syntax-table*)

(defcommand "XML Mode" (p)
     "Put current buffer in XML mode." 
     "Put current buffer in XML mode."  
  (declare (ignore p))
  (setf (buffer-major-mode (current-buffer)) "XML"))
