

(defstruct inbed-syntax
  nesting-pairs
  chunk-start
  chunk-end
  escape
  quote
  parents
  ) 


(defvar inbed-syntaxes (make-hash-table))

(defun inbed-get-syntax (syntax)
  (cond  ((inbed-syntax-p syntax) syntax)
	 ((symbolp syntax) (let ((s (gethash syntax inbed-syntaxes)))
			     (if s s
				 (error "The syntax %S is not defined." syntax))))
	 (error "Use expecting a symbol when getting syntax")))

(defun inbed-^keyword (keyword)
  (if (keywordp keyword)
      keyword
    (intern (concatenate 'string ":" (symbol-name keyword)))))

(defun inbed-^symbol (symbol)
  (if (keywordp symbol)
      (intern (substring (symbol-name symbol) 1))
    symbol)) 

(defun inbed-symcat (&rest syms)
  (intern (apply 'concatenate 'string (mapcar (lambda (s)
                                                (symbol-name (inbed-^symbol s)))
                                              syms))))



(defstruct inbed-syntax-nesting
  open
  close
  type
  syntax)

(lexical-let ((memo-hash (make-hash-table)))
  (defun inbed-merge-syntax-nesting (syntax-name &optional clear-memo)
    (when clear-memo (setq memo-hash (make-hash-table)))
    (let ((memo-val (gethash syntax-name memo-hash))
	  (syntax (inbed-get-syntax syntax-name)))
      (if memo-val
	  memo-val
	  (progn (setq memo-val
		       (remove-duplicates
			(loop for syntax in (cons syntax (inbed-syntax-parents syntax)) 
			   append (mapcar (lambda (spec)
					    (make-inbed-syntax-nesting
					     :open (first spec)
					     :close (second spec)
					     :type (third spec)
					     :syntax syntax))
					  (inbed-syntax-nesting-pairs syntax)))
			:test (lambda (a b)
				(if (and (equal (inbed-syntax-nesting-open a)
						(inbed-syntax-nesting-open b)))
				    (progn (when (not (equal (inbed-syntax-nesting-close a)
							     (inbed-syntax-nesting-close b)))
					     (error
					      "It's an error to override nesting syntax with a different closing syntax.
Earlier pairs is %S, and replaced with %S" b a))
					   t)
				    nil))
			:from-end t))
		 (puthash syntax-name memo-val memo-hash)
		 memo-val)))))


(defun inbed-merge-escape (value parents)
  (or value (loop for p in parents
                  thereis (inbed-syntax-escape p)))) 


(defmacro define-inbed-syntax (name parents &rest body)
  (flet ((collect (slot value)
                  ))
    (let ((parent-syntaxes (gensym "parent-syntaxes"))
          (new-syntax (gensym "new-syntax")))
      `(let ((,parent-syntaxes (mapcar 'inbed-get-syntax ',parents))
             (,new-syntax (make-inbed-syntax)))
         (setf
          (gethash ',name inbed-syntaxes)
          (make-inbed-syntax
           ,@(loop for (slot value) on body by 'cddr
                   append (list slot
                                (case slot
                                  ;; probably not too wise to merge syntaxes here...
                                  (:nesting-pairs `(inbed-merge-nesting-pairs ,value ,parent-syntaxes ))
                                  (:escape `(inbed-merge-escape ,value ,parent-syntaxes)))))))))
    ))

(defmacro define-inbed-syntax (name parents &rest body)
  `(setf (gethash ',name inbed-syntaxes)
         (make-inbed-syntax ,@(append `(:parents (list ,@(loop for p in parents
                                                               collect `(inbed-get-syntax ',p))))
                                      body))))


(defmacro inbed=~ (regex string &rest body)
  "regex matching similar to the =~ operator found in other languages."
  (let ((str (gensym)))
    `(lexical-let ((,str ,string)) 
       ;; Use lexical-let to make closures (in flet).
       (when (string-match ,regex ,str)
	 (symbol-macrolet ,(loop for i to 9 collect
				 (let ((sym (intern (concat "$" (number-to-string i)))))
				   `(,sym (match-string ,i ,str))))
	   (flet (($ (i) (match-string i ,str))
		  (sub (replacement &optional (i 0) &key fixedcase literal-string)
		       (replace-match replacement fixedcase literal-string ,str i)))
	     (symbol-macrolet ( ;;before
			       ($b (substring ,str 0 (match-beginning 0)))
			       ;;match
			       ($m (match-string 0 ,str))
			       ;;after
			       ($a (substring ,str (match-end 0) (length ,str)))) 
	       ,@body
	       )))))))


(define-inbed-syntax top ()
  :nesting-pairs '(("<" ">" directive)
                   ("<<" ">>" elisp-eval)
                   ("{" "}" inbed-block))
  :escape "\\"
  :quote '("\"" "\"") 
  )

;; when parsing, nesting is not in effect between pairs of quotes 

(define-inbed-syntax quote ()
  ;; the trickery here is that like python, triple quote indicates
  ;; literal quote. This works because quote is special in top, and
  ;; within quote, quote-quote is special. 
  :nesting-pairs '("#{" "}" interpolate)
  :escape "\\"
  :quote '("\"\"" "\"\"")
  :trim-whitespace nil
  )

(define-inbed-syntax rich-text (top)
  :nesting-pairs '(("*" "*" bold)
                   ("~" "~" italic)
                   ("_" "_" underline))
  )

(define-inbed-syntax note (rich-text)
  :nesting-pairs '(("[" "]" definition))
  :chunk-start '("-" :empty-line)
  :chunk-end '(:empty-line))

(define-inbed-syntax wiki (note)
  :nesting-pairs '(("[" "]" link-or-definition)))


;; (inbed-next-opening-fn (inbed-merge-nesting-pairs (inbed-get-syntax 'rich-text)))
;; (inbed-get-syntax 'wiki)


;; parsing should signal error when:
;; 1) improper nesting
;; {[}]
;; 2) improper closing
;; current opening on stack: {
;; closing found: something else


(defstruct inbed-node
  start
  end 
  type ;; if nil means plain text.
  text
  parent
  children)


(defvar inbed-debug-level 10)
(defvar inbed-indent-level 0)
(defvar inbed-debug-buffer (get-buffer "*scratch*"))



(defun inbed-trace (&rest args)
  (let* ((indentp (eql (car args) :indent))
	 (indent (if indentp
		     (second args)
		     0))
	 (args (if indentp
		   (subseq args 2)
		   args))
	 (msg (car args))
	 (objs (cdr args)))
      (when indent
	(setq inbed-indent-level (max (+ inbed-indent-level indent) 0)))
    (loop repeat indent do (princ "  " inbed-debug-buffer))
    (print (apply 'format msg objs) inbed-debug-buffer)))


(defun inbed-parse-buffer (buf syntax)
  (let* ((syntax (inbed-get-syntax syntax))
	 (beg (point))
	 (pairs (inbed-merge-syntax-nesting (inbed-get-syntax syntax)))
	 (opens (mapcar (lambda (p)
			  (cons (inbed-syntax-nesting-open p) p))
			pairs))
	 (closes (mapcar (lambda (p)
			   (cons (inbed-syntax-nesting-close p) p))
			 pairs))
	 (regexp (regexp-opt (mapcar 'car (append opens closes))))
	 (root (make-inbed-node)
	   ;; TODO: make a document root
	   )
	 (stack (list root)))
    (labels ((document-rootp (node) (eql node root))
	     (close-nesting-type (close)
	       (third (assoc* close pairs :key 'cadr)))
	     (open-nesting-type (open)
	       (third (assoc* open pairs :key 'car)))
	     (node-text-start-pos (node)
	       (+ (inbed-node-start node)
		  (length (inbed-syntax-nesting-open (inbed-node-type node)))))
	     (node-text-end-pos (node)
	       (+ (inbed-node-end node)
		  (length (inbed-syntax-nesting-close (inbed-node-type node))))) 
	     (push-and-maybe-add-text-node-before-opening (node)
	       ;; ( text _(_ cur))
	       ;; ( ... () text _(_ cur))
	       ;; ;; the underscores indictate the opening currently being examined
	       ;; ;; add the text part
	       (let* ((parent (inbed-node-parent node))
		      (children (inbed-node-children parent))
		      (previous-sibling (second children))
		      ;; we want to find the start and end of the text node (excluding opening and closing brackets.)
		      (start (if previous-sibling
				 (node-text-end-pos previous-sibling)
				 (if (document-rootp parent)
				     1
				     (node-text-start-pos parent))))
		      (end (- (inbed-node-start node) 1)))
		 ;; should not squeeze string here. Programmer should have a chance to handle spacing.
		 ;; ;; TODO: actually, have a parameter in syntax that determines what to do with spacing.
		 (if (> (- end start) 0)
		     ;; the new nodes are added in reversed order.
		     (append (list node
				   (make-inbed-node
				    :type nil ;; type nil means plain text
				    :start start :end end :parent parent 
				    :text (buffer-substring start end)))
			     children)
		     (push node children))))
	     (push-and-maybe-add-text-node-before-closing (node)
	       ;; ( ... () text  _)_
	       ;; ( text _)_
	       (let* ((parent (inbed-node-parent node))
		      (children (inbed-node-children node))
		      (last-child (last children))
		      (start (if last-child
				 (+ (inbed-node-end last-child)
				    1)
				 (node-text-start-pos node)))
		      (end (if (document-rootp parent)
			       (point-max)
			       (node-text-end-pos node))))
		 ;; upon closing, reverse the list (since they were added in reverse).
		 (nreverse (if (> (- end start) 0)
			       (push (make-inbed-node
				      :type nil
				      :start start :end end :parent node
				      :text (buffer-substring start end))
				     children)
			       children))))
	     (rec (syntax)
	       ;; TODO: allow escape here
	       (if (re-search-forward regexp (point-max) t)
		   (let* ((match-str (match-string 0))
			  (match (find match-str pairs :test (lambda (a b)
							       (or (equal a (inbed-syntax-nesting-open b))
								   (equal a (inbed-syntax-nesting-close b))))))
			  (cur (car stack)))
		     (cond ((find match-str opens :key 'car :test 'equal)
			    (progn (let ((new-node (make-inbed-node 
						    :type match
						    :start (match-beginning 0)
						    :parent cur)))
				     (push new-node stack)
				     (push-and-maybe-add-text-node-before-opening new-node)) 
				   (inbed-trace :indent 1 "Open %s" match-str)
				   ;; step down nesting
				   (rec (inbed-syntax-nesting-syntax match))
				   ;; step out nesting
				   ;; ;; this should conceptually be at the tail position of the next clause, but elisp is not tail-recursive, so do it here...
				   (rec syntax)))
			   ((find match-str closes :key 'car :test 'equal)
			    (inbed-trace :indent -1 "Close %s" match-str) 
			    (if (equal match
				       (inbed-node-type cur))
				(progn (setf (inbed-node-end cur) (match-end 0))
				       (setf (inbed-node-text cur)
					     (buffer-substring (inbed-node-start cur) (inbed-node-end cur)))
				       (push-and-maybe-add-text-node-before-closing cur) 
				       (pop stack))
				(error "Illegal nesting: expecting %S but found %S"
				       (assoc* (inbed-node-type cur) pairs :key 'third)
				       (assoc* (close-nesting-type match) pairs :key 'third))))))
		   (unless (document-rootp (car stack))
		     (error "Illegal nesting: end of input, but nesting stack not empty.")))))
      (inbed-trace "%S" regexp)
       ;;(inbed-trace "%S" `(:opens ,opens :closes ,closes))
      (with-current-buffer buf (rec syntax)
			   root))))


;; (define-inbed-syntax sexp ()
;;   :nesting-pairs '(("(" ")" paren))) 
;; (defun foo ()
;;   (interactive)
;;   (inbed-parse-buffer (current-buffer) 'sexp)) 
;; (error "shit!") 
;; (( () () ) ()  ()) 
;; (foo)

;; looks like it's parsing correctly
;; (foo)

;; (( ) () (()))

;; fewfwef 




