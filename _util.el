(defmacro* with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defun pr (&rest objs)
  (with-current-buffer (get-buffer-create "*scratch*")
    (save-excursion
      (end-of-line)
      (open-line 1)
      (next-line)
      (beginning-of-line)
      (prin1 objs (current-buffer))
      (terpri (current-buffer))))
  (car (last objs)))

(defun flagp (symbol)
  (and (keywordp symbol)
       (eql ?\: (aref (symbol-name symbol) 1))))

(defun symbol->keyword (s)
  (and (symbolp s)
       (not (flagp s))
       (if (keywordp s) s
	   (intern (concat ":" (symbol-name s))))))

(defun symbol->flag (s)
  (and (symbolp s)
       (not (keywordp s))
       (if (flagp s) s
	   (intern (concat "::" (symbol-name s))))))

(defmacro aif (test exp-t &optional exp-f)
  `(let ((it ,test))
     (if it ,exp-t ,exp-f)))

(defmacro awhen (test &rest body)
  `(aif ,test ,@body))
;; lambda utils


;; (my-parse-kfr '(:a a ::c :b b ::d ::e 1 2 3)) ;; ==
;; (my-parse-kfr '(:a a :b b ::c ::d ::e 1 2 3)) ;; => ((::e ::d ::c) ((:b . b) (:a . a)) (1 2 3))

(defmacro* fi (fn-list &rest body)
  "Specified an interactive lambda.
 (fi ((<var> <prompt-string>)*) <body>)
"
  `(lambda ,(mapcar 'car fn-list)
     (interactive ,(loop for (arg . rest) on (mapcar 'cadr fn-list)
		      append (cons arg (when rest (list "\n"))) into args
		      until (null rest)
		      finally (return (when args (apply 'concat args)))))
     ,@body))


(defmacro fn (fn-list &rest body)
  `(lambda ,fn-list ,@body))

(defun parse-kfr-args (args)
  "Parse an argument list of keys or flags, followed by the rest.
If the head is a keyword, eat up two items.
If the head is a flag, eat up one item.
If the head is neither, assume it's the start of the rest arugments.
"
    (let (flags keys rest)
      (labels ((rec (args)
		 (cond ((flagp (car args)) ;; flags are elisp keywords whose name starts with ':'. (e.g. ::flag)
			(push (first args) flags) (rec (cdr args)))
		       ((keywordp (car args)) ;; keys are elisp keywords. (e.g. :key)
			(push (cons (first args) (second args)) keys) (rec (cddr args)))
		       (t (setq rest args)))))
	(rec args)
	(values keys flags rest))))

(defun parse-fn-kfr-arglist (fn-kfr-args)
  (let ((markers '(&key &flag &rest))
	all reqs keys keyps flags flagps rest-arg)
    (labels ((eat-req (args) ;; eat required positional arguments
	       (let ((head (car args)))
		 (cond ((not (member head markers))
			(push-arg '&req head)
			(eat-req (cdr args)))
		       (t (setq reqs (reverse reqs))
			  (eat-kfr head (cdr args))))))
	     (eat-kfr (marker args) ;; eat
	       (when args
		 (let ((head (car args)))
		   (if (not (member head markers))
		       (progn (push-arg marker head)
			      (eat-kfr marker (rest args)))
		       (eat-kfr head (rest args))))))
	     (push-arg (marker arg)
	       ;; canonicalize the argument format, and check for errors
	       (let ((a (listfy arg)))
		 (when (member* (car a) all :key 'car)
		   (error "repeated argument %S in %S" arg fn-kfr-args))
		 (push a all) ;; push the argument immediately, to handle recursive case.
		 (ecase marker
		   (&req (or (and (push (car a) reqs))
			     (bitch arg)))
		   (&key (ecase (length a)
			   ((1 2) (or (and (symbolp (car a))
					   (push a keys))
				      (bitch arg)))
			   (3 (or (and (symbolp (first a))
				       (symbolp (third a))
				       (progn (push a keys)
					      (push-arg '&keyp (third a))))
				  (bitch arg)))))
		   (&keyp (or (and (symbolp (car a)) (push arg keyps))
			      (bitch arg)))
		   (&flag (ecase (length a)
			    (1 (or (and (symbolp (first a)) (push a flags))
				     (bitch arg)))
			    (2 (or (and (symbolp (first a))
					(symbolp (second a))
					(progn (push a flags)
					       (push-arg '&flagp (second a))))
				   (bitch arg)))))
		   (&flagp (or (and (symbolp (car a)) (push arg flagps))
			       (bitch arg)))
		   (&rest (if rest-arg (error "Accepts only one single &rest argument %S" fn-kfr-args)
			      (progn (or (and (symbolp (car a))
					      (setq rest-arg (car a)))
					 (bitch arg))))))))
	     (bitch (arg)
	       (error "Illegal argument %S in %S " arg fn-kfr-args)))
      (eat-req fn-kfr-args))
    (list reqs keys keyps flags flagps rest-arg)))



;; (parse-fn-kfr-arglist '(a b c &key e f g &flag h i j &rest r &key x y))

;; (parse-fn-kfr-arglist '(a b c &key (e nil ep) (f nil fp) (g 2) &flag h i (j nil jp) &rest o))

(defvar fn-kfr-null-value '(undefined))

(defmacro fn-kfr (args &rest body)
  "The argument list definition my contain positional arguments, keys, flags, and rest.
When called, the positional arguments come first, followed by keys and flags in whatever order, followed by rest.
"
  (multiple-value-bind (reqs keys keyps flags flagps rest-arg) (parse-fn-kfr-arglist args)
    (labels ((emit-keys (keys)
	       ))
      (with-gensyms (tmp-rest tmp-keys tmp-flags tmp-rest loop-var)
	`(lambda (,@reqs &rest ,tmp-rest)
	   (let (,@(mapcar (fn (k) (list (car k) 'fn-kfr-null-value)) ;;kludge... I don't know other easy way to distinguish between a user supplied nil or the nil of default let-binding.
			   keys)
		 ,@keyps
		 ,@(mapcar (fn (f) (list (car f) 'fn-kfr-null-value)) flags)
		 ,@flagps
		 ,rest-arg)
	     (multiple-value-bind (,tmp-keys ,tmp-flags ,tmp-rest) (parse-kfr-args ,tmp-rest)
	       ;;(to-scratch `((keys ,,tmp-keys) (flags ,,tmp-flags) (rest ,,tmp-rest)))
	       ;; ;; set args
	       ;;set keys
	       (loop for ,loop-var in ,tmp-keys
		  do (ecase (car ,loop-var)
		       ,@(loop for k-spec in keys collect
			      `(,(symbol->keyword (car k-spec))
				 ;;(to-scratch 'setting ',(car k-spec) 'to (cdr ,loop-var))
				 (setq ,(car k-spec) (cdr ,loop-var))
				 ,(when (third k-spec)
					`(setq ,(third k-spec) t))))))
	       ;;set flags
	       (loop for ,loop-var in ,tmp-flags
		  do (ecase ,loop-var
		       ,@(loop for f-spec in flags collect
			      `(,(symbol->flag (car f-spec))
				 (setq ,(first f-spec) t)
				 ;;(to-scratch 'toggling ',(car f-spec) '-> ,(first f-spec))
				 ,(when (second f-spec)
					`(setq ,(second f-spec) t))))))
	       ;;set rest args
	       (setq ,rest-arg ,tmp-rest)
	       ;; ;; set defaults and supplied-ps
	       ;; keys
	       (progn ,@(loop for a in keys collect
			     (let ((v (car a)))
			       (ecase (length a)
				 (1 `(when (eq ,v fn-kfr-null-value)
				       (setq ,v nil)))
				 ((2 3) `(when (eq ,v fn-kfr-null-value)
					   ;;(to-scratch 'defaulting-key ',v 'to ,(second a))
					   (setq ,v ,(second a))))))))
	       ;; flags
	       (progn ,@(loop for a in flags collect
			     (let ((v (car a)))
			       (ecase (length a)
				 ((1 2) `(when (progn ;; (to-scratch 'test ',v '= ,v)
						      (eq ,v fn-kfr-null-value))
					   (to-scratch 'defaulting-flag ',v)
					   (setq ,v nil))))))))
	     ,@body
	     ))))))


(defmacro defun-kfr (name args &rest body)
  `(progn (fset ',name (fn-kfr ,args ,@body))
	  ',name))

;; (defun-kfr foo (a &key b (c 2 cp) (d 3) &flag (e ep) (f2 f2p) (f fp) &rest g)
;;   `(,a (:b ,b) (:c ,c ,cp) (:d ,d) (:e ,e) (:f ,f ,fp) (:f2 ,f2 ,f2p) (&rest ,g)))

;; (funcall (fn-kfr (a &key b (c 2 cp) (d 3) &flag (e ep) (f2 f2p) (f fp) &rest g)
;; 		 `(,a (:b ,b) (:c ,c ,cp) (:d ,d) (:e ,e) (:f ,f ,fp) (:f2 ,f2 ,f2p) (&rest ,g)))
;; 	 1 :b 2 :c 3 :d 4 ::f ::e ::f2 5 6)
;; or
;; (foo 1 :b 2 :c 3 :d 4 ::f ::e ::f2 5 6)

;; => (1 (:b 2) (:c 3 t) (:d 4) (:e t) (:f t t) (:f2 t t) (&rest (5 6)))


(defmacro =~ (regex string &rest body)
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

;; List utils

(defun listfy (o)
  (if (not (listp o)) (list o)
      o))

(defun rotate-list-right (lst)
  (if lst
      (let* ((lst (reverse lst))
	     (tail (car lst))
	     (lst (cdr lst))
	     (lst (reverse lst)))
	(cons tail lst))
      nil))

(defun rotate-list-left (lst)
  (if lst
      (append (cdr lst) (list (car lst)))
      nil))

(defun mapc* (function &rest seqs)
  "Apply FUNCTION to successive cars of all ARGS. Values not accumulated"
  (if (not (memq 'nil seqs))
      (progn (apply function (mapcar 'car seqs))
	     (apply 'mapc* function (mapcar 'cdr seqs)))))



;(rotate-list-right '(1 2 3 4 5))
;(rotate-list-left '(1 2 3 4 5))


(defmacro* def-toggle (on-name (fn1 fn2) &optional (keymap 'global-map))
  (let ((off-name (intern (concat (symbol-name on-name) "-off"))))
    `(progn (defun ,on-name ()
	      (interactive)
	      (define-key ,keymap (this-command-keys)
		',off-name)
	      ,fn1)
	    (defun ,off-name ()
		(interactive)
	      (define-key ,keymap (this-command-keys)
		',on-name)
	      ,fn2)
	    ',on-name
	    )))


(def-toggle toggle-kbd-macro-recording
    ((start-kbd-macro nil)
     (end-kbd-macro)))

(def-toggle toggle-narrow-to-region
    ((region-points (beg end)
		    (narrow-to-region beg end))
     (widen)))
