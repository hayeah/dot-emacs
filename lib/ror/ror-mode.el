


;; look into emacs docs for temporary display 38.8


;; remapping
;; ;; suppose a file doesn't follow naming convention, and is associate


(defconst *ror-default-directories*
  (mapcar 'symbol-name
	  '(app config  db lib  log  public script  test tmp vendor))
  "The default ROR directories at project root.")

(defun my-debug (&rest args)
  (pp args (get-buffer "*scratch*"))
  (car (last args)))

;; utils
(defmacro ror-fn (arglist &rest body)
  `(function* (lambda ,arglist ,@body)))

(defmacro ror-defmethod (name args &rest body)
  ;; depends on the generic taking &rest
  (let (flag fixed-args other-args)
    (when (find args '(:STATIC :BEFORE :AFTER :PRIMARY))
	   (setq flag args)
	   (setq args (car body))
	   (setq body (cdr body)))
    (loop for rest on args
       for a = (car rest)
       until (find a '(&optional &key &rest &aux))
       collect a into fixed
       finally (progn (setq fixed-args fixed)
		      (when rest (setq other-args rest))))
    (let ((rest-var (gensym)))
      `(defmethod ,name ,@(and flag (list flag)) ,(append fixed-args (list '&rest rest-var))
	 (apply (ror-fn ,other-args ,@body)
		,rest-var)
	 ))))

(defun* ror-directory-files (path &key full hidden test)
  (let ((files (cond (hidden (directory-files path full))
		     (t (remove-if-not (lambda (path)
					 (ror=~ "^[^.].*$" (file-name-nondirectory path)))
				       (directory-files path full))))))
    (if test
	(remove-if-not test files)
	files)))

(defun ror-list-unique-p (lst)
  "Test by equal whether elements in list are unique."
  (labels ((rec (head tail to-test)
	     (if (null tail)
		 (member to-test head)
		 (or
		  (member to-test head)
		  (member to-test tail)
		  (rec (cons to-test head)
		       (cdr tail)
		       (car tail))))
	     ))
    (not (rec nil (cdr lst) (car lst)))))

(defun* ror-assoc (key list &optional (nth-val 0))
  "return the nth-car of an association list by key."
  (nth nth-val (assoc key list)))

(defmacro ror-aif (test yes &optional no)
  `(let ((it ,test))
     (if it ,yes ,no)))

(defmacro ror-awhen (test &rest body)
  `(ror-aif ,test (progn ,@body)))



(defmacro* ror-with-match-data ((&key in-string) &rest body)
  (let ((str (gensym))
	(md (gensym)))
    `(let ((,str ,in-string)
	   (,md (match-data)))
       ;;,(when match-data `(set-match-data ,match-data))
       (symbol-macrolet ,(loop for i to 9 append
			      (let ((match (intern (concat "$" (number-to-string i))))
				    (beg (intern (concat "<" (number-to-string i))))
				    (end (intern (concat ">" (number-to-string i)))))
				(list `(,match (match-string ,i ,str))
				      `(,beg (match-beginning ,i))
				      `(,end (match-end ,i)))))
	 (macrolet (($ (i)
		      `(match-string ,i ,',str))
		    (sub (i replacement &key fixedcase literal-string)
		      `(replace-match ,replacement ,fixedcase ,literal-string ,',str ,i)))
	   (symbol-macrolet (;; no convinient way o support before/after match for buffer search
			     ;;before
			     ;;($b (substring ,str 0 (match-beginning 0)))
			     ;;after
			     ;;($a (substring ,str (match-end 0) (length ,str)))
			     ;;match
			     ($m (match-string 0 ,str))
			     (foo ,str)
			     )
	     (set-match-data ,md)
	     ,@body))))))

(defmacro* ror=~ (regex string &rest body)
  (let ((str (gensym)))
    `(let ((,str ,string))
       (when (string-match ,regex ,str)
	      (ror-with-match-data (:in-string ,str)
		,@(or body '(t)))))))

(defmacro* ror=~by (search-form &rest body)
  `(when ,search-form
     (ror-with-match-data () ,@body)))


(defun ror-keyword->symbol (keyword)
  (or (and (symbolp keyword)
	   (not (keywordp keyword))
	   keyword)
      (intern (substring (symbol-name keyword) 1))))

(defun ror-symbol->keyword (symbol)
  (or (and (keywordp symbol) symbol)
      (intern (concat ":" (symbol-name symbol)))))

(defun ror-mklist (object)
  (if (listp object)
      object
      (list object)))


(defmacro* ror-with-temp-buffer ((name) &rest body)
  `(progn (ignore-errors (kill-buffer ,name))
	  (with-current-buffer (get-buffer-create ,name)
	    ,@body
	    (display-buffer (current-buffer) t)
	    (shrink-window-if-larger-than-buffer
	     (get-buffer-window (current-buffer))))
	  ))

(defun ror-read-opt-char (opt-chars)
  (unless (ror-list-unique-p opt-chars)
    (error "Options given are not unique: %s." opt-chars))
  (let* ((opt-chars (sort opt-chars 'string<))
	 (prompt (concat "Pick an option: ["
			 (apply 'concat opt-chars) "]"))
	 result)
    (save-window-excursion (select-window (minibuffer-window))
      (while (not (setq result
			(find (char-to-string (read-char prompt))
			      opt-chars
			      :test 'equal)))
	(my-debug "here")))
    result))

(defun ror-do-options (options)
  (let* ((help-fn (lambda () (ror-with-temp-buffer ("*RoR options help*")
			       (loop for (key _fn help-txt) in options2
				  do (insert (format "%s:\t%s\n" key help-txt)))
			       (help-mode))
			  (prog1 (ror-do-options options)
			    (ignore-errors (delete-windows-on "*RoR options help*")))))
	 (options2 (cons (list "?" help-fn "display help") options))
	 (opt-chars (mapcar 'car options2)))
    (funcall (ror-assoc (ror-read-opt-char opt-chars) options2 1))))

;; (ror-do-options '(("a" (lambda () 1) "option a")
;; 		      ("b" (lambda () 2) "option b")
;; 		      ))



;; (defstruct ror-pod
;;   ;; plain 'ol datastructure
;;   key
;;   val
;;   children
;;   )


;; (defun ror-pod-get (pod keys)
;;   (labels ((rec (pod keys &aux (key (car keys)))
;; 	     (if keys
;; 		 (ror-awhen (find-if (lambda (child) (eq (ror-pod-key child) key))
;; 				   (ror-pod-children pod))
;; 			  (rec it (cdr keys)))
;; 		 (ror-pod-val pod))))
;;     (rec pod keys)))

;; (defun ror-pod-set (pod &rest keyss/val)
;;   (labels ((rec (pod keys val &aux (key (car keys)))
;; 	     (if keys
;; 		 (ror-aif (find-if (lambda (child) (eq (ror-pod-key child) key))
;; 				   (ror-pod-children pod))
;; 			  (rec it (cdr keys) val)
;; 			  (let ((empty-pod (make-ror-pod :key key)))
;; 			    (push empty-pod (ror-pod-children pod))
;; 			    (rec empty-pod (cdr keys) val)))
;; 		 (setf (ror-pod-val pod) val)
;; 		 )))
;;     (loop for (keys val . rest) on keyss/val by 'cddr
;;        while (and keys val)
;;        do (rec pod keys val)))
;;   pod)

;; (defun ror-pod-new (&rest keyss/vals)
;;   (let ((new (make-ror-pod)))
;;     (apply 'ror-pod-set new keyss/vals)
;;     new))

;; (defun ror-pod->tree (pod)
;;   (cons (ror-pod-key pod)
;; 	(list (ror-pod-val pod )
;; 	 (mapcar 'ror-pod->alist
;; 		 (ror-pod-children pod)))))


;; (setq foo (make-ror-pod))
;; (ror-pod-get foo '(:a :b))
;; (ror-pod-set foo '(:a :b) 20)
;; (ror-pod-set foo '(:a :b :c :d) 20)
;; (ror-pod-set foo '(:a :c) 20)
;; (ror-pod-set foo '(:b :c) 20)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inflection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar ror-inflection-singulars    nil)
(defvar ror-inflection-plurals      nil)
(defvar ror-inflection-irregulars   nil)
(defvar ror-inflection-uncountables nil)

(defmacro define-ror-inflectors (&rest specs)
  (loop for (type . rest) in specs do
        (case type
          (:singular (push rest ror-inflection-singulars))
          (:plural (push rest ror-inflection-plurals))
          (:irregular (push rest ror-inflection-irregulars))
          (:uncountable (setf ror-inflection-uncountables
                              (append rest ror-inflection-uncountables))))))

(define-ror-inflectors
  (:plural "$" "s")
  (:plural "s$" "s")
  (:plural "\\(ax\\|test\\)is$" "\\1es")
  (:plural "\\(octop\\|vir\\)us$" "\\1i")
  (:plural "\\(alias\\|status\\)$" "\\1es")
  (:plural "\\(bu\\)s$" "\\1ses")
  (:plural "\\(buffal\\|tomat\\)o$" "\\1oes")
  (:plural "\\([ti]\\)um$" "\\1a")
  (:plural "sis$" "ses")
  (:plural "\\(?:\\([^f]\\)fe\\|\\([lr]\\)f\\)$" "\\1\\2ves")
  (:plural "\\(hive\\)$" "\\1s")
  (:plural "\\([^aeiouy]\\|qu\\)y$" "\\1ies")
  (:plural "\\(x\\|ch\\|ss\\|sh\\)$" "\\1es")
  (:plural "\\(matr\\|vert\\|ind\\)ix\\|ex$" "\\1ices")
  (:plural "\\([m\\|l]\\)ouse$" "\\1ice")
  (:plural "^\\(ox\\)$" "\\1en")
  (:plural "\\(quiz\\)$" "\\1zes")

  (:singular "s$" "")
  (:singular "\\(n\\)ews$" "\\1ews")
  (:singular "\\([ti]\\)a$" "\\1um")
  (:singular "\\(\\(a\\)naly\\|\\(b\\)a\\|\\(d\\)iagno\\|\\(p\\)arenthe\\|\\(p\\)rogno\\|\\(s\\)ynop\\|\\(t\\)he\\)ses$" "\\1\\2sis")
  (:singular "\\(^analy\\)ses$" "\\1sis")
  (:singular "\\([^f]\\)ves$" "\\1fe")
  (:singular "\\(hive\\)s$" "\\1")
  (:singular "\\(tive\\)s$" "\\1")
  (:singular "\\([lr]\\)ves$" "\\1f")
  (:singular "\\([^aeiouy]\\|qu\\)ies$" "\\1y")
  (:singular "\\(s\\)eries$" "\\1eries")
  (:singular "\\(m\\)ovies$" "\\1ovie")
  (:singular "\\(x\\|ch\\|ss\\|sh\\)es$" "\\1")
  (:singular "\\([m\\|l]\\)ice$" "\\1ouse")
  (:singular "\\(bus\\)es$" "\\1")
  (:singular "\\(o\\)es$" "\\1")
  (:singular "\\(shoe\\)s$" "\\1")
  (:singular "\\(cris\\|ax\\|test\\)es$" "\\1is")
  (:singular "\\(octop\\|vir\\)i$" "\\1us")
  (:singular "\\(alias\\|status\\)es$" "\\1")
  (:singular "^\\(ox\\)en" "\\1")
  (:singular "\\(vert\\|ind\\)ices$" "\\1ex")
  (:singular "\\(matr\\)ices$" "\\1ix")
  (:singular "\\(quiz\\)zes$" "\\1")

  (:irregular "person" "people")
  (:irregular "man" "men")
  (:irregular "child" "children")
  (:irregular "sex" "sexes")
  (:irregular "move" "moves")

  (:uncountable "equipment" "information" "rice" "money" "species" "series" "fish" "sheep"))

(defun ror-singularize (str)
  (when (stringp str)
    (or (car (member str ror-inflection-uncountables))
        (caar (member* str ror-inflection-irregulars :key 'cadr :test 'equal))
        (loop for (from to) in ror-inflection-singulars
              for singular = (ror=~ from str (sub 0 to))
              when singular do (return singular))
        str)))

(defun ror-pluralize (str)
  (when (stringp str)
    (or (car (member str ror-inflection-uncountables))
        (cadar (member* str ror-inflection-irregulars :key 'car :test 'equal))
        (loop for (from to) in ror-inflection-plurals
              for plurals = (ror=~ from str (sub 0 to))
              when plurals do (return plurals))
        str)))


(defmacro* ror-try-inflections ((noun bind) &body body)
  "Non-deterministically  try both the plural and singular forms of a noun, return if body evaluates to non-nil."
  (let ((noun-value (gensym))
	(plural (gensym))
	(singular (gensym))
	(body-fn (gensym)))
    `(let ((,noun-value ,noun)
	   ,bind)
       (flet ((,body-fn () ,@body))
	 (or (and (setq ,bind (ror-singularize ,noun-value))
		  (,body-fn))
	     (and (setq ,bind (ror-pluralize ,noun-value))
		  (,body-fn)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; .rb inspection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ror-ruby-block-type ()
  (ruby-beginning-of-block)
  (ror=~by (re-search-backward "\\(module\\|class\\|def\\)")
    $1))

(defun ror-ruby-defun-name ()
  ;; doesn't know abou self.<singleton-name>
  (save-excursion
    (let ((old-point (point))
	  def-point end-point)
      (or (looking-at "\\bdef\\b") (re-search-backward "\\bdef\\b"))
      (setq def-point (point))
      (ruby-forward-sexp)
      (setq end-point (point))
      (when (and (<= def-point old-point)
		 (< old-point end-point))
	(ror=~by (re-search-backward "\\bdef\\s-+\\([a-zA-Z0-9_.!?]*\\)")
	  $1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Environment Detection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst *ror-debug-root* "/home/howard/rails/mashem/")

(defun ror-root (&rest path-strings)
  "Return the path of the ROR project root, or "" if not currently in an ROR project directory tree.
If additional arguments are supplied, they are concatenated onto the root to form an absolute path.
"
  
 (let ((root
	(or *ror-debug-root*
	    (loop 
	       for path = (file-name-directory (buffer-file-name)) then (concat path  "../")
	       for true-path = (expand-file-name path)
	       if (every (lambda (dir)
			   (let ((path-to-test (concat true-path dir)))
			     ;;(my-debug dir path-to-test)
			     (file-directory-p path-to-test)))
			 *ror-default-directories*) 
	       do (return true-path) end
	       while (not (equal true-path "/"))))))
   (apply 'concat (or root "") path-strings)
   ))

(defun ror-project-p ()
  "Test to see if the buffer's file is in an ROR project."
  (not (equal (ror-root) "")))

(defvar *ror-template-extensions*
  '("erb" "rhtml" "rxml" "rjs" "haml" "liquid")
  )


;; (defvar *ror-type->directory*
;;   '((:controller       "app/controllers/")
;;     (:layout           "app/layouts/" )
;;     (:view             "app/views/")
;;     ;;(:observer         "app/models/" (lambda (file) (rails-core:observer-p file)))
;;     ;;(:mailer           "app/models/" (lambda (file) (rails-core:mailer-p file)))
;;     (:model            "app/models/")
;;     (:helper           "app/helpers/")
;;     (:plugin           "vendor/plugins/")
;;     (:unit-test        "test/unit/")
;;     (:functional-test  "test/functional/")
;;     (:fixture          "test/fixtures/")
;;     (:migration        "db/migrate")))

(defun ror-type->dir (type &optional full-p)
  (let ((dir (ror-assoc type *ror-path<->type* 1)))
    (if full-p
	(ror-root dir)
	dir)))

(defvar *ror-path<->type*)

(setf
 *ror-path<->type*
 '((ror-controller "app/controllers/")
   (ror-model "app/models/")
   (ror-view "app/views/")
   ))

(defun ror-file-type-by-path (path)
  (loop
     for (type path-pattern) in *ror-path<->type*
     thereis (and (if (functionp path-pattern)
		      (funcall path-pattern path)
		      (string-match path-pattern path))
		  type)))


(defclass ror-file ()
  ((root :initarg :root)
   (path :initarg :path)
   (filename :initarg :filename)
   (model :initarg :model))
  )

(defclass ror-model (ror-file)
  ())

(defclass ror-view (ror-file)
  ((layout :initarg :layout)
   (partial :initarg :partial)
   (action :initarg :action)
   (shared :initarg :action)))

(defclass ror-controller (ror-file)
  ((action :initarg :action)))


(defun ror-file->alist (f)
  (cons (list :type (class-of f))
	(loop for s in (object-slots f)
	   if (slot-boundp f s) collect (list (ror-symbol->keyword s)
					      (slot-value f s))
	   )))

(defun ror-buffer-info ()
  (interactive)
  (pp (ror-file->alist (ror-detect-buffer))))

(defun ror-detect-buffer ()
  (ror-detect-file (buffer-file-name)))

(defun ror-detect-file (full-path)
  (let* ((root (ror-root))
	 (path (let* ((prefix-position (progn (string-match root full-path)
					      (match-end 0)))
		      (relative-path (substring full-path prefix-position)))
		 relative-path))
	 (filename (file-name-nondirectory path)))
    (ror-awhen (ror-file-type-by-path path)
        	       (let ((file (make-instance it :root root :path path :filename filename)))
		 (ror-detect file)
		 file
		 ))))


(defgeneric ror-detect (ror-file))

(defmethod ror-detect ((f ror-file))
  (loop for (field value . _rest) on (ror-destruct-path (class-of f) (oref f path)) by 'cddr
     when value do (set-slot-value f (ror-keyword->symbol field) value)))

(defmethod ror-detect ((f ror-controller))
  (call-next-method)
  (ror-awhen (ror-ruby-defun-name)
    (oset f action it)))

;; (ror-file->alist (ror-detect-file (ror-root "app/views/foo/moo.rhtml")))

;; (ror-file->alist (ror-detect-file (ror-root "app/models/fooo.rb")))

;; (ror-file->alist (ror-detect-file (ror-root "app/controllers/fooo_controller.rb"))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Path destructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric ror-destruct-path (file-type path)
  "Static function on file types. Destructs 
 Destructs the path of a type into a p-list.
 The p-list may be used by `ror-construct-path' of the same file type to build the path back.")

(defmethod ror-destruct-path :STATIC ((type ror-model) path)
  (when (eql (ror-file-type-by-path path) type)
    (let ((filename (file-name-nondirectory path)))
      (ror=~ "\\(.*\\).rb" filename
	(list :model $1)))))

(defmethod ror-destruct-path :STATIC ((type ror-controller) path)
  (when (eql (ror-file-type-by-path path) type)
    (let ((filename (file-name-nondirectory path)))
      (or (ror=~ "\\(.*\\)_controller.rb" filename
	    (list :model $1))
	  (ror=~ "application.rb" filename
	    (list :model "application"))))))

(defmethod ror-destruct-path :STATIC ((type ror-view) path)
  (when (eql (ror-file-type-by-path path) type)
    (let* ((dir (file-name-directory path))
	   (file-name (file-name-nondirectory path))
	   (extension (file-name-extension file-name))
	   part1 part2
	   model action partial layout shared) 
      (ror=~ (concat (ror-type->dir 'ror-view t) "\\([^/]+\\)/\\([^.].*\\)$")
	  path
	(setq part1 $1)
	(setq part2 (file-name-sans-extension $2))

	(cond ((equal part1 "layouts")
	       (setq layout t)
	       (setq model part2))
	      ((equal part1 "shared")
	       (setq shared t))
	      (t
	       (setq model part1)))
	
	(when(or shared model)
	  (if (eq (aref part2 0) ?_)
	      (setq partial (substring part2 1))
	      (setq action part2)))
	
	(list :model model :action action
	      :partial partial :layout layout
	      :shared shared)))))


(defgeneric ror-construct-path (type &rest args))

(ror-defmethod ror-construct-path :STATIC ((type ror-model) &key model)
  (and model (concat (ror-type->dir 'ror-model t) model ".rb")))

;; (ror-construct-path 'ror-model :model "source")

(ror-defmethod ror-construct-path :STATIC ((type ror-controller) &key model) 
  (and model (concat (ror-type->dir 'ror-controller t) model "_controller.rb")))

(ror-defmethod ror-construct-path :STATIC ((type ror-view)
					   &key
					   (extension (car *ror-template-extensions*))
					   model action partial layout shared )
  (let ((dir (ror-type->dir type t)) 
	part1 part2)
    (setq part1 (or (and layout "layouts")
		    (and shared "shared")
		    model))
    (setq part2 (or action (and partial (concat "_" partial)) (and layout model)))
    (concat dir part1 "/" part2 "." extension)))



;; (ror-construct-path 'ror-view :model "source" :action "ha")
;; (ror-construct-path 'ror-view :model "source" :partial "ha")
;; (ror-construct-path 'ror-view :model "source" :layout t)
;; (ror-construct-path 'ror-view :action "foo" :shared t)


;; (defun* ror-build-path/model (model &key)
;;   (let ((dir (ror-root (ror-type->dir :model))))
;;     (ror-make-goto :model model
;;       :files (if (eq model :all)
;;              (directory-files dir t "\\.rb$")
;;              (concat dir model ".rb")))))


;; (defun* ror-build-path/controller (model &key action)
;;   ;; later on can use the key to build up fancy "goto" object.
;;   (let ((dir (ror-root (ror-type->dir :controller))))
;;     (ror-make-goto :controller model
;;       :files (if (eq model :all)
;; 		 (directory-files dir t "\\.rb$")
;; 		 (ror-try-inflections (model model)
;; 		   (let ((file (concat dir model "_controller.rb")))
;; 		     (and (file-exists-p file) file))))
;;       )))

;; (defun* ror-build-path/view (&key model action partial)
;;   ;; TODO: I think partial would take :priority over :action
;;   ;; ;; as used in a line render ":partial => <somehwere>" which could occur inside an action
;;   (when (and action partial) (error "A view can't be both the view of an action and a partial."))
;;   (unless model (error "Don't know how to find view when model is not provided."))
;;   (let ((ext (regexp-opt *ror-template-extensions*))
;; 	(name (or action (and partial (concat "_" partial)))))
;;     (when model
;;       (ror-make-goto :view model
;; 	:files (ror-try-inflections (model model)
;; 		 (directory-files (ror-root (ror-type->dir :view) model "/")
;; 				  t (concat "/" (or name "") "\\.\\(" ext "\\)$")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File listing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric ror-ls (obj))

(defmethod ror-ls :STATIC ((type ror-file))
  (let* ((dir (ror-type->dir type t)))
    (loop for f in (ror-directory-files dir :full t)
       for info = (ror-destruct-path type f)
       if info collect (list f info))))

(defmethod ror-ls :STATIC ((type ror-view))
  (let* ((dir (ror-type->dir type t))
	 (view-dirs (ror-directory-files dir :full t :test 'file-directory-p)))
    (apply 'nconc
	   (mapcar (lambda (dir)
		     (loop for f in (ror-directory-files dir :full t)
			for info = (ror-destruct-path type f)
			if info collect (list f info)))
		   view-dirs))   
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Goto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric ror-goto (obj))

;; damn, this is the perfect place to use multi-dispatch. Alas, not implemented by the feeble CLOS clone that is eieio.

(defmethod ror-goto :STATIC ((type ror-model))
  (ror-goto-model-from (ror-detect-buffer)))

(defgeneric ror-goto-model-from ((f ror-file)))

(defmethod ror-goto-model-from ((f ror-model))
  (ror-list-picker (ror-ls 'ror-model)
   :show (lambda (item) (insert (format "%s\n\t%s\n"
					(getf (second item) :model)
					(car item))))
   :filters '((:model (lambda (lst regex)
			(remove-if-not (lambda (item)
					 (ror=~ regex (getf (second item) :model)))))
	       ))
   ))

(defun* ror-list-picker (lst &key show filters)
  (ror-with-temp-buffer ("ror picker")
    (dolist (item lst)
      (funcall show item))))

;; (ror-ls 'ror-model)
;; (ror-ls 'ror-controller)
;; (ror-ls 'ror-view)

;; (defun ror-make-goto (type val &rest keyss/val)
;;   (let ((new-goto (make-ror-pod :key type :val val)))
;;     (apply 'ror-pod-set new-goto keyss/val)
;;     new-goto))


;; '(((lambda () (ror-env-get :model)) :view)
;;   )


;; (defun ror-go (&key to)
;;   ((ror-env-get :type)))

;; (defun* ror-go-from/view ()
;;   ;; only one of :action and :partial is non-nil
;;   (ror-goto (ror-build-path/view :model (ror-env-get :model)
;; 				 :action (ror-env-get :action)
;; 				 :partial (ror-env-get :partial))))

;; (defun* ror-go-from/controller ()
;;   ;; only one of :action and :partial is non-nil
;;   (ror-goto (ror-build-path/controller
;; 	     :model (ror-env-get :model)
;; 	     :action (ror-env-get :action)
;; 	     ;; TODO: perhaps can list the places where a partial is rendered
;; 	     ;; :partial (ror-env-get :partial)
;; 	     )))

;; (defun* ror-go-from/model ()
;;   ;; only one of :action and :partial is non-nil
;;   (ror-goto (ror-build-path/controller
;; 	     :model (ror-env-get :model)
;; 	     )))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Goto Dispatcher
;; ;; - the dispatcher is basically a makeshift generic function.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar *ror-goto-dispatchers*
;;   nil)

;; (setq *ror-goto-dispatchers*
;;       '((:files ror-goto/files
;; 	 :view ror-goto/view
;; 	 :controller ror-goto/controller
;; 	 :model ror-goto/model
;; 	 )))

;; (defun ror-goto (goto-spec)
;;   (loop for (type-checker dispatch) in *ror-goto-dispatchers*
;;      until (eq type-checker (ror-pod-key goto-spec))
;;      finally (funcall dispatch goto-spec)))

;; (defun* ror-goto/files (files &key create-p)
;;   "Finds a file, by completing from a list of possible files. Accepts a string as argument to find that file."
;;   (when files ;; do nothing if `files' is nil.
;;     (or
;;      ;; only one file to go to.
;;      (and (eql (type-of files) 'string) (find-file files))
;;      (and (eql (second files) nil) (find-file (first files)))
;;      ;; many possible files, so prompt first.
;;      (find-file
;;       ;; the below jumping through the hoops is to completing-read
;;       ;; with the filename and return the fullpath.
;;       (let ((completion (mapcar (lambda (f) (cons (file-name-nondirectory f) f))
;; 				files)))
;; 	(cdr (assoc (completing-read "Find File: " completion)
;; 		    completion)))))))

;; (defun ror-goto/view (spec)
;;   ;; GARRGH! I wish I have generic function! Fuck. Fuck. Fuck.
;;   (ror-goto/files (ror-pod-get '(:files) spec)))

;; (defun ror-goto/controller (spec)
;;   (ror-goto/files (ror-pod-get '(:files) spec))
;;   (my-debug "goto" (ror-pod-get '(:action) spec)))

;; (defun ror-goto/model (spec)
;;   (ror-goto/files (ror-pod-get '(:files) spec)))




;; ;; (defun ror-goto/type (type)
;; ;;   )


;; (defvar ror-minor-mode-map (make-keymap))

;; (define-minor-mode ror-minor-mode
;;   "RubyOnRails"
;;   nil
;;   " RoR"
;;   ror-minor-mode-map

;; ;;   (abbrev-mode -1)
;; ;;   (make-local-variable 'tags-file-name)
;; ;;   (make-local-variable 'rails-primary-switch-func)
;; ;;   (make-local-variable 'rails-secondary-switch-func)
;; ;;   (rails-features:install)

;;   )


;; (defvar ror-find-map (make-sparse-keymap))
;; ;;(defvar ror-goto-map (make-sparse-keymap))

;; (my-defkeys ror-find-map
;; 	    ((kbd "m") (fi () (ror-goto (ror-build-path :model :all))))
;; 	    ((kbd "c")
;; 	     (fi ((model "MModel of Controller: ")) (ror-goto (ror-build-path :controller :all))))
;; 	    ((kbd "v")
;; 	     (fi ((model "MModel of View: ")) (ror-goto (ror-build-path :view :model model)))))

;; (my-defkeys ror-minor-mode-map
;; 	    ((kbd "C-c C-t")
;; 	     'ror-show-env)
	    
;; 	    ((kbd "C-c C-g")
;; 	     (fi () (ror-goto (ror-build-path :model :all)))))


;; (add-hook 'find-file-hook (lambda () (when (ror-project-p) (ror-minor-mode))))

