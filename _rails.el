;; (add-to-list 'load-path "~/el/ruby/emacs-rails/")

;; (defun try-complete-abbrev (old)
;;   (if (expand-abbrev) t nil))

;; (setq hippie-expand-try-functions-list
;;       '(try-complete-abbrev
;;         try-complete-file-name
;;         try-expand-dabbrev))

;; (require 'rails)

;; (add-to-list 'rails-templates-list "haml")
;; ;; (setq  rails-api-root "~/doc/rails")

;; (add-hook 'ruby-mode-hook
;;            (lambda ()
;;              (abbrev-mode)))

;; (add-hook 'ruby-mode-hook
;;           (lambda()
;;             (add-hook 'local-write-file-hooks
;;                       '(lambda()
;;                          (save-excursion
;;                            (untabify (point-min) (point-max))
;;                            (delete-trailing-whitespace)
;;                            )))
;;             (set (make-local-variable 'indent-tabs-mode) 'nil)
;;             (set (make-local-variable 'tab-width) 2)
;;             (imenu-add-to-menubar "IMENU")
;;             (require 'ruby-electric)
;;             (ruby-electric-mode t)
;;             ))

;; ;; (add-hook 'ruby-mode-hook
;; ;;           (lambda()
;; ;;             (add-hook 'local-write-file-hooks
;; ;;                       '(lambda()
;; ;;                          (save-excursion
;; ;;                            (untabify (point-min) (point-max))
;; ;;                            (delete-trailing-whitespace)
;; ;;                            )))
;; ;;             (set (make-local-variable 'indent-tabs-mode) 'nil)
;; ;;             (set (make-local-variable 'tab-width) 2)
;; ;;             (imenu-add-to-menubar "IMENU")
;; ;;             (require 'ruby-electric)
;; ;;          (ruby-electric-mode t)
;; ;;          (abbrev-mode)
;; ;;             (when (rails-minor-mode) (rails-core:root))
;; ;;             ))


;; (defun ruby-indent-or-complete ()
;;     "Complete if point is at end of a word, otherwise indent line."
;;     (interactive)
;;     (unless
;;         (when (and (boundp 'snippet) snippet)
;;           (snippet-next-field))
;;       (if (looking-at "\\>")
;;           (expand-abbrev)
;;           (indent-for-tab-command))))

;; (my-defkeys ruby-mode-map
;;             ((kbd "<tab>") 'ruby-indent-or-complete))

;; (defun create-snippets-and-menumap-from-dsl (body &optional path menu keymap abbrev-table)
;;   (unless path (setq path (list)))
;;   (unless menu (setq menu (list)))
;;   (unless abbrev-table (setq abbrev-table (list)))
;;   (unless keymap (setq keymap (make-sparse-keymap "Snippets")))
;;   (dolist (tail body)
;;     (let ((p path)
;;           (a (nth 0 tail))
;;           (b (nth 1 tail))
;;           (c (cddr tail))
;;           (abbr abbrev-table))
;;       (if (eq a :m)
;;           (progn
;;             (while (not (listp (car c)))
;;               (add-to-list 'abbr (car c))
;;               (setq c (cdr c)))
;;             (add-to-list 'p b t)
;;             (define-key keymap
;;               (vconcat (mapcar #'make-symbol p))
;;               (cons b (make-sparse-keymap b)))
;;             (setq keymap (create-snippets-and-menumap-from-dsl c p menu keymap abbr)))
;;         (let ((c (car c)))
;;           (while (car abbr)
;;             (snippet-abbrev (car abbr) a  b)
;;             (setq abbr (cdr abbr)))
;;           (define-key keymap
;;             (vconcat (mapcar #'make-symbol (add-to-list 'p a t)))
;;             (cons (concat a " \t" c) (compile-snippet b)))))))
;;   keymap)


;; (setq rails-tags-command "rtags -f %s -R %s")

;; (defun rails-create-tags()
;;   "Create tags file"
;;   (interactive)
;;   (rails-core:in-root
;;    (message "Creating TAGS, please wait...")
;;    (let ((tags-file-name (rails-core:file "TAGS")))
;;      (message tags-file-name)
;;      (shell-command (format rails-tags-command tags-file-name (strings-join " " (mapcar #'rails-core:file rails-tags-dirs))))
;;      (visit-tags-table tags-file-name))))




;; (setq rails-minor-mode-map (make-sparse-keymap))
;; (setq rails-minor-mode-navmap (make-sparse-keymap))
;; (setq rails-minor-mode-findmap (make-sparse-keymap))

;; (my-defkeys rails-minor-mode-navmap
;;             ;; goto
;;             ((kbd "m") 'rails-nav:goto-models)
;;             ((kbd "c") 'rails-nav:goto-controllers)
;;             ((kbd "o") 'rails-nav:goto-observers)
;;             ((kbd "h") 'rails-nav:goto-helpers)
;;             ((kbd "l") 'rails-nav:goto-layouts)
;;             ((kbd "s") 'rails-nav:goto-stylesheets)
;;             ((kbd "j") 'rails-nav:goto-javascripts)
;;             ((kbd "g") 'rails-nav:goto-migrate)
;;             ((kbd "p") 'rails-nav:goto-plugins))

;; (my-defkeys rails-minor-mode-findmap
;;             ;; Rails finds
;;             ((kbd "m") 'rails-find-models)
;;             ((kbd "c") 'rails-find-controller)
;;             ((kbd "h") 'rails-find-helpers)
;;             ((kbd "l") 'rails-find-layout)
;;             ((kbd "s") 'rails-find-stylesheets)
;;             ((kbd "j") 'rails-find-javascripts)
;;             ((kbd "g") 'rails-find-migrate)

;;             ((kbd "v") 'rails-find-view)
;;             ((kbd "d") 'rails-find-db)
;;             ((kbd "p") 'rails-find-public)
;;             ((kbd "f") 'rails-find-fixtures)
;;             ((kbd "o") 'rails-find-config))


;; (my-defkeys rails-minor-mode-map
;;   ;;([menu-bar] rails-minor-mode-menu-bar-map)
;;   ;;([menu-bar snippets] (cons "Snippets" (create-snippets-and-menumap-from-dsl rails-snippets-menu-list)))

;; ;;  ((kbd "<tab>") 'rails-indent-or-complete)
;;   ;; Goto
;;   ((kbd "<f8> g") rails-minor-mode-navmap)
;;   ((kbd "<f8> f") rails-minor-mode-findmap)

;;   ;; Switch
;;   ((kbd "C-c <up>") 'rails-lib:run-primary-switch)
;;   ((kbd "C-c <down>") 'rails-lib:run-secondary-switch)
;;   ((kbd "\C-c \C-c \C-t") 'rails-create-tags)
;; ;;   ;; Scripts & SQL
;; ;;   ((kbd "e")   'rails-generate)
;; ;;   ((kbd "\C-c \C-c d")   'rails-destroy)
;; ;;   ((kbd "\C-c \C-c s c") 'rails-run-console)
;; ;;   ((kbd "\C-c \C-c s b") 'rails-run-breakpointer)
;; ;;   ((kbd "\C-c \C-c s s") 'rails-run-sql)
;; ;;   ((kbd "\C-c \C-c r")   'rails-rake)
;; ;;   ((kbd "\C-c \C-c w s") 'rails-ws:toggle-start-stop)
;; ;;   ((kbd "\C-c \C-c w d") 'rails-ws:start-development)
;; ;;   ((kbd "\C-c \C-c w p") 'rails-ws:start-production)
;; ;;   ((kbd "\C-c \C-c w t") 'rails-ws:start-test)
;; ;;   ((kbd "\C-c \C-c w i") 'rails-ws:print-status)

;;   ;; Tests
;;   ;; ((kbd "\C-c \C-c t") 'rails-rake-tests)

;; ;;   ;; Navigation
;; ;;   ((kbd "<C-return>") 'rails-goto-file-on-current-line)
;; ;;   ((kbd "<M-S-down>") 'rails-goto-file-from-file-with-menu)
;; ;;   ((kbd "<M-S-up>")   'rails-goto-file-from-file)

;; ;;   ((kbd "\C-c \C-c l") 'rails-log:open)
;; ;;   ((kbd "\C-c \C-c j") 'rails-create-project)
;; ;;   ;; Tags


;;   ;; Browser
;;   ((kbd "\C-c \C-c w a") 'rails-ws:auto-open-browser)

;; ;;   ;; Documentation
;;    ([f1]  'rails-search-doc)
;;    ([f1]  nil)
;;    ((kbd "<C-f1>")  'rails-browse-api-at-point)
;;    ((kbd "<C-f1>")  nil)
;;    ((kbd "C-c <f1>")  'rails-browse-api)
;;    ((kbd "C-c <f1>")  nil)

;;   ;;([f9]  'rails-svn-status-into-root)
;;   )

;; (define-minor-mode rails-minor-mode
;;   "RubyOnRails"
;;   nil
;;   " RoR"
;;   rails-minor-mode-map
;;   (abbrev-mode)
;;   (make-local-variable 'tags-file-name)
;;   (make-local-variable 'rails-primary-switch-func)
;;   (make-local-variable 'rails-secondary-switch-func))


;; (def-snips (ruby-mode-abbrev-table)
;;     ("vf" "validates_format_of :$${attrb}, :with => /$${regex}/")
;;   ("vi" "validates_inclusion_of :$${attribute}" "validates_inclusion_of")
;;   ("ve" "validates_exclusion_of :$${attribute}" "validates_exclusion_of")
;;   ("vn" "validates_numericality_of :$${attribute}" "validates_inclusion_of")

;;   ("vus" "validates_uniqueness_of :$${attribute}, :scope => \"$${column_name}\"" "validates_uniqueness_of")
;;   ("vu" "validates_uniqueness_of :$${attribute}" "validates_uniqueness_of")

;;   ("va" "validates_associated :$${attribute}" "validates_associated")
;;   ("vc" "validates_confirmation_of :$${attribute}" "validates_confirmation_of")

;;   ("vp" "validates_presence_of :$${attribute}" "validates_presence_of")
;;   ("vpif" "validates_presence_of :$${attribute}, :if => proc { |obj| $${condition} }" "validates_presence_of if")
;;   ("vl" "validates_length_of :$${attribute}, :within => $${20}" "validates_length_of")
;;   )

;; (def-snips (ruby-mode-abbrev-table)
;;     ("oivg" "instance_variable_get($${attrb})" "")
;;   ("oivs" "instance_variable_set($${attrb},$${val})" "")
;;   ("oivd?" "instance_variable_defined?($${attrb})" "")
;;   ("oriv" "remove_instance_variable?($${attrb})" "")
;;   ("ort?" "respond_to?($${msg})" "")
;;   ("oie" "instance_eval" "")
;;   )

;; (def-snips (ruby-mode-abbrev-table)
;;     ("asrt" "assert_redirected_to :action => \"$${index}\"" "assert_redirected_to")
;;   ("as" "assert($${test}, \"$${message}\")" "assert(...)")
;;   ("ase" "assert_equal($${expected}, $${actual})" "assert_equal(...)")
;;   ("asid" "assert_in_delta($${expected_float}, $${actual_float}, $${20})" "assert_in_delta(...)")
;;   ("asio" "assert_instance_of($${ExpectedClass}, $${actual_instance})" "assert_instance_of(...)")
;;   ("asko" "assert_kind_of($${ExpectedKind}, $${actual_instance})" "assert_kind_of(...)")
;;   ("asm" "assert_match(/$${expected_pattern}/, $${actual_string})" "assert_match(...)")
;;   ("asn" "assert_nil($${instance})" "assert_nil(...)")
;;   ("asne" "assert_not_equal($${unexpected}, $${actual})" "assert_not_equal(...)")
;;   ("asnm" "assert_no_match(/$${unexpected_pattern}/, $${actual_string})" "assert_no_match(...)")
;;   ("asnn" "assert_not_nil($${instance})" "assert_not_nil(...)")
;;   ("asnr" "assert_nothing_raised($${Exception}) { $. }" "assert_nothing_raised(...) { ... }")
;;   ("asns" "assert_not_same($${unexpected}, $${actual})" "assert_not_same(...)")
;;   ("asnt" "assert_nothing_thrown { $. }" "assert_nothing_thrown { ... }")
;;   ("aso" "assert_operator($${left}, :$${operator}, $${right})" "assert_operator(...)")
;;   ("asr" "assert_raise($${Exception}) { $. }" "assert_raise(...) { ... }")
;;   ("asre" "assert_response :$${success}" "assert_response")
;;   ("asrt" "assert_respond_to($${object}, :$${method})" "assert_respond_to(...)")
;;   ("ass" "assert_same($${expected}, $${actual})" "assert_same(...)")
;;   ("ass" "assert_send([$${object}, :$${message}, $${args}])" "assert_send(...)")
;;   ("ast" "assert_throws(:$${expected}) { $. }" "assert_throws(...) { ... }"))


;; ;; after_filter
;; ;; append_after_filter
;; ;; append_around_filter
;; ;; append_before_filter
;; ;; append_filter_to_chain
;; ;; around_filter
;; ;; before_filter
;; ;; condition_hash
;; ;; filter_chain
;; ;; find_or_create_filter
;; ;; prepend_after_filter
;; ;; prepend_around_filter
;; ;; prepend_before_filter
;; ;; prepend_filter_to_chain
;; ;; remove_actions_from_included_actions!
;; ;; skip_after_filter
;; ;; skip_before_filter
;; ;; skip_filter
;; ;; update_conditions