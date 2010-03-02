(add-to-list 'load-path "~/el/lib/ruby/")

(require 'ruby-mode)
(require 'haml-mode)
(require 'sass-mode)


(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(add-to-list 'auto-mode-alist '("^Rakefile$" . ruby-mode))

;; (add-to-list 'align-rules-list
;;              '(ruby-comma-delimiter
;;                (regexp . ",\\(\\s-*\\)[^# \t\n]")
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))
;; (add-to-list 'align-rules-list
;;              '(ruby-hash-literal
;;                (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))
;; (add-to-list 'align-rules-list
;;              '(ruby-assignment-literal
;;                (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
;;                (repeat . t)
;;                (modes  . '(ruby-mode))))
;; (add-to-list 'align-rules-list          ;TODO add to rcodetools.el
;;              '(ruby-xmpfilter-mark
;;                (regexp . "\\(\\s-*\\)# => [^#\t\n]")
;;                (repeat . nil)
;;                (modes  . '(ruby-mode))))


(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           ;;(delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ;; (pabbrev-mode)
						;; (pabbrev-scavenge-buffer)
            ))


(defun ruby-backward-kill-sexp ()
  (interactive)
  (let ((beg (point)))
    (ruby-backward-sexp)
    (kill-region beg (point))))

(defun ruby-forward-kill-sexp ()
  (interactive)
  (let ((beg (point)))
    (ruby-forward-sexp)
    (kill-region beg (point))))

;; (defun ruby-reindent-then-newline-and-indent ()
;;   (interactive "*")
;;   (newline)
;;   (save-excursion
;;     (end-of-line 0)
;;     (indent-according-to-mode)
;;     (delete-region (point) (progn (skip-chars-backward " \t") (point))))
;;   (indent-according-to-mode))

(defkeys (ruby-mode-map)
    ("{" 'ruby-electric-brace)
  ("}" 'ruby-electric-brace)
  ("{" nil)
  ("}" nil)
  ((kbd "M-s-j") 'ruby-beginning-of-defun)
  ((kbd "M-s-l") 'ruby-end-of-defun)
  ((kbd "M-C-j") 'ruby-backward-sexp)
  ((kbd "M-C-l") 'ruby-forward-sexp)
  ((kbd "M-C-n") 'ruby-backward-kill-sexp)
  ((kbd "M-C-u") 'ruby-forward-kill-sexp)
;;(define-key ruby-mode-map "\e\C-h" 'ruby-mark-defun)
  ((kbd "C-\\") 'ruby-indent-exp)
  ("\t" 'ruby-indent-command)
  ;("\r" 'ruby-reindent-then-newline-and-indent)
  ([?\M-\r] (fi ()
		(delete-indentation)
		(ruby-indent-command)
		(ruby-indent-exp)))
  ((kbd "C-j") 'my-backward-word)
  ((kbd "<tab>") nil)
  )


;; (define-key ruby-mode-map (kbd "<tab>") 'ruby-indent-command)
;;(define-key ruby-mode-map [f9] 'abbrev-mode)




