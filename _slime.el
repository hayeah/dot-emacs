
(defvar slime-eval-map (make-sparse-keymap))

(my-defkeys (slime-eval-map)
            ((kbd "<f5>") 'slime-eval-last-expression)
            ((kbd "<f6>") 'slime-eval-print-last-expression))

(my-defkeys (slime-mode-map)
            ((kbd "<f1>") nil)
            ((kbd "<f5>") slime-eval-map)
            ((kbd "C-.") 'slime-edit-definition)
            ((kbd "C-,") 'slime-pop-find-definition-stack)
;;          ((kbd "M-.") 'slime-edit-definition-other-window)
            ((kbd "<tab>") 'slime-indent-and-complete-symbol)
            ((kbd "C-<tab>") 'slime-fuzzy-complete)

            ;;((kbd "C-n") nil)
            )


;; (define-key slime-mode-map (kbd "C-s-o") 'slime-selector)
;; (define-key slime-mode-map (kbd "<f3>")  'slime-apropos-all)
;; (define-key slime-mode-map (kbd "<f3>")  'slime-apropos-all)
;; (define-key slime-mode-map (kbd "<f1>")  nil)
;; (define-key slime-mode-map (kbd "<f5>")  'slime-eval-last-expression)
;; (define-key slime-mode-map (kbd "C-<f5>")  'slime-eval-print-last-expression)
;; ;; (define-key slime-mode-map (kbd "C-d")  paredit-map)
;; (define-key slime-mode-map (kbd "C-,") 'slime-edit-definition)
;; (define-key slime-mode-map [control ?\.]  'slime-pop-find-definition-stack)
;; (fset 'lisp-reader-comment "#+(or)")
;; (define-key slime-mode-map (kbd "s-;") 'lisp-reader-comment)
;; (define-key slime-mode-map [tab] 'slime-indent-and-complete-symbol)
;; (define-key slime-mode-map (kbd "M-<tab>") 'slime-complete-form)


;; (slime-define-keys slime-scratch-mode-map
;;   ("\C-j" nil))

;; (slime-define-keys slime-scratch-mode-map
;;   ((kbd "C-<return>") 'slime-eval-print-last-expression))

(defvar slime-keys
  '(;; Compiler notes
    ("\M-p" slime-previous-note)
    ("\M-n" slime-next-note)
    ("\M-c" slime-remove-notes :prefixed t)
    ("\C-k" slime-compile-and-load-file :prefixed t)
    ("\M-k" slime-compile-file :prefixed t)
    ("\C-c" slime-compile-defun :prefixed t)
    ("\C-l" slime-load-file :prefixed t)
    ;; Editing/navigating
    ("\M-\C-i" slime-complete-symbol :inferior t)
    ("\C-i" slime-complete-symbol :prefixed t :inferior t)
    ("\M-i" slime-fuzzy-complete-symbol :prefixed t :inferior t)
    ("\M-." slime-edit-definition :inferior t :sldb t)
    ("\C-x4." slime-edit-definition-other-window :inferior t :sldb t)
    ("\C-x5." slime-edit-definition-other-frame :inferior t :sldb t)
    ("\M-," slime-pop-find-definition-stack :inferior t :sldb t)
    ("\C-q" slime-close-parens-at-point :prefixed t :inferior t)
    ("\C-c\M-q" slime-reindent-defun :inferior t)
    ;; Evaluating
    ("\C-x\C-e" slime-eval-last-expression :inferior t)
    ("\C-x\M-e" slime-eval-last-expression-display-output :inferior t)
    ("\C-p" slime-pprint-eval-last-expression :prefixed t :inferior t)
    ("\C-r" slime-eval-region :prefixed t :inferior t)
    ("\C-\M-x" slime-eval-defun)
    (":"    slime-interactive-eval :prefixed t :sldb t)
    ("\C-e" slime-interactive-eval :prefixed t :sldb t :inferior t)
    ("\C-y" slime-call-defun :prefixed t)
    ("E"    slime-edit-value :prefixed t :sldb t :inferior t)
    ("\C-z" slime-switch-to-output-buffer :prefixed t :sldb t)
    ("\C-b" slime-interrupt :prefixed t :inferior t :sldb t)
    ("\M-g" slime-quit :prefixed t :inferior t :sldb t)
    ;; Documentation
    (" " slime-space :inferior t)
    ("\C-s" slime-complete-form :prefixed t :inferior t)
    ("\C-f" slime-describe-function :prefixed t :inferior t :sldb t)
    ("\M-d" slime-disassemble-symbol :prefixed t :inferior t :sldb t)
    ("\C-t" slime-toggle-trace-fdefinition :prefixed t :sldb t)
    ("\C-u" slime-undefine-function :prefixed t)
    ("\C-m" slime-macroexpand-1 :prefixed t :inferior t)
    ("\M-m" slime-macroexpand-all :prefixed t :inferior t)
    ("\M-0" slime-restore-window-configuration :prefixed t :inferior t)
    ([(control meta ?\.)] slime-next-location :inferior t)
    ;; Emacs20 on LinuxPPC signals a
    ;; "Invalid character: 400000040, 2147479172, 0xffffffd8"
    ;; for "\C- ".
    ;; ("\C- " slime-next-location :prefixed t :inferior t)
    ("~" slime-sync-package-and-default-directory :prefixed t :inferior t)
    ("\M-p" slime-repl-set-package :prefixed t :inferior t)
    ;; Cross reference
    ("<" slime-list-callers :prefixed t :inferior t :sldb t)
    (">" slime-list-callees :prefixed t :inferior t :sldb t)
    ;; "Other"
    ("\I"  slime-inspect :prefixed t :inferior t :sldb t)
    ("\C-]" slime-close-all-sexp :prefixed t :inferior t :sldb t)
    ("\C-xt" slime-list-threads :prefixed t :inferior t :sldb t)
    ("\C-xc" slime-list-connections :prefixed t :inferior t :sldb t)
    ;; Shadow unwanted bindings from inf-lisp
    ("\C-a" slime-nop :prefixed t :inferior t :sldb t)
    ("\C-v" slime-nop :prefixed t :inferior t :sldb t)))

(slime-define-keys slime-repl-mode-map
  ("\C-m" 'slime-repl-return)

  ("\C-j" 'slime-repl-newline-and-indent)
  ("\C-j" nil)

  ("\C-\M-m" 'slime-repl-closing-return)
  ([(control return)] 'slime-repl-closing-return)

  ("\C-a" 'slime-repl-bol)
  ("\C-a" nil)
  ([home] 'slime-repl-bol)

  ("\C-e" 'slime-repl-eol)
  ("\C-e" nil)

  ("\M-p" 'slime-repl-previous-input)
  ("\M-p" nil)
  ("\M-i" 'slime-repl-previous-input)
  ;;((kbd "C-<up>") 'slime-repl-previous-input)

  ("\M-n" 'slime-repl-next-input)
  ("\M-n" nil)
  ("\M-n" 'slime-repl-next-input)
  ;;((kbd "C-<down>") 'slime-repl-next-input)

  ("\M-r" 'slime-repl-previous-matching-input)
  ("\M-s" 'slime-repl-next-matching-input)
  ("\C-c\C-c" 'slime-interrupt)
  ("\C-c\C-b" 'slime-interrupt)
  ("\C-c:"    'slime-interactive-eval)
  ("\C-c\C-e" 'slime-interactive-eval)
  ("\C-cE"     'slime-edit-value)
  ;;("\t"   'slime-complete-symbol)
  ("\t"   'slime-indent-and-complete-symbol)
  (" "    'slime-space)
  ("\C-c\C-d" slime-doc-map)
  ("\C-c\C-w" slime-who-map)
  ("\C-\M-x" 'slime-eval-defun)
  ("\C-c\C-o" 'slime-repl-clear-output)
  ("\C-c\C-t" 'slime-repl-clear-buffer)
  ("\C-c\C-u" 'slime-repl-kill-input)

  ("\C-c\C-n" 'slime-repl-next-prompt)
  ((kbd "<down>") 'slime-repl-next-prompt)

  ("\C-c\C-p" 'slime-repl-previous-prompt)
  ((kbd "<up>") 'slime-repl-previous-prompt)

  ("\M-\C-a" 'slime-repl-beginning-of-defun)
  ("\M-\C-e" 'slime-repl-end-of-defun)
  ("\C-c\C-l" 'slime-load-file)
  ("\C-c\C-k" 'slime-compile-and-load-file)
  ("\C-c\C-z" 'slime-nop))