(require 'prolog)

;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
;; (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;; (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(setq prolog-system 'swi)  ; optional, the system you are using;
                           ; see `prolog-system' below for possible values

(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
                               auto-mode-alist))



;; (defun prolog-mode-keybindings-common (map)
;;   "Define keybindings common to both Prolog modes in MAP."
;;   (define-key map "\C-c?" 'prolog-help-on-predicate)
;;   (define-key map "\C-c/" 'prolog-help-apropos)
;;   (define-key map "\C-c\C-d" 'prolog-debug-on)
;;   (define-key map "\C-c\C-t" 'prolog-trace-on)
;;   (if (and (eq prolog-system 'sicstus)
;;            (prolog-atleast-version '(3 . 7)))
;;       (define-key map "\C-c\C-z" 'prolog-zip-on))
;;   (define-key map "\C-c\r" 'run-prolog))

;; (defun prolog-mode-keybindings-edit (map)
;;   "Define keybindings for Prolog mode in MAP."
;;   (define-key map "\s-j" 'prolog-beginning-of-clause)
;;   (define-key map "\s-l" 'prolog-end-of-clause)
;;   (define-key map "\M-q" 'prolog-fill-paragraph)
;;   (define-key map "\C-\M-a" 'prolog-beginning-of-predicate)
;;   (define-key map "\C-\M-e" 'prolog-end-of-predicate)
;;   (define-key map "\s-n" 'prolog-mark-clause)
;;   (define-key map "\M-\C-h" 'prolog-mark-predicate)
;;   (define-key map "\M-\C-l" 'prolog-forward-list)
;;   (define-key map "\M-\C-j" 'prolog-backward-list)
;;   (define-key map "\C-c\C-n" 'prolog-insert-predicate-template)
;;   (define-key map "\C-c\C-s" 'prolog-insert-predspec)
;;   (define-key map "\M-\r" 'prolog-insert-next-clause)
;;   (define-key map "\C-c\C-va" 'prolog-variables-to-anonymous)
;;   (define-key map "\C-c\C-v\C-s" 'prolog-view-predspec)

;;   (define-key map [Backspace] 'prolog-electric-delete)
;;   (define-key map "." 'prolog-electric-dot)
;;   (define-key map "_" 'prolog-electric-underscore)
;;   (if prolog-electric-newline-flag 
;;       (define-key map "\r" 'newline-and-indent))

;;   ;; If we're running SICStus, then map C-c C-c e/d to enabling
;;   ;; and disabling of the source-level debugging facilities.
;;   ;(if (and (eq prolog-system 'sicstus)
;;   ;         (prolog-atleast-version '(3 . 7)))
;;   ;    (progn
;;   ;      (define-key map "\C-c\C-ce" 'prolog-enable-sicstus-sd)
;;   ;      (define-key map "\C-c\C-cd" 'prolog-disable-sicstus-sd)
;;   ;      ))

;;   (if prolog-old-sicstus-keys-flag
;;       (progn
;;         (define-key map "\C-c\C-c" 'prolog-consult-predicate)
;;         (define-key map "\C-cc" 'prolog-consult-region)
;;         (define-key map "\C-cC" 'prolog-consult-buffer)
;;         (define-key map "\C-c\C-k" 'prolog-compile-predicate)
;;         (define-key map "\C-ck" 'prolog-compile-region)
;;         (define-key map "\C-cK" 'prolog-compile-buffer))
;;     (define-key map "\C-c\C-p" 'prolog-consult-predicate)
;;     (define-key map "\C-c\C-r" 'prolog-consult-region)
;;     (define-key map "\C-c\C-b" 'prolog-consult-buffer)
;;     (define-key map "\C-c\C-f" 'prolog-consult-file)
;;     (define-key map "\C-c\C-cp" 'prolog-compile-predicate)
;;     (define-key map "\C-c\C-cr" 'prolog-compile-region)
;;     (define-key map "\C-c\C-cb" 'prolog-compile-buffer)
;;     (define-key map "\C-c\C-cf" 'prolog-compile-file)))

;; (defun prolog-mode-keybindings-inferior (map)
;;   "Define keybindings for inferior Prolog mode in MAP."
;;   ;; No inferior mode specific keybindings now.
;;   )

;; (if prolog-mode-map
;;     ()
;;   (setq prolog-mode-map (make-sparse-keymap))
;;   (prolog-mode-keybindings-common prolog-mode-map)
;;   (prolog-mode-keybindings-edit prolog-mode-map)
;;   ;; System dependent keymaps for system dependent menus
;;   (let ((systems prolog-known-systems))
;;     (while systems
;;       (set (intern (concat "prolog-mode-map-"
;;                            (symbol-name (car systems))))
;;            ;(cons 'keymap prolog-mode-map)
;;            prolog-mode-map
;;            )
;;       (setq systems (cdr systems))))
;;   )