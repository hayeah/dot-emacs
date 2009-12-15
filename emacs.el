(add-to-list 'load-path "~/el/")
(add-to-list 'load-path "~/el/lib/")

(require 'cl)
(require 'outline-magic)
(require 'isearch-constraint)
(require 'pabbrev)

(require 'color-theme)
(color-theme-goldenrod)

(require 'show-wspace)

;; (show-ws-toggle-show-trailing-whitespace)



(require 'info)
(push (expand-file-name "~/info") Info-directory-list)

(loop for lib in
     '(_util
       _func
       isearch-constraint
       _keybind
       _ee

       ;;_org-mode
       _outline
       _icicle
       _ido
       _dir
       ;;      _scheme ;; stupid load order dependecy
       _lisp
       ;;_serl
       ;; 	_common-lisp

       _elisp
       _javascript

       ;;         _c-mode
       ;;         _js-mode
       ;;         _css-mode
       ;; 	_haskell
       _ruby

       _erlang
       ;;_php
       ;;      _rails
       ;;      _org-mode
       )
     do (load-library (symbol-name lib))
     )


;; Free up screen real-estate
;;
(setq default-frame-alist
	 (append
	  '((vertical-scroll-bars . nil)
	    (horizontal-scroll-bars . nil)
	    (scroll-bar-width . 0)
	    (internal-border-width . 0)
	    (menu-bar-lines . 0)
	    (tool-bar-lines . 0)
	    (line-spacing . 0))
	  default-frame-alist))
(fringe-mode (cons 0 0))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; default modeline

(setq default-mode-line-format
      '("%e"
	;; lame. I can't get eval to work if it's stored in a symbol.
	(:eval (mode-line-window-index))
	mode-line-mule-info
	mode-line-modified
	mode-line-frame-identification
	mode-line-buffer-identification
	mode-line-position
	(vc-mode vc-mode)
	mode-line-modes
	(which-func-mode
	 ("" which-func-format))
	global-mode-string))


;; (setq source-directory "/usr/share/emacs/22.1.50/lisp/emacs-lisp/")

;;help-mode-hook
;; misc
;;(type-break-mode)
;;(setq type-break-interval 5400)
;;(type-break-statistics)
(server-mode)
(desktop-save-mode 1)
(setq desktop-path (list "/home/howard/el/"))
(setq desktop-restore-eager 10)
;;(desktop-read)


(auto-compression-mode 1)
(eldoc-mode 1)
(partial-completion-mode 1)
(icomplete-mode 1)
(setq inhibit-startup-message t)
(put 'upcase-region 'disabled nil)
(column-number-mode 1)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Display page delimiter ^L as a horizontal line
(or standard-display-table (setq standard-display-table (make-display-table)))
(aset standard-display-table ?\f (vconcat (make-vector 64 ?-) "^L"))


;; no silly scrolling to middle nonsense
(setq scroll-step 5)
(mouse-wheel-mode t)


;;bookmark
;;(require 'bookmark+)
;;(setq bookmark-save-flag t)


;; Enable backup files.
(setq make-backup-files nil)
;;(setq version-control t)
;;(setq backup-directory-alist '((".*" . "~/.z_emacs_backups/")))

;; x-clipboard stuff
;;;; from http://www.oreillynet.com/lpt/wlg/6162
;;;; doesn't work well, becaues transient region becomes CLIPBOARD
;;;; a lot of stuff here: http://www.emacswiki.org/cgi-bin/wiki/CopyAndPaste#toc2
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;;;; some people suggest using the below functions.
;;;; But it won't work with other functions that save to the killring.
;; (global-set-key [(shift delete)]   'clipboard-kill-region)
;; (global-set-key [(control insert)] 'clipboard-kill-ring-save)
;; (global-set-key [(shift insert)]   'clipboard-yank)

(defun show-outline-structure ()
    "Show the outline-mode structure of the current buffer."
    (interactive)
    (occur (concat "^" outline-regexp)))


(put 'narrow-to-region 'disabled nil)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(adaptive-fill-mode nil)
 '(case-fold-search t)
 '(completion-max-candidates 5)
 '(completion-min-chars 4)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(ecb-options-version "2.33beta1")
 '(fill-column 50)
 '(global-font-lock-mode t nil (font-lock))
 '(icicle--sort-function icicle-historical-alphabetic-p)
 '(icicle-alternative-sort-function icicle-case-string-less-p)
 '(icicle-regexp-quote-flag t)
 '(icicle-reminder-prompt-flag nil)
 '(indent-tabs-mode nil)
 '(org-agenda-files (quote ("~/todo.org")))
 '(paragraph-start "\\|[      ]*$\\|-+.*$")
 '(quack-fontify-style (quote emacs))
 '(quack-pretty-lambda-p nil)
 '(quack-programs (quote ("/home/howard/bin/mzscheme" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(safe-local-variable-values (quote ((erlang-indent-level . 2))))
 '(save-place t nil (saveplace))
 '(show-paren-mode t nil (paren))
 '(tab-width 2))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-ws-trailing-whitespace ((t (:background "gray12")))))

(put 'downcase-region 'disabled nil)
