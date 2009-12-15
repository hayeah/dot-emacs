(add-to-list 'load-path "~/el/")
(add-to-list 'load-path "~/el/my/")
(add-to-list 'load-path "~/el/modes/")
(add-to-list 'load-path "~/el/lib/")

(require 'cl)

;; settings
(load "_config.el")

(load "_util.el")
(load "_func.el")
(load "_isearch-constraint.el")

(load "_keybind.el")

;; modes
(load "_ee.el")

(load "_icicle.el")
;;(load "_ido.el")
(load "_dir.el")

;; langs
(load "_lisp.el")
(load "_elisp.el")
(load "_lisp-indent.el")
(load "_ruby.el")
(load "_erlang.el")

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


