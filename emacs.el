(add-to-list 'load-path "~/el/")
(add-to-list 'load-path "~/el/my/")
(add-to-list 'load-path "~/el/modes/")
(add-to-list 'load-path "~/el/lib/")

(require 'cl)
(load "_config.el")

(load "_util.el")
(load "_func.el")
(load "_isearch-constraint.el")
      
(load "_keybind.el")

;;(require 'fuzzy-match)
;; modes
(load "_ee.el")
(load "_icicle.el")

(load "_ido.el")
(load "_dir.el")

(load "_auto-complete.el")

;; langs
(load "_lisp.el")
(load "_elisp.el")
(load "_lisp-indent.el")
(load "_ruby.el")
(load "_javascript.el")
;;(load "_erlang.el")

;;(load "_javascript.el")

(show-paren-mode t)

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
 '(espresso-indent-level 2)
 '(fill-column 50)
 '(global-font-lock-mode t nil (font-lock))
 '(icicle--sort-function icicle-historical-alphabetic-p)
 '(icicle-alternative-sort-function icicle-case-string-less-p)
 '(icicle-regexp-quote-flag t)
 '(icicle-reminder-prompt-flag nil t)
 '(icicle-top-level-key-bindings (quote (([pause] icicle-switch-to/from-minibuffer t) ("`" icicle-search-generic t) ("$" icicle-search-word t) ("^" icicle-search-keywords t) ("'" icicle-occur t) ("=" icicle-imenu t) ("\"" icicle-search-text-property t) ("/" icicle-complete-thesaurus-entry t) ([24 134217829] icicle-execute-named-keyboard-macro t) (" " icicle-command-abbrev t) ("5o" icicle-select-frame t) ("" icicle-describe-option-of-type t) ([S-f4] icicle-kmacro t) (abort-recursive-edit icicle-abort-recursive-edit t) (minibuffer-keyboard-quit icicle-abort-recursive-edit (fboundp (quote minibuffer-keyboard-quit))) (dired icicle-dired t) (dired-other-window icicle-dired-other-window t) (execute-extended-command icicle-execute-extended-command t) (switch-to-buffer icicle-buffer t) (switch-to-buffer-other-window icicle-buffer-other-window t) (insert-buffer icicle-insert-buffer t) (find-file icicle-file t) (find-file-other-window icicle-file-other-window t) (bookmarkp-dired-jump-other-window icicle-bookmark-dired-other-window t) (bookmarkp-file-jump-other-window icicle-bookmark-file-other-window t) (bookmarkp-gnus-jump-other-window icicle-bookmark-gnus-other-window t) (bookmarkp-info-jump-other-window icicle-bookmark-info-other-window t) (bookmarkp-man-jump-other-window icicle-bookmark-man-other-window t) (bookmarkp-region-jump-other-window icicle-bookmark-region-other-window t) (bookmarkp-w3m-jump-other-window icicle-bookmark-w3m-other-window t) (bookmarkp-non-file-jump-other-window icicle-bookmark-non-file-other-window t) (bookmarkp-local-file-jump-other-window icicle-bookmark-local-file-other-window t) (bookmarkp-remote-file-jump-other-window icicle-bookmark-remote-file-other-window t) (bookmark-set icicle-bookmark-cmd t) (bookmark-jump icicle-bookmark t) (bookmark-jump-other-window icicle-bookmark-other-window t) (pop-tag-mark icicle-pop-tag-mark (fboundp (quote command-remapping))) (find-tag icicle-find-tag (fboundp (quote command-remapping))) (eval-expression icicle-pp-eval-expression (fboundp (quote command-remapping))) (pp-eval-expression icicle-pp-eval-expression (fboundp (quote command-remapping))) (find-tag-other-window icicle-find-first-tag-other-window t) (kill-buffer icicle-kill-buffer t) (kill-buffer-and-its-windows icicle-kill-buffer t) (delete-window icicle-delete-window t) (delete-windows-for icicle-delete-window t) (other-window-or-frame icicle-other-window-or-frame t) (other-window icicle-other-window-or-frame t) (exchange-point-and-mark icicle-exchange-point-and-mark t) (where-is icicle-where-is t) (yank icicle-yank-maybe-completing t) (pop-global-mark icicle-goto-global-marker-or-pop-global-mark t) ([27 134217848] lacarte-execute-command (fboundp (quote lacarte-execute-command))) ([134217824] lacarte-execute-menu-command (fboundp (quote lacarte-execute-menu-command))) ([f10] lacarte-execute-menu-command (fboundp (quote lacarte-execute-menu-command))))))
 '(indent-tabs-mode nil)
 '(org-agenda-files (quote ("~/todo.org")))
 '(paragraph-start "\\|[      ]*$\\|-+.*$")
 '(safe-local-variable-values (quote ((erlang-indent-level . 2))))
 '(save-place t nil (saveplace))
 '(tab-width 2))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-ws-trailing-whitespace ((t (:background "gray12")))))


