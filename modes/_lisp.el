(require 'paredit)
(require 'cl-indent)

(add-hook 'lisp-mode-hook (lambda () (setq lisp-indent-function 'common-lisp-indent-function)))
(add-hook 'emacs-lisp-mode-hook (lambda () (setq lisp-indent-function 'common-lisp-indent-function)))


(my-defkeys (lisp-interaction-mode-map lisp-mode-map)
  ((kbd "C-j") nil))


(my-defkeys (lisp-mode-map emacs-lisp-mode-map lisp-interaction-mode-map ;;scheme-mode-map
) 
  ("\C-\\" 'indent-sexp) 
  (prefix "\C-d"
	  ("\C-r" 'my-replace-string-sexp)
	  ((kbd "i") 'paredit-splice-sexp)
	  ((kbd "C->") 'paredit-raise-sexp)
	  ((kbd "C-k") 'paredit-wrap-sexp)
	  ((kbd "C-l") 'paredit-forward-slurp-sexp)
	  ((kbd "M-l") 'paredit-forward-barf-sexp)
	  ((kbd "C-M-l") 'paredit-splice-sexp-killing-forward)
	  ((kbd "C-j") 'paredit-backward-slurp-sexp)
	  ((kbd "M-j") 'paredit-backward-barf-sexp)
	  ((kbd "C-M-j") 'paredit-splice-sexp-killing-backward)
	  ((kbd "C-b") 'paredit-recentre-on-sexp)))
