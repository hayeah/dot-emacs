
(defvar my-elisp-eval-map (make-sparse-keymap)) 


(defun elisp-complete/indent ()
  "complete symbol and indent"
  (interactive)
  (when (looking-at "\\>")
    (PC-lisp-complete-symbol))
  (lisp-indent-line))


(my-defkeys (emacs-lisp-mode-map lisp-interaction-mode-map) 
	    ([tab] 'elisp-complete/indent)) 



