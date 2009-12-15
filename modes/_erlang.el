
(add-to-list 'load-path "~/el/lib/erlang")
(add-to-list 'load-path "~/el/lib/erlang/distel/elisp")

(require 'distel)
(distel-setup)


(add-to-list 'auto-mode-alist '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl$" . erlang-mode))



(my-defkeys erlang-extended-mode-map
	    (prefix [f5]
		    ([?e] 'erl-eval-expression)) 
	    ([?\(] 'insert-pair)
	    ([?\r] 'newline-and-indent))

(my-defkeys erl-session-minor-mode-map
	    ((kbd "C-j") nil)
	    ((kbd "C-M-x") nil)
	    (prefix [f5]
		    
		    ([f5] 'erl-ie-eval-expression)
		    ([?d] 'erl-ie-eval-defun)
		    ([?k] 'erl-ie-kill-last-eval-process)))



;; (my-defkeys erlang-shell-mode-map
;; 	    ([tab] nil)
;; 	    ([?\C-i] (fi () (when inferior-erlang-process
;; 			      (comint-send-string inferior-erlang-process
;; 						  "v:\C-i")))))



