(require 'quack)
(setq scheme-program-name "/home/howard/bin/mzscheme")


(my-defkeys (scheme-mode-map)
	    ((kbd "(") nil)
	    ((kbd "[") nil)
	    ((kbd "{") nil)
	    ((kbd "<f5>") 'scheme-send-last-sexp)
	    )




