
(add-to-list 'load-path "~/el/lib/bmacs")
(add-to-list 'load-path "~/el/lib/bmacs/ude")
(require 'bmacs)
(require 'skribe)

(defkeys (skribe-mode-map)
    ((kbd "C-\\") 'skribe-indent-sexp))
