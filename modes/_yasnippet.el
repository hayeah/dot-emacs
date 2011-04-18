(add-to-list 'load-path "~/el/lib/yasnippet-0.6.1c/")
(require 'yasnippet)
(yas/initialize)


;; Develop and keep personal snippets under ~/emacs.d/mysnippets
(setq yas/root-directory "~/el/snippets")

;; Load the snippets
(yas/load-directory yas/root-directory)
