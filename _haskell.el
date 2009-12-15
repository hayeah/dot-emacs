(add-to-list 'load-path "~/el/lib/haskell-mode-2.1/")
(load "~/el/lib/haskell-mode-2.1/haskell-site-file")


(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-hook 'haskell-mode-hook 'font-lock-mode)