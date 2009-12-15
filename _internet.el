(add-to-list 'load-path "~/el/lib/w3m")
(require 'w3m-load)
(require 'w3m)


;; (defvar my-text-based-urls
;;   "")


(setq browse-url-generic-program "opera"
      browse-url-browser-function 
      '((".*" . browse-url-generic)))


;; (setq w3m-command-arguments
;;       (nconc w3m-command-arguments
;;              '("-o" "http_proxy=http://proxy.hogege.com:8000/")))


