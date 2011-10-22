(add-to-list 'load-path "~/el/lib/auto-complete/")
(require 'auto-complete)

(setq ac-ignore-case nil)
(setq ac-auto-show-menu t) ; no delay showing th emenu
(global-auto-complete-mode)


;; C-s to refine search
;; C-g to cancel search
