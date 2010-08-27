(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
;; (add-to-list 'auto-mode-alist '("\\.js" . espresso-mode))
(autoload 'espresso-mode "espresso" nil t)

(add-to-list 'ac-modes 'espresso-mode)

(add-to-list 'espresso-mode-hooks
             'font-lock-mode)




