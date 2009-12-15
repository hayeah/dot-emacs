;;(add-to-list 'load-path "~/emacs/lib/tramp/lisp/")
(require 'tramp)

;; You can also specify different methods for certain user/host
;; combinations, via the variable tramp-default-method-alist. For
;; example, the following two lines specify to use the ssh method for
;; all user names matching ‘john’ and the rsync method for all host
;; names matching ‘lily’. The third line specifies to use the su
;; method for the user ‘root’ on the machine ‘localhost’.

;; (add-to-list 'tramp-default-method-alist '("" "john" "ssh"))
;; (add-to-list 'tramp-default-method-alist '("lily" "" "rsync"))
;; (add-to-list 'tramp-default-method-alist '("\\`localhost\\'" "\\`root\\'" "su"))