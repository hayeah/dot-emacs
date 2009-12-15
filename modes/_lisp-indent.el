
;; elisp

(let ((l '((save-window-excursion (4 &rest 2))
	   (defkeys (4 &rest 2))
	   (define-derived-mode (2 2 2 &rest 2))
	   (my-defkeys (4 &rest 2))
	   )))
  (dolist (el l)
    (put (car el) 'common-lisp-indent-function
         (if (symbolp (cdr el))
             (get (cdr el) 'common-lisp-indent-function)
             (car (cdr el))))))


;; serl

(let ((l '((def (4 4 &rest 2))
	   (defm . def)
	   (defspecial (4 4 4 &rest 2 ))
	   )))
  (dolist (el l)
    (put (car el) 'common-lisp-indent-function
         (if (symbolp (cdr el))
             (get (cdr el) 'common-lisp-indent-function)
             (car (cdr el))))))

;; ror-mode.el

(let ((l '((ror-try-inflections (4 2))
	   (ror=~ (4 4 &rest 2))
	   (ror=~by (4 &rest 2))
	   (ror-make-goto (4 4 &rest 2))
	   (ror-with-temp-buffer (4 &rest 2))
	   (ror-fn . lambda)
	   (ror-defmethod . defmethod)
	   (ror-awhen . when)
	   )))
  (dolist (el l)
    (put (car el) 'common-lisp-indent-function
         (if (symbolp (cdr el))
             (get (cdr el) 'common-lisp-indent-function)
             (car (cdr el))))))
