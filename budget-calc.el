
;; (setq foo
;;       (with-temp-buffer
;; 	(insert-file-contents "/home/howard/rec.budget.el")
;; 	(loop for r = (condition-case err
;; 			  (read (current-buffer))
;; 			(error nil))
;; 	   while r collect r)))

;; (loop for (_date amount _desc . tags) in foo
;;    do (pr tags) 
;;    if (member* 'eatout tags)
;;    sum amount)
