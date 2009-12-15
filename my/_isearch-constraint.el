;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Sensitive iSearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Extend isearch so for context-sensitive search.
;;
;; If an isearch query has too many results, you can refine it by calling
;; `isearch-constraint-forward' or `isearch-constraint-backward'
;; to constrain results by the context around the initial query.
;; For example, having searched `foo', you can refine the query
;; by specifying only those foo's that are followed by a bar.


;;;; variables from isearch.el
;; `isearch-string' is the accumulator used by minibuffer to build up the search query.
;; `isearch-opoint' is where the search first started, to allow jump back.
;; `isearch-message' is used to display the search query in the minibuffer. I am not really using it properly.


(defvar isearch-constraint-anchor ""
  "This is the search string one started with. Go to that after the constraints.")
(defvar isearch-constraint-opoint 0
  "Where the search was first started.")

(defvar isearch-constraint-direction nil)
(defvar isearch-constraint-string "")
(defvar isearch-constraint-message "")

(defun isearch-constraint-forward ()
  (interactive)
  (isearch-with-constraint 'forward))

(defun isearch-constraint-backward ()
  (interactive)
  (isearch-with-constraint 'backward))

(defun isearch-with-constraint (direction)
  (interactive)
  (unless isearch-constraint-direction
    (setq isearch-constraint-anchor isearch-string)
    (setq isearch-constraint-opoint isearch-opoint))
  (setq isearch-constraint-string
	(isearch-current-constraint-string))
  (setq isearch-constraint-direction direction)
  ;;(pr "iwc" (isearch-current-constraint-string)) 
  (setq isearch-string "")
  (setq isearch-message "") 
  (isearch-search-and-update)
  )

(defun isearch-current-constraint-string ()
  (case isearch-constraint-direction
    (forward (concat
	      (if (equal "" isearch-constraint-string) ""
		  (concat isearch-constraint-string ".*?"))
	      (if isearch-regexp isearch-string
		  (regexp-quote isearch-string))))
    (backward (concat
	       (if isearch-regexp isearch-string
		   (regexp-quote isearch-string)) 
	       (if (equal "" isearch-constraint-string) ""
		   (concat ".*?" isearch-constraint-string))))
    (t (if isearch-regexp isearch-string
	   (regexp-quote isearch-string)))))

;; an after advice is needed for isearch-done to clear isearch-stored-string
(defadvice isearch-done (after isearch-clear-constraint)
  (when isearch-constraint-direction
    (setq isearch-constraint-string "")
    (setq isearch-constraint-message "")
    (setq isearch-constraint-direction nil)
    ;; Go to the initial search we were looking for.
    ;;; if last isearch is forward, we'd be at the end of the match
    ;;; Or we'd be at the beginning of the match 
    (if isearch-forward 
	(progn (re-search-backward isearch-constraint-anchor)
	       (re-search-forward isearch-constraint-anchor))
	(re-search-forward isearch-constraint-anchor))
    (set-mark isearch-constraint-opoint)
    (setq isearch-constraint-opoint 0)
    ))

;; two identical advices for the isearch functionalities I need.
;;; you might want others, i dunno. Dig into the source yourself.
;; These two allow search and repeated search.
(defadvice isearch-repeat (around isearch-with-constraint)
  "Perform a context sensitive search using the previous input as constraint."
  ;;(pr "isr" (isearch-current-constraint-string) isearch-forward)
  (let ((isearch-string (isearch-current-constraint-string))
	(isearch-regexp t))
    ad-do-it))

(defadvice isearch-search-and-update (around isearch-with-constraint)
  ;;(pr "isau" (isearch-current-constraint-string))
  (let ((isearch-string (isearch-current-constraint-string))
	(isearch-regexp t))
    ad-do-it))

(ad-activate 'isearch-repeat)
(ad-activate 'isearch-search-and-update)
(ad-activate 'isearch-done)

(provide 'isearch-constraint)

;; (ad-update-all)
