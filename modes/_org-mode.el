(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'load-path "~/el/lib/org-mode/lisp")

(require 'org-install)
(require 'org)
;;(define-key global-map "\C-cl" 'org-store-link)
;;(define-key global-map "\C-ca" 'org-agenda)

;; (setq org-tag-alist '(("@WORK" . ?w) ("@HOME" . ?h) ("Laptop" . ?l)))


(defkeys org-mode-map
  ("\C-k" nil) ;; org-kill-line
  ("\C-j" nil) ;; org-meta-return
  ("\M-j" nil)  ;; org-meta-return
  ((kbd "M-<return>") nil) ;; org-meta-return
  ((kbd "M-RET") nil)
  ([return] 'org-return)
  ([C-return] 'org-meta-return)
  ([C-M-return] 'org-insert-todo-heading)
  ([s-return] 'org-insert-subheading) 
  (prefix [f5]
	  ("f" 'org-sparse-tree)
	  ;; time related
	  (prefix "t"
		  ("i" 'org-clock-in)
		  ("o" 'org-clock-out)
		  ("t" 'org-time-stamp))
	  ;; tree
	  (modal [f5]
		 ;; motion
		 (C-i 'outline-up-heading) 
		 ("\C-j" 'outline-backward-same-level)
		 ("\M-j" 'outline-previous-visible-heading)
		 ("\C-l" 'outline-forward-same-level)
		 ("\M-l" 'outline-next-visible-heading)
		 ;; structure editing 
		 ((kbd "C--") 'org-shiftmetaleft)
		 ((kbd "C-=") 'org-shiftmetaright)
		 ("\C-w" 'org-cut-special)
		 ("\M-w" 'org-copy-special) 
		 ("\C-s" 'org-paste-special) 
		 ((kbd "C-\\") 'org-sort-entries-or-items)
		 ("\C-y" 'org-toggle-archive-tag)
		 ;;([C-return] 'org-insert-heading-after-current)
		 ))
  )

(current-local-map)
(setq org-cycle-include-plain-lists t)

(setq org-agenda-start-on-weekday nil)
(setq org-agenda-ndays 7)

;; (custom-set-variables 
;;  '(org-agenda-files (quote ("~/todo.org"))) 
;;  '(org-default-notes-file "~/notes.org") 
;;  '(org-agenda-ndays 7) 
;;  '(org-deadline-warning-days 14) 
;;  '(org-agenda-show-all-dates t) 
;;  '(org-agenda-skip-deadline-if-done t) 
;;  '(org-agenda-skip-scheduled-if-done t) 
;;  '(org-agenda-start-on-weekday nil) 
;;  '(org-reverse-note-order t) 
;;  '(org-fast-tag-selection-single-key (quote expert)) 
;;  '(org-agenda-custom-commands
;;    (quote (("d" todo "DELEGATED" nil) 
;;            ("c" todo "DONE|DEFERRED|CANCELLED" nil) 
;;            ("w" todo "WAITING" nil) 
;;            ("W" agenda "" ((org-agenda-ndays 21)))
;; 	   ("A" agenda ""
;; 		((org-agenda-skip-function
;; 		  (lambda nil
;; 		    (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) 
;; 		 (org-agenda-ndays 1)
;; 		 (org-agenda-overriding-header "Today's Priority #A tasks: ")))
;; 	   ("u" alltodo ""
;; 		((org-agenda-skip-function
;; 		  (lambda nil
;; 		    (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
;; 					      (quote regexp) "<[^>\n]+>")))
;; 		 (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
;;  '(org-remember-store-without-prompt t)
;;  '(org-remember-templates
;;    (quote ((116 "* TODO %?\n  %u" "~/todo.org" "Tasks")
;; 	   (110 "* %u %?" "~/notes.org" "Notes"))))
;;  '(remember-annotation-functions (quote (org-remember-annotation)))
;;  '(remember-handler-functions (quote (org-remember-handler))))
