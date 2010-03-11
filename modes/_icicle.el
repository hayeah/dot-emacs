(add-to-list 'load-path "~/el/lib/icicles/")

;; these two should be set here before icicle starts to take effect.
(setq icicle-prefix-complete-keys '([S-tab]))
(setq icicle-apropos-complete-keys '([tab]))

(require 'icicles)

(setq icicle-search-from-isearch-keys '())

(icicle-mode 1)
(setq insert-default-directory t)
(setq icicle-reminder-prompt-flag nil)


;; (setq icicle-apropos-cycle-previous-keys (list C-i))
;; (setq icicle-apropos-cycle-next-keys (list (kbd "C-k")))

;; icicle-apropos-complete-keys
;; icicle-apropos-complete-no-display-keys
;; icicle-apropos-cycle-next-action-keys
;; icicle-apropos-cycle-next-alt-action-keys 
;; icicle-apropos-cycle-next-help-keys
;; icicle-apropos-cycle-next-keys 
;; icicle-apropos-cycle-previous-action-keys
;; icicle-apropos-cycle-previous-alt-action-keys 
;; icicle-apropos-cycle-previous-help-keys
;; icicle-apropos-cycle-previous-keys 
;; icicle-complete-keys-alist
;; icicle-complete-keys-self-insert-flag 
;; icicle-completing-read+insert-keys
;; icicle-isearch-complete-keys 
;; icicle-key-complete-keys
;; icicle-modal-cycle-down-action-keys 
;; icicle-modal-cycle-down-alt-action-keys
;; icicle-modal-cycle-down-help-keys 
;; icicle-modal-cycle-down-keys
;; icicle-modal-cycle-up-action-keys 
;; icicle-modal-cycle-up-alt-action-keys
;; icicle-modal-cycle-up-help-keys 
;; icicle-modal-cycle-up-keys
;; icicle-prefix-complete-keys 
;; icicle-prefix-complete-no-display-keys
;; icicle-prefix-cycle-next-action-keys 
;; icicle-prefix-cycle-next-alt-action-keys
;; icicle-prefix-cycle-next-help-keys 
;; icicle-prefix-cycle-next-keys
;; icicle-prefix-cycle-previous-action-keys 
;; icicle-prefix-cycle-previous-alt-action-keys
;; icicle-prefix-cycle-previous-help-keys 
;; icicle-prefix-cycle-previous-keys
;; icicle-previous-candidate-keys 
;; icicle-read+insert-file-name-keys
;; icicle-search-from-isearch-keys 
;; icicle-word-completion-keys 


;;(setq icicle-cycling-respects-completion-mode-flag t)


;; (setq icicle-download-dir "~/el/lib/icicles/")

;; (require 'icicles-install)

;; (add-hook 'icicle-mode-hook 'my-icicle-keybind)

;; (defvar minibuffer-local-filename-completion-map (make-sparse-keymap))



;; (defun my-icicle-keybind ()
;;   (my-defkeys icicle-mode-map
;; 	      ([f5] nil))
  
;;   (my-defkeys (minibuffer-local-completion-map minibuffer-local-must-match-map minibuffer-local-filename-completion-map)
;; 	      ([?\M-n] nil)
;; 	      ([?\r] 'icicle-exit-minibuffer)
;; 	      ([tab] 'icicle-apropos-complete)
;; 	      ;; progressive completion
;; 	      (prefix [escape]
;; 		      ;; refinement
;; 		      ([escape] 'icicle-narrow-candidates)
;; 		      ([?=] 'icicle-candidate-set-union)
;; 		      ([?-] 'icicle-candidate-set-difference)
;; 		      ([?`] 'icicle-candidate-set-complement)
;; 		      ;; save/retrieve
;; 		      ([?,] 'icicle-candidate-set-save)
;; 		      ([?\C-,] 'icicle-candidate-set-save-to-variable)
;; 		      ([?.] 'icicle-candidate-set-retrieve)
;; 		      ([?\C-.] 'icicle-candidate-set-retrieve-from-variable)
;; 		      ;;([?s] 'icicle-candidate-set-swap)
;; 		      )
;; 	      ([?\C-k] 'icicle-next-apropos-candidate)
;; 	      (my-C-i 'icicle-previous-apropos-candidate) 
;; 	      ;; completion history
;; 	      (prefix [f1]
;; 		      ;;([f1] 'icicle-insert-history-element)
;; 		      ;;([] 'icicle-keep-only-past-inputs)
;; 		      ([f1] 'icicle-history)
;; 		      )
;; 	      ((kbd "<C-return>") 'icicle-candidate-action) ;; does a command to selected candidate
;; 	      ((kbd "<C-M-return>") 'icicle-all-candidates-action) ;; does multi-command to all candidates
;; 	      ([next] 'icicle-next-apropos-candidate-action)
;; 	      ([prior] 'icicle-previous-apropos-candidate-action)
;; 	      ([C-next] 'icicle-help-on-next-apropos-candidate) ;; shows associated help of a candidate
;; 	      ([C-prior] 'icicle-help-on-previous-apropos-candidate)
;; 	      ([?\C-r] 'icicle-toggle-regexp-quote)
;; 	      ((kbd "<s-return>") 'icicle-exit-minibuffer)
;; 	      )
;;   )


;; ;; list buffers by access time
;; (setq icicle-buffer-sort 'icicle-buffer-time-sort)

;; (setq icicle-buffer-no-match-regexp
;;       (regexp-opt (list "menubar.ee")))

;; (defun icicle-buffer-time-sort (b1 b2)
;;   "Compare two buffers by their access time. *-buffers to the end of the list."
;;   (flet ((more-recent (b1 b2)
;; 	   (> (with-current-buffer b1
;; 		(float-time buffer-display-time))
;; 	      (with-current-buffer b2
;; 		(float-time buffer-display-time)))
;; 	   ))
;;     (if (string-match "^\\*" b1)
;; 	(and (string-match "^\\*" b2) (more-recent b1 b2))
;; 	(or (string-match "^\\*" b2) (more-recent b1 b2)))))







;;‘icicle-candidate-set-intersection’, bound to ‘C-*’. Replace the current candidate set by its intersection with the saved set of candidates. Unlike the set intersection provided by ‘M-*’, ‘C-*’ is, in itself, a one-time operation. ‘M-*’ can be repeated, using the previous intersection as one of the sets to be intersected in a new operation. Both ‘C-*’ and ‘M-*’ use the current set of matching candidates as one of the sets being intersected. But ‘M-*’ reads another input regexp to define the other set to be intersected, whereas ‘C-*’ uses the saved candidates set as the other set. ‘M-*’ is useful for chaining, to achieve progressive approximation. ‘C-*’ is useful to perform an intersection on a set from a previous input reading.


;; (defun icicle-search-region-beg-end ()
;;     "Return the start and end of the search region as a list.
;; If the region is not active or empty, then bob and eob are used.

;; I don't like the icicle default. Search entire buffer unless i am using transient-mark-mode to mark a region.
;; "
;;   (if (or (not transient-mark-mode) (null (mark)) (= (point) (mark)))
;;       (list (point-min) (point-max))
;;     (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))


;; this has to come after for keybindings to work for restored buffers.



;;(fboundp 2C-command)

;;(symbol-function '2C-command)

;; (custom-set-variables
;;  ;; i can't get these two to work, damn it.
;;  '(icicle--sort-function icicle-historical-alphabetic-p)
;;  '(icicle-alternative-sort-function icicle-case-string-less-p)
;;  )

