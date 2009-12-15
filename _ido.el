

(require 'ido)
(ido-mode 1)

(defvar ido-make-choice-list-hook nil)

(defun ido-make-choice-list (default)
  ;; Return the current list of choices.
  ;; If DEFAULT is non-nil, and corresponds to an element of choices,
  ;; it is put to the start of the list.
  (let ((ido-temp-list ido-choice-list))
    (if default
	(progn
	  (setq ido-temp-list
		(delete default ido-temp-list))
	  (setq ido-temp-list
		(cons default ido-temp-list))))
    ;; ido commented out the choice-list hook for some reason.
    (run-hooks 'ido-make-choice-list-hook)
    ido-temp-list))

(setq ido-make-buffer-list-hook
      (list (fn ()
		(setq ido-temp-list
		      (sort (remove-if
			     (fn (b)
				 (string-match icicle-buffer-no-match-regexp b))
			     ido-temp-list)
			    'icicle-buffer-time-sort)))))

(setq ido-make-choice-list-hook
      (list
       (fn ()
	   (setq ido-temp-list
		 (sort ido-temp-list
		       (fn (b1 b2)
			   (let ((t1 (ido-bookmark-timestamp b1))
				 (t2 (ido-bookmark-timestamp b2)))
			     (if t2
				 (and t1 (> t1 t2 ))
				 t1))))))))


(defkeys (ido-common-completion-map
	  ido-buffer-completion-map
	  ido-file-completion-map)
  (C-i 'ido-prev-match)
  ((kbd "C-k") 'ido-next-match)
  ([escape] 'ido-fallback-command)
  )



;;ido-fallback-command

(defkeys ido-file-completion-map
  ;; C-f  switch to icicle-find-file
  ;; C-e  edit file string
  ("\M-i" 'ido-prev-match-dir)
  ("\M-k" 'ido-next-match-dir)
  (prefix "\C-d"
	  ("\C-p" 'ido-enter-dired)
	  ("\C-m" 'ido-make-directory)
	  ))


(defvar ido-bookmark-timestamps
  (make-hash-table :test 'equal))
(defvar bookmark-after-jump-hook nil)

(defun ido-bookmark-timestamp (bm)
  (gethash bm ido-bookmark-timestamps))

(defun ido-bookmark-timestamp-set (last-bm)
  (when last-bm
    (setf (gethash last-bm ido-bookmark-timestamps)
	  (float-time))))

(defun ido-bookmark-timestamp-update ()
  (ido-bookmark-timestamp-set (car bookmark-history)))


(push 'ido-bookmark-timestamp-update
      bookmark-after-jump-hook)


;; (defvar ido-bookmark-completion-map nil)

;; (unless ido-bookmark-completion-map
;;   (progn
;;     (setq ido-bookmark-completion-map (make-sparse-keymap))
;;     (defkeys ido-bookmark-completion-map
;;       ("C-a" (fi () (message "foo")))
;;       )
;;     (set-keymap-parent ido-bookmark-completion-map
;; 		       ido-common-completion-map)))

(defun ido-bookmark-completing-read (prompt)
  (ido-completing-read
   prompt
   (mapcar 'car bookmark-alist)))

;; (defun ido-bookmark-jump (bm)
;;   ;; (ido-completing-read) calls
;;   ;; (ido-read-internal) calls
;;   ;; (ido-setup-completion-map)
;;   ;; it's probably possible to dynamically shadow `ido-common-completion-map'
;;   ;; to have a custom keymap for bookmark-completion. The switching
;;   ;; of different keymaps is hardwired in.
;;   (interactive
;;    (list ))
;;   (bookmark-jump bm))

;; ido-make-file-list-hook
;; ido-make-dir-list-hook

(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)