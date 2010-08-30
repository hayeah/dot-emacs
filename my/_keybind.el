(case (my-os)
  (Linux
   ;; a cleaner way to get around the C-i == TAB identity confusion.
   (keyboard-translate ?\C-i ?\C->))
  (Darwin
   (keyboard-translate ?\C-i ?\C->)
   (setq mac-command-modifier 'meta)
   (setq mac-option-modifier 'super)
   (setq mac-control-modifier 'control)
   ))

(defvar my-C-i [?\C->])
(defvar C-i [?\C->])
(defvar my-f11-key [f11])
(defvar my-f12-key [f12])

;;;; emacs prefix-keys
;; esc-map is the global keymap for the <ESC> prefix key. Thus, the global definitions of all meta characters are actually found here. This map is also the function definition of ESC-prefix.
;; help-map is the global keymap for the C-h prefix key.
;; mode-specific-map is the global keymap for the prefix key C-c. This map is actually global, not mode-specific, but its name provides useful information about C-c in the output of C-h b (display-bindings), since the main use of this prefix key is for mode-specific bindings.
;; ctl-x-map is the global keymap used for the C-x prefix key. This map is found via the function cell of the symbol Control-X-prefix.
;; mule-keymap is the global keymap used for the C-x <RET> prefix key.
;; ctl-x-4-map is the global keymap used for the C-x 4 prefix key.
;; ctl-x-5-map is the global keymap used for the C-x 5 prefix key.
;; 2C-mode-map is the global keymap used for the C-x 6 prefix key.
;; vc-prefix-map is the global keymap used for the C-x v prefix key.
;; facemenu-keymap is the global keymap used for the M-o prefix key.
;; The other Emacs prefix keys are M-g, C-x @, C-x a i, C-x <ESC> and <ESC> <ESC>. They use keymaps that have no special names.

;; ;; Translate the meta-chars to 0.
;; ;; ;; Meta-chars are those ranging from 128-255. It's old emacs thing, having to do with the bits won't fit in a string, so string cannot be used to represent event sequence.

;;;; Prefix Key Craziness
;; (setq meta-prefix-char 0) ;; 0 is the ASCII null, not used elsewhere.
;; (define-key global-map [0] 'ESC-prefix) ;; bind 0 as the meta prefix map.
;; (define-key global-map [27] (make-sparse-keymap)) ;; bind ESC to a new prefix map. This would break keybindings using 27. Should be rare.
;; (define-key global-map [f1] (make-sparse-keymap))
;; (define-key function-key-map [escape] nil) ;; don't translate escape to 27. Probably safer to bind [escape] then [27].
;; (define-key global-map [escape] (make-sparse-keymap)) ;; also make escape a prefix key.
;; (define-key global-map "\C-c" (make-keymap))

(define-key global-map (kbd "C-h") help-map)
;; unbind f1 from help-map
(define-key global-map (kbd "<f1>") (make-sparse-keymap))

;; it is probably good to for prefix
;; C-x -> f1
;; ;; so f1-4  would be available for global prefix keys
;; ;; ditto for f9-12
;; C-c -> f5
;; ;; so f5-8  would be available for mode specific prefix keys

(defun my-canonicalize-active-keymaps ()
  (interactive)
  (my-canonicalize-keymaps
   (if overriding-local-map
       (list overriding-local-map)
       (cons (current-local-map) (current-minor-mode-maps))))
  )

(defun my-canonicalize-keymaps (keymaps)
  "C-i -> tab ;  C-M-i -> M-tab "
  (let ((keys `(([9] . [tab])
		(,(kbd "C-M-i") . [M-tab])

		)))
    (loop for m in keymaps do
	 (loop for (from . to) in keys
	    for bind = (lookup-key m from)
	    when bind do (progn (define-key m to bind)
				(define-key m from nil))))))

(defmacro my-defkeys (map/s &rest bindings)
  "define keys"
  (let ((maps (if (listp map/s)
		  map/s
		  (list map/s))))
    (labels ((emit-binding (map b)
	       (case (first b)
		 (prefix (let ((prefix-key (second b))
			       (bindings (nthcdr 2 b))
			       (tmp (gensym "prefix-map")))
			   `(let* ((,tmp (lookup-key ,map ,prefix-key))
				   ;; don't replace existing prefix-key.
				   ;; This allows separate definitions to merge.
				   (,tmp (if (keymapp ,tmp) ,tmp
					     (make-sparse-keymap))))
			      (define-key ,map ,prefix-key ,tmp)
			      ,@(loop for b in bindings
				   collect (emit-binding tmp b)))))
		 ;; TODO should add a hook to bufferchange to exit modal keymap
		 (modal (let ((modal-key (second b))
			      (bindings (nthcdr 2 b))
			      (modal-map (gensym "modal-map")))
			  `(lexical-let* ((,modal-map (lookup-key ,map ,modal-key))
					  (,modal-map (if (keymapp ,modal-map) ,modal-map
							  (make-sparse-keymap))))
			     (define-key ,map ,modal-key
			       (fi () (newb-enter-modal ,modal-map)))
			     ,@(loop for b in bindings
				    collect (emit-binding modal-map b))
			     ;; escape always gets you out of mode
			     (define-key ,modal-map [escape]
			       (fi () (newb-exit-modal ,modal-map))))))
		 (t `(define-key ,map ,(first b) ,(second b))))))
      `(progn ,@(loop for map in maps nconc
		     (loop for b in bindings collect
			  (emit-binding map b)))
	      ',map/s))))

(defvar newb-modal-idle-timer nil)
(make-variable-buffer-local 'newb-modal-idle-timer)

(defvar newb-exit-idle-time 30
  "Exits modal if emacs idles for this amount of time.")

(defun newb-enter-modal (modal-map)
  "Enters into a modal. Takes care not to mess up other buffers.
   Exit if idle for more than a threshold."
  ;; FIXME for some reason, the first event after timeout is still in the modal.
  (message "In modal. Hit Esc to get out.")
  (make-local-variable 'overriding-local-map)
  (set-keymap-parent modal-map (current-local-map))
  (setq overriding-local-map modal-map)
  (setq newb-modal-idle-timer
	(run-with-idle-timer newb-exit-idle-time
	 nil 'newb-exit-modal modal-map)))

(defmacro defkeys (&rest body)
  `(my-defkeys ,@body))

;; ;; misc personal bindings
;; (my-defkeys org-mode-map
;;   ("\C-a" 'org-agenda))

;; prefix keys
(my-defkeys global-map
  ;; window management
  (prefix [f1]
	  ;; (prefix [?o])
	  ([f1] 'split-window-smartly)
    ([?`] 'my-hide-temp-windows)
	  ;;([f1] 'split-window-vertically)
	  ;;([f2] 'split-window-horizontally)
	  ([escape] 'delete-other-windows)
	  ([?d] 'delete-window-by-index)
	  ([?s] 'swap-windows-by-index)
	  ;; five is probably enough...
	  ([?1] (fi () (select-window-by-index 1)))
	  ([?2] (fi () (select-window-by-index 2)))
	  ([?3] (fi () (select-window-by-index 3)))
	  ([?4] (fi () (select-window-by-index 4)))
	  ([?5] (fi () (select-window-by-index 5)))
	  ;;([?6] (fi () (select-window-by-index 6)))
	  ;;([?7] (fi () (select-window-by-index 7)))
	  ;;([?8] (fi () (select-window-by-index 8)))
	  ;;([?9] (fi () (select-window-by-index 9)))
	  ;;(prefix [?t]
    ([?a] 'my-etags-add-buffer)
    ;;([?l] 'etags-list-tags)
    ([?n] 'toggle-narrow-to-region)
    ((kbd "<prior>") 'winner-redo)
    ((kbd "<next>") 'winner-undo)

    ([?i] 'beginning-of-buffer)
    ([?k] 'end-of-buffer)
    )

	  ;; (modal [?a]
		;;  ;; TODO buffer scrolling
		;;  (my-C-i 'enlarge-window)
		;;  ([?\C-k] 'shrink-window)
		;;  ([?\C-l] 'enlarge-window-horizontally)
		;;  ([?\C-j] 'shrink-window-horizontally)
		;;  ([?s] 'shrink-window-if-larger-than-buffer)
		;;  ([?f] 'fit-window-to-buffer))
	  ;; (prefix [?w]
		;;   ([?r] 'restore-window-configuration)
		;;   ([?s] 'save-window-configuration)
		;;   ([?c] 'clear-window-configurations))
	  ;; recursive edit
	  ;; (prefix [?r]
		;;   ([?r] 'recursive-edit)
		;;   ([?q] 'exit-recursive-edit )
		;;   ([escape] 'top-level ))
    
  ;; elisp eval
  (prefix [escape]
	  ;;([f6]  (fi () (pp-eval-last-sexp 1)))
	  ([escape]  'pp-eval-last-sexp)
	  ([?e] 'eval-expression)
	  ([?r] 'eval-region)
	  ([?q] 'icicle-execute-extended-command)
	  ([?t] (fi () (insert (format-time-string "(%Y %m %d)" (current-time)))))
	  )
  ;; keyboard macro
  (prefix [f9]
	  ([f9] (fn (&optional n) (interactive "p")
		    (if n (loop for i to (- n 1) do (with-kbd-undo (call-last-kbd-macro)))
			(with-kbd-undo (call-last-kbd-macro)))))
	  ([?s] 'start-kbd-macro)
	  ([?q] 'kbd-macro-query)
	  ([?p] (fi ((str "MString to insert: "))
		    (insert str)))
	  ([?e] 'end-kbd-macro)
	  ([?n] 'name-last-kbd-macro)
	  ;; maybe insert to a kbd-macro-file
	  ([?i] 'insert-kbd-macro)
	  ([return] 'icicle-kmacro)
	  ([?c] 'select-named-macro)
	  ([?r] 'apply-macro-to-region-lines))
  ;; file
  (prefix my-f12-key
	  ([?\C-f] 'ido-find-file)
	  ([?f] 'icicle-find-file)
	  (my-f12-key 'save-buffer)
	  ;;([?r] 'rename-file-and-buffer)
    ([?r] 'my-rename-buffer-file)
    ([?b] 'rename-buffer)
		([?q] 'kill-this-buffer)
	  ([?m] 'move-buffer-file)
	  ([?s] 'server-edit)
	  ([?c] 'my-count-chars-words-lines)
	  ([?d] 'dired)
	  ([?\C-d] 'find-grep-dired)
	  )
    ;; mode switch
  (prefix my-f11-key
	  (prefix [?l]
		  ([?l] (fi () (auto-fill-mode 0) (longlines-mode)))
		  ([?f] (fi () (longlines-mode 0) (auto-fill-mode))))
	  ;;([?m] my-minor-mode)
	  ;;([?p] 'predictive-mode)
	  ([?i] 'icicle-mode)
    ([?t] 'my-set-tab-width)
    ([?f] 'my-set-mac-font)
	  ;; ([?s] 'flyspell-mode)
	  ([?m] 'menu-bar-mode)
	  ([?o] 'outline-minor-mode)
    ([?s] 'show-ws-toggle-show-trailing-whitespace)
    )
  ;; search map
  (prefix [?\C-f]
	  ([?\C-j] 'isearch-backward)
	  ([?\C-l] 'isearch-forward)
	  (prefix [?\C-r]
		  ([?\C-j] 're-search-backward)
		  ([?\C-l] 're-search-forward))
	  ([?b] 'icicle-search-buffer)
	  ([?f] 'icicle-search-file)
	  ([?r] 'icicle-search-region)
	  ([?\C-o] 'icicle-occur)
	  ([?\C-s] 'icicle-search)
	  ([?\C-g] 'goto-line)
	  )
  ;; bookmark
  (prefix "\C-p"
	  ("\C-s" 'bookmark-set)
	  ("\C-p" (fi ()
		      (bookmark-jump (ido-bookmark-completing-read "Bookmark Jump: "))))
	  ("p" (fi ()
		   (bookmark-jump-other-window (ido-bookmark-completing-read "Bookmark Jump: "))))
	  ("d" (fi ()
		   (bookmark-delete (ido-bookmark-completing-read "Delete Bookmark: "))))
	  ("\C-l" 'bookmark-bmenu-list))
  )

;; (event-modifiers (aref (kbd "C-M-r") 0))
;; (event-basic-type (aref (kbd "C-M-r") 0) )
;; (event-modifiers (aref (kbd "M-r") 0))

(my-defkeys isearch-mode-map
	    ((kbd "<tab>") 'icicle-isearch-complete)
  ;; ((kbd "C-r") 'isearch-toggle-regexp)
  ((kbd "C-r") 'isearch-query-replace)
  ;; ;; why the fuck would the following cause problem?? It says C-M-r doesn't start with prefix key.
  ;; ((kbd "C-M-r") 'isearch-query-replace)
  ((kbd "C-e") 'isearch-edit-string)
  ((kbd "C-a") 'isearch-repeat-backward)
  ((kbd "C-s") 'isearch-repeat-forward)
  ((kbd "C->") 'isearch-ring-retreat)
  ((kbd "C-k") 'isearch-ring-advance)
  ((kbd "C-l") 'isearch-constraint-forward)
  ((kbd "C-j") 'isearch-constraint-backward)
  )

(setq parens-require-spaces nil)

(my-defkeys global-map
  ("[" 'insert-pair)
  ("(" 'insert-pair)
  ("{" 'insert-pair)
  ;; ("<" 'insert-pair)
  ("\"" 'insert-pair))


;; (lexical-let ((my-mode-map (make-sparse-keymap)))
;;   (my-defkeys my-mode-map
;;     ("(" 'self-insert-command)
;;     (")" 'self-insert-command)
;;     ("[" 'self-insert-command)
;;     ("\"" 'self-insert-command)
;;     ("'" 'self-insert-command)
;;     ("<" 'self-insert-command)
;;     ("{" 'self-insert-command))
;;   (define-minor-mode my-minor-mode
;;       "no electric pairs"
;;     nil
;;     "my"
;;     my-mode-map
;;     ))

;; navigation
(my-defkeys global-map
  ;; ;; left
  ;;((kbd "C-j") 'backward-word)
  ((kbd "C-j") 'my-backward-word)
  ;; ((kbd "M-j") 'isearch-backward)
  ((kbd "M-j") 'backward-char)
  ((kbd "C-M-j") 'backward-sexp)
  ((kbd "s-j") 'my-first-char-of-line)
  ;; ;; right
  ;;((kbd "C-l") 'forward-word)
  ((kbd "C-l") 'my-forward-word)
  ;; ((kbd "M-l") 'isearch-forward)
  ((kbd "M-l") 'forward-char)
  ((kbd "C-M-l") 'forward-sexp)
  ((kbd "s-l") 'my-last-line-or-indent-code-rigidly)
  ;; ;; up
  ([?\C->] 'previous-line)
  ((kbd "M-i") 'backward-paragraph)
  ;; ;; down
  ((kbd "C-k") 'next-line)
  ((kbd "M-k") 'forward-paragraph)
  ;; ;; scroll
  ((kbd "s-k") 'my-View-scroll-line-forward)
  ((kbd "s-i") 'my-View-scroll-line-backward)

  ((kbd "M-s-i") 'scroll-down)
  ((kbd "M-s-k") 'scroll-up)

  ;;   ((kbd "C-(") 'my-previous-global-mark)
  ;;   ((kbd "C-)") 'my-next-global-mark)

  ((kbd "M-(") 'my-delete-matching-parens)
  ((kbd "C-(") 'my-wrap-region-in-parens)

  ((kbd "M-s-l") 'kill-line)

  ((kbd "M-`") nil)
  )

;; line kill & yanks
(defkeys global-map
		(prefix (kbd "C-v")
						((kbd "C-v") 'my-kill-whole-line)
						((kbd "C-d") 'my-save-whole-line)
						((kbd "C-l") 'kill-line)
						((kbd "C-j") (fi () (kill-line -1)))))


(defkeys global-map
		((kbd "M-g") 'goto-line)
	((kbd "C-b") 'fill-paragraph)
	;;((kbd "C-b") 'recenter)
  ;; misc top-level keys
  ((kbd "C-`") 'universal-argument)
  ((kbd "C-z") 'keyboard-quit)
  ((kbd "C-g") 'keyboard-quit)
  ([ctrl ?q] 'quoted-insert)
  ("\C-a" 'append-to-buffer)
  ("\M-a" 'prepend-to-buffer)
  ((kbd "M-/") 'repeat)

  ;; etags
  ((kbd "C-.") (fi () (find-tag (find-tag-default))))
  ((kbd "C-,") 'pop-tag-mark)

  ;; indentation
  ([?\r] 'newline-and-indent)
  ((kbd "M-<return>") 'my-delete-indentation)
  ;; open line below
  ((kbd "s-<return>") 'my-open-line-below)
  ;; open line above
  ((kbd "M-s-<return>") 'my-open-line-above)
  ;;((kbd "C-<return>") (fi () (end-of-line) (newline) (indent-according-to-mode)))
  ;;((kbd "s-<return>") (fi () (newline) (newline) (indent-according-to-mode) (previous-line) (indent-according-to-mode)))
  ("\C-\\" 'indent-region)
  ((kbd "s-\\") 'my-align-regexp)

  ;; comment
  ((kbd "C-;") 'comment-region)
  ((kbd "s-;") 'my-insert-comment)
  ((kbd "M-;") 'uncomment-region)
  ("\C-s" 'yank)
  ;;("\C-s" 'clipboard-yank)
  ("\M-s" 'yank-pop)

  ;; kill
  ;; ;; forward
  ((kbd "<delete>") 'delete-char)
  ((kbd "C-u") 'my-delete-word)
  ((kbd "M-u") 'delete-char)
  ((kbd "C-M-u") 'kill-sexp)
  ((kbd "s-u") 'my-save-sexp)
  ((kbd "s-w") 'my-kill-whole-line)
  ;; ;; backward
  ((kbd "<backspace>") 'my-backward-delete-word)
  ((kbd "C-<backspace>") 'backward-delete-char)
  ((kbd "C-n") 'my-backward-delete-word)
  ((kbd "M-n") 'backward-delete-char)
  ((kbd "C-M-n") 'backward-kill-sexp)
  ((kbd "s-n") 'my-backward-save-sexp)

  ;; buffer management
  ((kbd "C-o") 'switch-to-buffer)
  ((kbd "M-o") 'switch-to-buffer-other-window)
  ((kbd "C-M-o") 'other-window)
  ((kbd "s-o") 'ee-buffers)
  ;;((kbd "C-y") 'kill-buffer)
  ;;((kbd "M-y") 'kill-some-buffers)
  ;;((kbd "C-M-y") 'kill-buffer-and-window)
  ;; ((kbd "C-M->") 'balance-windows)

  ;; register
  ((kbd "C-e") 'jump-to-register)
  ((kbd "M-e") 'point-to-register)
  ;;          ((kbd "C-q") 'bookmark-jump)
  ;;          ((kbd "M-q") 'bookmark-set)
  ;;          ((kbd "C-M-q") 'bookmark-save)
  ;;          ((kbd "s-q") 'list-bookmarks)

  ;;((kbd "<tab>") 'my-indent-or-complete)
  ;;((kbd "C-<tab>") 'indent-for-tab-command)

  ((kbd "C-t") 'transpose-lines)

  ((kbd "M-v") nil)

  ((kbd "<prior>") 'pop-global-mark)
  )

;; mode specific maps

(require 'help-mode)
(defkeys (help-mode-map)
  ((kbd "<next>") 'help-go-forward)
  ((kbd "<prior>") 'help-go-back))


;; (defkeys view-mode-map
;;   ((kbd "C-j") nil))

;; bunch of minibuffer maps
;; minibuffer-local-completion-map
;; minibuffer-local-filename-completion-map
;; minibuffer-local-isearch-map
;; minibuffer-local-map
;; minibuffer-local-must-match-filename-map
;; minibuffer-local-must-match-map
;; minibuffer-local-ns-map

(defkeys (minibuffer-local-filename-completion-map)
  ((kbd "C-j") nil)
	((kbd "C-l") nil))
