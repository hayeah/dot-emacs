(defun my-indent-or-complete (arg)
    "Complete if point is at end of a word, otherwise indent line."
    (interactive "*P")
    (if (looking-at "\\>")
        (dabbrev-expand arg)
        (indent-for-tab-command)))

(defmacro* region-points ((a b) &rest body)
  `(let ((,a (point))
	 (,b (if (mark) (mark)
		 (point))))
     (when (< ,b ,a)
       (psetf ,a ,b
	      ,b ,a))
     ,@body))


(defun* my-completing-read (prompt collection
				   &key
				   predicate
				   require-match
				   initial
				   hist
				   default
				   inherit-input-method)
  (completing-read
   prompt collection predicate
   require-match initial hist default inherit-input-method))


(defun my-insert-comment ()
  (interactive)
  (if comment-start
      (insert comment-start)
      (message "no comment syntax defined")))

(defun my-etags-add-buffer ()
  (interactive)
  (let ((curfile (buffer-file-name))
	(tmp-name (concat "/tmp/TAG-" (mapconcat 'int-to-string (current-time) "-"))))
    (unless
	(and curfile
	     (= 0 (shell-command (concat "etags -o " tmp-name " " curfile)))
	     (visit-tags-table tmp-name))
      (message "Unable to add etag."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard Macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-named-macro () 
  (intern
   (my-completing-read
    "Call named macro: "
    (loop for s across obarray
       if (and (commandp s) (arrayp (symbol-function s)))
       collect (symbol-name s))
    :hist 'icicle-kmacro-history)))

(defun select-named-macro (prefix-arg)
  "Set a named kbd macro as the last-kbd-macro, then calls it
   (unless prefix-arg is negative) "
  (interactive "p") 
  (let ((pa prefix-arg))
    ;; for some stupid reason, prefix-arg is reset to nil by read-named-macro.
    (setq last-kbd-macro (symbol-function (read-named-macro))) 
    (when (> pa 0) (call-last-kbd-macro))
    ))

(defmacro with-kbd-undo (&rest body)
  "For keyboard macros, consider the whole macro one atomic undo action."
  `(setq buffer-undo-list
	 (nconc (let ((buffer-undo-list nil))
		  ,@body
		  (remove nil buffer-undo-list))
		buffer-undo-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun window-pixel-dimension (&optional window)
    (multiple-value-bind (left top right bottom)
	(window-inside-pixel-edges window)
      (let ((w (- right left))
	    (h (- bottom top)))
	(list w h))))

(defun split-window-smartly (&optional window)
  (interactive)
  (multiple-value-bind (w h) (window-pixel-dimension)
    (if (> w h)
	(split-window-horizontally)
	(split-window-vertically))))

;;(setq display-buffer-function 'my-display-buffer-function)

;; (defun my-display-buffer-function (buffer not-this) 
;;   (cond ((and (not not-this) (get-buffer-window buffer))
;; 	 (select-window (get-buffer-window buffer)))
;; 	((and (equal (minibuffer-window) (selected-window))
;; 	      (member (buffer-name buffer) icicle-buffer-names))
;; 	 (let ((display-buffer-function nil))
;; 	   (display-buffer buffer not-this))) 
;; 	(t (progn (split-window-smartly)
;; 		  (select-window (next-window)) 
;; 		  (set-window-buffer (selected-window) buffer)
;; 		  ;; display-buffer expects the selected window be returned.
;; 		  ;; else emacs crashes mysteriously
;; 		  (selected-window)
;; 		  ))))


(defun mode-line-window-index ()
    "Find the position in the window-tree of the window associated with the mode line."
    (concat "["
     (number-to-string
      (+ 1 (position (get-buffer-window (format-mode-line "%b"))
		     (flatten-window-tree)))) "]"))

(defun flatten-window-tree ()
  (let (acc) 
    (labels ((rec (tr)
	       (cond ((windowp tr)
		      (push tr acc))
		     (t (loop for k in (nthcdr 2 tr)
			   do (rec k))))))
      (rec (car (window-tree)))
      (nreverse acc))))


(defun get-window-by-index (n &optional window-tree)
  (nth (- n 1) (aif window-tree it
		    (flatten-window-tree))))

(defun select-window-by-index (n)
  (interactive "NDelete Window Index:")
  "Select a window by index (1-based)."
  (awhen (get-window-by-index n)
	 (select-window it)))

(defun delete-window-by-index (n)
  (interactive "NDelete Window Index:")
  (awhen (get-window-by-index n)
	 (delete-window it)))

(defun swap-windows-by-index (n1 n2)
  (interactive "nSwap Window: \nnWith: ")
  (let* ((tr (flatten-window-tree))
	 (w1 (get-window-by-index n1 tr))
	 (w2 (get-window-by-index n2 tr))
	 (b1 (window-buffer w1))
	 (b2 (window-buffer w2)))
    (set-window-buffer w1 b2)
    (set-window-buffer w2 b1)))

;; TODO I want to have default switching to the side.
;; display-buffer-function

;; (setq display-buffer-function
;;       '(lambda (buf cur-window-ok)
;; 	(if cur-window-ok
;; 	    (switch-to-buffer buf)
;; 	    )))


(defvar saved-window-configurations '()) 

(defun save-window-configuration () 
  (interactive)
  (let* ((name (completing-read "Save window configuration as: "
				saved-window-configurations))
	 (config (assoc name saved-window-configurations))) 
    (if config
	(setf (cdr config) (current-window-configuration))
	(push (cons name (current-window-configuration))
	      saved-window-configurations))
    (message "window configuration saved as %s" name))) 

(defun restore-window-configuration ()
  (interactive)
  (let* ((name (completing-read "Restore window configuration: "
				saved-window-configurations))
	 (wins (assoc name saved-window-configurations)))
    (if wins
	(set-window-configuration (cdr wins))
	(message "No window configuration saved as %s" name))))

(defun clear-window-configurations ()
  (interactive)
  (setq saved-window-configurations '())
  (message "window configurations cleared")) 

(defvar pinned-buffers '())

(defun pin-buffer (buf)
  "pin a buffer so it doesn't rotate"
  (interactive "bSelect a buffer to pin:")
  (pushnew (get-buffer buf) pinned-buffers))
(defun unpin-buffer (buf)
  "unpin a buffer so it can rotate again"
  (interactive "bSelect a buffer to unpin:")
  (setq pinned-buffers (remove (get-buffer buf) pinned-buffers)))
(defun clear-pinned-buffers ()
  "clear all pinned buffers"
  (interactive)
  (setq pinned-buffers '()))


(defun rotate-windows (fn)
  "Rotate windows in some direction."
  (let* ((w-list (set-difference (window-list)
				 (mapcar (lambda (buf) (get-buffer-window buf))
					 pinned-buffers)))
	 (b-list (funcall fn
		  (mapcar 'window-buffer w-list)))
	 (s-list (funcall fn
		  (mapcar 'window-start w-list))))
    (mapc* (lambda (w b s)
	     (set-window-buffer w b)
	     (set-window-start w s))
	   w-list b-list s-list)))

(defun rotate-windows-down ()
  "Window 1 => Window 2 => Window 3 ... => Window N => Window 1"
  (interactive)
  (rotate-windows 'rotate-list-right))

(defun rotate-windows-up ()
  "Window 1 <= Window 2 <= Window 3 ... <= Window <= Window 1"
  (interactive)
  (rotate-windows 'rotate-list-left))

(defvar my-using-camelcase nil)
(defvar my-word-separator-regexp "[^[:alnum:]]")
(defvar my-word-separator-regexp "[^[:alnum:]]")


(defun my-use-camelcase ()
  (interactive)
  (make-local-variable 'my-using-camelcase)
  (setq my-using-camelcase t))

(defun my-prev-word ()
  (save-excursion
    (if my-using-camelcase
        (my-prev-camelword)
        (if (re-search-backward "[^[:alnum:]]" nil t)
            (+ 1 (point))
            (point-min)))))

(defun my-next-word ()
  (save-excursion
    (if my-using-camelcase
        (my-next-camelword)
        (if (re-search-forward "[^[:alnum:]]" nil t)
            (- (point) 1)
            (point-max)))))

(defun my-prev-camelword ()
  (let ((cur (point))
        (prev (my-beg-of-camelword)))
    (cond
      ;; punctuation
      ((null prev) (backward-char)
       (if (looking-at "[[:alnum:]]") (my-beg-of-camelword)))
      ;; already at beginning of cameword
      ((= cur prev) (backward-char) (my-beg-of-camelword))
      ;; done
      )
    (point)))

(defun my-next-camelword ()
  (my-beg-of-camelword)
  (forward-char)
  (point))

;; ^ (starting point)
;; * (examining point (the context to determine what to do))
;; _ (want to arrive here)
(defun my-beg-of-camelword ()
  (interactive)
  (let* ((case-fold-search nil)
         (orig (point))
         (casing (cond
                   ((looking-at "[A-Z]") 'upper)
                   ((looking-at "[a-z0-9]") 'lower)
                   (t nil))))
    (case casing
      (upper
       (cond
         ;; already at beginning
         ;; Aa
         ;; ^
         ((looking-at "[A-Z][a-z0-9]") 'done)
         (t (backward-char)
            (cond
              ;; ,A
              ;; *^
              ;; aA
              ;; ^*
              ;;  _
              ((looking-at "[^A-Z]") (forward-char))
              ;; aAAAAA
              ;; ,AAAAA
              ;; *_   ^
              (t (re-search-backward "[^A-Z]")
                 (forward-char)))))
       (point))
      (lower
       (re-search-backward "\\([A-Z]\\|[^[:alnum:]]\\)")
       (unless (looking-at "[A-Z]") (forward-char))
       (point))
      (t nil))))

(defun my-beg-of-camelwordp ()
  (save-excursion
    (let ((orig (point)))
      (= (my-beg-of-camelword) orig))))

(defun my-end-of-camelword ()
  (interactive)
  (let* ((case-fold-search nil)
         (orig (point))
         (casing (cond
                   ((looking-at "[A-Z]") 'upper)
                   ((looking-at "[a-z0-9]") 'lower)
                   (t nil))))
    (case casing
      (upper
       (cond
         ((my-beg-of-camelwordp)
          (forward-char)
          (if (null (my-end-of-camelword))
              ;; A,
              ;; ^*
              ;; _
              (backward-char)
              ;; Aaaa
              ;;  ^ *
              ;;    _
              'ok))
         (t (re-search-forward "\\([a-z0-9]\\|[^[:alnum:]]\\)")
                (backward-char)
                (cond
                  ;; AAAA,
                  ;; ^  _*
                  ((looking-at "[^[:alnum:]]") (backward-char))
                  ;; AAAABa
                  ;; ^  _ *
                  (t (backward-char 2)))))
       (point))
      (lower
       ;; aaaa,
       ;; ^  _*
       ;; aaaaB
       ;; ^  _*
       (re-search-forward "\\([A-Z]\\|[^[:alnum:]]\\)")
       (backward-char)
       (backward-char)
       (point))
      (t nil))))

  
(defun my-delete-word ()
  (interactive)
  (let* ((cur-point (point))
	 (kill-point (my-next-word))
	 (count (- kill-point cur-point))
	 (count (if (= count 0) 1 count)))
    (delete-char count)))

(defun my-backward-delete-word ()
  (interactive)
  (let* ((cur-point (point))
	 (kill-point (my-prev-word))
	 (count (- cur-point kill-point 0))
	 (count (if (= count 0) 1 count )))
    (delete-backward-char count)))

(defun my-backward-word ()
  (interactive)
  (let ((to (my-prev-word)))
    (if (= (point) to)
	(backward-char)
      (goto-char to))))

(defun my-forward-word ()
  (interactive)
  (let ((to (my-next-word)))
    (if (= (point) to)
	(forward-char)
      (goto-char to))))

(defun my-backward-save-sexp ()
  "Save the previous sexp onto kill ring"
  (interactive)
  (let* ((from (save-excursion (backward-list)))
	 (to (point)))
    (copy-region-as-kill from to)))

(defun my-save-sexp ()
  "Save the following sexp onto kill ring"
  (interactive)
  (let* ((to (save-excursion (forward-list)))
	 (from (point)))
    (copy-region-as-kill from to)))

(defun my-View-scroll-line-forward ()
  (interactive)
  (let ((p (point)))
    (loop repeat (min (/ (window-height) 2) 8)
       do (View-scroll-line-forward))
    (goto-char p)))

(defun my-View-scroll-line-backward ()
  (interactive)
  (let ((p (point)))
    (loop repeat (min (/ (window-height) 2) 8)
       do (View-scroll-line-backward)) 
    (goto-char p)))

(defun my-first-char-of-line ()
  (interactive)
  (beginning-of-line)
  (goto-char (re-search-forward "[[:blank:]]*"))
  (point))

(defun my-last-char-of-line ()
  (interactive)
  (end-of-line)
	(my-delete-trailing-spaces)
  (point))


;; (defun my-replace-string-sexp (regexp to-string)
;;   (interactive "MRegexp: \nMTo-string: ")
;;   (let ((from (point))
;; 	(to (forward-list)))
;;     (goto-char from)
;;     (save-restriction 
;;       (narrow-to-region from to) 
;;       (replace-regexp regexp to-string))
;;     (list from to)))

(defun my-replace-string-sexp (from-string to-string)
  (interactive "Min sexp From-string: \nMTo-string: ") 
  (let ((from (point))
	(to (forward-list)))
    (goto-char from)
    (save-restriction 
      (narrow-to-region from to) 
      (replace-string from-string to-string))))


(defun my-insert-pair (str)
  (interactive)
  (insert str) 
  (backward-char))

;; (defun rename-file-and-buffer (new-name)
;;   "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "FNew name: ") 
;;   (let ((name (buffer-name))
;;         (filename (buffer-file-name)))
;;     (if (not filename)
;;         (message "Buffer '%s' is not visiting a file!" name)
;;       (if (get-buffer new-name)
;; 	  (message "A buffer named '%s' already exists!" new-name)
;; 	(progn (rename-file name new-name 1)
;; 	       (rename-buffer new-name)
;; 	       (set-visited-file-name new-name)
;; 	       (set-buffer-modified-p nil))))))

(defun my-rename-buffer-file (target)
  (interactive"FRename To: ")
  (let ((old-buf-name (buffer-name)))
    (rename-file (buffer-file-name) target)
    (set-visited-file-name target)
    (rename-buffer old-buf-name)
    (set-buffer-modified-p nil)))

(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir (if (string-match dir "\\(?:/\\|\\\\)$")
		  (substring dir 0 -1)
		dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
	     (delete-file filename)
	     (set-visited-file-name newname)
	     (set-buffer-modified-p nil)
	     t))))

(defun syntax-highlight-region (start end)
  "Adds <font> tags into the region that correspond to the
current color of the text.  Throws the result into a temp
buffer, so you don't dork the original."
  (interactive "r")
  (let ((text (buffer-substring start end)))
    (with-output-to-temp-buffer "*html-syntax*"
      (set-buffer standard-output)
      (insert "<pre>")
      (save-excursion (insert text))
      (save-excursion (syntax-html-escape-text))
      (while (not (eobp))
        (let ((plist (text-properties-at (point)))
              (next-change
               (or (next-single-property-change
                    (point) 'face (current-buffer))
                   (point-max))))
          (syntax-add-font-tags (point) next-change)
          (goto-char next-change)))
      (insert "\n</pre>"))))

(defun syntax-add-font-tags (start end)
  "Puts <font> tag around text between START and END."
  (let (face color rgb name r g b)
    (and
     (setq face (get-text-property start 'face))
     (or (if (listp face) (setq face (car face))) t)
     (setq color (face-attribute face :foreground))
     (setq rgb (assoc (downcase color) color-name-rgb-alist))
     (destructuring-bind (name r g b) rgb
       (let ((text (buffer-substring-no-properties start end)))
         (delete-region start end)
         (insert (format "<font color=#%.2x%.2x%.2x>" r g b))
         (insert text)
         (insert "</font>"))))))

(defun syntax-html-escape-text ()
  "HTML-escapes all the text in the current buffer,
starting at (point)."
  (save-excursion (replace-string "<" "&lt;"))
  (save-excursion (replace-string ">" "&gt;"))) 


(defun my-count-words-region (beginning end)
  "Print number of words in the region."
  (interactive "r")
  )

(defun my-count-chars-words-lines (&optional beg end)
  "wc (roughly) in the region." 
  (interactive "r")
  (let (words
	chars
	lines
	(beg (if beg beg (point-min)))
	(end (if end end (point-max)))
	)
    (save-excursion
      (let ((c 0))
	(goto-char beg)
	(while (and (< (point) end)
		    (re-search-forward "\\w+\\W*" end t))
	  (incf c))
	(setq words c)))
    (save-excursion
      (goto-char beg)
      (let ((c 0))
	(while (< (point) end)
	  (forward-char)
	  (incf c))
	(setq chars c)))
    (save-excursion
      (goto-char beg)
      (let ((c 0))
	(while (< (point) end)
	  (forward-line)
	  (incf c))
	(setq lines c)))
    (message "c: %d\nw: %d\nl: %d" chars words lines)
    (list chars words lines)
    ))

(defun my-open-line-below ()
  (interactive)
  (next-line)
  (beginning-of-line)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode)
  )

(defun my-open-line-above ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent)
  )

(defun my-kill-whole-line ()
  (interactive)
  (let ((beg (my-first-char-of-line))
				(end (my-last-char-of-line)))
    (kill-region beg end))
  (delete-indentation)
	(my-delete-trailing-spaces)
	(next-line)
  (indent-according-to-mode)
  (my-first-char-of-line))

(defun my-kill-whole-line ()
  (interactive)
  (kill-whole-line 1)
  (my-first-char-of-line))

(defun my-save-whole-line ()
  (interactive)
  (let ((beg (my-first-char-of-line))
        (end (my-last-char-of-line)))
    (kill-region beg end)
    (yank)))

(defun my-delete-trailing-spaces ()
  "delete trailing spaces for current line"
  (interactive)
  (save-excursion
    (let ((point-1 (progn (end-of-line)
                          (re-search-backward "[^[:blank:]]")
                          (forward-char)
                          (point)))
          (point-2 (progn (or (and (search-forward-regexp "\n" nil t)
                                   (progn (backward-char)
                                          (point)))
                              (point-max)
                              ))))
      (if (not (eql point-1 point-2))
          (delete-region point-1 point-2)))))

(defun my-newline-and-indent ()
  (interactive)
  (newline-and-indent)
  (save-excursion
    (previous-line)
    (my-delete-trailing-spaces)))


(defun my-delete-indentation ()
  (interactive)
  (delete-indentation) ;; kill trailing whitespaces
  (my-delete-trailing-spaces)
  (indent-according-to-mode))


(defun my-buffer-file-name-to-kill-ring ()
  (interactive)
  (aif (buffer-file-name)
       (with-temp-buffer
         (insert it)
         (clipboard-kill-ring-save (point-min) (point-max))
         (message it))
       (message "no file path associated with buffer")))



(defun my-rotate-global-mark (direction)
  "Pop off global mark ring and jump to the top location."
  ;; Remove marks to non-existent buffers.
  (delete-if (fn (m) (not (marker-buffer m)))
             global-mark-ring)
  (or global-mark-ring
      (error "No global mark set"))
  (setq global-mark-ring
        (funcall (ecase direction
                   (left 'rotate-list-left)
                   (right 'rotate-list-right))
                 global-mark-ring))
  (let* ((marker (car global-mark-ring))
         (buffer (marker-buffer marker))
         (position (marker-position marker)))
    (set-buffer buffer)
    (or (and (>= position (point-min))
             (<= position (point-max)))
        (if widen-automatically
            (widen)
            (error "Global mark position is outside accessible part of buffer")))
    (goto-char position)
    (switch-to-buffer buffer)))

(defun my-previous-global-mark ()
  (interactive)
  (my-rotate-global-mark 'left))

(defun my-next-global-mark ()
  (interactive)
  (my-rotate-global-mark 'right))


(defun my-os ()
  ;; strip newline
  (let ((s (shell-command-to-string "uname")))
    (intern (substring s 0 (1- (length s))))))

(defmacro* my-for-os (os &body body)
  (if (equal (my-os) os)
      `(progn ,@body)
      nil))


(defun my-set-tab-width (n)
  (interactive "nTab Width: ")
  (setq tab-width n))


;; stolen from paren.el
(defun my-matching-parens ()
  (let ((oldpos (point))
        (dir (cond ((eq (syntax-class (syntax-after (1- (point)))) 5) -1)
                   ((eq (syntax-class (syntax-after (point)))      4) 1)))
        pos mismatch face)
    ;;
    ;; Find the other end of the sexp.
    (when dir
      (save-excursion
        (save-restriction
          ;; Scan across one sexp within that range.
          ;; Errors or nil mean there is a mismatch.
          (condition-case ()
              (setq pos (scan-sexps (point) dir))
            (error (setq pos t mismatch t)))
          ;; Move back the other way and verify we get back to the
          ;; starting point.  If not, these two parens don't really match.
          ;; Maybe the one at point is escaped and doesn't really count.
          (when (integerp pos)
            (unless (condition-case ()
                        (eq (point) (scan-sexps pos (- dir)))
                      (error nil))
              (setq pos nil)))
          ;; If found a "matching" paren, see if it is the right
          ;; kind of paren to match the one we started at.
          (when (integerp pos)
            (let ((beg (min pos oldpos)) (end (max pos oldpos)))
              (unless (eq (syntax-class (syntax-after beg)) 8)
                (setq mismatch
                      (not (or (eq (char-before end)
                                   ;; This can give nil.
                                   (cdr (syntax-after beg)))
                               (eq (char-after beg)
                                   ;; This can give nil.
                                   (cdr (syntax-after (1- end))))
                               ;; The cdr might hold a new paren-class
                               ;; info rather than a matching-char info,
                               ;; in which case the two CDRs should match.
                               (eq (cdr (syntax-after (1- end)))
                                   (cdr (syntax-after beg))))))))))))
    
    (when (and pos (null mismatch))
      (destructuring-bind (a b)
            (sort (list oldpos pos) '<)
        (cons a (1- b))))))

(defun my-delete-matching-parens ()
  (interactive)
  (save-excursion
    (awhen (my-matching-parens)
           (goto-char (car it))
           (delete-char 1)
           (goto-char (cdr it))
           (backward-delete-char 1))))

(defun my-wrap-region-in-parens ()
  (interactive)
  (region-points (a b)
   (save-excursion
     (goto-char b)
     (insert-char ?\) 1)
     (goto-char a)
     (insert-char ?\( 1))))

(labels ((indent (n)
           (save-excursion
             (let* ((here (point))
                    (rb (region-beginning))
                    (re (region-end))
                    (beg (progn (goto-char rb)
                                (beginning-of-line)
                                (point)))
                    (end (progn (goto-char re)
                                (end-of-line)
                                (point))))
               (indent-code-rigidly beg end n)))))
  (defun* my-last-char-of-line-or-indent-code-rigidly (&optional n-chars)
    (interactive "P")
    (if n-chars
        (indent n-chars)
        (my-last-char-of-line)))
  (defun* my-first-char-of-line-or-indent-code-rigidly (&optional n-chars)
    (interactive "P")
    (if n-chars
        (indent (* -1 n-chars))
        (my-first-char-of-line))))


(defun my-align-regexp (start end regexp)
    "repeat alignment with respect to 
     the given regular expression"
    (interactive "r\nsAlign regexp: ")
    (align-regexp start end 
        (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun my-set-mac-font (name  size)
  (interactive
   (list (completing-read "font-name: "
                          (font-family-list) nil t)
         (read-number "size: " 12)))
  (set-face-attribute 'default nil 
                      :family name
                      :slant  'normal
                      :weight 'extra-light
                      :width  'normal
                      :height (* 10 size)))

(defun my-hide-temp-windows ()
    (interactive)
  (loop for w in (window-list)
     do (let* ((buffer (window-buffer w)))
          (when (eql (aref (buffer-name buffer) 0) ?*)
            (delete-window w)))))
