(defvar my-outline-keymap (make-sparse-keymap))

(my-defkeys my-outline-keymap
            ((kbd "M-d") 'outline-cycle)
            ((kbd "M-a") 'show-all)
            ((kbd "M-h") 'hide-sublevels)
            ((kbd "M-b") 'show-branches)
            )

(my-defkeys outline-minor-mode-map
            ((kbd "M-d") my-outline-keymap))

