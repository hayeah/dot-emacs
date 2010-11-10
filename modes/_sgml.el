
(my-defkeys sgml-mode-map
  ((kbd "C-M-l") 'sgml-skip-tag-forward)
  ((kbd "C-M-j") 'sgml-skip-tag-backward)
  ((kbd "C-<return>") 'sgml-close-tag))

(add-to-list 'auto-mode-alist '("\\.dryml$" . html-mode))