
(my-defkeys dired-mode-map
  ((kbd "C-o") nil)
  ((kbd "C-<return>") 'dired-find-file-other-window)
  ((kbd "C-j") 'dired-prev-dirline)
  ((kbd "C-l") 'dired-next-dirline)
  ((kbd "M-i") 'dired-tree-up)
  ((kbd "M-k") 'dired-tree-down)
  ((kbd "M-l") 'dired-next-subdir)
  ((kbd "M-j") 'dired-prev-subdir)
  ((kbd "s-j") 'dired-prev-marked-file)
  ((kbd "s-l") 'dired-next-marked-file)
  )

