(defun my/cleanup-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'my/cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'my/cleanup-buffer)
