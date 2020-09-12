(defun pjs-add-eval-buffer-binding ()
  (local-set-key (kbd "C-c C-k") 'eval-buffer)
  (setq fill-column 80))

(provide 'pjs-emacs-lisp)
