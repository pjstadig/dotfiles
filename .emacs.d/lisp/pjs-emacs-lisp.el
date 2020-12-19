;;; pjs-emacs-lisp.el --- Emacs lisp configuration and utilities -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(defun pjs-add-eval-buffer-binding ()
  (local-set-key (kbd "C-c C-k") 'eval-buffer)
  (setq fill-column 80))

(provide 'pjs-emacs-lisp)
;;; pjs-emacs-lisp.el ends here
