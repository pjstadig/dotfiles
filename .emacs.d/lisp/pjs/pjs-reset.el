;;; pjs-reset.el --- Reset after changing configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'pjs-system)
(require 'pjs-exwm)

(defun pjs-reset ()
  "Recompile and reload all Emacs Lisp.  If EXWM is configured, also reset that."
  (interactive)
  (byte-recompile-file (concat user-emacs-directory "init.el") nil 0)
  (when (file-exists-p pjs-system-file)
    (byte-recompile-file pjs-system-file nil 0))
  (byte-recompile-directory (concat user-emacs-directory "elpa") 0)
  (byte-recompile-directory (concat user-emacs-directory "lisp") 0)
  (load (concat user-emacs-directory "init.el"))
  (pjs-load-system-file)
  (when pjs-exwm-configured-p
    (pjs-configure-exwm)
    (exwm-reset)))

(provide 'pjs-reset)
;;; pjs-reset.el ends here
