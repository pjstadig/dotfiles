;;; pjs-reset.el --- Reset after changing configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; I use exwm, which means I have a long running Emacs instance, but I don't
;; want to load update configuration into Emacs.
;;
;; `pjs-reset' will retangle, recompile, and reload all my Emacs Lisp.
;;
;;; Code:
(require 'pjs-system)
(require 'pjs-exwm)

(defun pjs-reset ()
  "Retangle, recompile and reload all Emacs Lisp.

If EXWM is configured, also reset that."
  (interactive)
  (org-babel-tangle-file (concat user-emacs-directory "init.org"))
  (byte-recompile-file (concat user-emacs-directory "init.el") nil 0)
  (when (file-exists-p pjs-system-file)
    (byte-recompile-file pjs-system-file nil 0))
  (byte-recompile-directory (concat user-emacs-directory "elpa") 0)
  (byte-recompile-directory (concat user-emacs-directory "lisp") 0)
  (load (concat user-emacs-directory "init"))
  (pjs-load-system-file)
  (when pjs-exwm-configured-p
    (pjs-configure-exwm)
    (exwm-reset)))

(provide 'pjs-reset)
;;; pjs-reset.el ends here
