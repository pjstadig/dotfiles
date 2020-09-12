;;; pjs-system.el --- System local configuration -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; I sometimes need to configure Emacs differently for different systems (e.g. home versus
;; work).  This gives me a hook to do that.
;;
;; Emacs will set pjs-system-file to a file path named for the current system, and
;; pjs-load-system-file will load the system file if it exists.
;;
;; I must call pjs-load-system-file at the end of my init.el, and it will load system
;; specific tweaks.
;;
;;; Code:
(defvar pjs-system-file
  (expand-file-name (concat user-emacs-directory (system-name) ".el")))

(defun pjs-load-system-file ()
  (when (file-exists-p pjs-system-file)
    (load pjs-system-file)))

(provide 'pjs-system)
;;; pjs-system.el ends here
