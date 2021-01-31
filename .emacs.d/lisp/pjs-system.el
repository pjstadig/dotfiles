;;; pjs-system.el --- System local configuration -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;;
;;; I sometimes need to configure Emacs differently for different systems
;;; (e.g. home versus work).  The system specific config should run last.
;;;
;;; This creates a pjs-load-system-file function and adds it as an
;;; after-init-hook.  The system file will be 'pjs-(system-name).el' and it
;;; should be in the 'pjs' module.
;;;
;;; Code:
(defun pjs-load-system-file ()
  (load (concat "pjs-" (system-name) ".el") t))

(add-hook 'after-init-hook #'pjs-load-system-file 100)

(provide 'pjs-system)
;;; pjs-system.el ends here
