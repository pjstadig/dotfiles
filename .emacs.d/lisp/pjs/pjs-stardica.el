;;; pjs-stardica.el --- Stardica code and config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'pjs)
(setq pjs-inhibit-clojure-align-on-save 't)
(setq pjs-inhibit-cleanup-on-save 't)
(setq default-directory "~/")

(require 'org)
(require 'projectile)
(setq projectile-project-root-files nil)

(add-to-list 'org-agenda-files "~/clubhouse/clubhouse.org" t)

(remove-hook 'prog-mode-hook 'whitespace-mode)

(use-package clubhouse-backend
  :load-path "~/src/backend/elisp"
  :hook (clojure-mode . clubhouse-backend-font-lock)
  :bind (("C-c C-r" . clubhouse-backend-goto-defresource)))

(provide 'pjs-stardica)
;;; pjs-stardica.el ends here
