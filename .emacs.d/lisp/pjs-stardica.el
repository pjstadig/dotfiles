;;; pjs-stardica.el --- Stardica code and config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'pjs)
(require 'projectile)

(setq default-directory "~/"
      pjs-inhibit-clojure-align-on-save 't
      pjs-inhibit-cleanup-on-save 't
      projectile-project-root-files nil)

(remove-hook 'prog-mode-hook 'whitespace-mode)

(use-package clubhouse-backend
  :load-path "~/src/backend/elisp"
  :if (file-exists-p "~/src/backend/elisp")
  :init
  (unbind-key "s-c")
  :hook (clojure-mode . clubhouse-backend-font-lock)
  :bind (("s-c r" . clubhouse-backend-goto-defresource)
         ("s-c j" . clubhouse-backend-jack-in-dev-system)
         ("s-c a" . clubhouse-backend-insert-co-authored-by)))

(provide 'pjs-stardica)
;;; pjs-stardica.el ends here
