(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(defvar pjs-local-file
  (expand-file-name (concat user-emacs-directory "local.el")))

(when (file-exists-p pjs-local-file)
  (load pjs-local-file))

(defun pjs-add-eval-buffer-binding ()
  (local-set-key (kbd "C-c C-k") 'eval-buffer))

(add-hook 'emacs-lisp-mode-hook 'pjs-add-eval-buffer-binding)

(require 'package)
(add-to-list 'package-archives
             (cons "melpa-stable" "https://stable.melpa.org/packages/")
             :append)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package gnu-elpa-keyring-update
  :ensure t
  :config (gnu-elpa-keyring-update))
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))
(use-package pinentry
  :ensure t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
(use-package deft
  :ensure t
  :bind ("C-c d" . deft))
(use-package exwm
  :ensure t)
(use-package pjs
  :load-path "lisp/pjs")

(when (not (eq (server-running-p) 't))
  (server-start))
