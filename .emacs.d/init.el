(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Set fonts
(set-face-attribute 'default nil :family "Fira Mono")
(set-face-attribute 'fixed-pitch nil :family "Fira Mono")
(set-face-attribute 'variable-pitch nil :family "Fira Sans" :weight 'light)

(require 'package)
(add-to-list 'package-archives
             (cons "melpa-stable" "https://stable.melpa.org/packages/")
             :append)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package gnu-elpa-keyring-update
  :ensure t
  :config (gnu-elpa-keyring-update))
(use-package better-defaults
  :ensure t)
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))
(use-package pinentry
  :ensure t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))

(setq my/lib-dir (concat user-emacs-directory "lib"))

(defun my/recompile-libs ()
  (interactive)
  (dolist (f (directory-files my/lib-dir t "\\.el$"))
    (byte-compile-file f)))

(add-to-list 'load-path my/lib-dir)
(dolist (f (directory-files my/lib-dir nil "\\.el$"))
  (load (file-name-sans-extension f)))
(put 'narrow-to-region 'disabled nil)
