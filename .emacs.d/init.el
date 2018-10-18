;; Set fonts
(set-face-attribute 'default nil :family "Source Code Pro" :height 130)
(set-face-attribute 'fixed-pitch nil :family "Source Code Pro")
(set-face-attribute 'variable-pitch nil :family "Source Serif Pro" :height 140)

(require 'package)
(add-to-list 'package-archives (cons "melpa-stable" "https://stable.melpa.org/packages/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(use-package better-defaults
  :ensure t)
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(setq my/lib-dir (concat user-emacs-directory "lib"))

(defun my/recompile-libs ()
  (interactive)
  (dolist (f (directory-files my/lib-dir t "\\.el$"))
    (byte-compile-file f)))

(add-to-list 'load-path my/lib-dir)
(dolist (f (directory-files my/lib-dir nil "\\.el$"))
  (load (file-name-sans-extension f)))
