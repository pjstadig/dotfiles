(use-package flycheck-clj-kondo
  :ensure t)
(use-package clojure-mode
  :ensure t
  :config (require 'flycheck-clj-kondo))
(use-package cider
  :ensure t)
(use-package company
  :ensure t)
(use-package paredit
  :ensure t)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'eldoc-mode)

(use-package helm
  :ensure t
  :bind (("C-c i" . helm-imenu)
         ("C-c C-i" . helm-imenu)
         ("M-x" . helm-M-x)))

(use-package helm-ag
  :ensure t
  :bind ("C-c a" . helm-ag))

(use-package helm-projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
