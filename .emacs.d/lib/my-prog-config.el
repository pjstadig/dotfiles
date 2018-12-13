(use-package cider
  :ensure t)
(use-package paredit
  :ensure t)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(setq linum-format "%d ")
(add-hook 'prog-mode-hook 'linum-mode)

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
