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
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x))
  :config
  (helm-mode))

(use-package helm-ag
  :ensure t)

(use-package helm-projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(defun my/sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))
