(use-package cider
  :ensure t)
(use-package paredit
  :ensure t)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(setq linum-format "%d ")
(add-hook 'prog-mode-hook 'linum-mode)
