(use-package visual-fill-column
  :ensure t)
(use-package writegood-mode
  :ensure t)

(defun my/configure-text-mode-fill-column ()
  (setq fill-column 80))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'writegood-mode)
(add-hook 'text-mode-hook 'my/configure-text-mode-fill-column)

(add-hook 'visual-fill-column-mode-hook 'visual-line-mode)
(setq split-window-preferred-function 'visual-fill-column-split-window-sensibly)
(advice-add 'text-scale-adjust :after 'visual-fill-column-adjust)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)

(with-eval-after-load 'org
  (set-face-attribute 'org-checkbox nil :family "Source Code Pro" :weight 'bold)
  (set-face-attribute 'org-table nil :family "Source Code Pro")
  (require 'ob-shell))