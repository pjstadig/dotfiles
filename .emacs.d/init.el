(setq custom-file (concat user-emacs-directory "custom.el"))

;;; Setup package
(require 'package)
(add-to-list 'package-archives
             (cons "melpa-stable" "https://stable.melpa.org/packages/")
             :append)
(package-initialize)

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; Configure packages
(use-package cider
  :ensure t
  :custom
  (cider-auto-jump-to-error nil)
  (cider-auto-select-error-buffer nil)
  (cider-auto-select-test-report-buffer nil)
  (cider-debug-prompt 'minibuffer)
  (cider-docview-fill-column 80)
  (cider-eldoc-display-context-dependent-info t)
  (cider-jdk-src-paths '("~/.cache/openjdk-8u192b26/"))
  (cider-preferred-build-tool 'lein)
  (cider-prompt-for-symbol nil)
  (cider-repl-display-help-banner nil)
  (cider-repl-history-file ".cider-history")
  (cider-repl-history-size 1000)
  (cider-repl-pop-to-buffer-on-connect 'display-only)
  (cider-repl-print-level 10)
  (cider-repl-use-pretty-printing t)
  (cider-save-file-on-load t))
(use-package cljstyle-mode
  :load-path "lisp/cljstyle-mode-0.1"
  :bind (("C-c C-n" . cljstyle)))
(use-package clojure-mode
  :ensure t
  :hook (clojure-mode . eldoc-mode)
  :custom
  (clojure-docstring-fill-column 80))
(use-package company
  :ensure t)
(use-package exec-path-from-shell
  :ensure t
  :commands exec-path-from-shell-initialize
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "JAVA_HOME" "ASPELL_CONF"))
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))
(use-package exwm
  :ensure t)
(use-package flycheck-clj-kondo
  :ensure t
  :after (clojure-mode))
(use-package gnu-elpa-keyring-update
  :ensure t
  :commands gnu-elpa-keyring-update
  :config
  (gnu-elpa-keyring-update))
(use-package helm
  :ensure t
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x))
  :config
  (helm-mode))
(use-package helm-ag
  :ensure t
  :after (helm))
(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :commands helm-projectile-on
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))
(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . variable-pitch-mode))
(use-package ob-shell
  :after (org))
(use-package org
  :ensure t
  :hook ((org-mode . variable-pitch-mode))
  :bind (("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         ("C-c o o" . org-cycle-agenda-files))
  :custom
  (org-startup-indented t))
(use-package org-autolist
  :ensure t
  :after (org)
  :hook (org-mode . org-autolist-mode))
(use-package org-drill
  :ensure t
  :commands org-drill-hide-region
  :after (org)
  :custom
  (org-drill-left-cloze-delimiter "{")
  (org-drill-question-tag "NOTE")
  (org-drill-right-cloze-delimiter "}")
  (org-drill-save-buffers-after-drill-sessions-p t)
  ;; TODO: can this just be "notes.org"?
  (org-drill-scope '("~/org/notes.org"))
  :config
  (defun pjs-org-drill-hide-comments ()
    "Hide comments."
    (save-excursion
      (while (re-search-forward "^#[^+].*$" nil t)
        (org-drill-hide-region (match-beginning 0) (match-end 0)))))
  (advice-add 'org-drill-hide-comments :override 'pjs-org-drill-hide-comments))
(use-package org-protocol
  :after (org))
(use-package paredit
  :ensure t
  :hook (clojure-mode . paredit-mode))
(use-package pdf-tools
  :ensure t
  :commands pdf-tools-install
  :config
  (pdf-tools-install))
(use-package pinentry
  :ensure t
  :commands pinentry-start
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
(use-package pjs
  :load-path "lisp/pjs"
  :bind (("<XF86Tools>" . pjs-show-xfce-settings)
         ("C-c e s" . pjs-suspend)
         ("C-c e l" . pjs-lock-screen)
         ("C-c r" . pjs-revert)
         ("C-c u" . pjs-pop-read-queue)))
(use-package pjs-emacs-lisp
  :load-path "lisp/pjs"
  :hook (emacs-lisp-mode . pjs-add-eval-buffer-binding))
(use-package pjs-org
  :load-path "lisp/pjs"
  :bind (("C-c a" . pjs-org-agenda)))
(use-package pjs-org-cosmetics
  :load-path "lisp/pjs"
  :after (org))
(use-package projectile
  :ensure t
  :commands projectile-mode
  :config
  (projectile-mode +1)
  :bind
  (("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map)))
(use-package tc
  :load-path "lisp/tc"
  :after (magit)
  :bind (:map git-commit-mode-map
              ("C-c l" . tc/insert-clubhouse-story-url)
              ("C-c C-l" . tc/insert-clubhouse-story-url)
              ("C-c a" . tc/insert-co-authored-by)
              ("C-c C-a" . tc/insert-co-authored-by)))
(use-package typo
  :ensure t
  :hook ((markdown-mode org-mode) . typo-mode))
(use-package visual-fill-column
  :ensure t
  :hook ((markdown-mode . visual-fill-column-mode)
         (org-mode . visual-fill-column-mode)
         (visual-fill-column-mode . visual-line-mode))
  :config
  (setq split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  (advice-add 'text-scale-adjust :after 'visual-fill-column-adjust))
(use-package writegood-mode
  :ensure t
  :hook text-mode)
(use-package zk
  :load-path "lisp/zk-0.1"
  :custom
  (zk-directory "~/org/zk/")
  (zk-extensions (quote ("org" "txt" "text" "md" "markdown")))
  (zk-strip-summary-regexp "\\([
        ]\\|^#\\+[[:upper:]_]+:.*$\\|^:[^:]+:.*$\\)")
  :hook (org-mode . zk-navigate-keys)
  :bind (("C-c z z" . zk))
  :after (org))

;; Configuration
(global-set-key (kbd "C-c D") 'er-delete-file-and-buffer)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c C-i") 'imenu)
;; (global-set-key (kbd "C-c t") 'bh/org-todo)
;; (global-set-key (kbd "C-c w") 'bh/widen)
(global-set-key (kbd "C-x n r") 'narrow-to-region)

;;; Copied from better-defaults package.
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)

(when (not (eq (server-running-p) 't))
  (server-start))

(with-eval-after-load 'pjs
  (when (file-exists-p pjs-system-file)
    (load pjs-system-file)))

(put 'narrow-to-region 'disabled nil)

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)
;;; init.el ends here
