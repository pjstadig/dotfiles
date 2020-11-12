;;; Configure Emacs
(setq custom-file (concat user-emacs-directory "custom.el")
      load-prefer-newer t)

(setq-default fill-column 90
              indent-tabs-mode nil)

;;; Setup package
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             :append)
(package-initialize)

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; Add load paths for lisp directories
(dolist (dir (directory-files (concat user-emacs-directory "lisp")))
  (when (not (member dir '("." "..")))
    (add-to-list 'load-path (concat user-emacs-directory "lisp" "/" dir))))

;; ;;; Configure packages
(use-package auth-source
  :defer t
  :custom
  (auth-sources '("~/.netrc.gpg")))
(use-package checkdoc
  :hook (emacs-lisp-mode . checkdoc-minor-mode)
  :custom
  (checkdoc-force-docstrings-flag nil))
(use-package cider
  :ensure t
  :after (clojure-mode)
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
  :after (clojure-mode)
  :bind (:map clojure-mode-map
              ("C-c C-n" . cljstyle)))
(use-package clojure-mode
  :ensure t
  :defer t
  :custom
  (clojure-docstring-fill-column 80))
(use-package company
  :ensure t
  :hook (prog-mode . company-mode))
(use-package doc-view
  :demand t
  :custom
  (doc-view-continuous t))
(use-package eldoc
  :hook ((clojure-mode . eldoc-mode)
         (emacs-lisp-mode . eldoc-mode)))
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :if (memq window-system '(mac ns))
  :init
  (eval-when-compile
    (message "AGAIN CURRENT FILE: %s" byte-compile-current-file))
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "JAVA_HOME" "ASPELL_CONF"))
  :config
  (declare-function exec-path-from-shell-initialize "exec-path-from-shell.el")
  (exec-path-from-shell-initialize))
(use-package exwm
  :ensure t
  :defer t
  :custom
  (exwm-input-prefix-keys '(24          ; ""
                            21          ; ""
                            8           ; ""
                            [134217848]
                            [134217824]
                            [134217766]
                            [134217786]))
  (exwm-layout-show-all-buffers nil)
  (exwm-manage-configurations '(((string-equal exwm-class-name "Xfce4-terminal")
                                 workspace 0)
                                ((string-equal exwm-class-name "Firefox")
                                 simulation-keys (("" . [left])
                                                  ("" . [right])
                                                  ("" . [up])
                                                  ("" . [down])
                                                  ("" . [home])
                                                  ("" . [end])
                                                  ([134217846] . [prior])
                                                  ("" . [next])
                                                  ("" . [delete])
                                                  ("" . [S-end delete])
                                                  ([134217847] . "")
                                                  ("" . "")
                                                  ("" . "")
                                                  ([134217751] . ""))
                                 workspace 1)
                                ((string-equal exwm-class-name "Google-chrome")
                                 simulation-keys (("" . [left])
                                                  ("" . [right])
                                                  ("" . [up])
                                                  ("" . [down])
                                                  ("" . [home])
                                                  ("" . [end])
                                                  ([134217846] . [prior])
                                                  ("" . [next])
                                                  ("" . [delete])
                                                  ("" . [S-end delete])
                                                  ([134217847] . "")
                                                  ("" . "")
                                                  ("" . "")
                                                  ([134217751] . ""))
                                 workspace 1)
                                ((string-equal exwm-class-name "Anki")
                                 simulation-keys (("" . [left])
                                                  ("" . [right])
                                                  ("" . [up])
                                                  ("" . [down])
                                                  ("" . [home])
                                                  ("" . [end])
                                                  ([134217846] . [prior])
                                                  ("" . [next])
                                                  ("" . [delete])
                                                  ("" . [S-end delete])
                                                  ([134217847] . "")
                                                  ("" . "")
                                                  ("" . "")
                                                  ([134217751] . ""))
                                 workspace 1)))
  (exwm-randr-workspace-monitor-plist '(9 "HDMI1"))
  (exwm-replace t)
  (exwm-update-class-hook '(pjs-set-exwm-buffer-name-to-class))
  (exwm-workspace-index-map (lambda (n) (number-to-string (1+ n))))
  (exwm-workspace-number 10)
  (exwm-workspace-show-all-buffers nil)
  (exwm-workspace-switch-create-limit 10))
(use-package exwm-edit
  :ensure t
  :after (exwm))
(use-package files
  :defer t
  :custom
  (backup-directory-alist '(("." . "~/.emacs.d/backups")))
  (require-final-newline t)
  :config
  (declare-function auto-save-visited-mode "files.el")
  (auto-save-visited-mode))
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :commands (flycheck-next-error flycheck-previous-error)
  :after (prog-mode)
  :bind (:map prog-mode-map
              ("C-c e n" . flycheck-next-error)
              ("C-c e p" . flycheck-previous-error))
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))
(use-package flycheck-clj-kondo
  :ensure t
  :after (clojure-mode flycheck))
(use-package flyspell
  :hook (prog-mode . flyspell-prog-mode))
(use-package gnu-elpa-keyring-update
  :demand t
  :ensure t
  :config
  (declare-function gnu-elpa-keyring-update "gnu-elpa-keyring-update.el")
  (gnu-elpa-keyring-update))
(use-package helm
  :ensure t
  :demand t
  :bind (("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x))
  :config
  (helm-mode))
(use-package helm-ag
  :ensure t
  :after (helm))
(use-package helm-org
  :ensure t
  :bind (("C-c j j" . helm-org-agenda-files-headings))
  :custom
  (helm-org-format-outline-path t))
(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :commands helm-projectile-on
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))
(use-package imenu
  :bind (("C-c i" . imenu)
         ("C-c C-i" . imenu)))
(use-package isearch
  :bind (("C-s" . isearch-forward-regexp)
         ("C-r" . isearch-backward-regexp)
         ("C-M-s" . isearch-forward)
         ("C-M-r" . isearch-backward)))
(use-package jwiegley-flycheck
  :after (flycheck)
  :hook (flycheck-after-syntax-check-hook
         jwiegley+magnars/adjust-flycheck-automatic-syntax-eagerness)
  :config
  ;; Remove newline checks, since they would trigger an immediate check
  ;; when we want the idle-change-delay to be in effect while editing.
  (setq-default flycheck-check-syntax-automatically '(save
                                                      idle-change
                                                      mode-enabled)))
(use-package linum
  :hook (prog-mode . linum-mode)
  :custom
  (linum-format "%d "))
(use-package lisp-mode
  :defer t
  :custom
  (emacs-lisp-docstring-fill-column t))
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))
(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . variable-pitch-mode))
(use-package menu-bar
  :defer t
  :custom
  (menu-bar-mode nil))
(use-package mouse
  :defer t
  :custom
  (mouse-yank-at-point t))
(use-package ob-shell
  :after (org))
(use-package org
  :ensure t
  :demand t
  :hook ((org-mode . variable-pitch-mode))
  :bind (("C-c b" . org-switchb)
         ("C-c o o" . org-cycle-agenda-files)
         ("C-c j r". org-refile-goto-last-stored)
         ("C-c j c". org-capture-goto-last-stored))
  :custom
  (org-startup-indented t))
(use-package org-autolist
  :ensure t
  :after (org)
  :hook (org-mode . org-autolist-mode))
(use-package org-capture
  :bind (("C-c c" . org-capture)))
(use-package org-drill
  :ensure t
  :defer t
  :commands (org-drill)
  :custom
  (org-drill-left-cloze-delimiter "{")
  (org-drill-question-tag "REVIEW")
  (org-drill-right-cloze-delimiter "}")
  (org-drill-save-buffers-after-drill-sessions-p nil)
  (org-drill-scope 'agenda)
  :bind (("C-c d" . pjs-org-drill-or-resume))
  :config
  (declare-function org-drill-hide-region "org-drill.el")
  (defun pjs-org-drill-hide-comments ()
    "Hide comments."
    (save-excursion
      (while (re-search-forward "^#[^+].*$" nil t)
        (org-drill-hide-region (match-beginning 0) (match-end 0)))))
  (advice-add 'org-drill-hide-comments :override 'pjs-org-drill-hide-comments)
  (defun pjs-org-drill-or-resume ()
    (interactive)
    (if (and org-drill-last-session
             (org-drill-entries-pending-p org-drill-last-session))
        (org-drill-resume)
      (org-drill))))
(use-package org-habit
  :after (org))
(use-package org-id
  :after (org))
(use-package org-protocol
  :after (org))
(use-package paredit
  :ensure t
  :hook ((clojure-mode emacs-lisp-mode) . paredit-mode))
(use-package paren
  :demand t
  :custom
  (show-paren-delay 0.25)
  :config
  (declare-function show-paren-mode "paren.el")
  (show-paren-mode 1))
(use-package pdf-tools
  :ensure t
  :demand t
  :config
  (declare-function pdf-tools-install "pdf-tools.el")
  (pdf-tools-install))
(use-package pinentry
  :ensure t
  :demand t
  :custom
  (epa-pinentry-mode 'loopback)
  :config
  (declare-function pinentry-start "pinentry.el")
  (pinentry-start))
(use-package pjs
  :hook (prog-mode . pjs-prog-mode-local-bindings)
  :bind (("<XF86Tools>" . pjs-show-xfce-settings)
         ("C-c e s" . pjs-suspend)
         ("C-c e l" . pjs-lock-screen)
         ("C-c r" . pjs-revert)
         ("C-c u" . pjs-pop-read-queue)
         ("C-c D" . er-delete-file-and-buffer)))
(use-package pjs-clubhouse
  :hook (clojure-mode . pjs-given-when-then-font-lock))
(use-package pjs-emacs-lisp
  :hook (emacs-lisp-mode . pjs-add-eval-buffer-binding))
(use-package pjs-exwm
  :commands pjs-configure-exwm
  :hook (exwm-init . pjs-start-initial-programs))
(use-package pjs-org
  :commands (pjs-ensure-ending-newline)
  :bind (("C-c a" . pjs-org-agenda)
         :map org-agenda-mode-map
         ("C-c C-x ^" . pjs-org-agenda-restrict-to-heading)
         :map org-mode-map
         ("C-x n u" . pjs-org-narrow-to-parent))
  :hook (org-insert-heading . pjs-org-insert-created-property))
(use-package pjs-org-cosmetics
  :after (org))
(use-package pjs-prog-mode
  :hook (prog-mode . pjs-todo-font-lock))
(use-package pjs-reset
  :bind (("s-r" . pjs-reset)))
(use-package pjs-secrets)
(use-package pjs-system
  :commands pjs-load-system-file)
(use-package projectile
  :ensure t
  :bind-keymap
  (("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map))
  :config
  (declare-function projectile-mode "projectile.el")
  (projectile-mode +1))
(use-package saveplace
  :demand t
  :custom
  (save-place-file "~/.emacs.d/places")
  :config
  (setq-default save-place t))
(use-package scroll-bar
  :demand t
  :config
  (declare-function scroll-bar-mode "scroll-bar.el")
  (scroll-bar-mode -1))
(use-package simple
  :hook (prog-mode . column-number-mode)
  :custom
  (save-interprogram-paste-before-kill t))
(use-package startup
  :defer t
  :custom
  (inhibit-startup-screen t))
(use-package solar
  :defer t
  :custom
  (calendar-latitude 38.0718912)
  (calendar-longitude -78.7225072))
(use-package tc
  :after (magit)
  :bind (:map git-commit-mode-map
              ("C-c l" . tc/insert-clubhouse-story-url)
              ("C-c C-l" . tc/insert-clubhouse-story-url)
              ("C-c a" . tc/insert-co-authored-by)
              ("C-c C-a" . tc/insert-co-authored-by)))
(use-package tool-bar
  :demand t
  :custom
  (tool-bar-mode nil))
(use-package typo
  :ensure t
  :hook ((markdown-mode org-mode) . typo-mode))
(use-package visual-fill-column
  :ensure t
  :hook (((markdown-mode org-mode) . visual-fill-column-mode)
         (visual-fill-column-mode . visual-line-mode))
  :custom
  (split-window-preferred-function 'visual-fill-column-split-window-sensibly)
  :config
  (advice-add 'text-scale-adjust :after 'visual-fill-column-adjust))
(use-package uniquify
  :demand t
  :custom
  (uniquify-buffer-name-style 'forward))
(use-package whitespace
  :hook (prog-mode . whitespace-mode))
(use-package writegood-mode
  :ensure t
  :hook text-mode)
(use-package zk
  :custom
  (zk-directory "~/org/zk/")
  (zk-extensions (quote ("org" "txt" "text" "md" "markdown")))
  (zk-strip-summary-regexp "\\([
        ]\\|^#\\+[[:upper:]_]+:.*$\\|^:[^:]+:.*$\\)")
  :hook (org-mode . zk-navigate-keys)
  :bind (("C-c z z" . zk)))

;; Configuration
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(put 'narrow-to-region 'disabled nil)

(when (file-exists-p custom-file)
  (load custom-file))

(require 'server)
(when (not (eq (server-running-p) 't))
  (server-start))

(require 'pjs-system)
(pjs-load-system-file)

(provide 'init)
;;; init.el ends here
