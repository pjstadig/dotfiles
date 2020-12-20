;;; init.el --- Emacs initialization -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Bootstrap
;; These need to be set before doing anything else.
(setq custom-file (concat user-emacs-directory "custom.el")
      load-prefer-newer t)

(setq-default fill-column 90
              indent-tabs-mode nil)

;;; Initialize packages
;; My basic unit of code is the package, so before anything else can happen I
;; need to initialize the package system ...
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             :append)
(package-initialize)

;; ...and install `use-package'.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Add to load-path directory for my code.
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Some "packages" are not on elpa.  Add them to load-path.
(dolist (dir (directory-files (concat user-emacs-directory "not-elpa")))
  (when (not (member dir '("." "..")))
    (add-to-list 'load-path (concat user-emacs-directory "not-elpa/" dir))))
(use-package pjs-system)

;; Activate customization for `org-babel'.
;; - add a hook to disable evaluation confirmation on a per-file basis
(use-package pjs-org-babel :after (ob-core)
  :hook (hack-local-variables . pjs-org-babel-set-confirm-evaluate))
;; Configure languages for org-babel

(use-package ob-clojure :after (org)
  :custom
  (org-babel-default-header-args:clojure '((:session "*ob-clojure*"))))
(use-package ob-dot :after (org))
;; Learning quantum computing and deep neural networks, I use linear algebra,
;; and `ob-octave' is very helpful for doing ad-hoc linear algebra.
(use-package ob-octave :after (org))
;; `ob-http' is indispensable for testing REST APIs.
(use-package ob-http :ensure t :after (org))
;; Legacy initialization

(use-package checkdoc
  :hook (emacs-lisp-mode . checkdoc-minor-mode))
(use-package cider
  :ensure t
  :after (clojure-mode)
  ;; :custom
  ;; TODO: do I want to set this?
  ;; (cider-jdk-src-paths '("~/.cache/openjdk-8u192b26/"))
  )
(use-package cljstyle-mode
  :after (clojure-mode)
  :bind (:map clojure-mode-map
              ("C-c C-n" . cljstyle)))
(use-package clojure-mode
  :ensure t
  :defer t)
(use-package company
  :ensure t
  :hook (prog-mode . company-mode))
(use-package eldoc
  :hook ((clojure-mode . eldoc-mode)
         (emacs-lisp-mode . eldoc-mode)))
;; TODO: move to OSX specific config
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :if (memq window-system '(mac ns))
  :custom
  (exec-path-from-shell-variables '("PATH" "MANPATH" "JAVA_HOME" "ASPELL_CONF"))
  :config
  (declare-function exec-path-from-shell-initialize "exec-path-from-shell.el")
  (exec-path-from-shell-initialize))
(use-package exwm
  :ensure t
  :if (memq window-system '(x)))
(use-package exwm-edit
  :ensure t
  :after (exwm))
;; (use-package files
;;   :defer t
;;   :config
;;   (declare-function auto-save-visited-mode "files.el")
;;   (auto-save-visited-mode))
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
  :bind (("C-c j j" . helm-org-agenda-files-headings)))
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
  :hook (prog-mode . linum-mode))
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
  :demand t
  :hook ((org-mode . variable-pitch-mode))
  :bind (("C-c b" . org-switchb)
         ("C-c o o" . org-cycle-agenda-files)
         ("C-c j r". org-refile-goto-last-stored)
         ("C-c j c". org-capture-goto-last-stored)))
(use-package org-autolist
  :ensure t
  :after (org)
  :hook (org-mode . org-autolist-mode))
(use-package org-capture
  :bind (("C-c c" . org-capture)))
(use-package org-drill
  :ensure t
  :commands (org-drill))
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
  :config
  (declare-function pinentry-start "pinentry.el")
  (pinentry-start))
(use-package pjs
  :demand t
  :hook (prog-mode . pjs-prog-mode-local-bindings)
  :bind (("<XF86Tools>" . pjs-show-xfce-settings)
         ("C-c e s" . pjs-suspend)
         ("C-c e l" . pjs-lock-screen)
         ("C-c r" . pjs-revert)
         ("C-c u" . pjs-pop-read-queue)
         ("C-c D" . er-delete-file-and-buffer)))
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
(use-package pjs-org-drill
  :bind (("C-c d" . pjs-org-drill-or-resume)))
(use-package pjs-prog-mode
  :hook (prog-mode . pjs-todo-font-lock))
(use-package pjs-reset
  :bind (("s-r" . pjs-reset)))
(use-package pjs-secrets)
(use-package projectile
  :ensure t
  :bind-keymap
  (("s-p" . projectile-command-map)
   ("C-c p" . projectile-command-map))
  :config
  (declare-function projectile-mode "projectile.el")
  (projectile-mode +1))
(use-package scroll-bar
  :demand t
  :config
  (declare-function scroll-bar-mode "scroll-bar.el")
  (scroll-bar-mode -1))
(use-package simple
  :hook (prog-mode . column-number-mode))
(use-package tc
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
  :hook (((markdown-mode org-mode) . visual-fill-column-mode)
         (visual-fill-column-mode . visual-line-mode))
  :config
  (advice-add 'text-scale-adjust :after 'visual-fill-column-adjust))
(use-package whitespace
  :hook (prog-mode . whitespace-mode))
(use-package writegood-mode
  :ensure t
  :hook text-mode)
(use-package zk
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
;; Epilogue

(provide 'init)
;;; init.el ends here
