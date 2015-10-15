;;; My general personalizations

;; Random stuff

(require 'cl)

(setq custom-file (expand-file-name "~/.emacs.d/custom.el")
      ispell-extra-args '("--keyboard=dvorak")
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      org-default-notes-file "~/.dotfiles/.notes.org"
      org-remember-default-headline 'bottom
      org-completion-use-ido t
      epa-armor t
      visible-bell t
      inhibit-startup-message t)

(when window-system
  (setq scroll-conservatively 1))

(load custom-file t)

(add-to-list 'load-path (concat user-emacs-directory "/lisp"))

;; Packages

(when (not (require 'package nil t))
  (require 'package "package-23.el"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa-stable.milkbox.net/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(better-defaults cider clojure-mode diminish
                                      elisp-slime-nav find-file-in-project
                                      fill-column-indicator idle-highlight-mode
                                      magit markdown-mode page-break-lines
                                      paredit parenface-plus scpaste smex
                                      yaml-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(mapc 'load (directory-files (concat user-emacs-directory user-login-name)
                             t "^[^#].*el$"))

;; activation

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

(require 'ido-hacks "ido-hacks.el") ; still not on marmalade uuuugh
(ido-hacks-mode)

(global-set-key (kbd "M-x") 'smex) ; has to happen after ido-hacks-mode

(defalias 'yes-or-no-p 'y-or-n-p)

(column-number-mode t)
