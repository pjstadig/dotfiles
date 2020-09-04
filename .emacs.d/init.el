(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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
  :ensure t)
(use-package cljstyle-mode
  :load-path "lisp/cljstyle-mode-0.1"
  :bind (("C-c C-n" . cljstyle)))
(use-package clojure-mode
  :ensure t
  :after (flycheck-clj-kondo)
  :config
  (require 'flycheck-clj-kondo)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode))
(use-package company
  :ensure t)
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))
(use-package exwm
  :ensure t)
(use-package flycheck-clj-kondo
  :ensure t)
(use-package gnu-elpa-keyring-update
  :ensure t
  :config
  (gnu-elpa-keyring-update))
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
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))
(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))
(use-package org
  :ensure t
  :hook (org-mode . org-indent-mode)
  :config
  (require 'ob-shell)
  (require 'org-protocol))
(use-package org-autolist
  :ensure t
  :hook (org-mode . org-autolist-mode))
(use-package org-drill
  :ensure t)
(use-package paredit
  :ensure t)
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install))
(use-package pinentry
  :ensure t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
(use-package pjs
  :load-path "lisp/pjs")
(use-package pjs-emacs-lisp
  :load-path "lisp/pjs"
  :hook (emacs-lisp-mode . pjs-add-eval-buffer-binding))
(use-package pjs-org-cosmetics
  :load-path "lisp/pjs"
  :after (org))
(use-package projectile
  :ensure t
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
  :hook ((markdown-mode . typo-mode)
         (org-mode . typo-mode)))
(use-package visual-fill-column
  :ensure t
  :hook ((markdown-mode . visual-fill-column-mode)
         (org-mode . visual-fill-column-mode)
         (visual-fill-column-mode . visual-line-mode)))
(use-package writegood-mode
  :ensure t)
(use-package zk
  :load-path "lisp/zk-0.1"
  :hook (org-mode . zk-navigate-keys))
;; (require 'org-agenda)

;; Setup variable pitch mode
(add-hook 'org-mode-hook #'variable-pitch-mode)
(add-hook 'markdown-mode-hook #'variable-pitch-mode)

;; Configuration
(defgroup pjs nil
  "My config variables."
  :group 'default)

(defvar pjs/exwm-configured-p nil)

(defun pjs/configure-exwm ()
  (require 'exwm)
  (setq exwm-input-global-keys
        '(([?\s-w] . exwm-workspace-switch)
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))
          ([?\s-1] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 0)))
          ([?\s-2] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 1)))
          ([?\s-3] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 2)))
          ([?\s-4] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 3)))
          ([?\s-5] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 4)))
          ([?\s-6] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 5)))
          ([?\s-7] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 6)))
          ([?\s-8] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 7)))
          ([?\s-9] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 8)))
          ([?\s-0] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch-create 9)))
          ([?\s-r] . pjs/reset)))
  (exwm-enable)
  (fringe-mode 1)

  (require 'exwm-randr)
  (exwm-randr-enable)
  (setq pjs/exwm-configured-p 't))

(defvar pjs/system-file
  (expand-file-name (concat user-emacs-directory (system-name) ".el")))

(defun pjs/reset ()
  (interactive)
  (byte-recompile-file (concat user-emacs-directory "init.el") nil 0)
  (when (file-exists-p pjs/system-file)
    (byte-recompile-file pjs/system-file nil 0))
  (byte-recompile-directory (concat user-emacs-directory "elpa") 0)
  (byte-recompile-directory (concat user-emacs-directory "lisp") 0)
  (load (concat user-emacs-directory "init.el"))
  (when (file-exists-p pjs/system-file)
    (load pjs/system-file))
  (when pjs/exwm-configured-p
    (exwm-reset)))

(defun pjs/set-exwm-buffer-name-to-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun pjs/start-initial-programs ()
  (start-process-shell-command "firefox" nil "firefox")
  (start-process-shell-command "xfce4-terminal" nil "xfce4-terminal"))

(defun pjs/erc-connect (server)
  (interactive "Mserver: ")
  (let ((znc-password-file "~/.private/pjs-znc-password.el"))
    (if (file-exists-p znc-password-file)
        (load znc-password-file)
      (eval-when-compile
        (defvar pjs/znc-password))))
  (erc-tls :server server
           :port "6697"
           :nick "paul"
           :password (concat "paul:" pjs/znc-password)))

(defcustom pjs/inhibit-cleanup-on-save nil
  "If true will disable buffer cleanup on save."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defcustom pjs/inhibit-indent-on-save nil
  "If true will disable indenting on save."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defcustom pjs/inhibit-clojure-sort-ns-on-save nil
  "If true will disable sorting clojure 'ns on save."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defcustom pjs/inhibit-clojure-align-on-save nil
  "If true will disable aligning clojure 'let on save."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defun pjs/cleanup-buffer ()
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let ((inhibit-redisplay 't))
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max))
      (when (not pjs/inhibit-indent-on-save)
        (indent-region (point-min) (point-max)))
      (when (derived-mode-p 'clojure-mode)
        (when (not pjs/inhibit-clojure-sort-ns-on-save)
          (ignore-errors (clojure-sort-ns)))
        (when (not pjs/inhibit-clojure-align-on-save)
          (clojure-align (point-min) (point-max)))))))

(defun save-buffer-advice (old-save-buffer &optional arg)
  (interactive "p")
  (when (and (= (or arg 1) 1)
             (not pjs/inhibit-cleanup-on-save))
    (pjs/cleanup-buffer))
  (when old-save-buffer
    (funcall old-save-buffer)))

(advice-add 'save-buffer :around 'save-buffer-advice)

(defun pjs/restart-network-manager ()
  (interactive)
  (let ((display-buffer-alist
         '(("*Async Shell Command*" display-buffer-no-window))))
    (async-shell-command "sudo systemctl restart network-manager" nil)))

(defun pjs/suspend ()
  (interactive)
  (start-process-shell-command "suspend" nil "systemctl suspend"))

(defun pjs/lock-screen ()
  (interactive)
  (start-process-shell-command "lock-screen" nil "dm-tool lock"))

(defun pjs/show-xfce-settings ()
  (interactive)
  (start-process-shell-command "show-xfce-settings" nil "xfce4-settings-manager"))

(defun pjs/revert ()
  (interactive)
  (revert-buffer 'ignore-auto 'noconfirm 'preserve-mode))

;; Key binding conventions
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;;
;; C-c [letter] is reserved for users
;; <f5> through <f9> are reserved for users

(defun pjs/org-agenda ()
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (switch-to-buffer "*Org Agenda*")
    (org-agenda)))

(defun er-delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c a") 'pjs/org-agenda)
(global-set-key (kbd "C-c l") 'org-switchb)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c D") 'er-delete-file-and-buffer)
(global-set-key (kbd "C-c e n") 'pjs/restart-network-manager)
(global-set-key (kbd "C-c e s") 'pjs/suspend)
(global-set-key (kbd "C-c e l") 'pjs/lock-screen)
(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c C-i") 'imenu)
(global-set-key (kbd "C-c o o") 'org-cycle-agenda-files)
;; (global-set-key (kbd "C-c o a") 'bh/show-org-agenda)
(global-set-key (kbd "C-c r") 'pjs/revert)
(global-set-key (kbd "C-c u") 'pjs/pop-read-queue)
;; (global-set-key (kbd "C-c t") 'bh/org-todo)
;; (global-set-key (kbd "C-c w") 'bh/widen)
(global-set-key (kbd "C-c z z") 'zk)
(global-set-key (kbd "<XF86Tools>") 'pjs/show-xfce-settings)
(global-set-key (kbd "C-x n r") 'narrow-to-region)

(defun pjs/prog-mode-local-bindings ()
  (local-set-key (kbd "C-c n") 'pjs/cleanup-buffer))

(defun pjs/sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun pjs/configure-text-mode-fill-column ()
  (setq fill-column 80))

(setq split-window-preferred-function 'visual-fill-column-split-window-sensibly)
(advice-add 'text-scale-adjust :after 'visual-fill-column-adjust)

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

(defun pjs/pop-read-queue ()
  (interactive)
  (save-excursion
    (find-file-existing (concat org-directory "/read.org"))
    (goto-char (point-min))
    (org-next-link)
    (org-open-at-point)
    (org-cut-subtree)
    (save-buffer))
  (exwm-workspace-switch-to-buffer "Firefox"))

(when (not (eq (server-running-p) 't))
  (server-start))

(when (file-exists-p pjs/system-file)
  (load pjs/system-file))
(put 'narrow-to-region 'disabled nil)
