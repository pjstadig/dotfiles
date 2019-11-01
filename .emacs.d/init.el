(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(defun pjs-add-eval-buffer-binding ()
  (local-set-key (kbd "C-c C-k") 'eval-buffer))

(add-hook 'emacs-lisp-mode-hook 'pjs-add-eval-buffer-binding)

;; Setup package
(require 'package)
(add-to-list 'package-archives
             (cons "melpa-stable" "https://stable.melpa.org/packages/")
             :append)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Dependencies
(use-package gnu-elpa-keyring-update
  :ensure t
  :config (gnu-elpa-keyring-update))
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))
(use-package pinentry
  :ensure t
  :config
  (setq epa-pinentry-mode 'loopback)
  (pinentry-start))
(use-package deft
  :ensure t
  :bind ("C-c d" . deft))
(use-package exwm
  :ensure t)
(use-package org-autolist
  :ensure t)
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
(use-package visual-fill-column
  :ensure t)
(use-package writegood-mode
  :ensure t)
(use-package typo
  :ensure t)
(require 'org-agenda)
(use-package org-bullets
  :ensure t)

;; Configuration
(defgroup pjs nil
  "My config variables."
  :group 'default)

(defvar pjs-exwm-configured-p nil)

(defun pjs-configure-exwm ()
  (require 'exwm)
  (exwm-enable)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (fringe-mode 1)

  (require 'exwm-randr)
  (exwm-randr-enable)
  (setq pjs-exwm-configured-p 't))

(defun pjs-reset ()
  (interactive)
  (byte-recompile-file (concat user-emacs-directory "init.el"))
  (load (concat user-emacs-directory "init.el"))
  (byte-recompile-directory (concat user-emacs-directory "elpa") 0)
  (when pjs-exwm-configured-p
    (exwm-reset)))

(defun pjs-set-exwm-buffer-name-to-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun pjs-start-initial-programs ()
  (start-process-shell-command "firefox" nil "firefox")
  (start-process-shell-command "xfce4-terminal" nil "xfce4-terminal"))

(defun pjs-erc-connect (server)
  (interactive "Mserver: ")
  (let ((znc-password-file "~/.private/pjs-znc-password.el"))
    (if (file-exists-p znc-password-file)
        (load znc-password-file)
      (eval-when-compile
        (defvar pjs-znc-password))))
  (erc-tls :server server
           :port "6697"
           :nick "paul"
           :password (concat "paul:" pjs-znc-password)))

(defcustom pjs-inhibit-cleanup nil
  "If true will disable buffer cleanup on save."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defun pjs-cleanup-buffer ()
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let ((inhibit-redisplay 't))
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (when (derived-mode-p 'clojure-mode)
        (ignore-errors (clojure-sort-ns))))))

(defun save-buffer-advice (old-save-buffer &optional arg)
  (interactive "p")
  (when (and (not (> arg 1))
             (not pjs-inhibit-cleanup))
    (pjs-cleanup-buffer))
  (when old-save-buffer
    (funcall old-save-buffer)))

(advice-add 'save-buffer :around 'save-buffer-advice)

(defun pjs-restart-network-manager ()
  (interactive)
  (let ((display-buffer-alist
         '(("*Async Shell Command*" display-buffer-no-window))))
    (async-shell-command "sudo systemctl restart network-manager" nil)))

(global-set-key (kbd "C-c e n") 'pjs-restart-network-manager)

(defun pjs-suspend ()
  (interactive)
  (start-process-shell-command "suspend" nil "systemctl suspend"))

(global-set-key (kbd "C-c e s") 'pjs-suspend)

(defun pjs-lock-screen ()
  (interactive)
  (start-process-shell-command "lock-screen" nil "dm-tool lock"))

(global-set-key (kbd "C-c e l") 'pjs-lock-screen)

(defun pjs-show-xfce-settings ()
  (interactive)
  (start-process-shell-command "show-xfce-settings" nil "xfce4-settings-manager"))

(global-set-key (kbd "<XF86Tools>") 'pjs-show-xfce-settings)

(defun pjs-revert ()
  (interactive)
  (revert-buffer 'ignore-auto 'noconfirm 'preserve-mode))

(global-set-key (kbd "C-c r") 'pjs-revert)

;; Key binding conventions
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;;
;; C-c [letter] is reserved for users
;; <f5> through <f9> are reserved for users

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c n") 'pjs-cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'pjs-cleanup-buffer)
(global-set-key (kbd "C-c o a") 'bh/show-org-agenda)

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

(global-set-key (kbd "C-c o o") 'org-cycle-agenda-files)

;; TODO org clock with C-c k prefix?

(global-set-key (kbd "C-c i") 'imenu)
(global-set-key (kbd "C-c C-i") 'imenu)

(global-set-key (kbd "C-c t") 'bh/org-todo)
(global-set-key (kbd "C-c w") 'bh/widen)

(global-set-key (kbd "C-x n r") 'narrow-to-region)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'eldoc-mode)

(defun pjs-sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun pjs-configure-text-mode-fill-column ()
  (setq fill-column 80))

(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'writegood-mode)
(add-hook 'text-mode-hook 'pjs-configure-text-mode-fill-column)

(add-hook 'visual-fill-column-mode-hook 'visual-line-mode)
(setq split-window-preferred-function 'visual-fill-column-split-window-sensibly)
(advice-add 'text-scale-adjust :after 'visual-fill-column-adjust)

;; Normally I would prefer to customize `org-mode-hook`, but when org loads, it
;; transitively loads org-babel, which calls `add-hook` on `org-mode-hook`
;; before defining the `org-mode-hook` custom variable.  `add-hook` will set a
;; value for `org-mode-hook`, and when the custom variable is finally defined it
;; does not use the custom value.
;;
;; I could either:
;; 1. Redefine `add-hook` to initialize a free, custom variable with its custom
;;    value...I guess?
;; 2. Define `org-mode-hook` as a custom variable before loading org.
;; 3. Change org to define `org-mode-hook` before loading org-babel.
;; 4. Use `add-hook` instead of customization.
;;
;; So ...
(add-hook 'org-mode-hook 'visual-fill-column-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'typo-mode)
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'org-autolist-mode)

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; taken from http://doc.norang.ca/org-mode.html
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at (point))))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components))
                             org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components))
                             org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components))
                           org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all
 subtasks.
  This is normally used by skipping functions where this variable is already
  local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all
 subtasks.
  This is normally used by skipping functions where this variable is already
  local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks
        (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks"
           (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next)
                          (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project
tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading)
                                             (point-max)))))
      (cond
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT
tasks, and loose tasks.  When not restricted, skip project and sub-project
tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading)
                                              (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading)
                                              (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(defun bh/org-todo (arg)
  (interactive "p")
  (when (derived-mode-p 'org-mode)
    (if (equal arg 4)
        (save-restriction
          (bh/narrow-to-org-subtree)
          (org-show-todo-tree nil))
      (bh/narrow-to-org-subtree)
      (org-show-todo-tree nil))))

(global-set-key (kbd "<S-f5>") 'bh/widen)

(defun bh/widen ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-agenda-remove-restriction-lock)
        (when org-agenda-sticky
          (org-agenda-redo)))
    (widen)))

(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (org-defkey org-agenda-mode-map "W"
                         (lambda ()
                           (interactive)
                           (setq bh/hide-scheduled-and-waiting-next-tasks t)
                           (bh/widen))))
          'append)

(defun bh/restrict-to-file-or-follow (arg)
  "Set agenda restriction to 'file or with argument invoke follow mode.
I don't use follow mode very often but I restrict to file all the time
so change the default 'F' binding in the agenda to allow both"
  (interactive "p")
  (if (equal arg 4)
      (org-agenda-follow-mode)
    (widen)
    (bh/set-agenda-restriction-lock 4)
    (org-agenda-redo)
    (goto-char (point-min))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "F" 'bh/restrict-to-file-or-follow))
          'append)

(defun bh/narrow-to-org-subtree ()
  (widen)
  (org-narrow-to-subtree)
  (save-restriction
    (org-agenda-set-restriction-lock)))

(defun bh/narrow-to-subtree ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (bh/narrow-to-org-subtree))
        (when org-agenda-sticky
          (org-agenda-redo)))
    (bh/narrow-to-org-subtree)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "N" 'bh/narrow-to-subtree))
          'append)

(defun bh/narrow-up-one-org-level ()
  (widen)
  (save-excursion
    (outline-up-heading 1 'invisible-ok)
    (bh/narrow-to-org-subtree)))

(defun bh/get-pom-from-agenda-restriction-or-point ()
  (or (and (marker-position org-agenda-restrict-begin) org-agenda-restrict-begin)
      (org-get-at-bol 'org-hd-marker)
      (and (equal major-mode 'org-mode) (point))
      org-clock-marker))

(defun bh/narrow-up-one-level ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-up-one-org-level))
        (org-agenda-redo))
    (bh/narrow-up-one-org-level)))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "U" 'bh/narrow-up-one-level))
          'append)

(defun bh/narrow-to-org-project ()
  (widen)
  (save-excursion
    (bh/find-project-task)
    (bh/narrow-to-org-subtree)))

(defun bh/narrow-to-project ()
  (interactive)
  (if (equal major-mode 'org-agenda-mode)
      (progn
        (org-with-point-at (bh/get-pom-from-agenda-restriction-or-point)
          (bh/narrow-to-org-project)
          (save-excursion
            (bh/find-project-task)
            (org-agenda-set-restriction-lock)))
        (org-agenda-redo)
        (goto-char (point-min)))
    (bh/narrow-to-org-project)
    (save-restriction
      (org-agenda-set-restriction-lock))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "P" 'bh/narrow-to-project))
          'append)

(defvar bh/project-list nil)

(defun bh/view-next-project ()
  (interactive)
  (let (num-project-left current-project)
    (unless (marker-position org-agenda-restrict-begin)
      (goto-char (point-min))
                                        ; Clear all of the existing markers on the list
      (while bh/project-list
        (set-marker (pop bh/project-list) nil))
      (re-search-forward "Tasks to Refile")
      (forward-visible-line 1))

                                        ; Build a new project marker list
    (unless bh/project-list
      (while (< (point) (point-max))
        (while (and (< (point) (point-max))
                    (or (not (org-get-at-bol 'org-hd-marker))
                        (org-with-point-at (org-get-at-bol 'org-hd-marker)
                          (or (not (bh/is-project-p))
                              (bh/is-project-subtree-p)))))
          (forward-visible-line 1))
        (when (< (point) (point-max))
          (add-to-list 'bh/project-list (copy-marker (org-get-at-bol 'org-hd-marker)) 'append))
        (forward-visible-line 1)))

                                        ; Pop off the first marker on the list and display
    (setq current-project (pop bh/project-list))
    (when current-project
      (org-with-point-at current-project
        (setq bh/hide-scheduled-and-waiting-next-tasks nil)
        (bh/narrow-to-project))
                                        ; Remove the marker
      (setq current-project nil)
      (org-agenda-redo)
      (goto-char (point-min))
      (setq num-projects-left (length bh/project-list))
      (if (> num-projects-left 0)
          (message "%s projects left to view" num-projects-left)
        (goto-char (point-min))
        (setq bh/hide-scheduled-and-waiting-next-tasks t)
        (error "All projects viewed.")))))

(add-hook 'org-agenda-mode-hook
          '(lambda () (org-defkey org-agenda-mode-map "V" 'bh/view-next-project))
          'append)

(defun bh/org-auto-exclude-function (tag)
  "Automatic task exclusion in the agenda with / RET"
  (when (string= tag "hold")
    (concat "-" tag)))

(defun bh/mark-next-parent-tasks-todo ()
  "Visit each parent task and change NEXT states to TODO"
  (let ((mystate (or (and (fboundp 'org-state)
                          state)
                     (nth 2 (org-heading-components)))))
    (when mystate
      (save-excursion
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) (list "NEXT"))
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'bh/mark-next-parent-tasks-todo 'append)

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (save-restriction
    (widen)
    ;; Consider only tasks with done todo headings as archivable candidates
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
          (subtree-end (save-excursion (org-end-of-subtree t))))
      (if (member (org-get-todo-state) org-todo-keywords-1)
          (if (member (org-get-todo-state) org-done-keywords)
              (let* ((daynr (string-to-number (format-time-string "%d" (current-time))))
                     (a-month-ago (* 60 60 24 (+ daynr 1)))
                     (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
                     (this-month (format-time-string "%Y-%m-" (current-time)))
                     (subtree-is-current (save-excursion
                                           (forward-line 1)
                                           (and (< (point) subtree-end)
                                                (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
                (if subtree-is-current
                    subtree-end ; Has a date in this month or last month, skip it
                  nil))  ; available to archive
            (or subtree-end (point-max)))
        next-headline))))

(add-hook 'markdown-mode-hook 'visual-fill-column-mode)
(add-hook 'markdown-mode-hook 'variable-pitch-mode)
(add-hook 'markdown-mode-hook 'typo-mode)

(with-eval-after-load 'org
  (set-face-attribute 'org-checkbox nil :family "Fira Mono")
  (set-face-attribute 'org-table nil :family "Fira Mono")
  (require 'ob-shell)
  (require 'org-protocol))

;;; Copied from better-defaults package, but removing ido-mode since I prefer
;;; helm, and ido-mode is interfering with helm.
(progn
  ;; (ido-mode t)
  ;; (setq ido-enable-flex-matching t)

  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

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
  (setq-default indent-tabs-mode nil)
  (setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        require-final-newline t
        visible-bell t
        load-prefer-newer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))

(when (not (eq (server-running-p) 't))
  (server-start))

(defvar pjs-local-file
  (expand-file-name (concat user-emacs-directory "local.el")))

(when (file-exists-p pjs-local-file)
  (load pjs-local-file))
