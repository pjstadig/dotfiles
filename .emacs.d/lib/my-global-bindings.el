(defgroup my nil
  "My config variables."
  :prefix "my/")

(defcustom my/inhibit-cleanup nil
  "If true will disable buffer cleanup on save."
  :group 'my
  :type 'boolean
  :safe #'booleanp)

(defun my/cleanup-buffer (&optional arg)
  (interactive "p")
  (when (and (derived-mode-p 'prog-mode)
             (and (eq arg 1)
                  (not my/inhibit-cleanup)))
    (let ((inhibit-redisplay 't))
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max))
      (indent-region (point-min) (point-max))
      (when (derived-mode-p 'clojure-mode)
        (ignore-errors (clojure-sort-ns))))))

(defun my/start-power-manager ()
  (interactive)
  (start-process "xfce4-power-manager" nil "xfce4-power-manager"))

(defun my/restart-network-manager ()
  (interactive)
  (let ((display-buffer-alist
         '(("*Async Shell Command*" display-buffer-no-window))))
    (async-shell-command "sudo systemctl restart network-manager" nil)))

(defun my/suspend ()
  (interactive)
  (start-process-shell-command "suspend" nil "systemctl suspend"))

(defun my/lock-screen ()
  (interactive)
  (start-process-shell-command "lock-screen" nil "dm-tool lock"))

(defun my/show-time ()
  (interactive)
  (start-process-shell-command "show-time" nil "date | dzen2 -p 10"))

(defun my/show-xfce-settings ()
  (interactive)
  (start-process-shell-command "show-xfce-settings" nil "xfce4-settings-manager"))

(defun my/save-buffer (&optional arg)
  (interactive "p")
  (my/cleanup-buffer arg)
  (save-buffer))

(defun my/revert ()
  (interactive)
  (revert-buffer 'ignore-auto 'noconfirm 'preserve-mode))

;; Key binding conventions
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;;
;; C-c [letter] is reserved for users
;; <f5> through <f9> are reserved for users

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; TODO is there a helm autocomplete I could use?
(global-set-key (kbd "C-c b") 'org-switchb)
(global-set-key (kbd "C-c h") 'bh/hide-other)
(global-set-key (kbd "C-c n") 'bh/toggle-next-task-display)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(global-set-key (kbd "C-c o a") 'bh/show-org-agenda)

(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))

(global-set-key (kbd "C-c o o") 'org-cycle-agenda-files)

;; TODO org clock with C-c k prefix?

(global-set-key (kbd "C-c r") 'my/revert)
(global-set-key (kbd "C-c t") 'bh/org-todo)
(global-set-key (kbd "C-c w") 'bh/widen)

(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-x C-s") 'my/save-buffer)

(global-set-key (kbd "C-c e n") 'my/restart-network-manager)
(global-set-key (kbd "C-c e p") 'my/start-power-manager)
(global-set-key (kbd "C-c e l") 'my/lock-screen)
(global-set-key (kbd "C-c e s") 'my/suspend)
(global-set-key (kbd "C-c e t") 'my/show-time)
(global-set-key (kbd "<XF86Tools>") 'my/show-xfce-settings)
