(defgroup my nil
  "My config variables."
  :prefix "my/")

(defcustom my/inhibit-cleanup nil
  "If true will disable buffer cleanup on save."
  :group 'my
  :type 'boolean
  :safe #'booleanp)

(defun my/cleanup-buffer ()
  (interactive)
  (when (and (derived-mode-p 'prog-mode) (not my/inhibit-cleanup))
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
  (let ((display-buffer-alist '(("*Async Shell Command*" display-buffer-no-window))))
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

(defun my/save-buffer (&optional arg)
  (interactive "p")
  (my/cleanup-buffer)
  (save-buffer arg))

(defun my/revert ()
  (interactive)
  (revert-buffer 'ignore-auto 'noconfirm 'preserve-mode))

(global-set-key (kbd "C-c c") 'my/cleanup-buffer)
(global-set-key (kbd "C-c r") 'my/revert)
(global-set-key (kbd "C-x C-s") 'my/save-buffer)

(global-set-key (kbd "C-c e n") 'my/restart-network-manager)
(global-set-key (kbd "C-c e p") 'my/start-power-manager)
(global-set-key (kbd "C-c e l") 'my/lock-screen)
(global-set-key (kbd "C-c e s") 'my/suspend)
(global-set-key (kbd "C-c e t") 'my/show-time)
