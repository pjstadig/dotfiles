;;; pjs.el --- General configuration and utilities -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(defgroup pjs nil
  "My config variables."
  :group 'default)

(defun pjs-set-exwm-buffer-name-to-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

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

(defcustom pjs-inhibit-cleanup-on-save nil
  "If true will disable buffer cleanup on save."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defcustom pjs-inhibit-indent-on-save nil
  "If true will disable indenting on save."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defcustom pjs-inhibit-clojure-sort-ns-on-save nil
  "If true will disable sorting clojure 'ns on save."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defcustom pjs-inhibit-clojure-align-on-save nil
  "If true will disable aligning clojure 'let on save."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defun pjs-cleanup-buffer ()
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let ((inhibit-redisplay 't))
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max))
      (when (not pjs-inhibit-indent-on-save)
        (indent-region (point-min) (point-max)))
      (when (derived-mode-p 'clojure-mode)
        (when (not pjs-inhibit-clojure-sort-ns-on-save)
          (ignore-errors (clojure-sort-ns)))
        (when (not pjs-inhibit-clojure-align-on-save)
          (clojure-align (point-min) (point-max)))))))

(defun save-buffer-advice (old-save-buffer &optional arg)
  (interactive "p")
  (when (and (= (or arg 1) 1)
             (not pjs-inhibit-cleanup-on-save))
    (pjs-cleanup-buffer))
  (when old-save-buffer
    (funcall old-save-buffer)))

(advice-add 'save-buffer :around 'save-buffer-advice)

(defun pjs-restart-network-manager ()
  (interactive)
  (let ((display-buffer-alist
         '(("*Async Shell Command*" display-buffer-no-window))))
    (async-shell-command "sudo systemctl restart network-manager" nil)))

(defun pjs-suspend ()
  (interactive)
  (start-process-shell-command "suspend" nil "systemctl suspend"))

(defun pjs-lock-screen ()
  (interactive)
  (start-process-shell-command "lock-screen" nil "dm-tool lock"))

(defun pjs-show-xfce-settings ()
  (interactive)
  (start-process-shell-command "show-xfce-settings" nil "xfce4-settings-manager"))

(defun pjs-revert ()
  (interactive)
  (revert-buffer 'ignore-auto 'noconfirm 'preserve-mode))

;; Key binding conventions
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
;;
;; C-c [letter] is reserved for users
;; <f5> through <f9> are reserved for users

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

(defun pjs-prog-mode-local-bindings ()
  (local-set-key (kbd "C-c n") 'pjs-cleanup-buffer))

(defun pjs-sort-symbols (reverse beg end)
  "Sort symbols in region alphabetically, in REVERSE if negative.
    See `sort-words'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun pjs-configure-text-mode-fill-column ()
  (setq fill-column 80))

(defun pjs-pop-read-queue ()
  (interactive)
  (save-excursion
    (find-file-existing "~/org/personal/mobile/toread.org")
    (goto-char (point-min))
    (org-next-link)
    (org-open-at-point)
    (org-cut-subtree)
    (save-buffer))
  (exwm-workspace-switch-to-buffer "Firefox"))

(defcustom pjs-org-insert-created-property-p nil
  "If true will insert CREATED property for new headings."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(defcustom pjs-org-confirm-babel-evaluate t
  "If true will confirm before evaluating code blocks."
  :group 'pjs
  :type 'boolean
  :safe #'booleanp)

(provide 'pjs)
;;; pjs.el ends here
