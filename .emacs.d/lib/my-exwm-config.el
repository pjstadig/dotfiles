(use-package exwm
  :ensure t)

(defun my/xfce-logout (&optional arg)
  ;; copied from `save-buffers-kill-emacs`
  "Offer to save each buffer, then kill this xfce session.
With prefix ARG, silently save all file-visiting buffers without asking.
If there are active processes where `process-query-on-exit-flag'
returns non-nil and `confirm-kill-processes' is non-nil,
asks whether processes should be killed.
Runs the members of `kill-emacs-query-functions' in turn and stops
if any returns nil.  If `confirm-kill-emacs' is non-nil, calls it."
  (interactive "P")
  ;; Don't use save-some-buffers-default-predicate, because we want
  ;; to ask about all the buffers before killing Emacs.
  (save-some-buffers arg t)
  (let ((confirm confirm-kill-emacs))
    (and
     (or (not (memq t (mapcar (function
                               (lambda (buf) (and (buffer-file-name buf)
                                                  (buffer-modified-p buf))))
                              (buffer-list))))
         (progn (setq confirm nil)
                (yes-or-no-p "Modified buffers exist; exit anyway? ")))
     (or (not (fboundp 'process-list))
         ;; process-list is not defined on MSDOS.
         (not confirm-kill-processes)
         (let ((processes (process-list))
               active)
           (while processes
             (and (memq (process-status (car processes)) '(run stop open listen))
                  (process-query-on-exit-flag (car processes))
                  (setq active t))
             (setq processes (cdr processes)))
           (or (not active)
               (with-current-buffer-window
                (get-buffer-create "*Process List*") nil
                #'(lambda (window _value)
                    (with-selected-window window
                      (unwind-protect
                          (progn
                            (setq confirm nil)
                            (yes-or-no-p "Active processes exist; kill them and exit anyway? "))
                        (when (window-live-p window)
                          (quit-restore-window window 'kill)))))
                (list-processes t)))))
     ;; Query the user for other things, perhaps.
     (run-hook-with-args-until-failure 'kill-emacs-query-functions)
     (or (null confirm)
         (funcall confirm "Really exit Emacs? "))
     (start-process "xfce4-session-logout" nil "xfce4-session-logout" "-l" "-f"))))

(defun my/configure-exwm ()
  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default)

  (require 'exwm-randr)
  (exwm-randr-enable)
  (global-set-key (kbd "C-x C-c") 'my/xfce-logout))

(provide 'my-exwm-config)
