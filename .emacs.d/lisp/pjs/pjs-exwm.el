;;; pjs-exwm.el --- EXWM configuration and utilities -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'pjs)

(defun pjs-start-initial-programs ()
  (start-process-shell-command "firefox" nil "firefox")
  (start-process-shell-command "xfce4-terminal" nil "xfce4-terminal"))

(defun pjs-banish-cursor ()
  "Move the X mouse cursor out of the way."
  (interactive)
  (start-process-shell-command "banish-cursor" nil "banish-cursor"))

(defvar pjs-exwm-configured-p nil)

(defun pjs-configure-exwm ()
  "Require and configure EXWM.

Configures EXWM global bindings and EXWM support for xrandr.  It will set
pjs-exwm-configured-p to t."
  (interactive)
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
          ([?\s-r] . pjs-reset)
          ([?\M-\s-`] . pjs-banish-cursor)))
  (exwm-enable)
  (fringe-mode 1)

  (require 'exwm-randr)
  (exwm-randr-enable)
  (setq pjs-exwm-configured-p 't))

(provide 'pjs-exwm)
;;; pjs-exwm.el ends here
