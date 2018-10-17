(use-package exwm
  :ensure t)

(defun my/configure-exwm ()
  (require 'exwm-systemtray)
  (setq exwm-systemtray-height 16)
  (exwm-systemtray-enable)

  (require 'exwm)
  (require 'exwm-config)
  (exwm-config-default))

(provide 'my-exwm-config)
