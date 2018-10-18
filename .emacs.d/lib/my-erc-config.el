(defun my/erc-connect (server)
  (interactive "Mserver: ")
  (let ((znc-password-file "~/.private/my-znc-password.el"))
    (if (file-exists-p znc-password-file)
        (load znc-password-file)
      (eval-when-compile
        (defvar my/znc-password)
        (message "Failed to load ZNC password file."))))
  (erc-tls :server server
           :port "6697"
           :nick "paul"
           :password (concat "paul:" my/znc-password)))
