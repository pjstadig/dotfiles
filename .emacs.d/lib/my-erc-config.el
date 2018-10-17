(defun my/erc-connect (server)
  (interactive "Mserver: ")
  (let ((znc-password-file "~/.private/my-znc-password.el"))
    (if (file-exists-p znc-password-file)
        (progn (load znc-password-file)
               (erc-tls :server server
                        :port "6697"
                        :nick "paul"
                        :password (concat "paul:" my/znc-password)))
      (message "Failed to load ZNC password file."))))
