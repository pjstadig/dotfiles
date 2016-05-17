(add-to-list 'load-path "~/src/org-mode")

(define-key global-map "\C-cc" 'org-capture)
(setq org-directory (expand-file-name "~/org"))
(setq org-agenda-files (list org-directory))
(setq org-completion-use-ido 't)
(setq org-capture-templates
      '(("t" "Todo" entry (file "inbox.org")
         "* TODO %i%?")
        ("p" "Project" entry (file "gtd.org")
         "* %i%? :project:")
        ("j" "Journal" entry (file+datetree "journal.org")
         "* %i%?\n  Entered on %T")))
(setq org-refile-targets `((,(expand-file-name "~/org/gtd.org") . (:level . 1))))
(setq org-todo-keywords
      '((sequence "TODO" "|" "DONE" "WAITING")))

(setq org-mobile-directory (expand-file-name "~/Dropbox/mobileorg"))
(setq org-mobile-files nil)
(setq org-mobile-inbox-for-pull (expand-file-name "~/org/inbox.org"))
(setq org-mobile-force-id-on-agenda-items nil)

(defun pjs-set-org-mode-whitespace-style ()
  (setq whitespace-style '(face lines-tail tabs)))

;; Thanks, Lee!
(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))

(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

(add-hook 'org-mode-hook 'whitespace-mode)
(add-hook 'org-mode-hook 'pjs-set-org-mode-whitespace-style)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'turn-off-fci-mode)
