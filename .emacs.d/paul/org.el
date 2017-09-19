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

(defun pjs-load-babel-clojure ()
  (require 'ob-clojure))

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
<<<<<<< HEAD
(add-hook 'org-mode-hook 'pjs-load-babel-clojure)

(require 'org)
(require 'ob-clojure)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (sh . t)
   (emacs-lisp . t)))

(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-support-shift-select 'always)

(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)
(setq org-hide-emphasis-markers t)
(setq org-babel-clojure-nrepl-timeout nil)

(add-to-list 'load-path (expand-file-name "~/src/org-present"))
(autoload 'org-present "org-present" nil t)
=======
(add-hook 'org-mode-hook 'turn-off-fci-mode)
>>>>>>> 0c2143395103d59fba439b29cdfb4131cb58985f
