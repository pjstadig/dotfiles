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

(add-hook 'org-mode-hook 'whitespace-mode)
(add-hook 'org-mode-hook 'pjs-set-org-mode-whitespace-style)
(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
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
