;; -*- lexical-binding: t; -*-
(require 'pjs)
(setq pjs-inhibit-clojure-align-on-save 't)
(setq pjs-inhibit-cleanup-on-save 't)
(setq default-directory "~/")

(require 'projectile)
(setq projectile-project-root-files nil)

(add-to-list 'org-agenda-files "~/clubhouse/clubhouse.org" t)

(remove-hook 'prog-mode-hook 'whitespace-mode)

(require 'cider)

(defun clubhouse-cider-jack-in-clj ()
  "Start an nREPL server for the current project and connect to it.
PARAMS is a plist optionally containing :project-dir and :jack-in-cmd.
With the prefix argument, allow editing of the jack in command; with a
double prefix prompt for all these parameters."
  (interactive)
  ;; (setq default-directory "/Users/paul/src/backend/dev-system")
  ;; (shell-command "make")
  (let ((params (thread-first (plist-put '() :project-dir "/Users/paul/src/backend/dev-system")
                  (cider--update-project-dir)
                  (cider--check-existing-session)
                  (cider--update-jack-in-cmd))))
    (nrepl-start-server-process
     (plist-get params :project-dir)
     (plist-get params :jack-in-cmd)
     (lambda (server-buffer)
       (cider-connect-sibling-clj params server-buffer)))))
