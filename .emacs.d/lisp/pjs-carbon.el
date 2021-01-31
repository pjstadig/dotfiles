;;; pjs-carbon.el --- Stardica code and config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'pjs)

(require 'org)

(setq org-directory "~/org/personal"
      org-agenda-files (append '("~/org/personal/in.org"
                                 "~/org/personal/todo.org"
                                 "~/org/personal/toread.org"
                                 "~/org/personal/tolisten.org"
                                 "~/org/personal/towatch.org"
                                 "~/org/personal/review.org"
                                 "~/org/personal/journal.org"
                                 "~/org/personal/reference.org")
                               org-agenda-files))

(provide 'pjs-carbon)
;;; pjs-carbon.el ends here
