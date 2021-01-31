;;; pjs-carbon.el --- Carbon code and config -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'org)
(require 'org-agenda)

(setq org-agenda-diary-file "~/org/personal/journal.org"
      org-agenda-files (append '("~/org/personal/in.org"
                                 "~/org/personal/todo.org"
                                 "~/org/personal/toread.org"
                                 "~/org/personal/tolisten.org"
                                 "~/org/personal/towatch.org"
                                 "~/org/personal/review.org"
                                 "~/org/personal/journal.org"
                                 "~/org/personal/reference.org")
                               org-agenda-files)
      org-archive-location "~/org/personal/archive.org::"
      org-default-notes-file "~/org/personal/in.org"
      org-directory "~/org/personal")

(provide 'pjs-carbon)
;;; pjs-carbon.el ends here
