;;; pjs-org-drill --- Org-mode configuration and utilities -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'org-drill)

(defun pjs-org-drill-hide-comments ()
  "Hide comments."
  (save-excursion
    (while (re-search-forward "^#[^+].*$" nil t)
      (org-drill-hide-region (match-beginning 0) (match-end 0)))))

(advice-add 'org-drill-hide-comments :override 'pjs-org-drill-hide-comments)

(defun pjs-org-drill-or-resume ()
  (interactive)
  (if (and org-drill-last-session
           (org-drill-entries-pending-p org-drill-last-session))
      (org-drill-resume)
    (org-drill)))

(provide 'pjs-org-drill)
;;; pjs-org-drill.el ends here
