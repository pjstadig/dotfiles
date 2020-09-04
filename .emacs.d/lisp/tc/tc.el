;;; -*- lexical-binding: t; -*-
;;; Code from Toby Crawley

(defun tc/insert-clubhouse-story-url ()
  "Looks forward in the buffer for a story branch, and uses the
story id to generate and insert a url to the story."
  (interactive)
  (insert
   (save-excursion
     (when (re-search-forward "branch .*/ch\\([0-9]+\\)/")
       (format "https://app.clubhouse.io/internal/story/%s/" (match-string 1))))))

(defun tc/get-recent-git-authors (n)
  "Returns an alpha sorted list of the unique authors of the last
  n commits for the current git repo."
  (let ((default-directory (file-name-directory (buffer-file-name))))
    (split-string
     (shell-command-to-string
      (format "git log HEAD~%s.. --pretty='%%an <%%ae>' | sort | uniq" n))
     "\n")))

(defun tc/insert-co-authored-by ()
  "Inserts a Co-authored-by header from a selection from authors
  for the 50 most recent commits."
  (interactive)
  (insert (format "Co-authored-by: %s"
                  (completing-read "co-author: "
                                   (save-excursion
                                     (tc/get-recent-git-authors 50))))))

(provide 'tc)
