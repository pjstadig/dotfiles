;;; pjs-org.el --- Org-mode configuration and utilities -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)

(defun pjs-org-agenda-skip-entry-if (&rest conditions)
  (pjs-org-agenda-skip-if nil conditions))

(defun pjs-org-agenda-skip-subtree-if (&rest conditions)
  (pjs-org-agenda-skip-if 't conditions))

(defun pjs-org-agenda-skip-if (subtree conditions)
  "Checks current entity for CONDITIONS.
If SUBTREE is non-nil, the entire subtree is checked.  Otherwise, only
the entry (i.e. the text before the next heading) is checked.

CONDITIONS is a list of symbols, boolean OR is used to combine the results
from different tests.  Valid conditions are:

scheduled     Check if there is a scheduled cookie
notscheduled  Check if there is no scheduled cookie
deadline      Check if there is a deadline
notdeadline   Check if there is no deadline
timestamp     Check if there is a timestamp (also deadline or scheduled)
nottimestamp  Check if there is no timestamp (also deadline or scheduled)
regexp        Check if regexp matches
notregexp     Check if regexp does not match.
todo          Check if TODO keyword matches
nottodo       Check if TODO keyword does not match
tag           Check if tag matches
nottag        Check if tag does not match
category      Check if category matches
notcategory   Check if category does not match
habit         Check if there is a STYLE property with value \"habit\"
nothabit      Check if there is not a STYLE property with value \"habit\"
task          Check if task
nottask       Check if not task
project       Check if project
notproject    Check if not project
active        Check if active project
notactive     Check if not active project
stuck         Check if stuck project between START and END
notstuck      Check if not stuck project between START and END
priority      Check if priority matches
notpriority   Check if priority does not mach

The regexp is taken from the conditions list, it must come right after
the `regexp' or `notregexp' element.

`todo' and `nottodo' accept as an argument a list of todo
keywords, which may include \"*\" to match any todo keyword.

    (pjs-org-agenda-skip-entry-if \\='todo \\='(\"TODO\" \"WAITING\"))

would skip all entries with \"TODO\" or \"WAITING\" keywords.

Instead of a list, a keyword class may be given.  For example:

    (pjs-org-agenda-skip-entry-if \\='nottodo \\='done)

would skip entries that haven't been marked with any of \"DONE\"
keywords.  Possible classes are: `todo', `done', `any'.

`tag' and `nottag' accept as an argument tag(s) as a single string or list of
strings.

`category' and `notcategory' accept as an argument categor(ies) as a single
string or list of strings.

`stuck' and `notstuck' accept as arguments the START and END (as days) defining
the date range during which an entry must have no activity.

`priority' and `notpriority' accept as an argument a priority as a single
character integer value.

If any of these conditions is met, this function returns the end point of
the entity, causing the search to continue from there.  This is a function
that can be put into `org-agenda-skip-function' for the duration of a command."
  (org-back-to-heading t)
  (let* ((end (if subtree (save-excursion (org-end-of-subtree t) (point))
                (org-entry-end-position)))
         m)
    (and
     (or (and (setq m (memq 'tag conditions))
              (pjs-org-has-tag-p (nth 1 m)))
         (and (setq m (memq 'nottag conditions))
              (not (pjs-org-has-tag-p (nth 1 m))))
         (and (setq m (memq 'category conditions))
              (pjs-org-has-category-p (nth 1 m)))
         (and (setq m (memq 'notcategory conditions))
              (not (pjs-org-has-category-p (nth 1 m))))
         (and (memq 'habit conditions)
              (pjs-org-habit-p))
         (and (memq 'nothabit conditions)
              (not (pjs-org-habit-p)))
         (and (memq 'task conditions)
              (pjs-org-task-p))
         (and (memq 'nottask conditions)
              (not (pjs-org-task-p)))
         (and (memq 'project conditions)
              (pjs-org-project-p))
         (and (memq 'notproject conditions)
              (not (pjs-org-project-p)))
         (and (memq 'active conditions)
              (pjs-org-active-project-p))
         (and (memq 'notactive conditions)
              (not (pjs-org-active-project-p)))
         (and (setq m (memq 'stuck conditions))
              (pjs-org-stuck-project-p (nth 1 m) (nth 2 m)))
         (and (setq m (memq 'notstuck conditions))
              (not (pjs-org-stuck-project-p (nth 1 m) (nth 2 m))))
         (and (setq m (memq 'priority conditions))
              (pjs-org-has-priority-p (nth 1 m)))
         (and (setq m (memq 'notpriority conditions))
              (not (pjs-org-has-priority-p (nth 1 m))))
         (org-agenda-skip-if subtree conditions))
     end)))

(defun pjs-org-has-tag-p (tag)
  (if (listp tag)
      (if (member (first tag) (org-get-tags))
          t
        (when (cdr tag)
          (pjs-org-has-tag-p (cdr tag))))
    (member tag (org-get-tags))))

(defun pjs-org-has-category-p (categories)
  (if (listp categories)
      (if (string-equal (first categories) (org-get-category))
          t
        (when (cdr categories)
          (pjs-org-has-category-p (cdr categories))))
    (string-equal categories (org-get-category))))

(defun pjs-org-habit-p ()
  (string= (org-entry-get nil "STYLE") "habit"))

(defun pjs-org-todo-p (&optional todo-keywords)
  "A TODO is an entry with a TODO cookie matching TODO-KEYWORDS."
  (let ((todo-keywords (or todo-keywords org-todo-keywords-1)))
    (member (nth 2 (org-heading-components)) todo-keywords)))

(defun pjs-org-task-p ()
  "A task is an entry with a TODO cookie and no sub-tasks."
  (and (pjs-org-todo-p)
       (not (pjs-org-sub-task-p))))

(defun pjs-org-todo-regex ()
  "A regular expression matching any entry with a TODO cookie."
  (concat "^[*]+ \\(?:"
          (string-join org-todo-keywords-1 "\\|")
          "\\) .*"))

(defun pjs-org-sub-task-p (&optional regex)
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (regex (or regex (pjs-org-todo-regex))))
    (save-excursion
      (end-of-line)
      (when (< (point) subtree-end)
        (re-search-forward regex subtree-end t)))))

(defun pjs-org-project-entry-p ()
  (and (string-equal "TODO" (org-get-category))
       (not (pjs-org-todo-p))))

(defun pjs-org-project-p ()
  (save-restriction
    (widen)
    (or (pjs-org-project-entry-p)
        (and (pjs-org-todo-p)
             (pjs-org-sub-task-p)))))

(defun pjs-org-done-todo-regex ()
  "A regular expression matching any entry with a done TODO cookie."
  (concat "^[*]+ \\(?:"
          (string-join org-done-keywords "\\|")
          "\\) .*"))

(defun pjs-org-not-done-todo-regex ()
  "A regular expression matching any entry with a not done TODO cookie."
  (concat "^[*]+ \\(?:"
          (string-join org-not-done-keywords "\\|")
          "\\) .*"))

(defun pjs-org-active-project-p ()
  (save-restriction
    (widen)
    (or (and (pjs-org-project-entry-p)
             (or (pjs-org-sub-task-p (pjs-org-not-done-todo-regex))
                 (not (pjs-org-sub-task-p (pjs-org-done-todo-regex)))))
        (or (and (pjs-org-todo-p)
                 (pjs-org-sub-task-p (pjs-org-not-done-todo-regex)))))))

(defun pjs-org-stuck-project-p (start end)
  "A stuck project has no task and no activity between START and END days ago."
  (save-restriction
    (widen)
    (when (and (pjs-org-project-p)
               (not (pjs-org-sub-task-p (pjs-org-not-done-todo-regex))))
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (stuck-p t)
            (start-d (org-read-date t t (concat "-" (number-to-string start) "d")))
            (end-d (when end
                     (org-read-date t t (concat "-" (number-to-string end) "d")))))
        (save-excursion
          (while (and stuck-p (< (point) subtree-end))
            (if (re-search-forward (org-re-timestamp 'all) subtree-end t)
                (let ((dt (org-read-date t t (match-string 1))))
                  (when (or (time-less-p start-d dt)
                            (and end-d
                                 (time-less-p dt end-d)))
                    (setq stuck-p nil)))
              (goto-char subtree-end)))
          stuck-p)))))

(defun pjs-org-has-priority-p (priority)
  ;; Source: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  (let ((pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (= pri-value pri-current)))

(defun pjs-org-capture-to-heading ()
  (let* ((link (plist-get org-capture-plist :annotation))
         (heading (org-find-exact-headline-in-buffer link (current-buffer) t)))
    (if heading
        (goto-char heading)
      (progn
        (goto-char (point-max))
        (org-insert-heading nil nil t)
        (insert link)))))

(defun pjs-ensure-ending-newline ()
  "Add a newline at the end of the buffer if there isn't any."
  ;; from https://emacs.stackexchange.com/questions/38754/capture-template-like-org-journal
  (save-excursion
    (save-restriction
      (goto-char (1- (point-max)))
      (if (not (looking-at "\n"))
          (progn
            (goto-char (point-max))
            (insert "\n"))))))

(defun pjs-org-agenda-sort-created (a b)
  (let* ((a-marker (get-text-property 0 'org-marker a))
         (b-marker (get-text-property 0 'org-marker b))
         (created-a (let ((created-a (org-entry-get a-marker "CREATED")))
                      (when created-a
                        (org-read-date t t created-a))))
         (created-b (let ((created-b (org-entry-get b-marker "CREATED")))
                      (when created-b
                        (org-read-date t t created-b)))))
    (cond
     ((and created-a created-b)
      (if (time-less-p created-a created-b)
          -1
        1))
     (created-a
      -1)
     (created-b
      1)
     (t 0))))

(defun pjs-org-find-child-heading (child)
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (found))
      (while (and (not found)
                  (< (point) subtree-end)
                  (outline-next-heading))
        (setq found (equal (nth 4 (org-heading-components)) child)))
      found)))

(defun pjs-org-ensure-journal-heading ()
  (when (not (pjs-org-find-child-heading "Journal"))
    (left-char)
    (org-insert-subheading '(4))
    (insert "Journal")
    (org-set-tags ":JOURNAL:")))

(defun pjs-org-capture-journal ()
  (if (org-clocking-p)
      (org-clock-goto)
    (helm-org-agenda-files-headings))
  (pjs-org-ensure-journal-heading))

(defun pjs-org-agenda ()
  "Dispatch agenda commands, or switch to existing agenda buffer."
  (interactive)
  (if (get-buffer "*Org Agenda*")
      (switch-to-buffer "*Org Agenda*")
    (org-agenda)))

(defun pjs-org-insert-created-property ()
  (org-set-property "CREATED"
                    (format-time-string (org-time-stamp-format t t)
                                        (current-time))))

(defun pjs-org-agenda-restrict-to-heading ()
  "Restrict agenda to completed heading."
  (interactive)
  (when (save-window-excursion
          (helm-org-agenda-files-headings)
          (when (derived-mode-p 'org-mode)
            (org-agenda-set-restriction-lock)
            t))
    (org-agenda-redo-all)))

(defun pjs-org-narrow-to-parent ()
  "Narrow buffer to parent of current heading."
  (interactive)
  (when (buffer-narrowed-p)
    (widen))
  (ignore-errors
    (outline-up-heading 1 t)
    (org-narrow-to-subtree)))

(defun pjs-org-valid-refile-target-p ()
  (if (or (pjs-org-todo-p org-not-done-keywords)
          (pjs-org-active-project-p))
      t
    (outline-next-heading)
    nil))

(provide 'pjs-org)
;;; pjs-org.el ends here
