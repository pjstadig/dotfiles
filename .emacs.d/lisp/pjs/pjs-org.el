(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-drill)

(defun pjs/org-agenda-skip-entry-if (&rest conditions)
  (pjs/org-agenda-skip-if nil conditions))

(defun pjs/org-agenda-skip-subtree-if (&rest conditions)
  (pjs/org-agenda-skip-if 't conditions))

(defun pjs/org-agenda-skip-if (subtree conditions)
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
habit         Check if there is a STYLE property with value \"habit\"
nothabit      Check if there is not a STYLE property with value \"habit\"
project       Check if project
notproject    Check if not project

The regexp is taken from the conditions list, it must come right after
the `regexp' or `notregexp' element.

`todo' and `nottodo' accept as an argument a list of todo
keywords, which may include \"*\" to match any todo keyword.

    (org-agenda-skip-entry-if \\='todo \\='(\"TODO\" \"WAITING\"))

would skip all entries with \"TODO\" or \"WAITING\" keywords.

Instead of a list, a keyword class may be given.  For example:

    (org-agenda-skip-entry-if \\='nottodo \\='done)

would skip entries that haven't been marked with any of \"DONE\"
keywords.  Possible classes are: `todo', `done', `any'.

If any of these conditions is met, this function returns the end point of
the entity, causing the search to continue from there.  This is a function
that can be put into `org-agenda-skip-function' for the duration of a command."
  (org-back-to-heading t)
  (let* ((beg (point))
         (end (if subtree (save-excursion (org-end-of-subtree t) (point))
                (org-entry-end-position)))
         (planning-end (if subtree end (line-end-position 2)))
         m)
    (and
     (or (and (setq m (memq 'tag conditions))
              (pjs/org-has-tag-p (nth 1 m)))
         (and (setq m (memq 'nottag conditions))
              (not (pjs/org-has-tag-p (nth 1 m))))
         (and (memq 'habit conditions)
              (pjs/org-is-habit-p))
         (and (memq 'nothabit conditions)
              (not (pjs/org-is-habit-p)))
         (and (memq 'project conditions)
              (bh/is-project-p))
         (and (memq 'notproject conditions)
              (not (bh/is-project-p)))
         (and (setq m (memq 'priority conditions))
              (pjs/org-has-priority-p (nth 1 m)))
         (and (setq m (memq 'notpriority conditions))
              (not (pjs/org-has-priority-p (nth 1 m))))
         (org-agenda-skip-if subtree conditions))
     end)))

(defun pjs/org-has-tag-p (tag)
  (member tag (org-get-tags)))

(defun pjs/org-is-habit-p ()
  (string= (org-entry-get nil "STYLE") "habit"))

(defun pjs/org-has-priority-p (priority)
  ;; Source: https://blog.aaronbieber.com/2016/09/24/an-agenda-for-life-with-org-mode.html
  (let ((pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (= pri-value pri-current)))

(defun pjs/org-capture-to-heading ()
  ;; :annotation is the link
  (let* ((link (plist-get org-capture-plist :annotation))
         (heading (org-find-exact-headline-in-buffer link (current-buffer) t)))
    (if heading
        (goto-char heading)
      (progn
        (goto-char (point-max))
        (org-insert-heading nil nil t)
        (insert link)))))

(defun pjs/ensure-ending-newline ()
  "Add a newline at the end of the buffer if there isn't any."
  ;; from https://emacs.stackexchange.com/questions/38754/capture-template-like-org-journal
  (save-excursion
    (save-restriction
      (goto-char (1- (point-max)))
      (if (not (looking-at "\n"))
          (progn
            (goto-char (point-max))
            (insert "\n"))))))

(defun pjs/org-agenda-sort-created (a b)
  (let* ((a-marker (get-text-property 0 'org-marker a))
         (b-marker (get-text-property 0 'org-marker b))
         (created-a (org-entry-get a-marker "CREATED"))
         (created-b (org-entry-get b-marker "CREATED")))
    (cond
     ((and created-a
           (or (null created-b)
               (string-greaterp created-a created-b)))
      1)
     ((and (or (null created-a)
               (string-lessp created-a created-b))
           created-b)
      -1)
     (t 0))))

(eval-after-load 'org-drill
  (progn (defun pjs/org-drill-hide-comments ()
           "Hide comments."
           (save-excursion
             (while (re-search-forward "^#[^+].*$" nil t)
               (org-drill-hide-region (match-beginning 0) (match-end 0)))))

         (advice-add 'org-drill-hide-comments :override 'pjs/org-drill-hide-comments)))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(provide 'pjs-org)
