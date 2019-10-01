(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources (quote ("~/.netrc.gpg")))
 '(cider-auto-jump-to-error nil)
 '(cider-auto-select-error-buffer nil)
 '(cider-auto-select-test-report-buffer nil)
 '(cider-debug-prompt (quote minibuffer))
 '(cider-docview-fill-column 80)
 '(cider-eldoc-display-context-dependent-info t)
 '(cider-jdk-src-paths (quote ("~/.cache/openjdk-8u192b26/")))
 '(cider-preferred-build-tool (quote lein))
 '(cider-prompt-for-symbol nil)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-history-file ".cider-history")
 '(cider-repl-history-size 1000)
 '(cider-repl-pop-to-buffer-on-connect (quote display-only))
 '(cider-repl-print-level 10)
 '(cider-repl-use-pretty-printing t)
 '(cider-save-file-on-load t)
 '(clojure-docstring-fill-column 80)
 '(doc-view-continuous t)
 '(emacs-lisp-docstring-fill-column t)
 '(exwm-layout-show-all-buffers t)
 '(exwm-replace t)
 '(exwm-workspace-show-all-buffers t)
 '(fill-column 80)
 '(inhibit-startup-screen t)
 '(linum-format "%d ")
 '(org-agenda-auto-exclude-function (quote bh/org-auto-exclude-function))
 '(org-agenda-compact-blocks t)
 '(org-agenda-custom-commands
   (quote
    (("N" "Notes" tags "NOTE"
      ((org-agenda-overriding-header "Notes")
       (org-tags-match-list-sublevels t)))
     ("h" "Habits" tags-todo "STYLE=\"habit\""
      ((org-agenda-overriding-header "Habits")
       (org-agenda-sorting-strategy
        (quote
         (todo-state-down effort-up category-keep)))))
     (" " "Agenda"
      ((agenda "" nil)
       (tags "REFILE"
             ((org-agenda-overriding-header "Tasks to Refile")
              (org-tags-match-list-sublevels nil)))
       (tags-todo "-CANCELLED/!"
                  ((org-agenda-overriding-header "Stuck Projects")
                   (org-agenda-skip-function
                    (quote bh/skip-non-stuck-projects))
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (tags-todo "-HOLD-CANCELLED/!"
                  ((org-agenda-overriding-header "Projects")
                   (org-agenda-skip-function
                    (quote bh/skip-non-projects))
                   (org-tags-match-list-sublevels
                    (quote indented))
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (tags-todo "-CANCELLED/!NEXT"
                  ((org-agenda-overriding-header
                    (concat "Project Next Tasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function
                    (quote bh/skip-projects-and-habits-and-single-tasks))
                   (org-tags-match-list-sublevels t)
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-sorting-strategy
                    (quote
                     (todo-state-down effort-up category-keep)))))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header
                    (concat "Project Subtasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function
                    (quote bh/skip-non-project-tasks))
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                  ((org-agenda-overriding-header
                    (concat "Standalone Tasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function
                    (quote bh/skip-project-tasks))
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (tags-todo "-CANCELLED+WAITING|HOLD/!"
                  ((org-agenda-overriding-header
                    (concat "Waiting and Postponed Tasks"
                            (if bh/hide-scheduled-and-waiting-next-tasks "" " (including WAITING and SCHEDULED tasks)")))
                   (org-agenda-skip-function
                    (quote bh/skip-non-tasks))
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                   (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
       (tags "-REFILE/"
             ((org-agenda-overriding-header "Tasks to Archive")
              (org-agenda-skip-function
               (quote bh/skip-non-archivable-tasks))
              (org-tags-match-list-sublevels nil))))
      nil))))
 '(org-agenda-diary-file "~/org/journal.org")
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-files
   (quote
    ("~/org/refile.org" "~/org/projects.org" "~/org/projects")))
 '(org-agenda-persistent-filter t)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-archive-location "%s_archive::* Archived Tasks")
 '(org-capture-templates
   (quote
    (("t" "todo" entry
      (file "~/org/refile.org")
      "* TODO %?
%U
" :clock-in t :clock-resume t)
     ("r" "respond" entry
      (file "~/org/refile.org")
      "* NEXT Respond to %:from on %:subject
SCHEDULED: %t
%U
" :immediate-finish t :clock-in t :clock-resume t)
     ("n" "note" entry
      (file "~/org/notes.org")
      "* %?
%U
" :clock-in t :clock-resume t)
     ("j" "journal entry" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?
%U
" :clock-in t :clock-resume t)
     ("w" "org-protocol" entry
      (file "~/org/refile.org")
      "* TODO Review %c
%U
" :immediate-finish t)
     ("m" "Meeting" entry
      (file "~/org/refile.org")
      "* MEETING with %? :MEETING:
%U" :clock-in t :clock-resume t)
     ("p" "Phone call" entry
      (file "~/org/refile.org")
      "* PHONE %? :PHONE:
%U" :clock-in t :clock-resume t)
     ("h" "Habit" entry
      (file "~/org/refile.org")
      "* NEXT %?
%U
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")
:PROPERTIES:
:STYLE: habit
:REPEAT_TO_STATE: NEXT
:END:
"))))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-default-notes-file "~/org/refile.org")
 '(org-directory "~/org")
 '(org-edit-src-content-indentation 0)
 '(org-enforce-todo-dependencies t)
 '(org-fast-tag-selection-single-key t)
 '(org-indirect-buffer-display (quote current-window))
 '(org-list-allow-alphabetical t)
 '(org-list-demote-modify-bullet
   (quote
    (("+" . "-")
     ("*" . "-")
     ("1." . "-")
     ("1)" . "-")
     ("A)" . "-")
     ("B)" . "-")
     ("a)" . "-")
     ("b)" . "-")
     ("A." . "-")
     ("B." . "-")
     ("a." . "-")
     ("b." . "-"))))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-outline-path-complete-in-steps nil)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-target-verify-function (quote bh/verify-refile-target))
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 9)
     (org-agenda-files :maxlevel . 9))))
 '(org-refile-use-outline-path (quote file))
 '(org-special-ctrl-a/e t)
 '(org-special-ctrl-k t)
 '(org-speed-commands-user
   (quote
    (("0" . ignore)
     ("1" . ignore)
     ("2" . ignore)
     ("3" . ignore)
     ("4" . ignore)
     ("5" . ignore)
     ("6" . ignore)
     ("7" . ignore)
     ("8" . ignore)
     ("9" . ignore)
     ("a" . ignore)
     ("d" . ignore)
     ("h" . bh/hide-other)
     ("i" progn
      (forward-char 1)
      (call-interactively
       (quote org-insert-heading-respect-content)))
     ("k" . org-kill-note-or-show-branches)
     ("l" . ignore)
     ("m" . ignore)
     ("q" . bh/show-org-agenda)
     ("r" . ignore)
     ("s" . org-save-all-org-buffers)
     ("w" . org-refile)
     ("x" . ignore)
     ("y" . ignore)
     ("z" . org-add-note)
     ("A" . ignore)
     ("B" . ignore)
     ("E" . ignore)
     ("F" . bh/restrict-to-file-or-follow)
     ("G" . ignore)
     ("H" . ignore)
     ("J" . org-clock-goto)
     ("K" . ignore)
     ("L" . ignore)
     ("M" . ignore)
     ("N" . bh/narrow-to-org-subtree)
     ("P" . bh/narrow-to-org-project)
     ("Q" . ignore)
     ("R" . ignore)
     ("S" . ignore)
     ("T" . bh/org-todo)
     ("U" . bh/narrow-up-one-org-level)
     ("V" . ignore)
     ("W" . bh/widen)
     ("X" . ignore)
     ("Y" . ignore)
     ("Z" . ignore))))
 '(org-stuck-projects (quote ("" nil nil "")))
 '(org-tag-alist
   (quote
    ((:startgroup)
     ("@errand" . 101)
     ("@office" . 111)
     ("@home" . 72)
     (:endgroup)
     ("WAITING" . 119)
     ("HOLD" . 104)
     ("PERSONAL" . 80)
     ("WORK" . 87)
     ("FARM" . 70)
     ("ORG" . 79)
     ("NORANG" . 78)
     ("crypt" . 69)
     ("NOTE" . 110)
     ("CANCELLED" . 99)
     ("FLAGGED" . 63)
     ("REFILE" . 114))))
 '(org-todo-keyword-faces
   (quote
    (("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold)
     ("PHONE" :foreground "forest green" :weight bold))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))
 '(org-todo-state-tags-triggers
   (quote
    (("CANCELLED"
      ("CANCELLED" . t))
     ("WAITING"
      ("WAITING" . t))
     ("HOLD"
      ("WAITING")
      ("HOLD" . t))
     (done
      ("WAITING")
      ("HOLD"))
     ("TODO"
      ("WAITING")
      ("CANCELLED")
      ("HOLD"))
     ("NEXT"
      ("WAITING")
      ("CANCELLED")
      ("HOLD"))
     ("DONE"
      ("WAITING")
      ("CANCELLED")
      ("HOLD")))))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-use-speed-commands t)
 '(org-yank-adjusted-subtrees t)
 '(package-selected-packages
   (quote
    (pinentry company typo org-bullets markdown-mode writegood-mode visual-fill-column use-package paredit magit helm-projectile helm-ag exwm cider better-defaults)))
 '(safe-local-variable-values
   (quote
    ((elisp-lint-indent-specs
      (if-let* . 2)
      (when-let* . 1)
      (let* . defun)
      (nrepl-dbind-response . 2)
      (cider-save-marker . 1)
      (cider-propertize-region . 1)
      (cider-map-repls . 1)
      (cider--jack-in . 1)
      (cider--make-result-overlay . 1)
      (multiline-comment-handler . defun)
      (insert-label . defun)
      (insert-align-label . defun)
      (insert-rect . defun)
      (cl-defun . 2)
      (with-parsed-tramp-file-name . 2)
      (thread-first . 1)
      (thread-last . 1))
     (checkdoc-package-keywords-flag)
     (whitespace-line-column)
     (eval ignore-errors
           (require
            (quote whitespace))
           (whitespace-mode 0)
           (whitespace-mode 1))
     (clojure-test-ns-segment-position . 1))))
 '(show-paren-delay 0.25)
 '(whitespace-style
   (quote
    (face trailing tabs lines-tail newline empty indentation::space))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
