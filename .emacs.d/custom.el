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
 '(dired-clean-confirm-killing-deleted-buffers nil)
 '(doc-view-continuous t)
 '(emacs-lisp-docstring-fill-column t)
 '(emacs-lisp-mode-hook (quote (eldoc-mode checkdoc-minor-mode paredit-mode)))
 '(exwm-init-hook (quote (exwm-randr--init my/start-initial-programs)))
 '(exwm-input-global-keys
   (quote
    (([8388722]
      . exwm-reset)
     ([8388727]
      . exwm-workspace-switch)
     ([8388646]
      lambda
      (command)
      (interactive
       (list
        (read-shell-command "$ ")))
      (start-process-shell-command command nil command))
     ([8388657]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 0))
     ([8388658]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 1))
     ([8388659]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 2))
     ([8388660]
      lambda nil
      (interactive)
      (exwm-workspace-switch-create 3)))))
 '(exwm-input-prefix-keys
   (quote
    ("" "" ""
     [134217848]
     [134217824]
     [134217766]
     [134217786])))
 '(exwm-layout-show-all-buffers t)
 '(exwm-manage-configurations
   (quote
    (((string-equal exwm-class-name "Xfce4-terminal")
      workspace 3)
     ((string-equal exwm-class-name "Firefox")
      simulation-keys
      (("" .
        [left])
       ("" .
        [right])
       ("" .
        [up])
       ("" .
        [down])
       ("" .
        [home])
       ("" .
        [end])
       ([134217846]
        .
        [prior])
       ("" .
        [next])
       ("" .
        [delete])
       ("" .
        [S-end delete])
       ([134217847]
        . "")
       ("" . "")
       ("" . "")
       ([134217751]
        . ""))
      workspace 2))))
 '(exwm-replace t)
 '(exwm-update-class-hook (quote (my/set-exwm-buffer-name-to-class)))
 '(exwm-workspace-index-map (lambda (n) (number-to-string (1+ n))))
 '(exwm-workspace-number 4)
 '(exwm-workspace-show-all-buffers t)
 '(exwm-workspace-switch-create-limit 4)
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
    (("t" "Todo" entry
      (file "~/Dropbox/orgzly/inbox.org")
      "* TODO %^{Title}
:PROPERTIES:
:CREATED: %U
:END:
%?")
     ("p" "Project" entry
      (file "~/Dropbox/orgzly/projects.org")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:" :prepend t)
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
     ("y" "org-protocol-link" entry
      (file "~/Dropbox/orgzly/inbox.org")
      "* %? [[%:link][%:description]]
:PROPERTIES:
:CREATED: %U
:END:" :immediate-finish t)
     ("z" "org-protocol" entry
      (file "~/Dropbox/orgzly/inbox.org")
      "* %^{Title}
:PROPERTIES:
:CREATED: %U
:END:
Source: %u, [[%:link][%:description]]
#+BEGIN_QUOTE
%i
#+END_QUOTE
%?"))))
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
    (flycheck-clj-kondo better-defaults cider company exwm gnu-elpa-keyring-update helm-ag helm-projectile magit markdown-mode org-bullets paredit pinentry typo use-package visual-fill-column writegood-mode)))
 '(prog-mode-hook
   (quote
    (flyspell-prog-mode linum-mode whitespace-mode company-mode column-number-mode flycheck-mode)))
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
 '(sql-input-ring-file-name "~/.sql-mode-history")
 '(whitespace-style
   (quote
    (face trailing tabs lines-tail newline empty indentation::space))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
