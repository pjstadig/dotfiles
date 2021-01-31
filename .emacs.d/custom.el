(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(auth-sources '("~/.netrc.gpg"))
 '(auto-save-visited-mode t)
 '(backup-directory-alist '(("." . "~/.emacs.d/backups")))
 '(calendar-latitude 38.0718912)
 '(calendar-longitude -78.7225072)
 '(checkdoc-force-docstrings-flag nil)
 '(cider-auto-jump-to-error nil)
 '(cider-auto-select-error-buffer nil)
 '(cider-auto-select-test-report-buffer nil)
 '(cider-debug-prompt 'minibuffer)
 '(cider-docview-fill-column 80)
 '(cider-eldoc-display-context-dependent-info t)
 '(cider-preferred-build-tool 'lein)
 '(cider-print-options '(("length" 20) ("level" 10) ("right-margin" 90)))
 '(cider-prompt-for-symbol nil)
 '(cider-repl-display-help-banner nil)
 '(cider-repl-history-file "~/.cider-history")
 '(cider-repl-pop-to-buffer-on-connect nil)
 '(cider-save-file-on-load t)
 '(clubhouse-backend-directory "~/src/backend")
 '(completion-styles '(flex))
 '(dired-clean-confirm-killing-deleted-buffers nil)
 '(doc-view-continuous t)
 '(emacs-lisp-docstring-fill-column t)
 '(epg-pinentry-mode 'loopback)
 '(exwm-manage-configurations
   '(((string-equal exwm-class-name "Xfce4-terminal")
      workspace 0)
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
      workspace 1)
     ((string-equal exwm-class-name "Google-chrome")
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
      workspace 1)
     ((string-equal exwm-class-name "Anki")
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
        . "")))))
 '(exwm-randr-workspace-monitor-plist '(9 "HDMI1"))
 '(exwm-replace t)
 '(exwm-update-class-hook '(pjs-set-exwm-buffer-name-to-class))
 '(exwm-workspace-index-map '(lambda (n) (number-to-string (1+ n))))
 '(exwm-workspace-number 10)
 '(helm-org-format-outline-path t)
 '(inhibit-startup-screen t)
 '(linum-format "%d ")
 '(menu-bar-mode nil)
 '(mouse-yank-at-point t)
 '(org-agenda-category-icon-alist
   '(("personal" "~/org/personal/icon.png" nil nil :ascent center :height 16)
     ("work" "~/org/work/icon.png" nil nil :ascent center :height 16)))
 '(org-agenda-cmp-user-defined 'pjs-org-agenda-sort-created)
 '(org-agenda-compact-blocks t)
 '(org-agenda-custom-commands
   '(("a" . "Agendas daily, weekly, or otherwise")
     ("ad" "Daily agenda; habits, priorities, reading, watching, listening"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-skip-function
                 '(or
                   (pjs-org-agenda-skip-subtree-if 'tag
                                                   '("REVIEW"))
                   (pjs-org-agenda-skip-entry-if 'todo 'done)))))
       (tags-todo "*"
                  ((org-agenda-overriding-header "==Priority tasks================================================================================================================================================================================")
                   (org-agenda-skip-function
                    '(pjs-org-agenda-skip-entry-if 'notpriority 65 'tag "WAITING" 'scheduled 'deadline 'active))))
       (tags "*"
             ((org-agenda-overriding-header "==Reading queue=================================================================================================================================================================================")
              (org-agenda-max-entries 10)
              (org-agenda-skip-function
               '(pjs-org-agenda-skip-entry-if 'nottag "TOREAD"))))
       (tags "TOWATCH"
             ((org-agenda-overriding-header "==Watch queue===================================================================================================================================================================================")
              (org-agenda-max-entries 10)
              (org-agenda-skip-function
               '(pjs-org-agenda-skip-entry-if 'nottag "TOWATCH"))))
       (tags "*"
             ((org-agenda-overriding-header "==Listen queue==================================================================================================================================================================================")
              (org-agenda-max-entries 10)
              (org-agenda-skip-function
               '(pjs-org-agenda-skip-entry-if 'nottag "TOLISTEN")))))
      nil nil)
     ("aw" "Weekly review agenda; agenda for past week, waiting, archivable, stuck projects, someday, maybe, hold"
      ((agenda ""
               ((org-agenda-span 'week)
                (org-agenda-start-day "-7d")
                (org-agenda-start-with-log t)
                (org-agenda-log-mode-items
                 '(closed clock state))
                (org-agenda-include-inactive-timestamps t)
                (org-agenda-start-on-weekday nil)))
       (tags "*"
             ((org-agenda-overriding-header "==Waiting=======================================================================================================================================================================================")
              (org-agenda-skip-function
               '(pjs-org-agenda-skip-entry-if 'nottag "WAITING" 'todo 'done))))
       (tags "CLOSED<\"<-7d>\"|TODO=\"DONE\"+CLOSED=\"\""
             ((org-agenda-overriding-header "==Archivable====================================================================================================================================================================================")
              (org-agenda-skip-function
               '(pjs-org-agenda-skip-subtree-if 'active))))
       (tags "*"
             ((org-agenda-overriding-header "==Stuck projects (15+ days)=====================================================================================================================================================================")
              (org-agenda-skip-function
               '(or
                 (pjs-org-agenda-skip-subtree-if 'tag
                                                 '("CANCELLED" "WAITING" "SOMEDAY" "MAYBE" "HOLD" "TOREAD" "TOLISTEN" "TOWATCH" "JOURNAL" "NOTE"))
                 (pjs-org-agenda-skip-entry-if 'notstuck 15 nil)))
              (org-agenda-max-entries 15)))
       (tags-todo "*"
                  ((org-agenda-overriding-header "==Someday=======================================================================================================================================================================================")
                   (org-agenda-skip-function
                    '(pjs-org-agenda-skip-entry-if 'nottag "SOMEDAY" 'project))))
       (tags-todo "*"
                  ((org-agenda-overriding-header "==Maybe=========================================================================================================================================================================================")
                   (org-agenda-skip-function
                    '(pjs-org-agenda-skip-entry-if 'nottag "MAYBE" 'project))))
       (tags-todo "*"
                  ((org-agenda-overriding-header "==Hold==========================================================================================================================================================================================")
                   (org-agenda-skip-function
                    '(pjs-org-agenda-skip-entry-if 'nottag '"HOLD" 'project)))))
      nil nil)
     ("i" "Inbox"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-skip-function
                 '(pjs-org-agenda-skip-subtree-if 'tag
                                                  '("NOTE" "REVIEW")
                                                  'todo 'done))))
       (tags "*"
             ((org-agenda-overriding-header "Inbox:")
              (org-agenda-skip-function
               '(and
                 (pjs-org-agenda-skip-entry-if 'nottag "IN" 'scheduled 'deadline)
                 (pjs-org-agenda-skip-entry-if 'nottag "FLAGGED"))))))
      nil)
     ("n" "Next tasks"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-skip-function
                 '(pjs-org-agenda-skip-subtree-if 'tag
                                                  '("NOTE" "REVIEW")
                                                  'todo 'done))))
       (tags "TODO=\"TODO\""
             ((org-agenda-overriding-header "Next tasks:")
              (org-agenda-skip-function
               '(pjs-org-agenda-skip-subtree-if 'habit 'scheduled 'deadline 'tag
                                                '("IN" "CANCELLED" "WAITING" "SOMEDAY" "MAYBE" "HOLD" "TOREAD" "TOLISTEN" "TOWATCH" "FLAGGED" "REVIEW" "NOTE" "SHOPPING")
                                                'project)))))
      nil)
     ("e" "Completed entries" tags "CLOSED<\"<-7d>\"|TODO=\"DONE\"+CLOSED=\"\""
      ((org-agenda-overriding-header "Completed entries:")
       (org-agenda-skip-function
        '(pjs-org-agenda-skip-entry-if 'active))))
     ("#" "Stuck projects"
      ((tags "*"
             ((org-agenda-overriding-header "Stuck projects (>90 days):")
              (org-agenda-skip-function
               '(pjs-org-agenda-skip-entry-if 'tag
                                              '("CANCELLED" "WAITING" "SOMEDAY" "MAYBE" "HOLD" "TOREAD" "TOLISTEN" "TOWATCH" "JOURNAL" "NOTE")
                                              'notproject 'notstuck 90 nil 'stuck 30 89 'stuck 7 29))))
       (tags "*"
             ((org-agenda-overriding-header "Stuck projects (30-90 days):")
              (org-agenda-skip-function
               '(pjs-org-agenda-skip-entry-if 'tag
                                              '("CANCELLED" "WAITING" "SOMEDAY" "MAYBE" "HOLD" "TOREAD" "TOLISTEN" "TOWATCH" "JOURNAL" "NOTE")
                                              'notproject 'stuck 90 nil 'notstuck 30 89 'stuck 7 29))))
       (tags "*"
             ((org-agenda-overriding-header "Stuck projects (7-30 days):")
              (org-agenda-skip-function
               '(pjs-org-agenda-skip-entry-if 'tag
                                              '("CANCELLED" "WAITING" "SOMEDAY" "MAYBE" "HOLD" "TOREAD" "TOLISTEN" "TOWATCH" "JOURNAL" "NOTE")
                                              'notproject 'stuck 90 nil 'stuck 30 89 'notstuck 7 29)))))
      nil nil)))
 '(org-agenda-diary-file "~/org/journal.org")
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-files
   '("~/org/work/in.org" "~/org/work/todo.org" "~/org/work/toread.org" "~/org/work/tolisten.org" "~/org/work/towatch.org" "~/org/work/review.org" "~/org/work/journal.org" "~/org/work/reference.org"))
 '(org-agenda-insert-diary-strategy 'date-tree-last)
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   '((agenda . " %i %?-12t% s%b")
     (todo . " %i %b")
     (tags . " %i %b")
     (search . " %i %b")))
 '(org-agenda-sorting-strategy
   '((agenda habit-down time-up priority-down category-keep)
     (todo priority-down category-keep user-defined-up)
     (tags priority-down category-keep user-defined-up)
     (search category-keep user-defined-up)))
 '(org-agenda-tags-column -96)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-window-setup 'only-window)
 '(org-archive-file-header-format "")
 '(org-archive-location "~/org/archive.org::")
 '(org-attach-id-dir "attachments/")
 '(org-babel-clojure-backend 'cider)
 '(org-capture-prepare-finalize-hook '(pjs-ensure-ending-newline))
 '(org-capture-templates
   '(("t" "Todo" entry
      (file "in.org")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:")
     ("r" "Review" entry
      (file "review.org")
      "* %? :REVIEW:
:PROPERTIES:
:CREATED: %U
:END:")
     ("n" "Note" entry
      (file "reference.org")
      "* %?
:PROPERTIES:
:CREATED: %U
:END:")
     ("l" "Linked")
     ("lt" "Todo" entry
      (file "in.org")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:
#+BEGIN_QUOTE
%i
#+END_QUOTE

From: %a")
     ("ln" "Note" entry
      (file "reference.org")
      "* %?
:PROPERTIES:
:CREATED: %U
:END:
#+BEGIN_QUOTE
%i
#+END_QUOTE

From: %a")
     ("j" "Journal")
     ("jj" "Journal to file" entry
      (file+olp+datetree "journal.org")
      "* %?
:PROPERTIES:
:CREATED: %U
:END:")
     ("jc" "Journal to clocked entry" entry #'pjs-org-capture-journal "* %?
:PROPERTIES:
:CREATED: %T
:END:")
     ("v" "Event" entry
      (file "todo.org")
      "* %^{Description}
%^T
:PROPERTIES:
:CREATED: %U
:END:" :immediate-finish t)
     ("y" "org-protocol-link" entry
      (file "toread.org")
      "* %? %a
:PROPERTIES:
:CREATED: %U
:END:")
     ("z" "org-protocol-quote" entry
      (file+function "review.org" pjs-org-capture-to-heading)
      "* %:description :REVIEW:
:PROPERTIES:
:CREATED: %U
:END:
#+BEGIN_QUOTE
%i
#+END_QUOTE
%?" :immediate-finish t)))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-default-notes-file "~/org/in.org")
 '(org-directory "~/org/work")
 '(org-drill-left-cloze-delimiter "{")
 '(org-drill-question-tag "REVIEW")
 '(org-drill-right-cloze-delimiter "}")
 '(org-drill-save-buffers-after-drill-sessions-p nil)
 '(org-drill-scope 'agenda)
 '(org-edit-src-content-indentation 0)
 '(org-emphasis-alist
   '(("*" bold)
     ("/" italic)
     ("_" underline)
     ("=" org-verbatim verbatim)
     ("~" org-code verbatim)
     ("+" org-strike-through)))
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-fast-tag-selection-single-key t)
 '(org-footnote-auto-adjust t)
 '(org-footnote-section nil)
 '(org-goto-interface 'outline-path-completion)
 '(org-habit-following-days 1)
 '(org-habit-graph-column 97)
 '(org-habit-preceding-days 14)
 '(org-hide-leading-stars t)
 '(org-id-link-to-org-use-id t)
 '(org-indirect-buffer-display 'current-window)
 '(org-list-allow-alphabetical t)
 '(org-list-demote-modify-bullet '(("-" . "+") ("+" . "*") ("*" . "-")))
 '(org-log-done 'time)
 '(org-log-into-drawer t)
 '(org-log-reschedule 'note)
 '(org-outline-path-complete-in-steps nil)
 '(org-pretty-entities t)
 '(org-refile-allow-creating-parent-nodes 'confirm)
 '(org-refile-target-verify-function 'pjs-org-valid-refile-target-p)
 '(org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
 '(org-refile-use-outline-path 'full-file-path)
 '(org-special-ctrl-a/e 'reversed)
 '(org-special-ctrl-k t)
 '(org-src-ask-before-returning-to-edit-buffer nil)
 '(org-startup-folded 'content)
 '(org-startup-indented t)
 '(org-stuck-projects
   '("+LEVEL=1+TODO=\"TODO\"+CATEGORY=\"Projects\"-SOMEDAY-MAYBE"
     ("NEXT")
     ("ROUTINE")
     ""))
 '(org-tag-alist
   '(("WAITING" . 119)
     ("HOLD" . 104)
     ("MOBILE" . 98)
     ("NOTE" . 110)
     ("CANCELLED" . 99)
     ("FLAGGED" . 63)
     ("SHOPPING" . 112)
     ("REVIEW" . 114)
     ("SOMEDAY" . 115)
     ("MAYBE" . 109)))
 '(org-tags-column 0)
 '(org-tags-exclude-from-inheritance '("FLAGGED"))
 '(org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold)))
 '(org-todo-keywords
   '((sequence "TODO(t)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
 '(org-todo-state-tags-triggers
   '(("CANCELLED"
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
     ("DONE"
      ("WAITING")
      ("CANCELLED")
      ("HOLD"))))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-use-fast-todo-selection 'expert)
 '(org-use-speed-commands t)
 '(org-use-sub-superscripts '{})
 '(org-yank-adjusted-subtrees t)
 '(package-selected-packages
   '(cider cljstyle-mode clojure-mode clojure-mode-extra-font-locking company deft exec-path-from-shell exwm exwm-edit flycheck-clj-kondo ghub gnu-elpa-keyring-update helm helm-ag helm-core helm-org helm-projectile magit markdown markdown-mode ob-http org org-autolist org-bullets org-drill paredit pdf-tools pinentry typo use-package visual-fill-column writegood-mode xelb))
 '(require-final-newline t)
 '(safe-local-variable-values
   '((org-archive-location . "~/org/work/archive.org::")
     (org-archive-location . "~/org/personal/archive.org::")
     (org-id-link-to-org-use-id quote use-existing)
     (elisp-lint-indent-specs
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
           (require 'whitespace)
           (whitespace-mode 0)
           (whitespace-mode 1))
     (clojure-test-ns-segment-position . 1)))
 '(save-interprogram-paste-before-kill t)
 '(save-place-mode t)
 '(sh-basic-offset 2)
 '(show-paren-delay 0.25)
 '(split-window-preferred-function 'visual-fill-column-split-window-sensibly)
 '(sql-input-ring-file-name "~/.sql-mode-history")
 '(text-mode-hook
   '(pjs-configure-text-mode-fill-column flyspell-mode text-mode-hook-identify))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(visible-bell t)
 '(whitespace-line-column nil)
 '(whitespace-style
   '(face trailing tabs lines-tail newline empty indentation::space))
 '(zk-directory "~/org/zk/")
 '(zk-extensions '("org" "txt" "text" "md" "markdown"))
 '(zk-strip-summary-regexp "\\([
        ]\\|^#\\+[[:upper:]_]+:.*$\\|^:[^:]+:.*$\\)"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "CTDB" :family "Fira Mono"))))
 '(clubhouse-backend-given-when-then-face ((t (:inherit font-lock-doc-face :weight bold))))
 '(fixed-pitch ((t (:height 120 :family "Fira Mono"))))
 '(italic ((t (:slant italic))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-checkbox ((t (:inherit fixed-pitch :weight bold :height 0.9))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-date ((t (:inherit fixed-pitch :foreground "Purple" :underline t))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-drawer ((t (:inherit fixed-pitch :foreground "Blue1"))))
 '(org-hide ((t (:inherit fixed-pitch :foreground "white"))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:inherit link :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-keyword-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "Blue1"))))
 '(org-tag ((t (:inherit fixed-pitch :weight bold))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:height 130 :family "Fira Sans")))))
