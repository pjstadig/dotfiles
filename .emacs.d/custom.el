(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-clean-confirm-killing-deleted-buffers nil)
 '(org-agenda-breadcrumbs-separator "/")
 '(org-agenda-compact-blocks t)
 '(org-agenda-custom-commands
   (quote
    (("n" "Next actions"
      ((tags-todo "+TODO=\"WAITING\""
                  ((org-agenda-overriding-header "Waiting")))
       (tags-todo "+TODO=\"NEXT\"-SOMEDAY-MAYBE-SCHEDULED>\"<now>\"|-SOMEDAY-MAYBE+SCHEDULED<=\"<now>\"|+CATEGORY=\"Standalone\"+LEVEL=1-SOMEDAY-MAYBE-SCHEDULED>\"<now>\""
                  ((org-agenda-overriding-header "Next actions"))))
      nil)
     ("d" "Daily agenda and all TODOs"
      ((agenda ""
               ((org-agenda-span
                 (quote day))
                (org-agenda-skip-function
                 (quote
                  (pjs-org-agenda-skip-subtree-if
                   (quote tag)
                   "NOTE")))))
       (tags "+FLAGGED|CATEGORY=\"IN\""
             ((org-agenda-overriding-header "Inbox:")))
       (tags-todo "+TODO=\"NEXT\"-SOMEDAY-MAYBE|+CATEGORY=\"Standalone\"+LEVEL=1-SOMEDAY-MAYBE"
                  ((org-agenda-overriding-header "Next actions:")
                   (org-agenda-skip-function
                    (quote
                     (or
                      (pjs-org-agenda-skip-subtree-if
                       (quote habit))
                      (pjs-org-agenda-skip-subtree-if
                       (quote project))
                      (pjs-org-agenda-skip-entry-if
                       (quote scheduled)
                       (quote deadline)))))
                   (org-agenda-max-entries 10)))
       (tags-todo "-CANCELLED/!"
                  ((org-agenda-overriding-header "Stuck projects")
                   (org-agenda-skip-function
                    (quote bh/skip-non-stuck-projects))
                   (org-agenda-sorting-strategy
                    (quote
                     (category-keep)))))
       (tags "*"
             ((org-agenda-overriding-header "Reading queue:")
              (org-agenda-files
               (quote
                ("~/org/orgzly/read.org")))
              (org-agenda-prefix-format "  ")
              (org-agenda-max-entries 10)))
       (tags "CLOSED<\"<-30d>\""
             ((org-agenda-overriding-header "Entries to be archived"))))
      nil)
     ("#" "Stuck projects" stuck ""
      ((org-agenda-overriding-header "Stuck projects:")
       (org-agenda-prefix-format "  ")
       (org-agenda-sorting-strategy
        (quote
         (user-defined-up alpha-up)))
       (org-agenda-cmp-user-defined
        (quote pjs-org-agenda-sort-created))))
     ("t" "Tasks" tags-todo "-someday-toread-towatch-SOMEDAY-MAYBE-TODO=\"NEXT\"-Category=\"Standalone\""
      ((org-agenda-skip-function
        (quote
         (or
          (pjs-org-agenda-skip-entry-if
           (quote habit))
          (pjs-org-agenda-skip-entry-if
           (quote project))
          (pjs-org-agenda-skip-entry-if
           (quote scheduled)
           (quote deadline)))))
       (org-agenda-overriding-header "Tasks"))))))
 '(org-agenda-diary-file "~/org/journal.org")
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-files
   (quote
    ("~/org/in.org" "~/org/orgzly/orgzly.org" "~/org/orgzly/tasks.org" "~/org/orgzly/projects.org" "~/org/someday-maybe.org" "~/org/notes.org" "~/org/habits.org" "~/org/journal.org" "~/org/orgzly/schedule.org")))
 '(org-agenda-insert-diary-strategy (quote date-tree-last))
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %?-12t% s%?b")
     (todo . "  %?b")
     (tags . "  %?b")
     (search . "  %?b"))))
 '(org-agenda-tags-column 0)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-agenda-window-setup (quote only-window))
 '(org-archive-file-header-format "")
 '(org-archive-location "~/org/archive.org::")
 '(org-attach-id-dir "attachments/")
 '(org-capture-prepare-finalize-hook (quote (pjs-ensure-ending-newline)))
 '(org-capture-templates
   (quote
    (("t" "todo" entry
      (file "~/org/in.org")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:")
     ("n" "note" entry
      (file "~/org/notes.org")
      "* %? :NOTE:
:PROPERTIES:
:CREATED: %U
:END:")
     ("y" "org-protocol-link" entry
      (file "~/org/orgzly/read.org")
      "* %? %a
:PROPERTIES:
:CREATED: %U
:END:" :immediate-finish t)
     ("z" "org-protocol-quote" entry
      (file+function "~/org/notes.org" pjs-org-capture-to-heading)
      "* %:description :NOTE:
:PROPERTIES:
:CREATED: %U
:END:
#+BEGIN_QUOTE
%i
#+END_QUOTE
%?" :immediate-finish t)
     ("j" "journal entries")
     ("jj" "Journal to file" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?
:PROPERTIES:
:CREATED: %U
:END:")
     ("jc" "Journal to clocked entry" entry
      (function pjs-org-capture-journal)
      "* %?
:PROPERTIES:
:CREATED: %T
:END:")
     ("v" "Event" entry
      (file "~/org/orgzly/schedule.org")
      "* %^{Description}
%^T
:PROPERTIES:
:CREATED: %U
:END:" :immediate-finish t))))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-default-notes-file "~/org/in.org")
 '(org-directory "~/org")
 '(org-edit-src-content-indentation 0)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-fast-tag-selection-single-key t)
 '(org-footnote-auto-adjust t)
 '(org-footnote-section nil)
 '(org-goto-interface (quote outline-path-completion))
 '(org-habit-following-days 1)
 '(org-habit-graph-column 75)
 '(org-habit-preceding-days 14)
 '(org-hide-leading-stars t)
 '(org-id-link-to-org-use-id (quote create-if-interactive-and-no-custom-id))
 '(org-indirect-buffer-display (quote current-window))
 '(org-list-allow-alphabetical t)
 '(org-list-demote-modify-bullet (quote (("-" . "+") ("+" . "*") ("*" . "-"))))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-log-reschedule (quote note))
 '(org-outline-path-complete-in-steps nil)
 '(org-pretty-entities t)
 '(org-refile-allow-creating-parent-nodes (quote confirm))
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 9)
     (org-agenda-files :maxlevel . 9)
     ("~/org/read.org" :level . 0)
     ("~/org/reference.org" :level . 0))))
 '(org-refile-use-outline-path (quote file))
 '(org-special-ctrl-a/e (quote reversed))
 '(org-special-ctrl-k t)
 '(org-startup-folded (quote content))
 '(org-stuck-projects
   (quote
    ("+LEVEL=1+TODO=\"TODO\"+CATEGORY=\"Projects\"-SOMEDAY-MAYBE"
     ("NEXT")
     ("ROUTINE")
     "")))
 '(org-tag-alist
   (quote
    (("WAITING" . 119)
     ("HOLD" . 104)
     ("PERSONAL" . 80)
     ("WORK" . 87)
     ("NOTE" . 110)
     ("CANCELLED" . 99)
     ("SOMEDAY" . 115)
     ("MAYBE" . 109)
     ("FLAGGED" . 63))))
 '(org-tags-column 0)
 '(org-tags-exclude-from-inheritance (quote ("ROUTINE")))
 '(org-todo-keyword-faces
   (quote
    (("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))
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
 '(org-use-sub-superscripts (quote {}))
 '(org-yank-adjusted-subtrees t)
 '(package-selected-packages
   (quote
    (cider cljstyle-mode clojure-mode clojure-mode-extra-font-locking company deft exec-path-from-shell exwm flycheck-clj-kondo ghub gnu-elpa-keyring-update helm helm-ag helm-core helm-projectile markdown markdown-mode org-autolist org-bullets org-drill paredit pdf-tools pinentry typo use-package visual-fill-column writegood-mode)))
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
 '(sh-basic-offset 2)
 '(sql-input-ring-file-name "~/.sql-mode-history")
 '(text-mode-hook
   (quote
    (pjs-configure-text-mode-fill-column flyspell-mode text-mode-hook-identify)))
 '(visible-bell t)
 '(whitespace-line-column nil)
 '(whitespace-style
   (quote
    (face trailing tabs lines-tail newline empty indentation::space))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "CTDB" :family "Fira Mono"))))
 '(fixed-pitch ((t (:height 120 :family "Fira Mono"))))
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
 '(org-tag ((t (:inherit fixed-pitch :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:height 130 :family "Fira Sans")))))
