(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-sources (quote ("~/.netrc.gpg")))
 '(auto-save-visited-file-name t)
 '(backup-directory-alist (quote (("." . "~/.emacs.d/backups"))))
 '(checkdoc-force-docstrings-flag nil)
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
 '(emacs-lisp-mode-hook
   (quote
    (pjs-add-eval-buffer-binding eldoc-mode checkdoc-minor-mode paredit-mode)))
 '(exec-path-from-shell-variables (quote ("PATH" "MANPATH" "JAVA_HOME")))
 '(exwm-init-hook (quote (exwm-randr--init pjs-start-initial-programs)))
 '(exwm-input-prefix-keys
   (quote
    ("" "" ""
     [134217848]
     [134217824]
     [134217766]
     [134217786])))
 '(exwm-layout-show-all-buffers nil)
 '(exwm-manage-configurations
   (quote
    (((string-equal exwm-class-name "Xfce4-terminal")
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
        . ""))
      workspace 1))))
 '(exwm-randr-workspace-monitor-plist (quote (9 "HDMI1")))
 '(exwm-replace t)
 '(exwm-update-class-hook (quote (pjs-set-exwm-buffer-name-to-class)))
 '(exwm-workspace-index-map (lambda (n) (number-to-string (1+ n))))
 '(exwm-workspace-number 10)
 '(exwm-workspace-show-all-buffers nil)
 '(exwm-workspace-switch-create-limit 10)
 '(fill-column 90)
 '(flycheck-checkers
   (quote
    (clj-kondo-edn clj-kondo-cljc clj-kondo-cljs clj-kondo-clj ada-gnat asciidoctor asciidoc c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint d-dmd dockerfile-hadolint elixir-dogma emacs-lisp erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-jscs javascript-standard json-jsonlint json-python-json less less-stylelint llvm-llc lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint markdown-mdl nix rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby)))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(linum-format "%d ")
 '(load-prefer-newer t)
 '(mouse-yank-at-point t)
 '(org-agenda-compact-blocks t)
 '(org-agenda-custom-commands
   (quote
    (("n" "Next actions"
      ((tags-todo "+TODO=\"WAITING\""
                  ((org-agenda-overriding-header "Waiting")))
       (tags-todo "+TODO=\"NEXT\"-SOMEDAY-MAYBE-SCHEDULED>\"<now>\"|-SOMEDAY-MAYBE+SCHEDULED<=\"<now>\"|+CATEGORY=\"Standalone\"+LEVEL=1-SOMEDAY-MAYBE-SCHEDULED>\"<now>\""
                  ((org-agenda-overriding-header "Next actions"))))
      nil)
     ("i" "Inbox & someday/maybe"
      ((tags "LEVEL=1"
             ((org-agenda-files
               (quote
                ("~/org/in.org")))
              (org-agenda-overriding-header "Inbox")))
       (tags-todo "+SOMEDAY+MAYBE"
                  ((org-agenda-overriding-header "Someday/maybe"))))
      nil)
     ("d" "Daily agenda and all TODOs"
      ((agenda ""
               ((org-agenda-span
                 (quote day))))
       (tags "LEVEL=1"
             ((org-agenda-files
               (quote
                ("~/org/in.org")))
              (org-agenda-overriding-header "Inbox:")))
       (tags "PRIORITY=\"A\""
             ((org-agenda-overriding-header "Priority tasks:")
              (org-agenda-skip-function
               (quote
                (org-agenda-skip-entry-if
                 (quote todo)
                 (quote done))))))
       (tags-todo "+TODO=\"NEXT\"-SOMEDAY-MAYBE|+CATEGORY=\"Standalone\"+LEVEL=1-SOMEDAY-MAYBE"
                  ((org-agenda-overriding-header "Next actions:")
                   (org-agenda-skip-function
                    (quote
                     (or
                      (air-org-skip-subtree-if-habit)
                      (air-org-skip-subtree-if-priority 65)
                      (pjs-org-skip-subtree-if-project)
                      (org-agenda-skip-if nil
                                          (quote
                                           (scheduled deadline))))))))
       (stuck "" nil))
      ((org-agenda-compact-blocks t))))))
 '(org-agenda-diary-file "~/org/journal.org")
 '(org-agenda-dim-blocked-tasks nil)
 '(org-agenda-files
   (quote
    ("~/org/in.org" "~/org/projects.org" "~/org/tasks.org" "~/org/someday-maybe.org" "~/org/notes.org")))
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %?-12t% s")
     (todo . "  ")
     (tags . "  ")
     (search . "  "))))
 '(org-agenda-tags-column 0)
 '(org-agenda-tags-todo-honor-ignore-options t)
 '(org-archive-file-header-format "")
 '(org-archive-location "~/org/archive.org::")
 '(org-attach-directory "attachments/")
 '(org-capture-templates
   (quote
    (("t" "todo" entry
      (file "~/org/in.org")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:" :prepend t :empty-lines-after 1)
     ("p" "project" entry
      (file "~/org/projects.org")
      "* TODO %?
:PROPERTIES:
:CREATED: %U
:END:" :prepend t :jump-to-captured t :empty-lines-after 1)
     ("n" "note" entry
      (file "~/org/in.org")
      "* %?
:PROPERTIES:
:CREATED: %U
:END:")
     ("j" "journal entry" entry
      (file+olp+datetree "~/org/journal.org")
      "* %?
:PROPERTIES:
:CREATED: %U
:END:")
     ("y" "org-protocol-link" entry
      (file "~/org/read.org")
      "* %? [[%:link][%:description]]
:PROPERTIES:
:CREATED: %U
:END:" :immediate-finish t)
     ("z" "org-protocol-quote" entry
      (file+function "~/org/notes.org" pjs-org-capture-to-heading)
      "* QUOTE
:PROPERTIES:
:CREATED: %U
:END:
#+BEGIN_QUOTE
%i
#+END_QUOTE
%?"))))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-default-notes-file "~/org/inbox.org")
 '(org-directory "~/org")
 '(org-edit-src-content-indentation 0)
 '(org-enforce-todo-dependencies t)
 '(org-fast-tag-selection-single-key t)
 '(org-footnote-auto-adjust t)
 '(org-footnote-section nil)
 '(org-habit-following-days 1)
 '(org-habit-graph-column 75)
 '(org-habit-preceding-days 14)
 '(org-habit-show-all-today t)
 '(org-id-link-to-org-use-id (quote create-if-interactive-and-no-custom-id))
 '(org-indirect-buffer-display (quote current-window))
 '(org-list-allow-alphabetical t)
 '(org-list-demote-modify-bullet (quote (("-" . "+") ("+" . "*") ("*" . "-"))))
 '(org-log-done (quote time))
 '(org-log-into-drawer t)
 '(org-log-reschedule (quote note))
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-docview org-gnus org-habit org-id org-info org-irc org-mhe org-rmail org-w3m)))
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
    ((:startgroup)
     ("@HOME" . 72)
     ("@MOBILE" . 77)
     (:endgroup)
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
     ("MEETING" :foreground "forest green" :weight bold)
     ("PHONE" :foreground "forest green" :weight bold))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
     (sequence "QUOTE(q)" "ITALIC(i!)" "|" "BOLD(b!)"))))
 '(org-treat-S-cursor-todo-selection-as-state-change nil)
 '(org-use-speed-commands t)
 '(org-yank-adjusted-subtrees t)
 '(package-selected-packages
   (quote
    (cider cljstyle-mode clojure-mode clojure-mode-extra-font-locking company dash deft exec-path-from-shell exwm flycheck-clj-kondo gnu-elpa-keyring-update helm helm-ag helm-projectile magit markdown-mode org-autolist org-bullets paredit pdf-tools pinentry typo use-package visual-fill-column writegood-mode)))
 '(prog-mode-hook
   (quote
    (flyspell-prog-mode linum-mode pjs-prog-mode-local-bindings whitespace-mode company-mode column-number-mode flycheck-mode)))
 '(require-final-newline t)
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
 '(save-interprogram-paste-before-kill t)
 '(save-place-file "~/.emacs.d/places")
 '(sh-basic-offset 2)
 '(show-paren-delay 0.25)
 '(sql-input-ring-file-name "~/.sql-mode-history")
 '(text-mode-hook
   (quote
    (pjs-configure-text-mode-fill-column writegood-mode flyspell-mode text-mode-hook-identify)))
 '(visible-bell t)
 '(whitespace-style
   (quote
    (face trailing tabs lines-tail newline empty indentation::space)))
 '(zk-directory "~/org/zk/")
 '(zk-extensions (quote ("org" "txt" "text" "md" "markdown")))
 '(zk-strip-summary-regexp "\\([
        ]\\|^#\\+[[:upper:]_]+:.*$\\|^:[^:]+:.*$\\)"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "CTDB" :family "Fira Mono"))))
 '(fixed-pitch ((t (:height 120 :family "Fira Mono"))))
 '(org-block ((t (:inherit shadow :family "Fira Mono"))))
 '(org-checkbox ((t (:inherit bold :family "Fira Mono"))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-table ((t (:foreground "Blue1" :family "Fira Mono"))))
 '(variable-pitch ((t (:weight light :height 120 :family "Fira Sans")))))
