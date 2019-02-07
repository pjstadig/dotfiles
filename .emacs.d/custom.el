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
 '(emacs-lisp-docstring-fill-column t)
 '(exwm-layout-show-all-buffers t)
 '(exwm-workspace-show-all-buffers t)
 '(fill-column 80)
 '(linum-format "%d ")
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
