;;; deft-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "deft" "deft.el" (0 0 0 0))
;;; Generated autoloads from deft.el

(autoload 'deft-find-file "deft" "\
Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `deft-directory', fall back to using `find-file'.

\(fn FILE)" t nil)

(autoload 'deft-new-file "deft" "\
Create a new file quickly.
Use either an automatically generated filename or the filter string if non-nil
and `deft-use-filter-string-for-filename' is set.  If the filter string is
non-nil and title is not from filename, use it as the title.

\(fn)" t nil)

(autoload 'deft "deft" "\
Switch to *Deft* buffer and load files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "deft" '("deft-" "org-deft-store-link")))

;;;***

;;;### (autoloads nil "zk" "zk.el" (23999 10016 572636 923000))
;;; Generated autoloads from zk.el

(autoload 'zk-find-file "zk" "\
Find FILE interactively using the minibuffer.
FILE must exist and be a relative or absolute path, with extension.
If FILE is not inside `zk-directory', fall back to using `find-file'.

\(fn FILE)" t nil)

(autoload 'zk-new-file "zk" "\
Create a new file quickly.
Use either an automatically generated filename or the filter string if non-nil
and `zk-use-filter-string-for-filename' is set.  If the filter string is
non-nil and title is not from filename, use it as the title.

\(fn)" t nil)

(autoload 'zk "zk" "\
Switch to *Zk* buffer and load files.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "zk" '(#("zk-" 0 2 (fontified nil face font-lock-function-name-face) 2 3 (fontified nil face font-lock-function-name-face)) #("org-zk-store-link" 0 4 (fontified nil) 4 6 (fontified nil) 6 17 (fontified nil)))))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; deft-autoloads.el ends here
