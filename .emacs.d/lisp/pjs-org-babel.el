;;; pjs-org-babel --- Customizations for org-babel -*- lexical-binding: t; -*-
;;; Commentary:
;;
;; There is one thing about `org-babel' that annoys me: by default it requires
;; confirmation to execute source.  On the one hand, this is a good default, but
;; for files I write I would like to set a file-local to turn it off.
;;
;; Instead of making `org-confirm-babel-evaluate' safe everywhere, my solution
;; is to introduce my own safe file-local and add a hook to set
;; `org-confirm-babel-evaluate' from it.
;;
;;; Code:
(require 'ob-core)

(defgroup pjs-org-babel nil
  "Customizations for org-babel."
  :tag "PJS Babel"
  :group 'pjs-org)

(defcustom pjs-org-confirm-babel-evaluate t
  "A file-local safe flag to control org-babel evaluation."
  :group 'pjs-org-babel
  :type 'boolean
  :safe #'booleanp)

(defun pjs-org-babel-set-confirm-evaluate ()
  (setq-local org-confirm-babel-evaluate pjs-org-confirm-babel-evaluate))

(provide 'pjs-org-babel)
;; pjs-org-babel ends here
