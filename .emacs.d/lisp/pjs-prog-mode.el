;;; pjs-prog-mode.el --- Clubhouse configuration and utilities -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(defun pjs-todo-font-lock ()
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\|NOCOMMIT\\):"
          1 font-lock-warning-face t))))

(provide 'pjs-prog-mode)
;;; pjs-prog-mode.el ends here
