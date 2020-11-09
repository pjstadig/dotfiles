;;; pjs-clubhouse.el --- Clubhouse configuration and utilities -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:
(defun pjs-given-when-then-font-lock ()
  (font-lock-add-keywords
   nil '(("\\<\\(Given\\|When\\|Then\\):"
          1 font-lock-warning-face t))))

(provide 'pjs-clubhouse)
;;; pjs-clubhouse.el ends here
