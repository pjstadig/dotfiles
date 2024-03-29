;;; pjs-org-cosmetics.el --- Org-mode cosmetics -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

;;; Heading bullets
;; Use fancy bullets for headings, and make the whole prefix fixed-pitch so headings
;; indent consistently.
(defface org-level-1-bullet '((t :inherit (org-level-1 fixed-pitch)))
  "Face used for level 1 headline bullets."
  :group 'org-faces)

(defface org-level-2-bullet '((t :inherit (org-level-2 fixed-pitch)))
  "Face used for level 1 headline bullets."
  :group 'org-faces)

(defface org-level-3-bullet '((t :inherit (org-level-3 fixed-pitch)))
  "Face used for level 3 headline bullets."
  :group 'org-faces)

(defface org-level-4-bullet '((t :inherit (org-level-4 fixed-pitch)))
  "Face used for level 4 headline bullets."
  :group 'org-faces)

(defface org-level-5-bullet '((t :inherit (org-level-5 fixed-pitch)))
  "Face used for level 5 headline bullets."
  :group 'org-faces)

(defface org-level-6-bullet '((t :inherit (org-level-6 fixed-pitch)))
  "Face used for level 6 headline bullets."
  :group 'org-faces)

(defface org-level-7-bullet '((t :inherit (org-level-7 fixed-pitch)))
  "Face used for level 7 headline bullets."
  :group 'org-faces)

(defface org-level-8-bullet '((t :inherit (org-level-8 fixed-pitch)))
  "Face used for level 8 headline bullets."
  :group 'org-faces)

(font-lock-add-keywords
 'org-mode
 '(("^\\(\\(\\*\\) \\)"
    (1 'org-level-1-bullet)
    (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "●"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{1\\}\\(\\(\\*\\) \\)"
    (1 'org-level-2-bullet)
    (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "◉"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{2\\}\\(\\(\\*\\) \\)"
    (1 'org-level-3-bullet)
    (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "○"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{3\\}\\(\\(\\*\\) \\)"
    (1 'org-level-4-bullet)
    (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "◆"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{4\\}\\(\\(\\*\\) \\)"
    (1 'org-level-5-bullet)
    (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "◇"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{5\\}\\(\\(\\*\\) \\)"
    (1 'org-level-6-bullet)
    (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "►"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{6\\}\\(\\(\\*\\) \\)"
    (1 'org-level-7-bullet)
    (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "●"))))))

(font-lock-add-keywords
 'org-mode
 '(("^\\*\\{7\\}\\(\\(\\*\\) \\)"
    (1 'org-level-8-bullet)
    (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "◉"))))))

;;; Plain list bullets
;; Use a fancy bullet for '*', and make the whole prefix fixed-pitch so plain lists indent
;; consistently
(font-lock-add-keywords
 'org-mode
 '(("^ *\\(?1:-\\) "
    (0 'fixed-pitch)
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "-"))))))

(font-lock-add-keywords
 'org-mode
 '(("^ *\\(?1:+\\) "
    (0 'fixed-pitch)
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "+"))))))

(font-lock-add-keywords
 'org-mode
 '(("^ *[0-9][.)] "
    (0 'fixed-pitch))))

(font-lock-add-keywords
 'org-mode
 '(("^ *[a-zA-Z][.)] "
    (0 'fixed-pitch))))

(font-lock-add-keywords
 'org-mode
 '(("^ +\\(?1:[*]\\) "
    (0 'fixed-pitch)
    (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;;; Checkbox
;; Make the space after a checkbox fixed-pitch, so plain lists with checkboxes indent
;; consistently.
(font-lock-add-keywords
 'org-mode
 '(("[-+*.)] \\[[ X-]\\]\\( \\)"
    (1 'fixed-pitch))))

(defface org-strike-through '((t :foreground "gray" :strike-through t))
  "Face used for strike through text."
  :group 'org-faces)

(defface org-highlight '((t :background "yellow"))
  "Face used for highlighted text."
  :group 'org-faces)

;; This is font-lock, because the emphasis parsing in org-mode is more
;; complicated than just customizing org-emphasis-alist. The parsing is
;; integrated pretty deeply.
(font-lock-add-keywords
 'org-mode
 '(("\\(\\^[^\\^|]+\\^\\)"
    (1 'org-highlight))))

(provide 'pjs-org-cosmetics)
;;; pjs-org-cosmetics.el ends here
