;;; zk.el --- quickly browse, filter, and edit plain text notes
;;; Copyright (C) 2011-2017 Jason R. Blevins <jblevins@xbeta.org>
;;; Copyright (C) 2018-2019 EFLS
;;; Copyright (C) 2019      Paul Stadig

;;; Version: 0.1
;; Package-Version: 0.1
;;; Author: Paul Stadig <paul@stadig.name>
;;; Keywords: plain text, notes, Simplenote, Notational Velocity
;;; URL: https://github.com/pjstadig/zk/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Code:

(require 'cl)
(require 'widget)
(require 'wid-edit)
(require 'org)

;; Customization

(defgroup zk nil
  "Emacs Zk mode."
  :group 'local)

(defcustom zk-directory (expand-file-name "~/.zk/")
  "Zk directory."
  :type 'directory
  :safe 'stringp
  :group 'zk)

(defcustom zk-extensions
  '("txt" "text" "md" "markdown" "org")
  "Files with these extensions will be listed.
The first element of the list is used as the default file
extension of newly created files, if `zk-default-extension' is
not set."
  :type '(repeat string)
  :group 'zk)

(defcustom zk-auto-save-interval 1.0
  "Idle time in seconds before automatically saving buffers opened by Zk.
Set to zero to disable."
  :type 'float
  :group 'zk)

(defcustom zk-time-format "%Y-%m-%d %H:%M"
  "Format string for modification times in the Zk browser.
Set to nil to hide."
  :type '(choice (string :tag "Time format")
                 (const :tag "Hide" nil))
  :group 'zk)

(defcustom zk-incremental-search t
  "Use incremental string search when non-nil and regexp search when nil.
During incremental string search, substrings separated by spaces are
treated as subfilters, each of which must match a file.  They need
not be adjacent and may appear in any order.  During regexp search, the
entire filter string is interpreted as a single regular expression."
  :type 'boolean
  :group 'zk)

(defcustom zk-recursive nil
  "Recursively search for files in subdirectories when non-nil."
  :type 'boolean
  :group 'zk)

(defcustom zk-recursive-ignore-dir-regexp
  (concat "\\(?:"
          "\\."
          "\\|\\.\\."
          "\\)$")
  "Regular expression for subdirectories to be ignored.
This variable is only effective when searching for files
recursively, that is, when `zk-recursive' is non-nil."
  :type 'regexp
  :safe 'stringp
  :group 'zk)

(defcustom zk-ignore-file-regexp
  (concat "\\(?:"
          "^$"
          "\\)")
  "Regular expression for files to be ignored."
  :type 'regexp
  :safe 'stringp
  :group 'zk)

(defcustom zk-parse-title-function 'zk-strip-title
  "Function for post-processing file titles."
  :type 'function
  :group 'zk)

(defcustom zk-strip-title-regexp
  (concat "\\(?:"
          "^%+" ; line beg with %
          "\\|^#\\+TITLE: *" ; org-mode title
          "\\|^[#* ]+" ; line beg with #, * and/or space
          "\\|-\\*-[[:alpha:]]+-\\*-" ; -*- .. -*- lines
          "\\|^Title:[\t ]*" ; MultiMarkdown metadata
          "\\|#+" ; line with just # chars
          "$\\)")
  "Regular expression to remove from file titles.
Presently, it removes leading LaTeX comment delimiters, leading
and trailing hash marks from Markdown ATX headings, leading
astersisks from Org Mode headings, and Emacs mode lines of the
form -*-mode-*-."
  :type 'regexp
  :safe 'stringp
  :group 'zk)

(defcustom zk-strip-summary-regexp
  (concat "\\("
          "[\n\t]" ;; blank
          "\\|^#\\+[[:upper:]_]+:.*$" ;; org-mode metadata
          "\\)")
  "Regular expression to remove file contents displayed in summary.
Presently removes blank lines and `org-mode' metadata statements."
  :type 'regexp
  :safe 'stringp
  :group 'zk)

(defcustom zk-generation-rules '(("org" . "tex") ("md" . "tex"))
  "Rules for omitting automatically generated files.
For example, .tex files may be generated from `org-mode' or Pandoc."
  :type '(repeat (cons string string))
  :group 'zk)

(defcustom zk-filter-only-filenames nil
  "Filter on file names only."
  :type 'boolean
  :group 'zk)

;; Faces

(defgroup zk-faces nil
  "Faces used in Zk mode"
  :group 'zk
  :group 'faces)

(defface zk-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for Zk header."
  :group 'zk-faces)

(defface zk-filter-string-face
  '((t :inherit font-lock-string-face))
  "Face for Zk filter string."
  :group 'zk-faces)

(defface zk-filter-string-error-face
  '((t :inherit font-lock-warning-face))
  "Face for Zk filter string when regexp is invalid."
  :group 'zk-faces)

(defface zk-title-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for Zk file titles."
  :group 'zk-faces)

(defface zk-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for Zk separator string."
  :group 'zk-faces)

(defface zk-summary-face
  '((t :inherit font-lock-comment-face))
  "Face for Zk file summary strings."
  :group 'zk-faces)

(defface zk-time-face
  '((t :inherit font-lock-variable-name-face))
  "Face for Zk last modified times."
  :group 'zk-faces)

;; Constants

(defconst zk-version "0.1")

(defconst zk-buffer "*Zk*"
  "Zk buffer name.")

(defconst zk-separator " --- "
  "Text used to separate file titles and summaries.")

(defconst zk-empty-file-title "[Empty file]"
  "Text to use as title for empty files.")

;; Global variables

(defvar zk-mode-hook nil
  "Hook run when entering Zk mode.")

(defvar zk-filter-hook nil
  "Hook run when the Zk filter string changes.")

(defvar zk-open-file-hook nil
  "Hook run after Zk opens a file.")

(defvar zk-filter-regexp nil
  "A list of string representing the current filter used by Zk.

In incremental search mode, when `zk-incremental-search' is
non-nil, the elements of this list are the individual words of
the filter string, in reverse order.  That is, the car of the
list is the last word in the filter string.

In regexp search mode, when `zk-incremental-search' is nil,
this list has a single element containing the entire filter
regexp.")

(defvar zk-current-files nil
  "List of files matching current filter.")

(defvar zk-all-files nil
  "List of all files in `zk-directory'.")

(defvar zk-hash-contents nil
  "Hash containing complete cached file contents, keyed by filename.")

(defvar zk-hash-mtimes nil
  "Hash containing cached file modification times, keyed by filename.")

(defvar zk-hash-titles nil
  "Hash containing cached file titles, keyed by filename.")

(defvar zk-hash-summaries nil
  "Hash containing cached file summaries, keyed by filename.")

(defvar zk-auto-save-buffers nil
  "List of buffers that will be automatically saved.")

(defvar zk-window-width nil
  "Width of Zk buffer.")

(defvar zk-filter-history nil
  "History of interactive filter strings.")

(defvar zk-regexp-error nil
  "Flag for indicating invalid regexp errors.")

(defvar zk-default-extension (copy-sequence (car zk-extensions))
  "Default file extension of newly created files.")

(defvar zk-pending-updates nil
  "Indicator of pending updates due to automatic saves, etc.")

(defvar zk-complete-fun nil
  "Function called when <RET> is pressed in the zk buffer.  If it is nil, then
  the file at point or the first result or a new buffer will be opened.")

(defvar zk-complete-insert-buffer nil
  "Buffer to insert a link into when search is complete.")

(defvar zk-navigation-history (make-ring 1000)
  "Navigation history from following links.")

;; Keymap definition

(defvar zk-mode-map
  (let ((i 0)
        (map (make-keymap)))
    ;; Make multibyte characters extend the filter string.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
                          'zk-filter-increment)
    ;; Extend the filter string by default.
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) 'zk-filter-increment)
      (setq i (1+ i)))
    ;; Handle backspace and delete
    (define-key map (kbd "DEL") 'zk-filter-decrement)
    (define-key map (kbd "M-DEL") 'zk-filter-decrement-word)
    ;; Handle return via completion or opening file
    (define-key map (kbd "RET") 'zk-complete)
    ;; Filtering
    (define-key map (kbd "C-c C-l") 'zk-filter)
    (define-key map (kbd "C-c C-c") 'zk-filter-clear)
    (define-key map (kbd "C-y") 'zk-filter-yank)
    ;; File creation
    (define-key map (kbd "C-c C-n") 'zk-new-file)
    ;; Settings
    (define-key map (kbd "C-c C-t") 'zk-toggle-incremental-search)
    (define-key map (kbd "C-c C-s") 'zk-toggle-sort-method)
    ;; Miscellaneous
    (define-key map (kbd "C-c C-g") 'zk-refresh)
    (define-key map (kbd "C-c C-q") 'zk-quit)
    ;; Widgets
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map [down-mouse-2] 'widget-button-click)
    (define-key map (kbd "M-n") 'widget-forward)
    (define-key map (kbd "M-p") 'widget-backward)
    (define-key map (kbd "<tab>") 'widget-forward)
    (define-key map (kbd "<backtab>") 'widget-backward)
    (define-key map (kbd "<S-tab>") 'widget-backward)
    (define-key map (kbd "C-o") 'zk-open-file-other-window)
    map)
  "Keymap for Zk mode.")

;; Helpers

(defun zk-whole-filter-regexp ()
  "Join incremental filters into one."
  (mapconcat 'identity (reverse zk-filter-regexp) " "))

(defun zk-search-forward (str)
  "Function to use when matching files against filter strings STR.
This function calls `search-forward' when `zk-incremental-search'
is non-nil and `re-search-forward' otherwise."
  (if zk-incremental-search
      (search-forward str nil t)
    (re-search-forward str nil t)))

(defun zk-set-mode-name ()
  "Set the mode line text based on search mode."
  (if zk-incremental-search
      (setq mode-name "Zk")
    (setq mode-name "Zk/R")))

(defun zk-toggle-incremental-search ()
  "Toggle the `zk-incremental-search' setting."
  (interactive)
  (cond
   (zk-incremental-search
    (setq zk-incremental-search nil)
    (message "Regexp search"))
   (t
    (setq zk-incremental-search t)
    (message "Incremental string search")))
  (zk-filter (zk-whole-filter-regexp) t)
  (zk-set-mode-name))

(defun zk-filter-regexp-as-regexp ()
  "Return a regular expression corresponding to the current filter string.
When `zk-incremental-search' is non-nil, we must combine each individual
whitespace separated string.  Otherwise, the `car' of `zk-filter-regexp'
is the complete regexp."
  (if zk-incremental-search
      (mapconcat 'regexp-quote (reverse zk-filter-regexp) "\\|")
    (car zk-filter-regexp)))

;; ID manipulation

(defun zk-last-idx (seq)
  (1- (length seq)))

(defun zk-alpha-inc (str zero max &optional one)
  (let* ((one (or one (1+ zero)))
         (new-str (copy-sequence str))
         (idx (zk-last-idx new-str))
         (continue t)
         c)
    (while continue
      (setq c (seq-elt new-str idx))
      (if (= c max)
          (progn
            (setf (seq-elt new-str idx) zero)
            (if (> idx 0)
                (setq idx (1- idx))
              (setq new-str (concat (string one) new-str))
              (setq continue nil)))
        (setf (seq-elt new-str idx) (1+ c))
        (setq continue nil)))
    new-str))

(defun zk-alpha-dec (str zero max &optional one)
  (let* ((one (or one (1+ zero)))
         (new-str (copy-sequence str))
         (idx (zk-last-idx new-str))
         (continue t)
         c)
    (while continue
      (setq c (seq-elt new-str idx))
      (if (not (= idx 0))
          (if (= c zero)
              (progn (setf (seq-elt new-str idx) max)
                     (setq idx (1- idx)))
            (progn (setf (seq-elt new-str idx) (1- c))
                   (setq continue nil)))
        (setq continue nil)
        (if (= c one)
            (setq new-str (subseq new-str 1))
          (setf (seq-elt new-str idx) (1- c)))))
    new-str))

(defun zk-id-strip-ancestors (id)
  (when (string-match (concat "\\(?:^\\|[^>]+>\\)"
                              "\\(\\(?:[0-9]+\\|[a-z]+\\)+\\)$")
                      id)
    (match-string 1 id)))

(defun zk-id-parse-first (id)
  (let ((id* (zk-id-strip-ancestors id)))
    (when (and id* (string-match "^\\([0-9]+\\)" id*))
      (match-string 1 id*))))

(defun zk-id-parse-last (id)
  (let ((id* (zk-id-strip-ancestors id)))
    (when (and id* (string-match "\\([0-9]+\\|[a-z]+\\)$" id*))
      (match-string 1 id*))))

(defun zk-id-but-last (id last)
  (substring id 0 (- (length id) (length last))))

(defun zk-number-string-p (str)
  (string-match "^[0-9]+$" str))

(defun zk-id-inc (prev-id)
  (let* ((last (zk-id-parse-last prev-id))
         (prefix (zk-id-but-last prev-id last)))
    (concat prefix (if (zk-number-string-p last)
                       (zk-alpha-inc last ?0 ?9)
                     (zk-alpha-inc last ?a ?z ?a)))))

(defun zk-id-dec (next-id)
  (let* ((last (zk-id-parse-last next-id))
         (prefix (zk-id-but-last next-id last)))
    (concat prefix (if (or (equal last "1")
                           (equal last "a"))
                       ""
                     (if (zk-number-string-p last)
                         (zk-alpha-dec last ?0 ?9)
                       (zk-alpha-dec last ?a ?z ?a))))))

(defun zk-id-insert (prev-id)
  (if (zk-number-string-p (zk-id-parse-last prev-id))
      (concat prev-id "a")
    (concat prev-id "1")))

(defun zk-id-used-p (id)
  (let ((file (zk-absolute-filename id)))
    (when (or (get-file-buffer file) (file-exists-p file))
      id)))

(defun zk-id-branch-inc (prev-id)
  (let* ((last (zk-id-parse-last prev-id))
         (prefix (zk-id-but-last prev-id last))
         (next-last (zk-id-parse-last prefix))
         (prefix (zk-id-but-last prefix next-last)))
    (concat prefix (zk-alpha-inc next-last ?a ?z ?a) last)))

(defun zk-id-branch (parent-id)
  (concat parent-id ">a1"))

(defun zk-id-new (prev-id)
  (let ((id (zk-id-inc prev-id)))
    (when (zk-id-used-p id)
      (setq id (zk-id-insert prev-id))
      (while (zk-id-used-p id)
        (setq id (zk-id-insert id))))
    id))

(defun zk-id-new-child (parent-id)
  (let ((id (zk-id-branch parent-id)))
    (while (zk-id-used-p id)
      (setq id (zk-id-branch-inc id)))
    id))

(defun zk-id-next (id)
  (let ((next-id (zk-id-inc id)))
    (if (zk-id-used-p next-id)
        ;; next id exists; check if there's anything in between
        (or (zk-id-used-p (zk-id-insert id)) next-id)
      ;; nothing in between
      (let* ((id-last (zk-id-parse-last id)))
        ;; is this the very last top-level id?
        (when (not (equal (zk-id-strip-ancestors id) id-last))
          (zk-id-used-p (zk-id-inc (zk-id-but-last id id-last))))))))

(defun zk-id-prev (id)
  (let ((prev-id (zk-id-dec id)))
    (when (zk-id-used-p prev-id)
      (while (not (equal id (zk-id-next prev-id)))
        (setq prev-id (zk-id-next prev-id)))
      prev-id)))

(defun zk-id-parent (id)
  (when (string-match (concat "^\\(.+\\)"
                              "\\(?:>\\(?:[0-9]+\\|[a-z]+\\)+\\)$")
                      id)
    (zk-id-used-p (match-string 1 id))))

;; File processing

(defun zk-chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" str))

(defun zk-find-all-files ()
  "Return a list of all files in the Zk directory.

See `zk-find-files'."
  (zk-find-files zk-directory))

(defun zk-find-all-files-no-prefix ()
  "List files in Zk directory with the Zk directory prefix removed.
See `zk-find-files' and `zk-find-all-files'."
  (let* ((dir (expand-file-name zk-directory))
         (files (mapcar (lambda (f) (replace-regexp-in-string dir "" f))
                        (zk-find-all-files))))
    files))

(defun zk-find-files (dir)
  "Return a list of all files in the directory DIR.

It is important to note that the return value is a list of
absolute filenames.  These absolute filenames are used as keys
for the various hash tables used for storing file metadata and
contents.  So, any functions looking up values in these hash
tables should use `expand-file-name' on filenames first.

If `zk-recursive' is non-nil, then search recursively in
subdirectories of `zk-directory').

See `zk-find-all-files'."
  (if (file-exists-p dir)
      (let ((files (directory-files dir t "." t))
            result)
        (dolist (file files)
          (cond
           ;; Recurse into subdirectory if `zk-recursive' is non-nil
           ;; and the directory is not "." or ".."
           ((file-directory-p file)
            (when (and zk-recursive
                       (not (string-match zk-recursive-ignore-dir-regexp file)))
              (setq result (append (zk-find-files file) result))))
           ;; Collect names of readable files ending in `zk-extension'
           ((and (file-readable-p file)
                 (not (string-match zk-ignore-file-regexp file))
                 (not (backup-file-name-p file))
                 (member (file-name-extension file) zk-extensions))
            (setq result (cons file result)))))
        (zk-apply-generation-rules result))))

(defun zk-apply-generation-rules (lst)
  "Apply `zk-generation-rules' to each file in LST.
Remove files which were likely automatically generated from others."
  (if zk-generation-rules
      (let ((result nil))
        (dolist (file lst)
          (when (not (zk-generated-file? file lst))
            (setq result (cons file result))))
        result)
    lst))

(defun zk-generated-file? (file-name files)
  "Determine whether FILE-NAME was likely generated from another in LST.
See `zk-generation-rules'."
  (let ((val nil))
    (dolist (rule zk-generation-rules)
      (let* ((orig-file-ext (file-name-extension file-name)))
        (when (equal (cdr rule) orig-file-ext)
          (let* ((new-file-ext (car rule))
                 (new-file-name (concat (file-name-sans-extension file-name)
                                        "." new-file-ext)))
            (when (not val)
              (when (member new-file-name files)
                (setq val t)))))))
    val))

(defun zk-strip-title (title)
  "Remove all strings matching `zk-strip-title-regexp' from TITLE."
  (zk-chomp (replace-regexp-in-string zk-strip-title-regexp "" title)))

(defun zk-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
The title is taken to be the first non-empty line of the FILE."
  (let ((begin (string-match "^.+$" contents)))
    (if begin
        (funcall zk-parse-title-function
                 (substring contents begin (match-end 0))))))

(defun zk-parse-summary (contents)
  "Parse the file CONTENTS and extract a summary."
  (zk-chomp
   (let ((case-fold-search nil))
     (replace-regexp-in-string zk-strip-summary-regexp " " contents))))

(defun zk-id> (id1 id2)
  (or (> (length id1) (length id2))
      (and (= (length id1) (length id2))
           (string> id1 id2))))

(defun zk-cache-file (file)
  "Update file cache if FILE exists."
  (when (file-exists-p file)
    (add-to-list 'zk-all-files file)
    (let ((mtime-cache (zk-file-mtime file))
          (mtime-file (nth 5 (file-attributes (file-truename file)))))
      (if (or (not mtime-cache)
              (time-less-p mtime-cache mtime-file))
          (zk-cache-newer-file file mtime-file)))))

(defun zk-cache-newer-file (file mtime)
  "Update cached information for FILE with given MTIME."
  ;; Modification time
  (puthash file mtime zk-hash-mtimes)
  (let (contents title)
    ;; Contents
    (with-current-buffer (get-buffer-create "*Zk temp*")
      (insert-file-contents file nil nil nil t)
      (setq contents (concat (buffer-string))))
    (puthash file contents zk-hash-contents)
    ;; Title
    (setq title (zk-parse-title file contents))
    (puthash file title zk-hash-titles)
    ;; Summary
    (puthash file (zk-parse-summary contents) zk-hash-summaries))
  (kill-buffer "*Zk temp*"))

(defun zk-file-newer-p (file1 file2)
  "Return non-nil if FILE1 was modified since FILE2 and nil otherwise."
  (let (time1 time2)
    (setq time1 (zk-file-mtime file1))
    (setq time2 (zk-file-mtime file2))
    (time-less-p time2 time1)))

(defun zk-cache-initialize ()
  "Initialize hash tables for caching files."
  (setq zk-hash-contents (make-hash-table :test 'equal))
  (setq zk-hash-mtimes (make-hash-table :test 'equal))
  (setq zk-hash-titles (make-hash-table :test 'equal))
  (setq zk-hash-summaries (make-hash-table :test 'equal)))

(defun zk-cache-update-all ()
  "Update file list and update cached information for each file."
  (setq zk-all-files (zk-find-all-files))             ; List all files
  (mapc 'zk-cache-file zk-all-files)                  ; Cache contents
  (setq zk-all-files (zk-sort-files zk-all-files))) ; Sort by mtime

(defun zk-cache-update-file (file)
  "Update cached information for a single file named FILE."
  (zk-cache-file file)                                  ; Cache contents
  (setq zk-all-files (zk-sort-files zk-all-files))) ; Sort by mtime

;; Cache access

(defun zk-file-contents (file)
  "Retrieve complete contents of FILE from cache."
  (gethash file zk-hash-contents))

(defun zk-file-mtime (file)
  "Retrieve modified time of FILE from cache."
  (gethash file zk-hash-mtimes))

(defun zk-file-title (file)
  "Retrieve title of FILE from cache."
  (gethash file zk-hash-titles))

(defun zk-file-summary (file)
  "Retrieve summary of FILE from cache."
  (gethash file zk-hash-summaries))

;; File list display

(defun zk-print-header ()
  "Prints the *Zk* buffer header."
  (if zk-filter-regexp
      (progn
        (widget-insert
         (propertize "Zk: " 'face 'zk-header-face))
        (widget-insert
         (propertize (zk-whole-filter-regexp) 'face
                     (if (and (not zk-incremental-search) zk-regexp-error)
                         'zk-filter-string-error-face
                       'zk-filter-string-face))))
    (widget-insert
     (propertize "Zk" 'face 'zk-header-face)))
  (widget-insert "\n\n"))

(defun zk-current-window-width ()
  "Return current width of window displaying `zk-buffer'.
If the frame has a fringe, it will absorb the newline.
Otherwise, we reduce the line length by a one-character offset."
  (let* ((window (get-buffer-window zk-buffer))
         (fringe-right (ceiling (or (cadr (window-fringes)) 0)))
         (offset (if (> fringe-right 0) 0 1)))
    (when window
      (- (window-text-width window) offset))))

(defun zk-buffer-setup (&optional refresh)
  "Render the file browser in the *Zk* buffer.
When REFRESH is true, attempt to restore the point afterwards."
  (let ((orig-line (line-number-at-pos))
        (orig-col (current-column)))
    (when (zk-buffer-visible-p)
      (setq zk-window-width (zk-current-window-width)))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (zk-print-header)

    ;; Print the files list
    (if (not (file-exists-p zk-directory))
        (widget-insert (zk-no-directory-message))
      (if zk-current-files
          (progn
            (mapc 'zk-file-widget zk-current-files))
        (widget-insert (zk-no-files-message))))

    (use-local-map zk-mode-map)
    (widget-setup)
    (setq zk-pending-updates nil)

    ;; Position or reposition point
    (goto-char (point-min))
    (forward-line (if refresh (1- orig-line) 2))
    (forward-char (if refresh orig-col 0))))

(defun zk-string-width (str)
  "Return 0 if STR is nil and call `string-width` otherwise.
This is simply a wrapper function for `string-width' which
handles nil values gracefully."
  (if str (string-width str) 0))

(defun zk-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (when file
    (let* ((id (zk-lift-id file))
           (title (zk-file-title file))
           (summary (or (zk-file-summary file) id))
           (mtime (when zk-time-format
                    (format-time-string zk-time-format (zk-file-mtime file))))
           (mtime-width (zk-string-width mtime))
           (line-width (- zk-window-width mtime-width))
           (title-width (min line-width (zk-string-width title)))
           (summary-width (min (zk-string-width summary)
                               (- line-width
                                  title-width
                                  (length zk-separator)))))
      (widget-create 'link
                     :button-prefix ""
                     :button-suffix ""
                     :button-face 'zk-title-face
                     :format "%[%v%]"
                     :tag file
                     :help-echo "Edit this file"
                     :notify (lambda (widget &rest ignore)
                               (zk-open-file (widget-get widget :tag)))
                     (if title (truncate-string-to-width title title-width)
                       zk-empty-file-title))
      (when (> summary-width 0)
        (widget-insert (propertize zk-separator 'face 'zk-separator-face))
        (widget-insert (propertize (truncate-string-to-width summary summary-width)
                                   'face 'zk-summary-face)))
      (when mtime
        (while (< (current-column) line-width)
          (widget-insert " "))
        (widget-insert (propertize mtime 'face 'zk-time-face)))
      (widget-insert "\n"))))

(defun zk-buffer-visible-p ()
  "Return non-nil if a window is displaying `zk-buffer'."
  (get-buffer-window zk-buffer))

(defun zk-window-size-change-function (frame)
  "Possibly refresh Zk buffer when size of a window in FRAME is changed.
If there are pending updates, refresh the filtered files list and
update the Zk browser.  Otherwise, if the window width changed,
only update the Zk browser."
  (when (zk-buffer-visible-p)
    (cond (zk-pending-updates (zk-refresh-filter))
          ((/= zk-window-width (zk-current-window-width))
           (zk-refresh-browser)))))

(defun zk-window-configuration-change-function ()
  "Possibly refresh Zk browser when window configuration is changed."
  (zk-window-size-change-function nil))

(defun zk-refresh ()
  "Update the file cache, reapply the filter, and refresh the *Zk* buffer."
  (interactive)
  (zk-cache-update-all)
  (zk-refresh-filter))

(defun zk-refresh-filter ()
  "Reapply the filter and refresh the *Zk* buffer.
Call this after any actions which update the cache."
  (interactive)
  (zk-filter-update)
  (zk-refresh-browser))

(defun zk-refresh-browser ()
  "Refresh the *Zk* buffer in the background.
Call this function after any actions which update the filter and file list."
  (when (get-buffer zk-buffer)
    (with-current-buffer zk-buffer
      (zk-buffer-setup t))))

(defun zk-no-directory-message ()
  "Return a short message to display when the Zk directory does not exist."
  (concat "Directory " zk-directory " does not exist.\n"))

(defun zk-no-files-message ()
  "Return a short message to display if no files are found."
  (if zk-filter-regexp
      "No files match the current filter string.\n"
    "No files found."))

;; File list file management actions

(defun zk-absolute-filename (id &optional extension)
  "Return an absolute filename to file named ID with optional EXTENSION.
If EXTENSION is not given, `zk-default-extension' is assumed."
  (concat (file-name-as-directory (expand-file-name zk-directory))
          id "." (or extension zk-default-extension)))

(defun zk-open-file (file &optional other switch)
  "Open FILE in a new buffer and setting its mode.
When OTHER is non-nil, open the file in another window.  When
OTHER and SWITCH are both non-nil, switch to the other window.
FILE must be a relative or absolute path, with extension."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (hack-local-variables)
      (when zk-filter-regexp
        (goto-char (point-min))
        (re-search-forward (zk-filter-regexp-as-regexp) nil t))
      ;; Ensure that Zk has been initialized
      (when (not (get-buffer zk-buffer))
        (with-current-buffer (get-buffer-create zk-buffer)
          (zk-mode)))
      ;; Set up auto save hooks
      (add-to-list 'zk-auto-save-buffers buffer)
      (add-hook 'after-save-hook
                (lambda () (save-excursion
                             (zk-cache-update-file buffer-file-name)
                             (if (zk-buffer-visible-p)
                                 (zk-refresh-filter)
                               (setq zk-pending-updates t))))
                nil t))
    (run-hooks 'zk-open-file-hook)
    (if other
        (if switch
            (switch-to-buffer-other-window buffer)
          (display-buffer buffer other))
      (switch-to-buffer buffer))))

(defun zk-new-file-named (id &optional content)
  "Create a new file named ID."
  (let ((file (zk-absolute-filename id)))
    (if (file-exists-p file)
        (message "Aborting, file already exists: %s" file)
      (zk-cache-update-file file)
      (zk-refresh-filter)
      (let ((buf (zk-open-file file)))
        (with-current-buffer buf
          (let ((undo-inhibit-record-point t))
            (insert "\nCreated: ")
            (org-time-stamp '(16) t)
            (insert "\n")
            (goto-char (point-min))
            (setq buffer-undo-list nil))
          (when content
            (insert content)))
        buf))))

;;;###autoload
(defun zk-new-file ()
  "Create a new file quickly using an automatically generated filename.  If the
filter string is non-nil, use it as the initial file contents."
  (interactive)
  (zk-new-file-named (zk-id-new-child "0")
                     (when zk-filter-regexp
                       (concat
                        (zk-whole-filter-regexp)
                        "\n"))))

(defun zk-filename-at-point ()
  "Return the name of the file represented by the widget at the point.
Return nil if the point is not on a file widget."
  (widget-get (widget-at) :tag))

(defun zk-open-file-other-window (&optional arg)
  "When the point is at a widget, open the file in the other window.
The argument ARG is passed to `zk-open-file'."
  (interactive "P")
  (let ((file (zk-filename-at-point)))
    (when file
      (zk-open-file file t arg))))

;; File list filtering

(defun zk-sort-files-by-mtime (files)
  "Sort FILES in reverse order by modified time."
  (sort files (lambda (f1 f2) (zk-file-newer-p f1 f2))))

(defun zk-sort-files (files)
  "Sort FILES by mtime."
  (zk-sort-files-by-mtime files))

(defun zk-filter-initialize ()
  "Initialize the filter string (nil) and files list (all files)."
  (interactive)
  (setq zk-filter-regexp nil)
  (setq zk-current-files zk-all-files))

(defun zk-filter-match-file (file &optional batch)
  "Return FILE if it is a match against the current filter regexp.
If BATCH is non-nil, treat `zk-filter-regexp' as a list and match
all elements."
  (with-temp-buffer
    (insert file)
    (let ((contents (if zk-filter-only-filenames "" (zk-file-contents file))))
      (when contents (insert contents)))
    (if batch
        (if (every (lambda (filter)
                     (goto-char (point-min))
                     (zk-search-forward filter))
                   zk-filter-regexp)
            file)
      (goto-char (point-min))
      (if (zk-search-forward (car zk-filter-regexp))
          file))))

(defun zk-filter-files (files)
  "Update `zk-current-files' given a list of paths, FILES.
Apply `zk-filter-match-file' to `zk-all-files', handling
any errors that occur."
  (delq nil
        (condition-case nil
            ;; Map `zk-filter-match-file' onto FILES.  Return
            ;; filtered files list and clear error flag if no error.
            (progn
              (setq zk-regexp-error nil)
              (mapcar (lambda (file) (zk-filter-match-file file t)) files))
          ;; Upon an error (`invalid-regexp'), set an error flag
          (error
           (progn
             (setq zk-regexp-error t)
             files)))))

(defun zk-filter-update ()
  "Update the filtered files list using the current filter regexp.
Starts from scratch using `zk-all-files'.  Does not refresh the
Zk buffer."
  (if (not zk-filter-regexp)
      (setq zk-current-files zk-all-files)
    (setq zk-current-files
          (zk-filter-files zk-all-files))))

;; Filters that cause a refresh

(defun zk-filter-clear ()
  "Clear the current filter string and refresh the file browser."
  (interactive)
  (when zk-filter-regexp
    (setq zk-filter-regexp nil)
    (setq zk-current-files zk-all-files)
    (zk-refresh)
    (run-hooks 'zk-filter-hook))
  (message "Filter cleared."))

(defun zk-filter (str &optional reset)
  "Update the filter with STR and update the file browser.

In incremental search mode, the car of `zk-filter-regexp' will
be replaced with STR.  If STR has zero length and the length of
the list is greater than one, the empty string will be retained
to simulate whitespace.  However, if STR has zero length and the
list is of length one, then the filter will be cleared.  If STR
is nil, then the car is removed from the list.

In regexp search mode, the current filter string will be replaced
with STR.

When called interactively, or when RESET is non-nil, always
replace the entire filter string."
  (interactive
   (list (read-from-minibuffer "Filter: " (zk-whole-filter-regexp)
                               nil nil 'zk-filter-history)))
  (if zk-incremental-search
      ;; Incremental search mode
      (if (or (called-interactively-p 'any) reset)
          ;; Called interactively or RESET non-nil
          (if (= (length str) 0)
              (setq zk-filter-regexp nil)
            (setq zk-filter-regexp (reverse (split-string str " "))))
        ;; Called noninteractively
        (if (not str)
            ;; If str is nil, remove it and filter with the cdr
            (setq zk-filter-regexp (cdr zk-filter-regexp))
          ;; Use STR it as the new car, even when empty (to simulate
          ;; whitespace), unless this is the only element in the list.
          (if (and (= (length zk-filter-regexp) 1)
                   (= (length str) 0))
              (setq zk-filter-regexp nil)
            (setcar zk-filter-regexp str))))
    ;; Regexp search mode
    (if (> (length str) 0)
        (setq zk-filter-regexp (list str))
      (setq zk-filter-regexp nil)))
  (zk-filter-update)
  (zk-refresh-browser)
  (run-hooks 'zk-filter-hook))

(defun zk-filter-increment ()
  "Append character to the filter regexp and update `zk-current-files'."
  (interactive)
  (let ((char last-command-event))
    (if (= char ?\S-\ )
        (setq char ?\s))
    (setq char (char-to-string char))
    (if (and zk-incremental-search (string= char " "))
        (setq zk-filter-regexp (cons "" zk-filter-regexp))
      (progn
        (if (car zk-filter-regexp)
            (setcar zk-filter-regexp (concat (car zk-filter-regexp) char))
          (setq zk-filter-regexp (list char)))
        (setq zk-current-files (zk-filter-files zk-current-files))
        (setq zk-current-files (delq nil zk-current-files))
        (zk-refresh-browser)
        (run-hooks 'zk-filter-hook)))))

(defun zk-filter-decrement ()
  "Remove last character from the filter, if possible, and update.

In incremental search mode, the elements of `zk-filter-regexp'
are the words of the filter string in reverse order.  In regexp
search mode, the list is a single element containing the entire
filter regexp.  Therefore, in both cases, only the car of
`zk-filter-regexp' is modified."
  (interactive)
  (let ((str (car zk-filter-regexp)))
    (zk-filter
     (if (> (length str) 0)
         ;; If the last string element has at least one character,
         ;; simply remove the last character.
         (substring str 0 -1)
       ;; Otherwise, return nil
       nil))))

(defun zk-filter-decrement-word ()
  "Remove last word from the filter, if possible, and update."
  (interactive)
  (zk-filter
   (if zk-incremental-search
       ;; In incremental search mode, remove the car
       nil
     ;; In regexp search mode, remove last "word" component
     ;; (replace-regexp-in-string "[[:space:]\n]*$" "" s)
     (let ((str (car zk-filter-regexp)))
       (if (> (length str) 0)
           (with-temp-buffer
             (insert (concat "\"" str "\""))
             (lisp-interaction-mode)
             (goto-char (- (point-max) 1))
             (backward-word 1)
             (buffer-substring 2 (point)))
         nil)))))

(defun zk-filter-yank ()
  "Append the most recently killed or yanked text to the filter."
  (interactive)
  (zk-filter
   (concat (zk-whole-filter-regexp) (current-kill 0 t)) t))

(defun zk-next-widget ()
  (let ((p (point)))
    (condition-case nil
        (progn
          (widget-move 1)
          (when (> (point) p)
            (widget-at)))
      (error nil))))

(defun zk-previous-widget ()
  (let ((p (point)))
    (condition-case nil
        (progn
          (widget-move -1)
          (when (< (point) p)
            (widget-at)))
      (error nil))))

(defun zk-complete ()
  "Complete the current action.
If there is a widget at the point, press it.  If a filter is
applied and there is at least one match, open the first matching
file.  If there is an active filter but there are no matches,
quick create a new file using the filter string as the initial content.
Otherwise, quick create a new file."
  (interactive)
  (if zk-complete-fun
      (funcall zk-complete-fun)
    (let ((widget (or (widget-at) (save-excursion (zk-previous-widget)))))
      (if widget
          (widget-apply-action widget)
        (zk-new-file)))))

(defun zk-quit ()
  (interactive)
  (setq zk-complete-fun nil)
  (quit-window))

;;; Automatic File Saving

(defun zk-auto-save ()
  "Save any modified files in the list of auto-save files."
  (dolist (buf zk-auto-save-buffers)
    (if (buffer-name buf)
        ;; Save open buffers that have been modified.
        (with-current-buffer buf
          (when (buffer-modified-p)
            (basic-save-buffer)))
      ;; If a buffer is no longer open, remove it from auto save list.
      (delq buf zk-auto-save-buffers))))

;;; Mode definition

(defun zk-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "Zk %s" zk-version))

(defun zk-setup ()
  "Prepare environment by creating the Zk notes directory."
  (interactive)
  (when (not (file-exists-p zk-directory))
    (make-directory zk-directory t))
  (zk-refresh))

;; Zk mode is suitable only for specially-prepared text
(put 'zk-mode 'mode-class 'special)

(defun zk-mode ()
  "Major mode for quickly browsing, filtering, and editing plain text notes.
Turning on `zk-mode' runs the hook `zk-mode-hook'.

\\{zk-mode-map}."
  (message "Zk initializing...")
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq default-directory (expand-file-name zk-directory))
  (setq zk-window-width (if (zk-buffer-visible-p)
                            (zk-current-window-width)
                          (frame-text-cols)))

  ;; Visual line mode causes lines to wrap, so turn it off.
  (when (fboundp 'visual-line-mode)
    (visual-line-mode 0))

  (use-local-map zk-mode-map)
  (zk-cache-initialize)
  (zk-cache-update-all)
  (zk-filter-initialize)
  (setq major-mode 'zk-mode)
  (zk-set-mode-name)
  (zk-buffer-setup)
  (add-hook 'window-size-change-functions
            'zk-window-size-change-function t)
  (add-hook 'window-configuration-change-hook
            'zk-window-configuration-change-function t)
  (when (> zk-auto-save-interval 0)
    (run-with-idle-timer zk-auto-save-interval t 'zk-auto-save))
  (run-mode-hooks 'zk-mode-hook)
  (message "Zk loaded %d files." (length zk-all-files)))

(put 'zk-mode 'mode-class 'special)

;;;###autoload
(defun zk ()
  "Switch to *Zk* buffer and load files."
  (interactive)
  (setq zk-complete-fun nil)
  (switch-to-buffer zk-buffer)
  (if (not (eq major-mode 'zk-mode))
      (zk-mode)))

;;; Searching

(defun zk-file-p (file)
  (when file
    (string-match-p
     (regexp-quote (file-truename zk-directory))
     file)))

(defun zk-search (&optional str filenames-only)
  "Search zk using STR if given, otherwise use either the region or the id of
the current file (if it is a zk file)."
  (interactive)
  (setq str
        (if (null str)
            (if (use-region-p)
                (buffer-substring-no-properties (region-beginning)
                                                (region-end))
              (when (zk-file-p (buffer-file-name))
                (concat "[[zk:" (zk-lift-id (buffer-file-name)) "]]")))
          str))
  ;; Sanitize the filter string
  (setq str (replace-regexp-in-string "[[:space:]\n]+" " " str))
  ;; Call zk search on the filter string
  (zk)
  (if filenames-only
      (let ((zk-filter-only-filenames t))
        (zk-filter str t))
    (let ((zk-incremental-search t))
      (zk-filter str t)))
  ;; If there is a single match, return it
  (when (eq (length zk-current-files) 1)
    (car zk-current-files)))

;;; Navigation

(defun zk-push-history (marker)
  (ring-insert-at-beginning zk-navigation-history marker))

(defun zk-pop-history ()
  (ring-remove zk-navigation-history))

(defun zk-follow ()
  (interactive)
  (let ((link (org-element-lineage (org-element-context) '(link) t)))
    (when (and link (equal (org-element-property :type link) "zk"))
      (zk-push-history (point-marker))
      (zk-open-file (zk-absolute-filename (org-element-property :path link))))))

(defun zk-back ()
  (interactive)
  (let ((marker (zk-pop-history)))
    (when marker
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker))
      (set-marker marker nil nil))))

(defun zk-current-id ()
  (zk-lift-id (buffer-file-name)))

(defun zk-next ()
  (interactive)
  (let ((next (zk-id-next (zk-current-id))))
    (if (null next)
        (message "No more nodes")
      (zk-push-history (point-marker))
      (zk-open-file (zk-absolute-filename next)))))

(defun zk-prev ()
  (interactive)
  (let ((next (zk-id-prev (zk-current-id))))
    (if (null next)
        (message "No more nodes")
      (zk-push-history (point-marker))
      (zk-open-file (zk-absolute-filename next)))))

(defun zk-up ()
  (interactive)
  (let ((next (zk-id-parent (zk-current-id))))
    (if (null next)
        (message "No more nodes")
      (zk-push-history (point-marker))
      (zk-open-file (zk-absolute-filename next)))))

(defun zk-next-result ()
  (interactive)
  (zk-push-history (point-marker))
  (when (not (with-current-buffer zk-buffer
               (let ((p (point))
                     (widget (zk-next-widget)))
                 (if widget
                     (progn
                       (widget-apply-action widget)
                       t)
                   (message "No more results")
                   (goto-char p)
                   nil))))
    (zk-pop-history)))

(defun zk-prev-result ()
  (interactive)
  (zk-push-history (point-marker))
  (when (not (with-current-buffer zk-buffer
               (let ((p (point))
                     (widget (zk-previous-widget)))
                 (if widget
                     (progn
                       (widget-apply-action widget)
                       t)
                   (message "No more results")
                   (goto-char p)
                   nil))))
    (zk-pop-history)))

;; this should probably be a minor mode?
(defun zk-navigate-keys ()
  (local-set-key (kbd "M-.") 'zk-follow)
  (local-set-key (kbd "M-,") 'zk-back)
  (local-set-key (kbd "M-g M-n") 'zk-next-result)
  (local-set-key (kbd "M-g M-p") 'zk-prev-result)
  (local-set-key (kbd "C-c n n") 'zk-new-next)
  (local-set-key (kbd "C-c n c") 'zk-new-child)
  (local-set-key (kbd "C-c n f") 'zk-new-file)
  (local-set-key (kbd "C-c l") 'zk-link)
  (local-set-key (kbd "C-c f n") 'zk-next)
  (local-set-key (kbd "C-c f p") 'zk-prev)
  (local-set-key (kbd "C-c f u") 'zk-up))

;;; Link management

(declare-function org-store-link-props "org")
(declare-function org-add-link-type "org")
(declare-function org-open-file-with-emacs "org")

(defun org-zk-store-link ()
  "Store the Zk widget at point as an org-mode link."
  (when (equal major-mode 'zk-mode)
    (let ((link (concat "zk:" (file-name-nondirectory (zk-filename-at-point))))
          (title (zk-file-title (zk-filename-at-point))))
      (org-store-link-props
       :type "zk"
       :link link
       :description title))))

(with-eval-after-load 'org
  (if (fboundp 'org-link-set-parameters)
      (org-link-set-parameters
       "zk" :follow 'zk--org-follow-link :store 'org-zk-store-link)
    (org-add-link-type
     "zk"
     (lambda (handle)
       (org-open-file-with-emacs
        (expand-file-name handle zk-directory))))
    (add-hook 'org-store-link-functions 'org-zk-store-link)))

(defun zk--org-follow-link (id)
  (org-open-file-with-emacs
   (zk-absolute-filename id)))

(defun zk-make-link (id)
  (let ((file (expand-file-name (concat zk-directory id ".org"))))
    (concat "[[zk:" id "][" (zk-file-title file) "]]")))

(defun zk-lift-id (fname)
  "Extract the zk ID from STR."
  (file-name-base fname))

(defun zk-complete-file-result ()
  (cond
   ;; Activate widget
   ((widget-at)
    (zk-filename-at-point))
   ;; Active filter string with match
   ((and zk-filter-regexp zk-current-files)
    (car zk-current-files))))

(defun zk-complete-id-result ()
  (let ((fname (zk-complete-file-result)))
    (when fname
      (zk-lift-id fname))))

(defun zk-complete-insert-link ()
  (let ((id (zk-complete-id-result)))
    (when id
      (setq zk-complete-fun nil)
      (switch-to-buffer zk-complete-insert-buffer)
      (insert (zk-make-link id)))))

(defun zk-link ()
  "Find zk file FILE and insert a link."
  (interactive)
  (let ((buf (current-buffer)))
    (zk)
    (setq zk-complete-insert-buffer buf)
    (setq zk-complete-fun 'zk-complete-insert-link)))

(defun zk-new-next ()
  (interactive)
  (let* ((curr-buf (current-buffer))
         (current-id (zk-current-id))
         (next-id (zk-id-new current-id)))
    (zk-push-history (point-marker))
    (zk-new-file-named next-id)))

(defun zk-new-child ()
  (interactive)
  (let* ((curr-buf (current-buffer))
         (current-id (zk-current-id))
         (child-id (zk-id-new-child current-id)))
    (zk-push-history (point-marker))
    (zk-new-file-named child-id)))

;;; Miscellaneous

(defun zk-count-words ()
  "Prints total number of words and notes in the minibuffer."
  (interactive)
  (let ((numWords 0))
    (dolist (zkFile zk-all-files)
      (with-temp-buffer
        (insert-file-contents zkFile)
        (setq numWords (+ numWords (count-words (point-min) (point-max))))))
    (message "Your zettelkasten contains %s notes with %s words in total."
             (length zk-all-files) numWords)))

(provide 'zk)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; indent-tabs-mode: nil
;; End:

;;; zk.el ends here
