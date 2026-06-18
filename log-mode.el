;;; log-mode.el --- Tag-based paragraph browser with read/unread state -*- lexical-binding: t -*-

;; Author: You
;; Version: 1.2
;; Description: Browse paragraphs by tag across multiple files, with
;;              read/unread tracking, read-filter toggle, inline editing,
;;              and pagination.

;;; Commentary:
;;
;; Tags live anywhere inside a paragraph, wrapped in square brackets,
;; comma-separated:  [video-games, pinball, history]
;;
;; Entry point:  M-x log-browse  (or M-x log)
;;
;; Inside the *Log* buffer:
;;   n / p         next / previous page
;;   TAB / S-TAB   next / previous paragraph
;;   r             toggle read/unread for paragraph at point
;;   u             force unread
;;   s             cycle read-state filter: ALL → unread-only → read-only → ALL
;;   e             edit paragraph at point (opens source file, jumps to it)
;;   f             filter with AND (all tags must match)
;;   F             filter with OR  (any tag matches)
;;   t             edit tag aliases (poem, poems → treated as one)
;;   l             open (or create) YEAR%-AGE.org journal file (uses clock.el)
;;   d             toggle date sort: ↓ newest first (default) / ↑ oldest first
;;   q             quit
;;
;; Date sort parses the YEAR%-AGE.org filename convention; age is the primary
;; sort key and year-percent the secondary one (e.g. 44-31 > 43-31 > 99-30).
;;
;; Status bar shows:  Log[tag] p.N/M  R read / U unread  [filter-state] [↓/↑date]

;;; Code:

(require 'cl-lib)
(require 'ispell)

;; ---------------------------------------------------------------------------
;; Improved race guards

(defun log-mode--flyspell-sanitize-otherchars (orig-fun &rest args)
  "Force `ispell-otherchars` to be a string before running the original function."
  (let ((ispell-otherchars (if (stringp (bound-and-true-p ispell-otherchars))
                               ispell-otherchars
                             "")))
    (apply orig-fun args)))

(with-eval-after-load 'flyspell
  ;; Advice existing functions
  (advice-add 'flyspell-word :around #'log-mode--flyspell-sanitize-otherchars)
  (advice-add 'flyspell-get-word :around #'log-mode--flyspell-sanitize-otherchars)
  (advice-add 'flyspell-post-command-hook :around #'log-mode--flyspell-sanitize-otherchars)
  (advice-add 'flyspell-after-change-function :around #'log-mode--flyspell-sanitize-otherchars))

;; Crucial: This specifically targets the org-element error you posted
(with-eval-after-load 'org-element
  (defun log-mode--silence-org-cache-error (orig-fun &rest args)
    (condition-case nil
        (apply orig-fun args)
      (wrong-type-argument nil))) ;; Silently drop the type error

  (advice-add 'org-element--cache-sync :around #'log-mode--silence-org-cache-error))

;; ---------------------------------------------------------------------------
;; Customisation

(defcustom log-mode-state-file
  (expand-file-name "log-mode-read-state.eld" user-emacs-directory)
  "File that persists read/unread state across sessions."
  :type 'file
  :group 'log-mode)

(defcustom log-mode-auto-export-tags t
  "If non-nil, automatically export tags to `log-mode-tags-export-file'
every time you run `M-x log'."
  :type 'boolean
  :group 'log-mode)

(defcustom log-mode-tags-export-file
  (expand-file-name "log-mode-exported-tags.txt" user-emacs-directory)
  "Path where `log-mode-export-tags' will save the list of collected tags.
Each tag is written on a new line, sorted by frequency."
  :type 'file
  :group 'log-mode)

(defgroup log-mode nil
  "Tag-based paragraph browser."
  :group 'convenience)

(defcustom log-mode-page-size 20
  "Number of paragraphs per page."
  :type 'integer
  :group 'log-mode)

(defcustom log-mode-search-path nil
  "Default directory (or list of directories) to search for text files.
When nil, you are prompted when calling `log'."
  :type '(choice (const nil)
                 directory
                 (repeat directory))
  :group 'log-mode)

(defcustom log-mode-file-extensions '("txt" "md" "org" "rst")
  "File extensions to scan."
  :type '(repeat string)
  :group 'log-mode)

(defcustom log-mode-alias-file
  (expand-file-name "log-mode-aliases.txt" user-emacs-directory)
  "Path to the tag alias file.
Each line contains comma-separated tags that are treated as equivalent.
Example:
  poem, poems
  tag, tags"
  :type 'file
  :group 'log-mode)

(defcustom log-mode-device-folder nil
  "Subfolder name for this device under `log-mode-search-path'.
When non-nil, the `l' key creates and opens journal files inside this
subdirectory rather than directly in the search path.
Example: set to \"desktop\" so pressing `l' opens
/your-log-path/desktop/45-30.org instead of /your-log-path/45-30.org.
Browsing with `log' always recurses into all subdirectories regardless."
  :type '(choice (const :tag "None (use search path directly)" nil)
                 (string :tag "Device subfolder name"))
  :group 'log-mode)

;; ---------------------------------------------------------------------------
;; Persistent read-state

(defvar log-mode--read-set (make-hash-table :test #'equal)
  "Hash-table: paragraph-id → t when read.")

(defun log-mode--state-load ()
  (when (file-exists-p log-mode-state-file)
    (with-temp-buffer
      (insert-file-contents log-mode-state-file)
      (let ((data (ignore-errors (read (current-buffer)))))
        (when (hash-table-p data)
          (setq log-mode--read-set data))))))

(defun log-mode--state-save ()
  (with-temp-file log-mode-state-file
    (prin1 log-mode--read-set (current-buffer))))

(defun log-mode--para-id (file char-offset)
  (format "%s::%d" (expand-file-name file) char-offset))

(defun log-mode--read-p (id)
  (gethash id log-mode--read-set))

(defun log-mode--set-read (id value)
  (if value
      (puthash id t log-mode--read-set)
    (remhash id log-mode--read-set))
  (log-mode--state-save))

;; ---------------------------------------------------------------------------
;; Tag aliases

(defvar log-mode--aliases nil
  "List of alias groups. Each group is a list of equivalent tag strings.")

(defun log-mode--aliases-load ()
  "Load tag aliases from `log-mode-alias-file'."
  (setq log-mode--aliases nil)
  (when (file-exists-p log-mode-alias-file)
    (with-temp-buffer
      (insert-file-contents log-mode-alias-file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (trimmed (string-trim line)))
          (unless (string-empty-p trimmed)
            (let ((group (cl-remove-if #'string-empty-p
                                       (mapcar #'string-trim
                                               (split-string trimmed ",")))))
              (when (>= (length group) 2)
                (push (mapcar #'downcase group) log-mode--aliases)))))
        (forward-line 1)))))

(defun log-mode--alias-group (tag)
  "Return the alias group containing TAG, or nil if none."
  (cl-find-if (lambda (group) (member tag group)) log-mode--aliases))

(defun log-mode--expand-tags (tags)
  "Expand TAGS by replacing each tag with its full alias group (union, deduped)."
  (let (expanded)
    (dolist (tag tags)
      (let ((group (log-mode--alias-group tag)))
        (if group
            (dolist (alias group)
              (cl-pushnew alias expanded :test #'equal))
          (cl-pushnew tag expanded :test #'equal))))
    expanded))

;; ---------------------------------------------------------------------------
;; Parsing

(defun log-mode--collect-files (paths)
  (let (files)
    (dolist (path (if (listp paths) paths (list paths)))
      (cond
       ((file-directory-p path)
        (dolist (ext log-mode-file-extensions)
          (dolist (f (directory-files-recursively
                      path
                      (concat "\\." (regexp-quote ext) "\\'")))
            (unless (string-match-p "/\\.#" f)
              (push f files)))))
       ((and (file-exists-p path)
             (not (string-match-p "/\\.#" path)))
        (push path files))))
    (delete-dups files)))

(defun log-mode--extract-tags (text)
  "Return list of tag strings found in TEXT, skipping org-mode [[links]].
A tag bracket must not be preceded or followed by another bracket."
  (let ((tags nil)
        (start 0))
    (while (string-match "\\[\\([^][]+\\)\\]" text start)
      (let* ((mbeg (match-beginning 0))
             (mend (match-end 0))
             (raw  (match-string 1 text))
             (pre  (and (> mbeg 0) (aref text (1- mbeg))))
             (post (and (< mend (length text)) (aref text mend))))
        (setq start mend)
        (unless (or (and pre  (or (= pre  ?\[) (= pre  ?\])))
                    (and post (or (= post ?\[) (= post ?\]))))
          (dolist (tag (split-string raw ","))
            (let ((trimmed (downcase (string-trim tag))))
              (unless (string-empty-p trimmed)
                (push trimmed tags)))))))
    (nreverse tags)))

(defun log-mode--paragraphs-in-file (file)
  "Return list of plists (:text :tags :id :file) for every paragraph in FILE."
  (with-temp-buffer
    ;; Null out ALL hooks that org-element/flyspell/jit-lock register
    ;; globally, so reading file content into this temp buffer doesn't
    ;; corrupt their state.
    (let ((inhibit-modification-hooks t)
          (after-change-functions nil)
          (before-change-functions nil)
          (after-insert-file-functions nil)
          (jit-lock-functions nil))
      (insert-file-contents file))
    (let ((paras nil))
      (goto-char (point-min))
      (while (not (eobp))
        ;; skip blank lines
        (while (and (not (eobp)) (looking-at "^[ \t]*$"))
          (forward-line 1))
        (unless (eobp)
          (let ((start (point)))
            ;; consume non-blank lines
            (while (and (not (eobp)) (not (looking-at "^[ \t]*$")))
              (forward-line 1))
            (let* ((text (string-trim
                          (buffer-substring-no-properties start (point))))
                   (tags (log-mode--extract-tags text)))
              (unless (string-empty-p text)
                (push (list :text text
                            :tags (copy-sequence tags)
                            :id   (log-mode--para-id file start)
                            :file file)
                      paras))))))
      (nreverse paras))))

(defun log-mode--all-tags (paragraphs)
  "Return list of unique tags across PARAGRAPHS, ordered by frequency (descending).
Tags with equal frequency are sorted alphabetically as a tiebreaker."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (p paragraphs)
      (dolist (tag (plist-get p :tags))
        (puthash tag (1+ (gethash tag counts 0)) counts)))
    (let (all)
      (maphash (lambda (tag _) (push tag all)) counts)
      (sort all (lambda (a b)
                  (let ((ca (gethash a counts 0))
                        (cb (gethash b counts 0)))
                    (if (= ca cb)
                        (string< a b)
                      (> ca cb))))))))

(defun log-mode--filter-paragraphs (paragraphs tags)
  "Keep PARAGRAPHS matching every tag in TAGS (AND semantics, alias-aware).
Each tag in TAGS is expanded to its alias group; a paragraph matches if it
contains at least one member of each group."
  (if (null tags)
      paragraphs
    (cl-remove-if-not
     (lambda (p)
       (let ((ptags (plist-get p :tags)))
         (cl-every
          (lambda (tag)
            (let ((group (or (log-mode--alias-group tag) (list tag))))
              (cl-some (lambda (alias) (member alias ptags)) group)))
          tags)))
     paragraphs)))

(defun log-mode--filter-paragraphs-or (paragraphs tags)
  "Keep PARAGRAPHS containing at least one tag in TAGS (OR semantics, alias-aware)."
  (if (null tags)
      paragraphs
    (cl-remove-if-not
     (lambda (p)
       (let ((ptags (plist-get p :tags)))
         (cl-some
          (lambda (tag)
            (let ((group (or (log-mode--alias-group tag) (list tag))))
              (cl-some (lambda (alias) (member alias ptags)) group)))
          tags)))
     paragraphs)))

(defun log-mode--apply-tag-filter (all-paragraphs tags mode)
  "Apply tag filter with MODE (and/or) to ALL-PARAGRAPHS."
  (if (eq mode 'or)
      (log-mode--filter-paragraphs-or all-paragraphs tags)
    (log-mode--filter-paragraphs all-paragraphs tags)))

(defun log-mode--apply-read-filter (paragraphs read-filter)
  "Apply READ-FILTER to PARAGRAPHS. Values: nil=all, unread, read."
  (cond
   ((eq read-filter 'unread)
    (cl-remove-if (lambda (p) (log-mode--read-p (plist-get p :id))) paragraphs))
   ((eq read-filter 'read)
    (cl-remove-if-not (lambda (p) (log-mode--read-p (plist-get p :id))) paragraphs))
   (t paragraphs)))

;; ---------------------------------------------------------------------------
;; Date sorting

(defun log-mode--filename-date-key (file)
  "Return a numeric sort key for FILE based on the YEAR%-AGE.org convention.
Parses the basename as YEARPCT-AGE and returns (AGE * 10000 + YEARPCT), so
age is the primary sort key and year-percent the secondary one.
Examples: 44-31.org → 310044, 43-31.org → 310043, 99-30.org → 300099.
Files that do not match the pattern return 0 and sort to the bottom."
  (let ((base (file-name-base (file-name-nondirectory file))))
    (if (string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" base)
        (+ (* (string-to-number (match-string 2 base)) 10000)
           (string-to-number (match-string 1 base)))
      0)))

(defun log-mode--sort-paragraphs-by-date (paragraphs order)
  "Return a sorted copy of PARAGRAPHS by their filename date key.
ORDER `desc' puts newest (highest key) first; `asc' puts oldest first."
  (cl-sort (copy-sequence paragraphs)
           (if (eq order 'asc) #'< #'>)
           :key (lambda (p)
                  (log-mode--filename-date-key (plist-get p :file)))))

(defun log-mode--date-sort-str ()
  "Return a short display string for the current date sort direction."
  (if (eq log-mode--date-sort 'asc) "↑date" "↓date"))

;; ---------------------------------------------------------------------------
;; Buffer state

(defvar-local log-mode--editing-para-id nil
  "ID of the paragraph most recently opened for editing.")
(defvar-local log-mode--editing-para-text nil
  "Text of the paragraph being edited, to track it if its offset shifts.")
(defvar-local log-mode--editing-prev-id nil
  "ID of the preceding paragraph, for fallback if the edited one is deleted.")
(defvar-local log-mode--editing-prev-text nil
  "Text of the preceding paragraph, for fallback if its offset shifts.")
(defvar-local log-mode--editing-next-id nil
  "ID of the following paragraph, preferred focus if the edited one is deleted.")
(defvar-local log-mode--editing-next-text nil
  "Text of the following paragraph, for fallback if its offset shifts.")
(defvar-local log-mode--paragraphs nil    "Tag-filtered paragraph list.")
(defvar-local log-mode--all-paragraphs nil "Unfiltered paragraph list.")
(defvar-local log-mode--filter-tags nil   "Active tag filter list.")
(defvar-local log-mode--filter-mode 'and  "Filter mode: and or or.")
(defvar-local log-mode--read-filter nil   "Read-state filter: nil, unread, or read.")
(defvar-local log-mode--page 1            "Current page (1-based).")
(defvar-local log-mode--total-pages 1     "Total pages.")
(defvar-local log-mode--para-markers nil  "Alist of (ID . marker) for current page.")
(defvar-local log-mode--date-sort 'desc
  "Date sort direction: desc (newest first) or asc (oldest first).")

(defun log-mode--visible-paragraphs ()
  "Return paragraphs after tag-filter, read-filter, and date sort."
  (log-mode--sort-paragraphs-by-date
   (log-mode--apply-read-filter log-mode--paragraphs log-mode--read-filter)
   log-mode--date-sort))

;; ---------------------------------------------------------------------------
;; Rendering

(defface log-mode-tag-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for tags inside paragraphs.")

(defface log-mode-read-face
  '((t :inherit shadow))
  "Face for paragraphs marked as read.")

(defface log-mode-unread-face
  '((t :inherit default :weight normal))
  "Face for unread paragraphs.")

(defface log-mode-header-face
  '((t :inherit header-line :height 1.1 :weight bold))
  "Face for the page header.")

(defface log-mode-file-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for the file name annotation.")

(defun log-mode--highlight-tags (text)
  "Return TEXT with tag brackets propertized, using string-match (no temp buffer)."
  (let ((result (copy-sequence text))
        (start 0))
    (while (string-match "\\[\\([^]]+\\)\\]" result start)
      (put-text-property (match-beginning 0) (match-end 0)
                         'face 'log-mode-tag-face result)
      (setq start (match-end 0)))
    result))

(defun log-mode--read-filter-str ()
  (cond ((eq log-mode--read-filter 'unread) "unread-only")
        ((eq log-mode--read-filter 'read)   "read-only")
        (t "all")))

(defun log-mode--render (&optional target-id)
  "Redraw the *Log* buffer for the current page.
If TARGET-ID is given and its paragraph is in the current visible list,
navigate to the page that contains it before rendering, then place point
on it.  If TARGET-ID is not in the visible list (filtered out or deleted),
render falls back to the first paragraph on the current page."
  (with-current-buffer (get-buffer-create "*Log*")
    (let* ((inhibit-read-only t)
           (visible (log-mode--visible-paragraphs))
           (total (length visible))
           (page-size log-mode-page-size)
           (total-pages (max 1 (ceiling total page-size)))
           ;; If the target paragraph exists in the visible list, jump to
           ;; whichever page holds it.  This must happen before (page …) is
           ;; bound so the slice is built for the right page.
           (_ (when target-id
                (let ((idx (cl-position target-id visible
                                        :key (lambda (p) (plist-get p :id))
                                        :test #'equal)))
                  (when idx
                    (setq log-mode--page (1+ (/ idx page-size)))))))
           (page (max 1 (min log-mode--page total-pages)))
           (slice-start (* (1- page) page-size))
           (slice (cl-subseq visible slice-start
                             (min (+ slice-start page-size) total)))
           (tag-str (if log-mode--filter-tags
                        (format "%s[%s]"
                                (if (eq log-mode--filter-mode 'or) "OR:" "AND:")
                                (mapconcat #'identity log-mode--filter-tags ", "))
                      "ALL"))
           (all-count (length log-mode--paragraphs))
           (read-count (cl-count-if
                        (lambda (p) (log-mode--read-p (plist-get p :id)))
                        log-mode--paragraphs))
           markers)
      (setq log-mode--page page
            log-mode--total-pages total-pages)
      (erase-buffer)
      (insert (propertize
               (format " log[%s]  %s  %s  page %d/%d  (%d shown · %d read · %d unread) \n"
                       tag-str
                       (log-mode--read-filter-str)
                       (log-mode--date-sort-str)
                       page total-pages total
                       read-count (- all-count read-count))
               'face 'log-mode-header-face))
      (insert "\n")
      (if (null slice)
          (insert "  (no paragraphs match)\n")
        (dolist (para slice)
          (let* ((id     (plist-get para :id))
                 (text   (plist-get para :text))
                 (file   (plist-get para :file))
                 (read   (log-mode--read-p id))
                 (face   (if read 'log-mode-read-face 'log-mode-unread-face))
                 (marker (make-marker)))
            (set-marker marker (point))
            (push (cons id marker) markers)
            (insert (propertize (if read "  ✓ " "  · ") 'face face))
            (let ((highlighted (log-mode--highlight-tags text)))
              (add-face-text-property 0 (length highlighted) face t highlighted)
              (insert highlighted))
            (insert "\n")
            (insert (propertize (format "    ↳ %s\n" (abbreviate-file-name file))
                                'face 'log-mode-file-face))
            (insert "\n"))))
      (setq log-mode--para-markers (nreverse markers))
      (let* ((first-pos (and log-mode--para-markers
                             (marker-position (cdar log-mode--para-markers))))
             (target-pos
              (cond
               (target-id
                (let ((cell (assoc target-id log-mode--para-markers)))
                  (if cell (marker-position (cdr cell)) (or first-pos (point-min)))))
               (t (or first-pos (point-min)))))
             (win (get-buffer-window (current-buffer) t)))
        (goto-char target-pos)
        (when win (set-window-point win target-pos)))
      (setq mode-line-format
            (list (format "  Log[%s]  %s  %s  p.%d/%d  %dR/%dU  "
                          tag-str
                          (log-mode--read-filter-str)
                          (log-mode--date-sort-str)
                          page total-pages
                          read-count (- all-count read-count))
                  'mode-line-end-spaces))
      (force-mode-line-update))))

;; ---------------------------------------------------------------------------
;; Navigation helpers

(defun log-mode--para-at-point ()
  "Return the paragraph ID whose marker is at or just before point."
  (let ((pos (point))
        best-id
        (best-dist most-positive-fixnum))
    (dolist (cell log-mode--para-markers)
      (let ((mpos (marker-position (cdr cell))))
        (when (and mpos (<= mpos pos))
          (let ((d (- pos mpos)))
            (when (< d best-dist)
              (setq best-dist d best-id (car cell)))))))
    best-id))

;; ---------------------------------------------------------------------------
;; Tag Minibuffer Auto-Suggestion
;; ---------------------------------------------------------------------------

(defvar log-mode--tag-popup-candidates nil)
(defvar log-mode--tag-popup-index 0)
(defconst log-mode--tag-popup-buf "*Log Tags*")
(defvar log-mode--minibuffer-all-tags nil)

(defface log-mode-tag-popup-selected-face
  '((t :inherit highlight :weight bold))
  "Face for the selected candidate in the tag popup.")

(defun log-mode--tag-popup-col-width (candidates)
  (+ 2 (apply #'max 0 (mapcar #'length candidates))))

(defun log-mode--tag-popup-ncols (candidates)
  (let* ((col-w  (log-mode--tag-popup-col-width candidates))
         (pop-win (get-buffer-window log-mode--tag-popup-buf))
         (win-w  (if pop-win (window-width pop-win) (window-width))))
    (max 1 (/ win-w (max 1 col-w)))))

(defun log-mode--tag-popup-render ()
  (let* ((candidates log-mode--tag-popup-candidates)
         (index      log-mode--tag-popup-index)
         (col-w      (log-mode--tag-popup-col-width candidates))
         (ncols      (log-mode--tag-popup-ncols candidates))
         (nrows      (max 1 (ceiling (length candidates) ncols)))
         (sel-row    (/ index ncols))
         (inhibit-read-only t)
         ;; Inhibit after-change-functions so flyspell / jit-lock don't
         ;; trigger on every keystroke-driven redraw of the popup buffer.
         (inhibit-modification-hooks t))
    (with-current-buffer (get-buffer-create log-mode--tag-popup-buf)
      (erase-buffer)
      (dotimes (row nrows)
        (dotimes (col ncols)
          (let ((i (+ (* row ncols) col)))
            (when (< i (length candidates))
              (let* ((cand   (nth i candidates))
                     (padded (concat cand (make-string (- col-w (length cand)) ?\s))))
                (insert (if (= i index)
                            (propertize padded 'face 'log-mode-tag-popup-selected-face)
                          padded))))))
        (insert "\n"))
      (goto-char (point-min))
      (forward-line sel-row)
      (let ((win (get-buffer-window log-mode--tag-popup-buf)))
        (when win (set-window-point win (point)))))))

(defun log-mode--tag-popup-show (candidates)
  (setq log-mode--tag-popup-candidates candidates)
  (when (>= log-mode--tag-popup-index (length candidates))
    (setq log-mode--tag-popup-index 0))
  (let ((pop-win (get-buffer-window log-mode--tag-popup-buf)))
    (unless pop-win
      (setq pop-win (display-buffer
                     (get-buffer-create log-mode--tag-popup-buf)
                     '((display-buffer-at-bottom)
                       (window-height . fit-window-to-buffer))))
      (set-window-dedicated-p pop-win t)
      (with-current-buffer log-mode--tag-popup-buf
        (setq buffer-read-only t cursor-type nil mode-line-format nil truncate-lines t)))
    (log-mode--tag-popup-render)
    (when pop-win (fit-window-to-buffer pop-win 10 2))))

(defun log-mode--tag-popup-hide ()
  (let ((win (get-buffer-window log-mode--tag-popup-buf)))
    (when win (delete-window win)))
  (setq log-mode--tag-popup-index 0))

(defun log-mode--tag-popup-move (direction)
  (let* ((candidates log-mode--tag-popup-candidates)
         (len        (length candidates)))
    (when (> len 0)
      (let* ((idx   log-mode--tag-popup-index)
             (ncols (log-mode--tag-popup-ncols candidates))
             (new-idx (pcase direction
                        ('right    (% (1+ idx) len))
                        ('left     (mod (1- idx) len))
                        ('down-col (% (+ idx ncols) len))
                        ('up-col   (mod (- idx ncols) len)))))
        (setq log-mode--tag-popup-index new-idx)
        (log-mode--tag-popup-render)))))

(defun log-mode--trim-left (s)
  "Trim leading whitespace from string S."
  (if (string-match "\\`[ \t]+" s)
      (substring s (match-end 0))
    s))

(defun log-mode--filter-candidates (prefix candidates)
  "Return candidates starting with PREFIX, preserving order."
  (if (string-empty-p prefix)
      candidates
    (let ((completion-ignore-case t))
      (cl-remove-if-not (lambda (c) (string-prefix-p prefix c t)) candidates))))

(defun log-mode--minibuffer-context (all-tags)
  "Return (tok-start prefix candidates) for the current tag being typed."
  (let* ((prompt-end (minibuffer-prompt-end))
         (pos (point))
         (text (buffer-substring-no-properties prompt-end pos))
         (last-comma-idx (let ((idx nil) (start 0))
                           (while (string-match "," text start)
                             (setq idx (match-beginning 0)
                                   start (match-end 0)))
                           idx))
         (tok-start (+ prompt-end (if last-comma-idx (1+ last-comma-idx) 0)))
         (prefix-raw (buffer-substring-no-properties tok-start pos))
         (prefix (log-mode--trim-left prefix-raw))
         (tok-start-adjusted (+ tok-start (- (length prefix-raw) (length prefix))))
         (full-text (buffer-substring-no-properties prompt-end (point-max)))
         (committed (mapcar #'string-trim (split-string full-text "," t)))
         (candidates (cl-remove-if (lambda (t_) (member t_ committed)) all-tags))
         (filtered (log-mode--filter-candidates prefix candidates)))
    (when filtered
      (list tok-start-adjusted prefix filtered))))

(defun log-mode--minibuffer-post-command ()
  (condition-case nil
      (let ((ctx (log-mode--minibuffer-context log-mode--minibuffer-all-tags)))
        (if ctx
            (log-mode--tag-popup-show (nth 2 ctx))
          (log-mode--tag-popup-hide)))
    (error (log-mode--tag-popup-hide))))

(defun log-mode--minibuffer-insert-tag ()
  "Insert the currently selected tag from the popup into the minibuffer."
  (interactive)
  (if (get-buffer-window log-mode--tag-popup-buf)
      (let ((choice (nth log-mode--tag-popup-index log-mode--tag-popup-candidates)))
        (when choice
          (log-mode--tag-popup-hide)
          (let ((ctx (log-mode--minibuffer-context log-mode--minibuffer-all-tags)))
            (when ctx
              (let ((tok-start (nth 0 ctx)))
                (delete-region tok-start (point))
                (insert choice ", "))))))
    (insert ", ")))

(defun log-mode--minibuffer-next ()
  (interactive)
  (if (get-buffer-window log-mode--tag-popup-buf)
      (log-mode--tag-popup-move 'right)
    (forward-word)))

(defun log-mode--minibuffer-prev ()
  (interactive)
  (if (get-buffer-window log-mode--tag-popup-buf)
      (log-mode--tag-popup-move 'left)
    (backward-word)))

(defun log-mode--minibuffer-next-row ()
  (interactive)
  (if (get-buffer-window log-mode--tag-popup-buf)
      (log-mode--tag-popup-move 'down-col)
    (next-line-or-history-element 1)))

(defun log-mode--minibuffer-prev-row ()
  (interactive)
  (if (get-buffer-window log-mode--tag-popup-buf)
      (log-mode--tag-popup-move 'up-col)
    (previous-line-or-history-element 1)))

(defvar log-mode-tag-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map (kbd "TAB")       #'log-mode--minibuffer-insert-tag)
    (define-key map (kbd "M-RET")     #'log-mode--minibuffer-insert-tag)
    (define-key map (kbd "M-<right>") #'log-mode--minibuffer-next)
    (define-key map (kbd "M-<left>")  #'log-mode--minibuffer-prev)
    (define-key map (kbd "M-<down>")  #'log-mode--minibuffer-next-row)
    (define-key map (kbd "M-<up>")    #'log-mode--minibuffer-prev-row)
    map))

(defun log-mode--read-tags (all-tags &optional initial-input)
  "Prompt for multiple tags with comma-separation and inline popup completion."
  (let* ((log-mode--minibuffer-all-tags all-tags)
         (input (unwind-protect
                    (minibuffer-with-setup-hook
                        (lambda ()
                          (add-hook 'post-command-hook #'log-mode--minibuffer-post-command nil t)
                          ;; Only insert initial-input if it is provided and not empty
                          (when (and initial-input (not (string-empty-p initial-input)))
                            (insert initial-input)))
                      (read-from-minibuffer "Tags (comma separated, TAB to complete, RET to finish): "
                                            nil log-mode-tag-minibuffer-map))
                  (log-mode--tag-popup-hide)
                  (remove-hook 'post-command-hook #'log-mode--minibuffer-post-command t))))
    (delete-dups (cl-remove-if #'string-empty-p
                               (mapcar #'string-trim (split-string input "," t))))))

;; ---------------------------------------------------------------------------
;; Commands

(defun log-mode-export-tags (&optional silent)
  "Export all collected tags to `log-mode-tags-export-file`.
If SILENT is non-nil, suppress the completion message in the echo area."
  (interactive)
  (unless log-mode--all-paragraphs
    (user-error "No paragraphs loaded. Run `log` first to initialize data."))
  (let ((tags (log-mode--all-tags log-mode--all-paragraphs))
        (target log-mode-tags-export-file))
    (with-temp-file target
      (insert "# Auto-generated list of all tags used in log-mode\n")
      (dolist (tag tags)
        (insert tag "\n")))
    (unless silent
      (message "Saved %d unique tags to %s" (length tags) target))))

(defun log-mode-next-page ()
  "Go to the next page."
  (interactive)
  (if (>= log-mode--page log-mode--total-pages)
      (message "Already on last page.")
    (setq log-mode--page (1+ log-mode--page))
    (log-mode--render)))

(defun log-mode-prev-page ()
  "Go to the previous page."
  (interactive)
  (if (<= log-mode--page 1)
      (message "Already on first page.")
    (setq log-mode--page (1- log-mode--page))
    (log-mode--render)))

(defun log-mode-next-paragraph ()
  "Move point to the next paragraph on this page."
  (interactive)
  (let ((pos (point)) next)
    (dolist (cell log-mode--para-markers)
      (let ((mpos (marker-position (cdr cell))))
        (when (and mpos (> mpos pos))
          (unless (and next (< (marker-position next) mpos))
            (setq next (cdr cell))))))
    (if next (goto-char (marker-position next))
      (message "No more paragraphs on this page."))))

(defun log-mode-prev-paragraph ()
  "Move point to the previous paragraph on this page."
  (interactive)
  (let ((pos (point)) prev)
    (dolist (cell log-mode--para-markers)
      (let ((mpos (marker-position (cdr cell))))
        (when (and mpos (< mpos pos))
          (when (or (null prev) (> mpos (marker-position prev)))
            (setq prev (cdr cell))))))
    (if prev (goto-char (marker-position prev))
      (message "Already at first paragraph."))))

(defun log-mode-mark-read ()
  "Toggle read/unread for paragraph at point, then advance to next paragraph."
  (interactive)
  (let ((id (log-mode--para-at-point)))
    (if (null id) (message "No paragraph at point.")
      (let* ((now-read (not (log-mode--read-p id)))
             ;; find the next paragraph id before we re-render
             (pos (point))
             (next-id (let (next-cell)
                        (dolist (cell log-mode--para-markers)
                          (let ((mpos (marker-position (cdr cell))))
                            (when (and mpos (> mpos pos))
                              (unless (and next-cell
                                           (< (marker-position (cdr next-cell)) mpos))
                                (setq next-cell cell)))))
                        (car next-cell))))
        (log-mode--set-read id now-read)
        (message "Marked as %s." (if now-read "read" "unread"))
        ;; advance to next para, or stay on current if last on page
        (log-mode--render (or next-id id))))))

(defun log-mode-mark-unread ()
  "Force unread for paragraph at point."
  (interactive)
  (let ((id (log-mode--para-at-point)))
    (if (null id) (message "No paragraph at point.")
      (log-mode--set-read id nil)
      (message "Marked as unread.")
      (log-mode--render id))))

(defun log-mode-cycle-read-filter ()
  "Cycle read-state filter: all → unread-only → read-only → all."
  (interactive)
  (setq log-mode--read-filter
        (cond ((null log-mode--read-filter) 'unread)
              ((eq log-mode--read-filter 'unread) 'read)
              (t nil))
        log-mode--page 1)
  (message "Showing: %s" (log-mode--read-filter-str))
  (log-mode--render))

(defun log-mode-edit-paragraph ()
  "Open the source file of the paragraph at point and jump to it in the current window."
  (interactive)
  (let ((id (log-mode--para-at-point)))
    (if (null id)
        (message "No paragraph at point.")
      (if (not (string-match "^\\(.*\\)::\\([0-9]+\\)$" id))
          (message "Cannot parse paragraph id: %s" id)
        (let* ((file   (match-string 1 id))
               (offset (string-to-number (match-string 2 id)))
               (para   (cl-find id log-mode--all-paragraphs
                                :key (lambda (p) (plist-get p :id))
                                :test #'equal))
               (text   (and para (plist-get para :text))))
          (if (not (file-exists-p file))
              (message "File not found: %s" file)
            (let ((log-buf (current-buffer)))
              (setq log-mode--editing-para-id id)
              (setq log-mode--editing-para-text text)
              (setq log-mode--editing-prev-id nil
                    log-mode--editing-prev-text nil)

              ;; Find the visual preceding paragraph
              (catch 'found
                (let ((prev-id nil) (prev-text nil) (found-current nil))
                  (dolist (p (log-mode--visible-paragraphs))
                    (cond
                     (found-current
                      (setq log-mode--editing-next-id   (plist-get p :id)
                            log-mode--editing-next-text (plist-get p :text))
                      (throw 'found t))
                     ((equal (plist-get p :id) id)
                      (setq log-mode--editing-prev-id   prev-id
              log-mode--editing-prev-text prev-text
              found-current               t))
                     (t
                      (setq prev-id   (plist-get p :id)
                            prev-text (plist-get p :text)))))))

              (find-file file)
              (goto-char (max (point-min) (min (1+ offset) (point-max))))
              (let ((hook-fn nil))
                (setq hook-fn
                      (lambda ()
                        (when (string= (expand-file-name buffer-file-name)
                                       (expand-file-name file))
                          (remove-hook 'after-save-hook hook-fn t)
                          (when (buffer-live-p log-buf)
                            (switch-to-buffer log-buf))
                          (log-mode--rescan-file file log-buf))))
                (add-hook 'after-save-hook hook-fn nil t))
              (message "edit freely. save to return"))))))))

(defun log-mode--best-focus-id (old-id candidates)
  "Return the best paragraph ID from CANDIDATES to focus on after a rescan."
  (let* ((old-file (when (string-match "^\\(.*\\)::\\([0-9]+\\)$" old-id)
                     (match-string 1 old-id)))
         (old-offset (when old-file (string-to-number (match-string 2 old-id)))))
    (or
     ;; 1. Exact match on OLD-ID (paragraph unchanged).
     (plist-get (cl-find old-id candidates :key (lambda (p) (plist-get p :id)) :test #'equal) :id)
     ;; 2. Match by text (paragraph offset shifted).
     (when (and old-file log-mode--editing-para-text)
       (plist-get (cl-find-if (lambda (p)
                                (and (string= (expand-file-name (plist-get p :file))
                                              (expand-file-name old-file))
                                     (string= (plist-get p :text) log-mode--editing-para-text)))
                              candidates)
                  :id))
     ;; 3. Match by next paragraph's ID (prefer forward on deletion).
     (when log-mode--editing-next-id
       (plist-get (cl-find log-mode--editing-next-id candidates
                           :key (lambda (p) (plist-get p :id)) :test #'equal) :id))
     ;; 4. Match by next paragraph's text (if its offset shifted).
     (when (and log-mode--editing-next-text old-file)
       (plist-get (cl-find-if (lambda (p)
                                (and (string= (expand-file-name (plist-get p :file))
                                              (expand-file-name old-file))
                                     (string= (plist-get p :text) log-mode--editing-next-text)))
                              candidates)
                  :id))
     ;; 5. Match by previous paragraph's ID.  ← was 3, renumber comments only
     (when old-offset
       (let ((same-file-cands
              (cl-remove-if-not
               (lambda (p)
                 (and (string= (expand-file-name (plist-get p :file))
                               (expand-file-name old-file))
                      (when (string-match "::\\([0-9]+\\)$" (plist-get p :id))
                        (<= (string-to-number (match-string 1 (plist-get p :id)))
                            old-offset))))
               candidates)))
         (when same-file-cands
           (plist-get (car (last same-file-cands)) :id)))))))

(defun log-mode--rescan-file (file log-buf)
  "Schedule a re-parse of FILE after a short idle delay.
Deferring avoids a race with org-element--cache-sync, which also fires
from after-save-hook and must finish before we read the buffer again."
  (run-with-idle-timer 0.3 nil #'log-mode--do-rescan-file file log-buf))

(defun log-mode--do-rescan-file (file log-buf)
  "Re-parse FILE and update paragraphs in LOG-BUF, then re-render."
  (when (buffer-live-p log-buf)
    (with-current-buffer log-buf
      (let* ((old-id      log-mode--editing-para-id)
             (new-paras   (log-mode--paragraphs-in-file file))
             (other-paras (cl-remove-if
                           (lambda (p) (string= (expand-file-name (plist-get p :file))
                                                (expand-file-name file)))
                           log-mode--all-paragraphs))
             (all         (append other-paras new-paras)))
        ;; Update buffer lists first
        (setq log-mode--all-paragraphs all
              log-mode--paragraphs     (log-mode--filter-paragraphs all log-mode--filter-tags)
              log-mode--page           1)
        ;; THEN find the target ID among items that are actually visible
        (let* ((visible   (log-mode--visible-paragraphs))
               (target-id (and old-id (log-mode--best-focus-id old-id visible))))
          (log-mode--render target-id)
          (message "Log refreshed after save."))))))

(defun log-mode-edit-aliases ()
  "Open the tag alias file for editing.
Each line: comma-separated tags treated as equivalent, e.g. poem, poems.
The Log refreshes automatically when you save."
  (interactive)
  (unless (file-exists-p log-mode-alias-file)
    (with-temp-file log-mode-alias-file
      (insert "# Tag aliases for log-mode\n"
              "# Each line: comma-separated equivalent tags\n"
              "# Example:\n"
              "#   poem, poems\n"
              "#   tag, tags\n")))
  (let ((log-buf (current-buffer)))
    (find-file-other-window log-mode-alias-file)
    (let ((hook-fn nil))
      (setq hook-fn
            (lambda ()
              (when (string= (expand-file-name buffer-file-name)
                             (expand-file-name log-mode-alias-file))
                (remove-hook 'after-save-hook hook-fn t)
                (log-mode--aliases-load)
                (when (buffer-live-p log-buf)
                  (with-current-buffer log-buf
                    (log-mode--render)
                    (message "Aliases reloaded."))))))
      (add-hook 'after-save-hook hook-fn nil t))
    (message "Edit aliases. Save to reload. Format: tag1, tag2 (one group per line)")))

(defun log-mode-change-filter ()
  "Prompt for a new tag filter and apply it using the current filter mode."
  (interactive)
  (let* ((all-tags (log-mode--all-tags log-mode--all-paragraphs))
         (tags     (log-mode--read-tags all-tags nil))) ;; nil = empty/blank
    (setq log-mode--filter-tags tags
          log-mode--paragraphs  (log-mode--apply-tag-filter
                                 log-mode--all-paragraphs tags log-mode--filter-mode)
          log-mode--page 1)
    (log-mode--render)))

(defun log-mode-edit-filter ()
  "Edit the current tag filter while preserving the current AND/OR mode."
  (interactive)
  (let* ((all-tags (log-mode--all-tags log-mode--all-paragraphs))
         (initial-input (mapconcat #'identity log-mode--filter-tags ", "))
         (mode log-mode--filter-mode) ;; Capture current mode
         (tags (log-mode--read-tags all-tags initial-input)))
    (setq log-mode--filter-tags tags
          log-mode--filter-mode mode ;; Preserve mode
          log-mode--paragraphs  (log-mode--apply-tag-filter
                                 log-mode--all-paragraphs tags mode)
          log-mode--page 1)
    (log-mode--render)))

(defun log-mode-change-filter-or (&optional edit-mode)
  "Prompt for new tag filter (OR mode). If EDIT-MODE is non-nil, pre-populate."
  (interactive "P")
  (let* ((all-tags (log-mode--all-tags log-mode--all-paragraphs))
         (initial-input (when edit-mode (mapconcat #'identity log-mode--filter-tags ", ")))
         (tags          (log-mode--read-tags all-tags initial-input)))
    (setq log-mode--filter-tags tags
          log-mode--filter-mode 'or
          log-mode--paragraphs  (log-mode--apply-tag-filter
                                 log-mode--all-paragraphs tags 'or)
          log-mode--page 1)
    (log-mode--render)))

(defun log-mode-open-clock-journal ()
  "Open (or create) the journal file for the current year% and age.
The filename is YEAR%-AGE.org, e.g. 44-30.org, resolved under
`log-mode-search-path'.  When `log-mode-device-folder' is set the file
lives in that subdirectory (e.g. /logs/desktop/44-30.org).
If the file already exists it is opened; otherwise it is created.
The file is visited in the current window, replacing the *Log* buffer."
  (interactive)
  (require 'clock)
  (let* ((year-pct  (truncate (clock-year-percent)))
         (age       (clock-age))
         (filename  (format "%d-%d.org" year-pct age))
         ;; Normalise search-path to a list of directories
         (dirs      (let ((sp log-mode-search-path))
                      (cond ((null sp)
                             (user-error
                              "log-mode-search-path is not set; \
please customise it first"))
                            ((stringp sp) (list sp))
                            (t sp))))
         ;; Scope each dir to the device subfolder when configured
         (effective-dirs
          (if log-mode-device-folder
              (mapcar (lambda (d) (expand-file-name log-mode-device-folder d))
                      dirs)
            dirs))
         ;; Look for an existing file in any of the effective directories
         (existing  (cl-find-if
                     (lambda (dir)
                       (file-exists-p (expand-file-name filename dir)))
                     effective-dirs))
         (target-dir (or existing (car effective-dirs)))
         (target     (expand-file-name filename target-dir)))
    (unless (file-exists-p target)
      (make-directory target-dir t)
      (message "Created %s" target))
    (find-file target)))

(defun log-mode-toggle-date-sort ()
  "Toggle the date sort order between descending (newest first) and ascending.
Paragraphs are sorted by the YEAR%-AGE.org filename convention: age is the
primary sort key and year-percent the secondary one, so e.g.
44-31.org > 43-31.org > 99-30.org in descending order."
  (interactive)
  (setq log-mode--date-sort
        (if (eq log-mode--date-sort 'desc) 'asc 'desc)
        log-mode--page 1)
  (message "Date sort: %s"
           (if (eq log-mode--date-sort 'desc)
               "newest first (↓)"
             "oldest first (↑)"))
  (log-mode--render))

(defun log-mode-quit ()
  "Quit the Log."
  (interactive)
  (kill-buffer (current-buffer)))


;; ---------------------------------------------------------------------------
;; Auto-refresh on focus

(defun log-mode--full-refresh ()
  "Rescan all source files and re-render the *Log* buffer.
Called automatically when the *Log* window receives focus."
  (interactive)
  (let ((log-buf (get-buffer "*Log*")))
    (when (buffer-live-p log-buf)
      (with-current-buffer log-buf
        ;; Capture where the cursor is NOW, before any rescan changes things.
        (let* ((current-id (log-mode--para-at-point))
               (sp   log-mode-search-path)
               (dirs (cond ((null sp)    nil)
                           ((stringp sp) (list sp))
                           (t            sp)))
               (files (and dirs (log-mode--collect-files dirs)))
               (all   (and files
                           (apply #'append
                                  (mapcar #'log-mode--paragraphs-in-file files)))))
          (when all
            (setq log-mode--all-paragraphs all
                  log-mode--paragraphs
                  (log-mode--apply-tag-filter all
                                             log-mode--filter-tags
                                             log-mode--filter-mode))
            ;; Pass current-id so log-mode--render puts point back on the
            ;; same paragraph (same logic as after-save via log-mode-edit-paragraph).
            (log-mode--render current-id)))))))

(defun log-mode--on-window-selected (frame)
  "Trigger a full refresh when the *Log* buffer becomes the selected window.
Added to `window-selection-change-functions' by `log-mode'."
  (when (eq (window-buffer (frame-selected-window frame))
            (get-buffer "*Log*"))
    ;; Defer the rescan by a short idle period so any in-flight
    ;; after-save-hook / org-element work can finish first.
    (run-with-idle-timer 0.4 nil #'log-mode--full-refresh)))

;; ---------------------------------------------------------------------------
;; Mode definition & keymap

(define-derived-mode log-mode special-mode "Log"
  "Major mode for the Log buffer."
  :group 'log-mode
  (setq buffer-read-only t
        truncate-lines nil)
  (visual-line-mode 1)
  ;; Refresh whenever this buffer's window is focused.
  (add-hook 'window-selection-change-functions
            #'log-mode--on-window-selected)
  ;; Remove the global hook when the *Log* buffer is killed.
  (add-hook 'kill-buffer-hook
            (lambda ()
              (remove-hook 'window-selection-change-functions
                           #'log-mode--on-window-selected))
            nil t))

(suppress-keymap log-mode-map)
(define-key log-mode-map (kbd "n")         #'log-mode-next-page)
(define-key log-mode-map (kbd "p")         #'log-mode-prev-page)
(define-key log-mode-map (kbd "g")         #'log-mode--full-refresh)
(define-key log-mode-map (kbd "r")         #'log-mode-mark-read)
(define-key log-mode-map (kbd "u")         #'log-mode-mark-unread)
(define-key log-mode-map (kbd "s")         #'log-mode-cycle-read-filter)
(define-key log-mode-map (kbd "e")         #'log-mode-edit-paragraph)
(define-key log-mode-map (kbd "t")         #'log-mode-edit-aliases)
(define-key log-mode-map (kbd "q")         #'log-mode-quit)
(define-key log-mode-map (kbd "l")         #'log-mode-open-clock-journal)
(define-key log-mode-map (kbd "d")         #'log-mode-toggle-date-sort)
(define-key log-mode-map (kbd "TAB")       #'log-mode-next-paragraph)
(define-key log-mode-map (kbd "<backtab>") #'log-mode-prev-paragraph)

;; Fresh-slate filters
(define-key log-mode-map (kbd "f") (lambda () (interactive)
                                     (setq log-mode--filter-mode 'and)
                                     (log-mode-change-filter)))
(define-key log-mode-map (kbd "F") (lambda () (interactive)
                                     (setq log-mode--filter-mode 'or)
                                     (log-mode-change-filter)))
;; Agnostic editor
(define-key log-mode-map (kbd "M-f") #'log-mode-edit-filter)

;; ---------------------------------------------------------------------------
;; Entry point

;;;###autoload
(defun log (&optional paths tags)
  "Browse paragraphs by tag across files in PATHS."
  (interactive)
  (log-mode--state-load)
  (log-mode--aliases-load)
  (let* ((search-paths (or paths
                           log-mode-search-path
                           (list (read-directory-name "Search directory: "))))
         (files        (log-mode--collect-files search-paths))
         (_ (unless files (user-error "No matching files found")))
         (all-paras    (apply #'append
                              (mapcar #'log-mode--paragraphs-in-file files)))
         (all-tags     (log-mode--all-tags all-paras))
         (init-tags    (or tags (log-mode--read-tags all-tags)))
         (filter-tags  (cl-remove-if #'string-empty-p init-tags))
         (filtered     (log-mode--filter-paragraphs all-paras filter-tags))
         (buf          (get-buffer-create "*Log*")))
    (with-current-buffer buf
      (log-mode)
      (setq log-mode--all-paragraphs all-paras
            log-mode--paragraphs     filtered
            log-mode--filter-tags    filter-tags
            log-mode--filter-mode    'and
            log-mode--read-filter    nil
            log-mode--date-sort      'desc
            log-mode--page           1)
      (log-mode--render)
      ;; Auto-export hook added here:
      (when log-mode-auto-export-tags
        (log-mode-export-tags t)))
    (switch-to-buffer buf)
    (message "l=log n/p=pages S-/TAB=cycle r=read s=show-filter e=edit f=filter d=sort g=refresh q=quit")))

(provide 'log-mode)
;;; log-mode.el ends here
