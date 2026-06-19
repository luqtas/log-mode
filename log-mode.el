;;; log-mode.el --- Tag-based paragraph browser with read/unread state -*- lexical-binding: t -*-

;; Author: You
;; Version: 1.4
;; Description: Browse paragraphs by tag across multiple files, with
;;              synced multi-device read/unread tracking and aliases.

;;; Code:

(require 'cl-lib)
(require 'ispell)
(require 'clock)

;; ---------------------------------------------------------------------------
;; Improved race guards

(defun log-mode--flyspell-sanitize-otherchars (orig-fun &rest args)
  "Force `ispell-otherchars` to be a string before running the original function."
  (let ((ispell-otherchars (if (stringp (bound-and-true-p ispell-otherchars))
                               ispell-otherchars
                             "")))
    (apply orig-fun args)))

(with-eval-after-load 'flyspell
  (advice-add 'flyspell-word :around #'log-mode--flyspell-sanitize-otherchars)
  (advice-add 'flyspell-get-word :around #'log-mode--flyspell-sanitize-otherchars)
  (advice-add 'flyspell-post-command-hook :around #'log-mode--flyspell-sanitize-otherchars)
  (advice-add 'flyspell-after-change-function :around #'log-mode--flyspell-sanitize-otherchars))

(with-eval-after-load 'org-element
  (defun log-mode--silence-org-cache-error (orig-fun &rest args)
    (condition-case nil
        (apply orig-fun args)
      (wrong-type-argument nil)))
  (advice-add 'org-element--cache-sync :around #'log-mode--silence-org-cache-error))

;; ---------------------------------------------------------------------------
;; Customisation

(defgroup log-mode nil
  "Tag-based paragraph browser."
  :group 'convenience)

(defcustom log-mode-settings-folder
  (expand-file-name "log-mode-settings" user-emacs-directory)
  "Shared directory where all device aliases and read-states are stored.
Sync this folder across devices to share read/unread states."
  :type 'directory
  :group 'log-mode)

(defcustom log-mode-device-name
  (system-name)
  "Unique identifier for this device (e.g., 'desktop', 'laptop').
Used to separate state files, alias files, and the journal subfolder to
prevent sync collisions. YOU MUST ENSURE THIS IS UNIQUE PER DEVICE."
  :type 'string
  :group 'log-mode)

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

;; ---------------------------------------------------------------------------
;; Multi-Device Read State (CRDT)

(defvar log-mode--read-set (make-hash-table :test #'equal)
  "Hash-table: paragraph-id -> (boolean-state . (age year-percent day-percent)).")

(defun log-mode--timestamp ()
  "Return the current clock paradigm timestamp as a list."
  (list (clock-age) (clock-year-percent) (clock-day-percent)))

(defun log-mode--timestamp-newer-p (ts1 ts2)
  "Return t if TS1 is strictly newer than TS2 based on age -> year% -> day%."
  (cond
   ((null ts2) t)
   ((null ts1) nil)
   ((> (nth 0 ts1) (nth 0 ts2)) t)
   ((< (nth 0 ts1) (nth 0 ts2)) nil)
   ((> (nth 1 ts1) (nth 1 ts2)) t)
   ((< (nth 1 ts1) (nth 1 ts2)) nil)
   ((> (nth 2 ts1) (nth 2 ts2)) t)
   (t nil)))

(defun log-mode--portable-path (file)
  "Convert absolute FILE path to a path relative to `log-mode-search-path'."
  (let ((expanded (expand-file-name file))
        (paths (if (listp log-mode-search-path)
                   log-mode-search-path
                 (list log-mode-search-path)))
        (rel-path nil))
    (catch 'found
      (dolist (p paths)
        (when p
          (let ((prefix (file-name-as-directory (expand-file-name p))))
            (when (string-prefix-p prefix expanded)
              (setq rel-path (substring expanded (length prefix)))
              (throw 'found t))))))
    (or rel-path (file-name-nondirectory expanded))))

(defun log-mode--portable-id (id)
  "Ensure ID uses a portable path to maintain sync compatibility."
  (if (string-match "^\\(.*\\)::\\([0-9]+\\)$" id)
      (let ((file (match-string 1 id))
            (offset (match-string 2 id)))
        (if (file-name-absolute-p file)
            (format "%s::%s" (log-mode--portable-path file) offset)
          id))
    id))

(defun log-mode--state-load ()
  "Load and merge all device read states from the settings folder.
Conflicts are resolved by keeping the state with the newest timestamp."
  (clrhash log-mode--read-set)
  (when (and log-mode-settings-folder (file-directory-p log-mode-settings-folder))
    (dolist (file (directory-files log-mode-settings-folder t "-read-state\\.eld\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (let ((data (ignore-errors (read (current-buffer)))))
          (when (hash-table-p data)
            (maphash
             (lambda (id val)
               ;; Auto-migrate legacy absolute-path IDs to portable IDs
               (let* ((new-id (log-mode--portable-id id))
                      (is-old (not (consp val)))
                      (state (if is-old val (car val)))
                      (ts (if is-old '(0 0 0) (cdr val)))
                      (existing (gethash new-id log-mode--read-set))
                      (existing-ts (if existing (cdr existing) nil)))
                 (when (or (null existing)
                           (log-mode--timestamp-newer-p ts existing-ts))
                   (puthash new-id (cons state ts) log-mode--read-set))))
             data)))))))

(defun log-mode--state-save ()
  "Save the current merged state to this specific device's state file."
  (when (and log-mode-settings-folder log-mode-device-name)
    (unless (file-directory-p log-mode-settings-folder)
      (make-directory log-mode-settings-folder t))
    (let ((file (expand-file-name (format "%s-read-state.eld" log-mode-device-name)
                                  log-mode-settings-folder))
          (print-length nil)
          (print-level nil))
      (with-temp-file file
        (prin1 log-mode--read-set (current-buffer))))))

(defun log-mode--para-id (file char-offset)
  (format "%s::%d" (log-mode--portable-path file) char-offset))

(defun log-mode--read-p (id)
  (let ((val (gethash id log-mode--read-set)))
    (if (consp val) (car val) val)))

(defun log-mode--set-read (id value)
  (puthash id (cons value (log-mode--timestamp)) log-mode--read-set)
  (log-mode--state-save))

;; ---------------------------------------------------------------------------
;; Shared Tag Aliases

(defvar log-mode--aliases nil
  "List of alias groups. Each group is a list of equivalent tag strings.")

(defun log-mode--aliases-load ()
  "Load and merge tag aliases from all alias files in the settings folder."
  (setq log-mode--aliases nil)
  (when (and log-mode-settings-folder (file-directory-p log-mode-settings-folder))
    (dolist (file (directory-files log-mode-settings-folder t "-aliases\\.txt\\'"))
      (with-temp-buffer
        (insert-file-contents file)
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
          (forward-line 1))))))

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
    (let ((inhibit-modification-hooks t)
          (after-change-functions nil)
          (before-change-functions nil)
          (after-insert-file-functions nil)
          (jit-lock-functions nil))
      (insert-file-contents file))
    (let ((paras nil))
      (goto-char (point-min))
      (while (not (eobp))
        (while (and (not (eobp)) (looking-at "^[ \t]*$"))
          (forward-line 1))
        (unless (eobp)
          (let ((start (point)))
            (while (and (not (eobp)) (not (looking-at "^[ \t]*$")))
              (forward-line 1))
            (let* ((text (string-trim
                          (buffer-substring-no-properties start (point))))
                   (tags (log-mode--extract-tags text)))
              (unless (string-empty-p text)
                (push (list :text text
                            :tags (copy-sequence tags)
                            :id   (log-mode--para-id file start)
                            :file (expand-file-name file))
                      paras))))))
      (nreverse paras))))

(defun log-mode--all-tags (paragraphs)
  "Return list of unique tags across PARAGRAPHS, ordered by frequency (descending)."
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
  "Keep PARAGRAPHS matching every tag in TAGS (AND semantics, alias-aware)."
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
  "Return a numeric sort key for FILE based on the YEAR%-AGE.org convention."
  (let ((base (file-name-base (file-name-nondirectory file))))
    (if (string-match "\\`\\([0-9]+\\)-\\([0-9]+\\)\\'" base)
        (+ (* (string-to-number (match-string 2 base)) 10000)
           (string-to-number (match-string 1 base)))
      0)))

(defun log-mode--sort-paragraphs-by-date (paragraphs order)
  "Return a sorted copy of PARAGRAPHS by their filename date key."
  (cl-sort (copy-sequence paragraphs)
           (if (eq order 'asc) #'< #'>)
           :key (lambda (p)
                  (log-mode--filename-date-key (plist-get p :file)))))

(defun log-mode--date-sort-str ()
  "Return a short display string for the current date sort direction."
  (if (eq log-mode--date-sort 'asc) "↑date" "↓date"))

;; ---------------------------------------------------------------------------
;; Buffer state

(defvar-local log-mode--editing-para-id nil)
(defvar-local log-mode--editing-para-file nil)
(defvar-local log-mode--editing-para-text nil)
(defvar-local log-mode--editing-prev-id nil)
(defvar-local log-mode--editing-prev-text nil)
(defvar-local log-mode--editing-next-id nil)
(defvar-local log-mode--editing-next-text nil)
(defvar-local log-mode--paragraphs nil)
(defvar-local log-mode--all-paragraphs nil)
(defvar-local log-mode--filter-tags nil)
(defvar-local log-mode--filter-mode 'and)
(defvar-local log-mode--read-filter nil)
(defvar-local log-mode--page 1)
(defvar-local log-mode--total-pages 1)
(defvar-local log-mode--para-markers nil)
(defvar-local log-mode--date-sort 'desc)

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
  "Redraw the *Log* buffer for the current page."
  (with-current-buffer (get-buffer-create "*Log*")
    (let* ((inhibit-read-only t)
           (visible (log-mode--visible-paragraphs))
           (total (length visible))
           (page-size log-mode-page-size)
           (total-pages (max 1 (ceiling total page-size)))
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

(defun log-mode--minibuffer-ret ()
  "RET in the tag minibuffer."
  (interactive)
  (let* ((ctx    (log-mode--minibuffer-context log-mode--minibuffer-all-tags))
         (prefix (and ctx (nth 1 ctx))))
    (if (and prefix (not (string-empty-p prefix)))
        (log-mode--minibuffer-insert-tag)
      (exit-minibuffer))))

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
    (define-key map (kbd "RET")       #'log-mode--minibuffer-ret)
    (define-key map (kbd "M-RET")     #'exit-minibuffer)
    (define-key map (kbd "TAB")       #'log-mode--minibuffer-insert-tag)
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
                          (when (and initial-input (not (string-empty-p initial-input)))
                            (insert initial-input)))
                      (read-from-minibuffer "Tags (RET=complete-or-search, M-RET=force-search, M-arrows=navigate): "
                                            nil log-mode-tag-minibuffer-map))
                  (log-mode--tag-popup-hide)
                  (remove-hook 'post-command-hook #'log-mode--minibuffer-post-command t))))
    (delete-dups (cl-remove-if #'string-empty-p
                               (mapcar #'string-trim (split-string input "," t))))))

;; ---------------------------------------------------------------------------
;; Commands

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
  "Cycle read-state filter: all -> unread-only -> read-only -> all."
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
      (let* ((para   (cl-find id log-mode--all-paragraphs
                              :key (lambda (p) (plist-get p :id))
                              :test #'equal))
             (file   (plist-get para :file))
             (offset (when (string-match "::\\([0-9]+\\)$" id)
                       (string-to-number (match-string 1 id))))
             (text   (and para (plist-get para :text))))
        (if (not (and file (file-exists-p file)))
            (message "File not found: %s" file)
          (let ((log-buf (current-buffer)))
            (setq log-mode--editing-para-id id)
            (setq log-mode--editing-para-file file)
            (setq log-mode--editing-para-text text)
            (setq log-mode--editing-prev-id nil
                  log-mode--editing-prev-text nil)

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
            (goto-char (max (point-min) (min (1+ (or offset 0)) (point-max))))
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
            (message "edit freely. save to return")))))))

(defun log-mode--best-focus-id (old-id candidates)
  "Return the best paragraph ID from CANDIDATES to focus on after a rescan."
  (let ((old-offset (when (and old-id (string-match "::\\([0-9]+\\)$" old-id))
                      (string-to-number (match-string 1 old-id)))))
    (or
     (plist-get (cl-find old-id candidates :key (lambda (p) (plist-get p :id)) :test #'equal) :id)
     (when (and log-mode--editing-para-file log-mode--editing-para-text)
       (plist-get (cl-find-if (lambda (p)
                                (and (string= (expand-file-name (plist-get p :file))
                                              (expand-file-name log-mode--editing-para-file))
                                     (string= (plist-get p :text) log-mode--editing-para-text)))
                              candidates)
                  :id))
     (when log-mode--editing-next-id
       (plist-get (cl-find log-mode--editing-next-id candidates
                           :key (lambda (p) (plist-get p :id)) :test #'equal) :id))
     (when (and log-mode--editing-next-text log-mode--editing-para-file)
       (plist-get (cl-find-if (lambda (p)
                                (and (string= (expand-file-name (plist-get p :file))
                                              (expand-file-name log-mode--editing-para-file))
                                     (string= (plist-get p :text) log-mode--editing-next-text)))
                              candidates)
                  :id))
     (when (and log-mode--editing-para-file old-offset)
       (let ((same-file-cands
              (cl-remove-if-not
               (lambda (p)
                 (and (string= (expand-file-name (plist-get p :file))
                               (expand-file-name log-mode--editing-para-file))
                      (when (string-match "::\\([0-9]+\\)$" (plist-get p :id))
                        (<= (string-to-number (match-string 1 (plist-get p :id)))
                            old-offset))))
               candidates)))
         (when same-file-cands
           (plist-get (car (last same-file-cands)) :id)))))))

(defun log-mode--rescan-file (file log-buf)
  "Schedule a re-parse of FILE after a short idle delay."
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
        (setq log-mode--all-paragraphs all
              log-mode--paragraphs     (log-mode--filter-paragraphs all log-mode--filter-tags)
              log-mode--page           1)
        (let* ((visible   (log-mode--visible-paragraphs))
               (target-id (and old-id (log-mode--best-focus-id old-id visible))))
          (log-mode--render target-id)
          (message "Log refreshed after save."))))))

(defun log-mode-edit-aliases ()
  "Open the tag alias file for this specific device for editing."
  (interactive)
  (unless (and log-mode-settings-folder log-mode-device-name)
    (user-error "Please configure `log-mode-settings-folder` and `log-mode-device-name` first"))
  (unless (file-directory-p log-mode-settings-folder)
    (make-directory log-mode-settings-folder t))

  (let ((alias-file (expand-file-name (format "%s-aliases.txt" log-mode-device-name)
                                      log-mode-settings-folder)))
    (unless (file-exists-p alias-file)
      (with-temp-file alias-file
        (insert "# Tag aliases for log-mode\n"
                "# Each line: comma-separated equivalent tags\n"
                "# Example:\n"
                "#   poem, poems\n"
                "#   tag, tags\n")))
    (let ((log-buf (current-buffer)))
      (find-file-other-window alias-file)
      (let ((hook-fn nil))
        (setq hook-fn
              (lambda ()
                (when (string= (expand-file-name buffer-file-name)
                               (expand-file-name alias-file))
                  (remove-hook 'after-save-hook hook-fn t)
                  (log-mode--aliases-load)
                  (when (buffer-live-p log-buf)
                    (with-current-buffer log-buf
                      (log-mode--render)
                      (message "Aliases merged and reloaded."))))))
        (add-hook 'after-save-hook hook-fn nil t))
      (message "Edit aliases. Save to merge & reload. Format: tag1, tag2 (one group per line)"))))

(defun log-mode-change-filter ()
  "Prompt for a new tag filter and apply it using the current filter mode."
  (interactive)
  (let* ((all-tags (log-mode--all-tags log-mode--all-paragraphs))
         (tags     (log-mode--read-tags all-tags nil)))
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
         (mode log-mode--filter-mode)
         (tags (log-mode--read-tags all-tags initial-input)))
    (setq log-mode--filter-tags tags
          log-mode--filter-mode mode
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
Uses `log-mode-device-name` to resolve a subfolder in `log-mode-search-path`."
  (interactive)
  (let* ((year-pct  (truncate (clock-year-percent)))
         (age       (clock-age))
         (filename  (format "%d-%d.org" year-pct age))
         (dirs      (let ((sp log-mode-search-path))
                      (cond ((null sp)
                             (user-error
                              "log-mode-search-path is not set; \
please customise it first"))
                            ((stringp sp) (list sp))
                            (t sp))))
         (effective-dirs
          (if (and log-mode-device-name (not (string-empty-p log-mode-device-name)))
              (mapcar (lambda (d) (expand-file-name log-mode-device-name d))
                      dirs)
            dirs))
         (existing  (cl-find-if
                     (lambda (dir)
                       (file-exists-p (expand-file-name filename dir)))
                     effective-dirs))
         (target-dir (or existing (car effective-dirs)))
         (target     (expand-file-name filename target-dir)))
    (unless (file-exists-p target)
      (make-directory target-dir t)
      (message "Created %s" target))
    (let* ((log-buf    (current-buffer))
           (current-id (log-mode--para-at-point))
           (current-para (and current-id
                              (cl-find current-id log-mode--all-paragraphs
                                       :key (lambda (p) (plist-get p :id))
                                       :test #'equal))))
      (setq log-mode--editing-para-id   current-id
            log-mode--editing-para-file (and current-para
                                             (plist-get current-para :file))
            log-mode--editing-para-text (and current-para
                                             (plist-get current-para :text))
            log-mode--editing-prev-id   nil
            log-mode--editing-prev-text nil
            log-mode--editing-next-id   nil
            log-mode--editing-next-text nil)
      (when current-id
        (catch 'found
          (let ((prev-id nil) (prev-text nil) (found-current nil))
            (dolist (p (log-mode--visible-paragraphs))
              (cond
               (found-current
                (setq log-mode--editing-next-id   (plist-get p :id)
                      log-mode--editing-next-text (plist-get p :text))
                (throw 'found t))
               ((equal (plist-get p :id) current-id)
                (setq log-mode--editing-prev-id   prev-id
                      log-mode--editing-prev-text prev-text
                      found-current               t))
               (t
                (setq prev-id   (plist-get p :id)
                      prev-text (plist-get p :text))))))))
      (find-file target)
      (let ((hook-fn nil))
        (setq hook-fn
              (lambda ()
                (when (string= (expand-file-name buffer-file-name)
                               (expand-file-name target))
                  (remove-hook 'after-save-hook hook-fn t)
                  (log-mode--rescan-file target log-buf))))
        (add-hook 'after-save-hook hook-fn nil t))
      (message "Editing journal. Log rescans on save, but stays here."))))

(defun log-mode-toggle-date-sort ()
  "Toggle the date sort order between descending and ascending."
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
  "Rescan all source files and re-render the *Log* buffer."
  (interactive)
  (let ((log-buf (get-buffer "*Log*")))
    (when (buffer-live-p log-buf)
      (with-current-buffer log-buf
        ;; IMPORTANT: Reload synced states whenever refreshing!
        (log-mode--state-load)
        (log-mode--aliases-load)
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
            (log-mode--render current-id)))))))

(defun log-mode--on-window-selected (frame)
  "Trigger a full refresh when the *Log* buffer becomes the selected window."
  (when (eq (window-buffer (frame-selected-window frame))
            (get-buffer "*Log*"))
    (run-with-idle-timer 0.4 nil #'log-mode--full-refresh)))

;; ---------------------------------------------------------------------------
;; Mode definition & keymap

(define-derived-mode log-mode special-mode "Log"
  "Major mode for the Log buffer."
  :group 'log-mode
  (setq buffer-read-only t
        truncate-lines nil)
  (visual-line-mode 1)
  (add-hook 'window-selection-change-functions
            #'log-mode--on-window-selected)
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
(define-key log-mode-map (kbd "f") (lambda () (interactive)
                                     (setq log-mode--filter-mode 'and)
                                     (log-mode-change-filter)))
(define-key log-mode-map (kbd "F") (lambda () (interactive)
                                     (setq log-mode--filter-mode 'or)
                                     (log-mode-change-filter)))
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
      (log-mode--render))
    (switch-to-buffer buf)
    (message "l=log n/p=pages S-/TAB=cycle r=read s=show-filter e=edit f=filter d=sort g=refresh q=quit")))

(provide 'log-mode)
;;; log-mode.el ends here
