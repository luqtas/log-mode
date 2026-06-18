;;; log-tag-completion.el --- Tag completion inside [...] brackets -*- lexical-binding: t -*-
;; Version: 3.2

(require 'cl-lib)
(declare-function log-mode--collect-files      "log-mode" (paths))
(declare-function log-mode--paragraphs-in-file "log-mode" (file))
(declare-function log-mode--all-tags           "log-mode" (paragraphs))
(declare-function log-mode--alias-group        "log-mode" (tag))
(defvar log-mode-search-path)
(defvar log-mode-file-extensions)

;; ---------------------------------------------------------------------------
;; Tag cache

(defvar log-tag-completion--cache nil)
(defvar log-tag-completion--cache-time 0)

(defcustom log-tag-completion-cache-ttl 120
  "Seconds before the tag cache is rebuilt."
  :type 'integer :group 'log-mode)

(defun log-tag-completion--build-cache ()
  (let* ((paths (cond
                 ((and (boundp 'log-mode-search-path) log-mode-search-path)
                  (if (listp log-mode-search-path)
                      log-mode-search-path
                    (list log-mode-search-path)))
                 ((buffer-file-name)
                  (list (file-name-directory (expand-file-name (buffer-file-name)))))
                 (t nil)))
         (files (and paths (log-mode--collect-files paths)))
         (paras (and files (apply #'append (mapcar #'log-mode--paragraphs-in-file files)))))
    (when paras
      ;; Count how many paragraphs each tag appears in, then sort most-used first.
      (let ((freq (make-hash-table :test #'equal)))
        (dolist (para paras)
          (dolist (tag (log-mode--all-tags (list para)))
            (puthash tag (1+ (gethash tag freq 0)) freq)))
        (sort (hash-table-keys freq)
              (lambda (a b) (> (gethash a freq 0)
                               (gethash b freq 0))))))))

(defun log-tag-completion--tags ()
  (when (> (- (float-time) log-tag-completion--cache-time) log-tag-completion-cache-ttl)
    (setq log-tag-completion--cache     (log-tag-completion--build-cache)
          log-tag-completion--cache-time (float-time)))
  log-tag-completion--cache)

(defun log-tag-completion-refresh-tags ()
  "Rebuild the tag cache immediately."
  (interactive)
  (setq log-tag-completion--cache     (log-tag-completion--build-cache)
        log-tag-completion--cache-time (float-time))
  (message "log-tag-completion: %d tags loaded." (length log-tag-completion--cache)))

;; ---------------------------------------------------------------------------
;; Bracket detection

(defun log-tag-completion--open-bracket ()
  "Return position of the opening [ around point, or nil. Skips [[links]]."
  (save-excursion
    (let ((origin (point))
          (limit  (max (point-min) (- (point) 500)))
          result)
      (while (and (not result) (search-backward "[" limit t))
        (let ((p (point)))
          (cond
           ((and (> p (point-min)) (= (char-before p) ?\[))
            (goto-char (point-min)))
           ((and (< (1+ p) origin) (= (char-after (1+ p)) ?\[))
            nil)
           (t
            (unless (string-match-p "\\]" (buffer-substring-no-properties (1+ p) origin))
              (setq result p))))))
      result)))

;; ---------------------------------------------------------------------------
;; Context

(defun log-tag-completion--inside-link-p ()
  "Return non-nil if point is inside an org [[link]].
Uses a plain text scan — never calls org-element-context, which would
poke the org-element cache on every keystroke and cause timer errors."
  (save-excursion
    (let* ((pos   (point))
           (limit (max (point-min) (- pos 500)))
           (open  (and (search-backward "[[" limit t) (point))))
      (when open
        ;; found [[; confirm no ]] closes it before original position
        (goto-char (+ open 2))
        (not (search-forward "]]" pos t))))))

(defun log-tag-completion--context ()
  "Return (tok-start prefix candidates) or nil."
  (unless (log-tag-completion--inside-link-p)
    (let ((open (log-tag-completion--open-bracket)))
      (when open
        (let* ((origin   (point))
               (inner    (buffer-substring-no-properties (1+ open) origin))
               (parts    (split-string inner ","))
               (committed (mapcar #'string-trim (butlast parts)))
               (tok-start
                (save-excursion
                  (goto-char (1+ open))
                  (let (last-comma)
                    (when (> origin (point))
                      (while (search-forward "," origin t)
                        (setq last-comma (point))))
                    (if last-comma
                        (progn (goto-char last-comma) (skip-chars-forward " \t") (point))
                      (goto-char (1+ open)) (skip-chars-forward " \t") (point)))))
               (prefix     (buffer-substring-no-properties tok-start origin))
               (candidates (cl-remove-if (lambda (tag) (member tag committed))
                                         (log-tag-completion--tags)))
               (filtered   (all-completions prefix candidates)))
          (when filtered (list tok-start prefix filtered)))))))

;; ---------------------------------------------------------------------------
;; State

(defvar log-tag-completion--source-buffer nil)
(defvar log-tag-completion--candidates nil)
(defvar log-tag-completion--index 0)
(defconst log-tag-completion--buf "*Log Tags*")

(defface log-tag-completion-selected-face
  '((t :inherit highlight :weight bold))
  "Face for the selected candidate.")

;; ---------------------------------------------------------------------------
;; Layout helpers

(defun log-tag-completion--col-width (candidates)
  (+ 2 (apply #'max (mapcar #'length candidates))))

(defun log-tag-completion--ncols (candidates)
  (let* ((col-w  (log-tag-completion--col-width candidates))
         (pop-win (get-buffer-window log-tag-completion--buf))
         (win-w  (if pop-win (window-width pop-win) (window-width))))
    (max 1 (/ win-w col-w))))

(defun log-tag-completion--nrows (candidates)
  (ceiling (length candidates) (log-tag-completion--ncols candidates)))

;; Given a flat index, return (row . col) in the column-major grid
(defun log-tag-completion--idx-to-rowcol (idx candidates)
  (let ((nrows (log-tag-completion--nrows candidates)))
    (cons (% idx nrows) (/ idx nrows))))

;; ---------------------------------------------------------------------------
;; Render

(defun log-tag-completion--render ()
  (let* ((candidates log-tag-completion--candidates)
         (index      log-tag-completion--index)
         (col-w  (log-tag-completion--col-width candidates))
         (ncols  (log-tag-completion--ncols candidates))
         (nrows  (ceiling (length candidates) ncols))
         ;; row-major: item i is at row=i/ncols, col=i%ncols
         (sel-row (/ index ncols))
         (inhibit-read-only t))
    (with-current-buffer (get-buffer-create log-tag-completion--buf)
      (erase-buffer)
      (dotimes (row nrows)
        (dotimes (col ncols)
          (let ((i (+ (* row ncols) col)))   ; row-major index
            (when (< i (length candidates))
              (let* ((cand   (nth i candidates))
                     (padded (concat cand (make-string (- col-w (length cand)) ?\s))))
                (insert (if (= i index)
                            (propertize padded 'face 'log-tag-completion-selected-face)
                          padded))))))
        (insert "\n"))
      (goto-char (point-min))
      (forward-line sel-row)
      (let ((win (get-buffer-window log-tag-completion--buf)))
        (when win (set-window-point win (point)))))))

;; ---------------------------------------------------------------------------
;; Show / hide

(defun log-tag-completion--show (tok-start candidates)
  (setq log-tag-completion--source-buffer (current-buffer)
        log-tag-completion--candidates    candidates)
  (when (>= log-tag-completion--index (length candidates))
    (setq log-tag-completion--index 0))
  ;; create window if needed
  (let* ((src-win (get-buffer-window log-tag-completion--source-buffer))
         (pop-win (get-buffer-window log-tag-completion--buf)))
    (unless pop-win
      (when src-win
        (setq pop-win (split-window src-win -6 'below))
        (set-window-buffer pop-win (get-buffer-create log-tag-completion--buf))
        (set-window-dedicated-p pop-win t)
        (with-current-buffer log-tag-completion--buf
          (log-tag-completion-popup-mode))))
    (log-tag-completion--render)
    (when pop-win (fit-window-to-buffer pop-win 10 2))))

(defun log-tag-completion--hide ()
  (let ((win (get-buffer-window log-tag-completion--buf)))
    (when win (delete-window win)))
  (setq log-tag-completion--index 0))

;; ---------------------------------------------------------------------------
;; Navigation

(defun log-tag-completion--move (direction)
  (let* ((candidates log-tag-completion--candidates)
         (len        (length candidates)))
    (when (> len 0)
      (let* ((idx   log-tag-completion--index)
             (ncols (log-tag-completion--ncols candidates))
             (new-idx (pcase direction
                        ('right    (% (1+ idx) len))
                        ('left     (mod (1- idx) len))
                        ('down-col (% (+ idx ncols) len))
                        ('up-col   (mod (- idx ncols) len)))))
        (setq log-tag-completion--index new-idx)
        (log-tag-completion--render)))))

;; ---------------------------------------------------------------------------
;; Insert

(defun log-tag-completion--insert-selected ()
  (let ((choice (nth log-tag-completion--index log-tag-completion--candidates)))
    (when (and choice log-tag-completion--source-buffer
               (buffer-live-p log-tag-completion--source-buffer))
      (log-tag-completion--hide)
      (with-current-buffer log-tag-completion--source-buffer
        (let ((ctx (log-tag-completion--context)))
          (when ctx
            (let ((tok-start (nth 0 ctx)))
              (delete-region tok-start (point))
              ;; Add a space if we are after a comma and no space exists
              (when (and (> tok-start (point-min))
                         (eq (char-before tok-start) ?,)
                         (not (eq (char-after tok-start) ?\s)))
                (insert " "))
              (insert choice))))))))

;; ---------------------------------------------------------------------------
;; Commands (called from source org buffer)

(defun log-tag-completion--source-ret ()
  (interactive)
  (if (get-buffer-window log-tag-completion--buf)
      (log-tag-completion--insert-selected)
    (org-return)))

(defun log-tag-completion--source-next ()
  (interactive)
  (if (get-buffer-window log-tag-completion--buf)
      (log-tag-completion--move 'right)
    (call-interactively #'next-line)))

(defun log-tag-completion--source-prev ()
  (interactive)
  (if (get-buffer-window log-tag-completion--buf)
      (log-tag-completion--move 'left)
    (call-interactively #'previous-line)))

(defun log-tag-completion--source-next-row ()
  (interactive)
  (if (get-buffer-window log-tag-completion--buf)
      (log-tag-completion--move 'down-col)
    (call-interactively #'next-line)))

(defun log-tag-completion--source-prev-row ()
  (interactive)
  (if (get-buffer-window log-tag-completion--buf)
      (log-tag-completion--move 'up-col)
    (call-interactively #'previous-line)))

(defun log-tag-completion--source-close ()
  (interactive)
  (log-tag-completion--hide))

;; ---------------------------------------------------------------------------
;; Popup mode

(defvar log-tag-completion-popup-mode-map
  (let ((m (make-sparse-keymap)))
    (suppress-keymap m)
    (define-key m (kbd "RET")      #'log-tag-completion--source-ret)
    (define-key m (kbd "M-<right>")   #'log-tag-completion--source-next)
    (define-key m (kbd "M-<left>")     #'log-tag-completion--source-prev)
    (define-key m (kbd "M-<down>")    #'log-tag-completion--source-next-row)
    (define-key m (kbd "M-<up>")      #'log-tag-completion--source-prev-row)
    (define-key m (kbd "<escape>") #'log-tag-completion--source-close)
    m))

(define-derived-mode log-tag-completion-popup-mode special-mode "Tags"
  (setq buffer-read-only t cursor-type nil mode-line-format nil truncate-lines t))

;; ---------------------------------------------------------------------------
;; Post-command

(defun log-tag-completion--post-command ()
  (condition-case _err
      (let ((ctx (log-tag-completion--context)))
        (if ctx
            (log-tag-completion--show (nth 0 ctx) (nth 2 ctx))
          (when (get-buffer-window log-tag-completion--buf)
            (log-tag-completion--hide))))
    (error (log-tag-completion--hide))))

;; ---------------------------------------------------------------------------
;; Enable

(defun log-tag-completion--enable ()
  (add-hook 'post-command-hook #'log-tag-completion--post-command nil t)
  (local-set-key (kbd "RET") #'log-tag-completion--source-ret)
  (local-set-key (kbd "M-RET") (lambda()(interactive)(log-tag-completion--source-ret)(search-forward "]" nil t)));;(right-char)
  (local-set-key (kbd "M-<right>") #'log-tag-completion--source-next)
  (local-set-key (kbd "M-<left>")  #'log-tag-completion--source-prev)
  (local-set-key (kbd "M-<down>")  #'log-tag-completion--source-next-row)
  (local-set-key (kbd "M-<up>")    #'log-tag-completion--source-prev-row)
  (local-set-key (kbd "<escape>") #'log-tag-completion--source-close))

;;;###autoload
(add-hook 'org-mode-hook #'log-tag-completion--enable)
(add-hook 'find-file-hook
          (lambda () (when (derived-mode-p 'org-mode) (log-tag-completion--enable))))

(defun log-tag-completion-enable-all-org-buffers ()
  "Enable tag completion in every org-mode buffer currently open."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'org-mode) (log-tag-completion--enable)))))

(log-tag-completion-enable-all-org-buffers)

(provide 'log-tag-completion)
;;; log-tag-completion.el ends here
