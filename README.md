# log-mode
cheap copy of Logseq with some extra goods

# how
i have no idea, ask Claude

set the directory folder on `M-x cus-g` (customize-group's shortcut) `log-mode` and other stuff to your taste and by entering the mode (with `M-x log`), you'll be asked to chose a tag to filter. tags are set from phrases/words between square brackets and separated by comma, e.g. [personal, todo, cooking], that's 3 tags... each paragraph is treated as block you can read, un-read and sort by date!

this is a block about [emacs]

and this is another block about [cooking] <br>
       - since there's no blank lines between the last line, this belongs to the block! you can also add another tag here like [todo5]

you can create block by pressing "l" in the log screen, after typing, if you save (will change this behavior to be exclusive to "e"), you'll return to the log screen and your blocks will be there! you can filter by AND by pressing "f" and typing stuff (RET auto-completes, pressing RET on a blank/filled tag does the search... commas will set new tags to be included), an OR search is done by "F" (SHIFT + f). adding new tags to the current search is about pressing M-f

you can set a custom folder for your machine, so you can sync stuff without worrying about conflicts in software like Syncthing!

the logic for creating files when you press "l" or "e" (edits the current focused block (you can navigate on them by TAB or SHIFT-TAB (backwards))) is: year% that has passed and your age (defined at clock.el). 100 files per year i think it's enough to not accumulate too much text in a file

opening a square bracket at org-mode will toggle the auto-completion. useful for adding existing tags without typing much!

this is what i have on my init.el!
```lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(require 'log-mode)
(require 'log-tag-completion)
(require 'clock)
(setq log-mode-search-path '("~/storage/shared/cloud/logs"))
```

i also have this on my init.el! <br>
it starts Emacs on log-mode and it will grab the highest number of a TODO tag... i prioritize notes like this: [todo27] > [todo5] > [todo1] > [todo] - so i always know what's more emergent!
```lisp
;; this ones starts log-mode at startup with our highest todoN tag as a filter
(defun log-open-top-todo ()
  "Start log-mode filtered to the highest todoN tag found in the search path."
  (let* ((files  (log-mode--collect-files log-mode-search-path))
         (all    (apply #'append (mapcar #'log-mode--paragraphs-in-file files)))
         (tags   (log-mode--all-tags all))
         (todos  (cl-remove-if-not
                  (lambda (tag) (string-match-p "^todo[0-9]+$" tag))
                  tags))
         (top    (car (sort todos
                            (lambda (a b)
                              (> (string-to-number (substring a 4))
                                 (string-to-number (substring b 4))))))))
    (if top
        (log log-mode-search-path (list top))
      (message "No todoN tags found."))))
(add-hook 'server-after-make-frame-hook #'log-open-top-todo)
```

i do also have a (done), which will grab the TODO tag (with any number) and it'll change to *done*... doesn't matter where it's like [cooking, todo5] will turn into [cooking, done]! guess this conflicts or overlaps with the "read/un-read" function in log-mode, so i guess some UX tinker is needed - maybe we can have (done) inside log-mode too?
```lisp
(defun done ()
  "Replace 'todo...' with 'done' on the current line, preserving cursor position."
  (interactive)
  (save-excursion
    (let ((start (line-beginning-position))
          (end   (line-end-position)))
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (while (re-search-forward "\\btodo[a-zA-Z0-9]*\\b" nil t)
            (replace-match "done" nil t)
          (message "No todo found on this line."))))))
(global-set-key (kbd "C-c d") 'done)
```
