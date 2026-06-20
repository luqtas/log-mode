# log-mode
cheap copy of Logseq with some extra goods

# how
i have no idea, ask Claude and Gemini

set the directory folder on `M-x cus-g` (customize-group's shortcut) `log-mode` and other stuff to your taste and by entering the mode (with `M-x log`), you'll be asked to chose a tag to filter. tags are set from phrases/words between square brackets and separated by comma, e.g. [personal, todo, cooking], that's 3 tags... each paragraph is treated as a block you can read, un-read and sort by date!

```
this is a block about [emacs] and [birds]

and this is another block about [cooking] <br>
       - since there's no blank lines between the last line, this belongs to the block! you can also add another tag here like [todo5] or whatever
                - this also belongs to the same block!
```

you can create a block by pressing "l" in the log screen, after typing and saving, you can return to the log screen and your blocks will be there (considering you didn't set any filter or what you typed has the filtered tag)! you can filter by an AND logic by pressing "f" (RET for confirming the auto-completion and RET again on a blank/filled tag will search... commas allows multiple tags search), an OR logic search is done by "F" (SHIFT + f). adding new tags to the current search is about pressing M-f

you can define aliases by pressing "t", there you can join multiple tags to be filtered (always) as one, e.g. [project], [prooojects], [things] and [projects] can be unified at the "t" buffer like this: `project, prooojects, things, projects` (each new line is a new definition of aliases), every time you filter one of those tags the others will also be included! useful if you like to keep the typing clean by abiding to plural or whatever grammatik rules

you can set a custom/shared config. folder for your machine (at `customize-group`), so you can sync stuff without worrying about conflicts in software like Syncthing! logs, aliases and read states will be shared among devices, free of conflict. the read/un-read state will have the date you toggled it and the last set is what'll define for all devices!

if created a new block in another device sharing the same database or marked something as read, you can press "g" or access the buffer once again to get the updates

the logic for creating files when you press "l" or "e" (edits the current focused block (you can navigate on them by TAB or SHIFT-TAB (backwards))) is: year% that has passed and your age (defined at clock.el). 100 files per year i think it's enough to not accumulate too much text in a file... when pressing "e" to edit a block, by saving the file, you'll return to *log-mode* buffer

`log-tag-completion.el` will make square brackets at org-mode toggle the auto-completion. useful for adding existing tags without typing much!

this is what i have on my init.el!
```lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(require 'log-mode)
(require 'log-tag-completion)
(require 'clock)
(setq log-mode-search-path '("~/storage/shared/cloud/logs"))
```

this will starts Emacs on log-mode and it will set the initial filter to the highest number of a TODO tag... i prioritize notes like this: [todo27] > [todo5] > [todo1] > [todo] - so i always know what's more emergent!
```lisp
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

`C-c d` will look for TODO tags and change them to *done*, it's position-aware, e.g.

[supermarket, todo99]<br>
&emsp; - beans [todo]<br>
&emsp; - rice [todo] (*cursor is here at log-mode and by pressing `C-c d` we get*)


[supermarket, todo99]<br>
&emsp; - beans [todo]<br>
&emsp; - rice [done]
