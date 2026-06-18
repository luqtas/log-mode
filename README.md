# log-mode
cheap copy of Logseq with some extra goods

# how
i have no idea, ask Claude

set the directory folder on `cus-g` `log-mode` and other stuff to your taste and by entering the mode (with `M-x log`), you'll be asked to chose a tag to filter. tags are set from phrases/words between square brackets and separated by comma, e.g. [personal, todo, cooking], that's 3 tags... each paragraph is treated as block you can read, un-read and sort by date!

this is a block about [emacs]

and this is another block about [cooking] <br>
       - since there's no blank lines between the last line, this belongs to the block! you can also add another tag here like [todo5]

you can create block by pressing "l" in the log screen, after typing, if you save (will change this behavior to be exclusive to "e"), you'll return to the log screen and your blocks will be there! you can filter by AND by pressing "f" and typing stuff (RET auto-completes, pressing RET on a blank/filled tag does the search... commas will set new tags to be included), an OR search is done by "F" (SHIFT + f). adding new tags to the current search is about pressing M-f

you can set a custom folder for your machine, so you can sync stuff without worrying about conflicts in software like Syncthing!

the logic for creating files when you press "l" or "e" (edits the current focused block (you can navigate onthem by TAB or SHIFT-TAB (backwards))) is: year% that has passed and your age (defined at clock.el). 100 files per year i think it's enough to not accumulate too much text in a file

opening a square bracket at org-mode will toggle the auto-completion. useful for adding existing tags without typing much!

this is what i have on my init.el
```lisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
(require 'log-mode)
(require 'log-tag-completion)
(require 'clock)
(setq log-mode-search-path '("~/storage/shared/cloud/logs"))
```
