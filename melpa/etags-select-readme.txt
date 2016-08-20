Open a buffer with file/lines of exact-match tags shown.  Select one by
going to a line and pressing return.  pop-tag-mark still works with this
code.

If there is only one match, you can skip opening the selection window by
setting a custom variable.  This means you could substitute the key binding
for find-tag-at-point with etags-select-find-tag-at-point, although it
won't play well with tags-loop-continue.  On the other hand, if you like
the behavior of tags-loop-continue you probably don't need this code.

I use this:
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
(global-set-key "\M-." 'etags-select-find-tag)

Contributers of ideas and/or code:
David Engster
James Ferguson
