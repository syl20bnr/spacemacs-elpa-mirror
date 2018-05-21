Highlight TODO and similar keywords in comments and strings.

You can either turn on `hl-todo-mode' in individual buffers or use
the the global variant `global-hl-todo-mode'.  Note that the option
`hl-todo-activate-in-modes' controls in what buffers the local mode
will be activated if you do the latter.  By default it will only be
activated in buffers whose major-mode derives from `prog-mode'.

This package also provides commands for moving to the next or
previous keyword and to invoke `occur' with a regexp that matches
all known keywords.  If you want to use these commands, then you
should bind them in `hl-todo-mode-map', e.g.:

  (define-key hl-todo-mode-map (kbd "C-c p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c o") 'hl-todo-occur)

See [[https://www.emacswiki.org/emacs/FixmeMode][this list]] on the Emacswiki for other packages that implement
the same basic features, but which might also provide additional
features that you might like, but which I don't deem necessary.
