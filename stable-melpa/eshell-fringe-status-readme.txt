Show an indicator of the status of the last command run in eshell.
To use, enable `eshell-fringe-status-mode' in `eshell-mode'.  The
easiest way to do this is by adding a hook:

: (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)

This mode uses a rather hackish way to try and keep everything
working in regard to `eshell-prompt-regexp', so if anything breaks
please let me know.

Some extra fringe bitmaps are provided.  In case you prefer either
or both of them over the default arrow bitmap.  These are
`efs-plus-bitmap' and `efs-minus-bitmap'.  These show a `+' and `-'
in the fringe respectively, instead of an arrow.  These can be used
by setting the `eshell-fringe-status-success-bitmap' and the
`eshell-fringe-status-failure-bitmap' options.
