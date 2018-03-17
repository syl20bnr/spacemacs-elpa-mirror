This package provides commands to show and hide lines of context around
errors in compilation buffers or around matches in grep buffers
(e.g. M-x grep).  Works with `wgrep', `ag-mode' and `ack-mode'.

Usage:

  (progn
    (require 'grep-context)
    (dolist (elt '((compile . compilation-mode-map)
                   (grep . grep-mode-map)
                   (ag . ag-mode-map)
                   (ack . ack-mode-map)))
      (eval-after-load (car elt)
        `(progn
            (define-key ,(cdr elt) (kbd "+")
               #'grep-context-more-around-point)
            (define-key ,(cdr elt) (kbd "-")
               #'grep-context-less-around-point)))))

After evaluating that you can open a grep buffer and navigate to a match,
then hit "+" to insert a line of context before and after that match.
This is almost the same as running grep with `-A 1 -B 1` flags, except
the context is inserted only around match at point, not everywhere.
It is also much faster than re-running grep with those flags.
Hitting "+" again will insert more context lines and "-" will kill
outermost context lines.

This package will work with any *compilation* buffer except it needs to
know how to format context lines.  If you want to use it in your mode,
you can add an entry to `grep-context-line-format-alist'.
You can also add an entry to `grep-context-separator-alist' to specify
a separator for non-contiguous regions of context.
