This package provides functions similar to `thing-at-point' of `thingatpt.el'.

The functions are:

xah-get-thing-at-cursor
xah-get-thing-or-selection

They get “thing” independent of syntax table, so you always get same thing regardless what's current major mode.

xah-get-thing-or-selection get text selection when there's active region.

Call describe-function on them to read the inline doc.
Home page: http://ergoemacs.org/emacs/elisp_get-selection-or-unit.html
