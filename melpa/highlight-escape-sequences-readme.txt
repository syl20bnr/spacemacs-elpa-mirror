This global minor mode highlights escape sequences in strings and
other kinds of literals with `hes-escape-sequence-face' which
inherits from `font-lock-regexp-grouping-construct' face by
default and with `hes-escape-backslash-face' which inherits from
`font-lock-regexp-grouping-backslash' face by default.

It currently supports `ruby-mode' and some simple modes:
both main JavaScript modes, Java mode, and C/C++/ObjC modes.

To enable it elsewhere, customize `hes-mode-alist'.

Put this in the init file:

(hes-mode)
