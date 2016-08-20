As you move through the lines of an output buffer (such as from
`grep' or `occur'), another window highlights the corresponding
line of the source buffer.

This is inspired by the table of contents code from reftex.el.
http://www.strw.leidenuniv.nl/~dominik/Tools/

Installation
To use the mode, do M-x fm-start in the output buffer.  Or just add
it to the mode hooks, e.g.:
(add-hook 'occur-mode-hook 'fm-start)
(add-hook 'compilation-mode-hook 'fm-start)
