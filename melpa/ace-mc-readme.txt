ace-mc.el is a package that allows you to quickly add and remove
multiple-cursors mode cursors using ace-jump.

This package adds two commands: `ace-mc-add-multiple-cursors' and
`ace-mc-add-single-cursor'.  Both commands act like
`ace-jump-mode', accepting simliar prefix arguments. However,
instead of jumping to the location, as with ace-jump, the command
will add a new multiple cursor mark. If one is already there, it
will be removed. This makes it easy to remove cursors you've added
accidentally.

`ace-mc-add-multiple-cursors' will continue to keep prompting for
places to add or remove cursors until you hit Enter. The command
`ace-mc-add-single-cursor' is a non-looping version.

If you have ace-jump bound on C-0, for example, I recommend the
following key bindings:

(global-set-key (kbd "C-)") 'ace-mc-add-multiple-cursors)
(global-set-key (kbd "C-M-)") 'ace-mc-add-single-cursor)
