`M-x sqlup-mode` and just type.
This mode supports the various built-in SQL modes as well as redis-mode.
The capitalization is triggered when you press the following keys:
* SPC
* RET
* ,
* ;
* (
* '

This package also provides a function to capitalize SQL keywords inside a
region as well as the whole bufer - always available, no need to activate
the minor mode to use it:

M-x sqlup-capitalize-keywords-in-region
M-x sqlup-capitalize-keywords-in-buffer

It is not bound to a keybinding. Here is an example of how you could do it:

(global-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)

Here follows an example setup to activate `sqlup-mode` automatically:

(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
(add-hook 'redis-mode-hook 'sqlup-mode)

Sqlup can be configured to ignore certain keywords by adding them to the list
`sqlup-blacklist`. For example if you use `name` as a column name it would be
annoying to have it upcased so you can prevent this by adding

(add-to-list 'sqlup-blacklist "name")

to your config (or do the equivalent through the `M-x customize` interface).
