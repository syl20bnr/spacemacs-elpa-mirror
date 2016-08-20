If you use a custom completion mechanism such as
`ido-ubiquitous-mode', you might notice that functions like
`describe-face' don't use it. This is because they use a function
called `completing-read-multiple' to read multiple values at once,
and this function doesn't use the standard completion
mechanisms. This package allows you to use the standard completion
mechanisms to replace `completing-read-multiple', allowing your
custom completion system to work for functions that use it.

When you turn on `crm-custom-mode', any command that uses
`completing-read-multiple' will now prompt you again each time you
enter an item. This is because it is reading a list of multiple
items. To end the completion and finish the list of items, simply
enter an empty string.
