`whitespace-cleanup' is a handy function, but putting it in
`before-save-hook' for every buffer is overkill, and causes messy diffs
when editing code that did not initially have clean whitespace.

Additionally, whitespace preferences are often project-specific, and
it's inconvenient to set up `before-save-hook' in a ".dir-locals.el" file.

`whitespace-cleanup-mode' is a minor mode which calls `whitespace-cleanup'
before saving the current buffer, but only if the whitespace in the buffer
was initially clean.

Set `whitespace-cleanup-mode' to t in ".dir-locals.el" to enable the mode
project-wide, or add it to the hook for the major mode(s) of your choice.

To clean up whitespace everywhere by default, enable
`global-whitespace-cleanup-mode'.

To clean up whitespace at save even if it was intitially dirty,
unset `whitespace-cleanup-mode-only-if-initially-clean'.

This mode is built upon some functionality provided by `whitespace-mode', namely
`whitespace-action': if you would rather see a warning when saving a file with
bogus whitespace, or even have the save aborted, then set that variable.
