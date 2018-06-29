Lognav-mode is a minor mode used for finding and navigating errors within a
buffer or a log file.  The keybinding M-n moves the cursor to the first error
within the log file.  M-p moves the cursor to the previous error.

Lognav-mode only highlights the errors that are visible on the screen rather
than highlighting all errors found within the buffer.  This is especially
useful when opening up large log files for analysis.

Add the following line in your .emacs file to use Lognav-mode:

(require 'lognav-mode)


The following bindings are created for Lognav-mode:
M-n   - Next log error            Moves the cursor to the next error
M-p   - Previous log error        Moves the cursor to the previous error
