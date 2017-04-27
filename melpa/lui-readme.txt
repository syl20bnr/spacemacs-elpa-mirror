Lui is a library for other Emacs Lisp programs and not useful by
itself.

This major mode provides a user interface for applications. The
user interface is quite simple, consisting of an input line, a
prompt, and some output area, but Lui includes a lot of common
options, such as time stamps, filling, colorization, etc.

Application programs should create modes derived from lui-mode.

The application API consists of:

lui-mode
lui-set-prompt
lui-insert
lui-input-function
lui-completion-function
and the 'lui-fool and 'lui-do-not-track text properties
