This package provides two functions to invoke programs with arguments
in a buffer which works as a terminal-emulator.


term-run-shell-command (command $optional new-buffer-p)

Run COMMAND in a terminal buffer.

This function is intended mainly to be called interactively and
asks the command-line to invoke.

If called with prefix argument, this function will generate new
terminal buffer for running COMMAND.  Otherwise, always use the buffer named
*Term-Run Shell Command*. In this case, the old process in the buffer will be
destroyed.


term-run (program &optional buffer-or-name &rest args)

Run PROGRAM in BUFFER-OR-NAME with ARGS in terminal buffer.

If BUFFER-OR-NAME is given, use this buffer.  In this case, old process in
the buffer will be destroyed.  Otherwise, new buffer will be generated
automatically from PROGRAM.

This function returns the buffer where the process starts running.
