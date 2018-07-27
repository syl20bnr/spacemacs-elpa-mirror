An Emacs minor mode to check and fix the syntax of Javascript
buffers with the Closure Linter.

The mode uses flymake to check the syntax of Javascript buffers
with the "gjslint" program. Errors will be highlighted via flymake
and some of them can be resolved by calling the "fixjsstyle"
program.

Key bindings:

C-c C-e  -  Show the error message of a line with errors.
C-c C-f  -  Fix the current buffer by calling fixjsstyle.
