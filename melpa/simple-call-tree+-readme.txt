Commentary:

Bitcoin donations gratefully accepted: 1AmWPmshr6i9gajMi1yqHgx7BYzpPKuzMz

Introduction:
This library is based on simple-call-tree.el by Alex Schroeder, but you
do not need that library to use it (this is a replacement).
It displays a buffer containing a call tree for functions in source
code files. You can easily & quickly navigate the call tree, displaying
the code in another window, and apply query-replace or other commands
to individual functions.

When the command `simple-call-tree-display-buffer' is executed
a call tree for the functions in the current buffer will be created.
The user is also prompted for other files to include in the call tree.
The call tree is displayed in a buffer called *Simple Call Tree*,
which has a dedicated menu in the menu-bar showing various commands
and their keybindings. Most of these commands are self explanatory
so try them out.
