Pygen is a package that allows the user to automatically generate
Python code.  This allows programmers to code first, and worry
about the functions they are calling later.

A description is available here, but the readme has more
information. You can view it at
http://www.github.com/jackcrawley/pygen


It provides the following code generation commands.  For animated
examples of each command in action, see the GitHub page.

Generating Classes & Functions
------------------------------

`pygen-generate-class' - Generate a python class from the reference
under point.

`pygen-generate-function' - Generate a python function from the
reference under point.

`pygen-generate-static-function' - Generate a static python
function from the reference under point.

Generating Variables
--------------------

`pygen-extract-variable' - Extract the current region into a
variable.

`pygen-make-keyword-argument' - Adds a keyword argument to the
current function.

`pygen-make-sequence-argument' - Adds a sequence argument to the
current function.

Automatic Decorators
--------------------

`pygen-add-decorator-to-function' - Adds a decorator in front of
the current function.

`pygen-make-static' - Turns the current function into a static
function.  WARNING: Not currently functional.

Modifying the "self" keyword:
-----------------------------

`pygen-selfify-symbol' - Puts the word 'self.' in front of the
current symbol.

`pygen-toggle-selfify-symbol' - Toggles the word 'self' in front of
the current symbol.

`pygen-unselfify-symbol' - Removes the word 'self.' from the
current symbol (if it exists).

Dynamic Boilerplate Code Generation
-----------------------------------

`pygen-insert-super' - Inserts a proper call to the current method
in the superclass.


Pygen leverages `elpy' and `python-mode'.  That's the package
called `python-mode', not just the mode.  As of Emacs 24.5.1,
`python-mode' is not the default Python mode but a separate
package.  The default package is called `python'.

Pygen won't work with a Python setup unless `python-mode' is
installed.  However, it can work with a setup that doesn't include
Elpy.  You just need to tell it how to navigate to function and
class definitions.  By default, this is handled by the command
`elpy-goto-definition' to navigate to definitions and the command
`pop-tag-mark'.  If you use a different system to navigate through
python code, you can set the variables
`pygen-navigate-to-definition-command' and `pygen-go-back-command'
to different functions.
