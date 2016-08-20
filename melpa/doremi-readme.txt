   Do Re Mi: Incremental change using arrow keys or mouse wheel.

When you invoke Do Re Mi commands, you can then press and hold an
up/down arrow key, or turn the mouse wheel, to run up and down the
scale: do, re, mi,...

Use the up/down arrow keys or the mouse wheel to:

 - Change nearly any parameter incrementally (dynamically).

 - Repeat an action.

 - Cycle through a set of values, without changing anything (for
   example, to choose an item).  In this use, think of choosing
   from a menu.  This is similar to using a minibuffer history.
   The input choices can take the form of any Emacs-Lisp sequence
   (list, array, string, vector) - this sequence is converted to a
   circular structure (ring).

 - Do just about anything: call a different function for each
   arrow.

This works with numerical parameters that can be incremented and
decremented, and it works with parameters that can take on one of a
number of values.  In fact, it is even more general than that: you
can use it to associate nearly any function or pair of functions
with the arrow keys and the mouse wheel.

By default, the up and down arrow keys are used, but any other keys
may be used instead.  Mouse wheel movements are recognized for
Emacs 20 and Emacs 21 (using library `mwheel.el').  `mouse-2'
presses are ignored, so that they won't interfere with rotating the
wheel.

See the doc string for function `doremi' for more information.

Code defining a few example commands is included here (but
commented out), so you can see how to use this.  For more examples
of using function `doremi', see files `doremi-frm.el' and
`doremi-cmd.el'.

If you use this library in terminal Emacs (no graphic display) then
some of the features, such as mouse-wheel actions, will not be
available to you.  But the basic features should work.

For Emacs prior to release 23, this library requires library
`ring+.el', which provides extensions to the standard library
`ring.el' to let you manipulate circular structures.  (Library
`ring+.el' is part of GNU Emacs 23 and later.)


 Non-interactive functions defined here:

   `doremi', `doremi-intersection', `doremi-limit',
   `doremi-set-new-value', `doremi-wrap'.

 User options (variables) defined here:

   `doremi-boost-down-keys', `doremi-boost-scale-factor',
   `doremi-boost-up-keys', `doremi-down-keys', `doremi-up-keys'.

 Add this to your initialization file (~/.emacs or ~/_emacs):

   (require 'doremi)

 See also these related libraries that make use of `doremi':

   `doremi-frm.el' - Incrementally adjust frame properties.
   `doremi-cmd.el' - Other Do Re Mi commands.
   `doremi-mac.el' - Macro to define Do Re Mi commands and
                     automatically add them to Do Re Mi menu.

 This has been tested on GNU Emacs 20, 21, and 22 on MS Windows.


TO DO?:

  - Replace `boost-*' keys by test for modifiers (as for wheel).
  - Combine with customize.  That is, have customize buffers use
    Do Re Mi commands to defined numeric or enumeration values.
  - Provide buttons (menu items) in menus that act like up & down
    arrows.
