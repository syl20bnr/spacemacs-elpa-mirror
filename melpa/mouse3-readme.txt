This library lets you customize the behavior of `mouse-3' in
several ways.

It redefines standard command `mouse-save-then-kill' in a trivial
way to give you custom behavior for a second `mouse-3' click at the
same spot.

Vanilla Emacs hard-wires the behavior to kill or delete the region
(depending on the value of `mouse-drag-copy-region').  That action
can be handy, but it is sometimes inappropriate (e.g., in a
read-only buffer).  In any case, it is only one possible action;
there are often other actions on the selected text that you might
want to take instead.

The redefined `mouse-save-then-kill' command in `mouse3.el' just
uses function `mouse3-second-click-command' to handle a second
click at the same spot.  That function returns the command that
`mouse-save-then-kill' invokes: either the command that is the
value of variable `mouse3-save-then-kill-command' or, if that is
nil the command that is the value of user option
`mouse3-second-click-default-command'.

Special contexts can bind variable `mouse3-save-then-kill-command'
to provide particular behavior.  For example, Icicles does this
during minibuffer completion so that a second `mouse-3' at the same
spot in buffer `*Completions*' marks the selected completions
(saves them for later reuse).

You can use option `mouse3-second-click-default-command' to
customize the behavior to either pop up a menu or invoke any
command you choose.  The default value is `mouse3-popup-menu',
which pops up a menu.  To obtain the vanilla Emacs behavior,
customize the option value to command `mouse3-kill/delete-region'.

Option `mouse3-double-click-command' associates a command with a
`mouse-3' double-click.  The default value is command
`mouse3-kill/delete-region', so with the default setup you can kill
or delete the selected text by double-clicking `mouse-3' instead of
single clicking.  In other words, in the default setup you have two
possibilities:

 2nd single-click: Pop up a menu so you can choose how to act on
                   the selected text.

 double-click:     Kill or delete the selection, according to
                   standard option `mouse-drag-copy-region'.

If you choose to customize one of these two options,
`mouse3-second-click-default-command' or
`mouse3-double-click-command', then you will probably want to
customize both of them.

To make either a single-click or a double-click do nothing,
customize the appropriate option to the command `ignore'.

Note:

I do not recommend that you try reversing the option values from
their defaults, so that a double-click pops up a menu and the
second single-click at the same spot deletes the selected text.
That does not work well in general because of a flaw (or a feature,
depending on your view) in the Emacs design.

Here's the problem: When you double-click to get the menu, Emacs
first sends a single-click event, before the double-click event.
(That's the flaw/feature.)  So if you double-click at the same spot
then the selection is first deleted, before the menu pops up.

If you double-click at a different spot then this does not happen,
but instead the selection is extended or reduced to match the
double-click position.  The selection might then not be the region
you want the menu items to act on.  Things work pretty well,
however, if you start selecting by double-clicking `mouse-1', click
`mouse-3' here and there to extend, and finally double-click
`mouse-3' near the end of the selection, because in that case the
double-click position does not extend the selection any more.

In sum, in general I do not recommend that you reverse the default
values.  If you want `mouse-3' to pop up a menu, it is better to
either (a) use the default setup so that the menu pops up on the
second single-click, not on a double-click or (b) let
`mouse3-double-click-command' pop up the menu, but set
`mouse3-second-click-default-command' to a command other than
`mouse3-kill/delete-region'.


Customized Pop-Up Menu
----------------------

If you choose for `mouse-3' to pop up a menu then you can customize
that menu using various user options.  The command that pops up the
menu is `mouse3-popup-menu'.  See the documentation for that
command for more explanation of the use of options.

A fairly complete default menu is provided out of the box, so you
do not need to customize anything unless you want to.

`mouse3-popup-menu' ultimately invokes standard function
`x-popup-menu' to do its work.  The menu definition used by
`x-popup-menu' can take two alternative forms, which are quite
different.  Which form you choose to use is controlled by option
`mouse3-popup-x-popup-panes-flag'.

If that option is nil (the default value), you can use keymaps and
extended menu items to define the popup menu.  This is recommended.
If the value is non-nil then you can use options
`mouse3-region-popup-x-popup-panes' and
`mouse3-noregion-popup-x-popup-panes' to make use of the
alternative, `x-popup-menu'-specific form.

That alternative form is easy to use, but it does not offer you all
of the possibilities that a standard menu definition does.  In
particular, it does not let you provide keywords such as `:visible'
and `:enable' that control the display and makeup of submenus and
menu items.  Examples of both methods are provided in this file.

The default behavior takes advantage of keywords to dynamically
remove or disable submenus and menu items that are inappropriate
for the current context.  For example, it removes or disables
buffer-modifying items and submenus if the current buffer is
read-only.

The rest of the description here assumes that
`mouse3-popup-x-popup-panes-flag' is nil (the default value and
recommended).

You can optionally include, at the beginning of the menu, a submenu
that has, as its own submenus, the global menus that are currently
available.  (These are the same menus that are popped up by
`C-mouse-3'.)  This is controlled by option
`mouse3-popup-include-global-menus-flag'.  If the menu bar is not
shown currently, then these submenus are the menu-bar menus.
Otherwise they are the major-mode menus.

For example, if the menu bar is showing, then in Dired mode the
first submenu is `Dired by name', which itself has submenus
`Operate', `Mark', `Regexp', `Immediate', and `Subdir'.

You define the rest of the popup menu (other than the global part)
using submenu keymaps and `menu-item' bindings (extended menu
items).  You do this by customizing options
`mouse3-region-popup-entries' and `mouse3-noregion-popup-entries'.
You can reuse existing keymaps or create menu items and submenus
from scratch.  See the documentation for
`mouse3-(no)region-popup-entries'.  If you reuse existing keymaps
you can add their menu items either as a submenu or as individual
items.


Mode-Specific Popup Menu
------------------------

You can provide mode-specific behavior by either replacing the
default `mouse-3' popup menu or augmenting it with mode-specific
entries.

`mouse3.el' provides useful behavior for Dired mode.  You can do
the same for other modes you use.  Simple example code is also
provided here for Picture mode.  The menu implementation for these
two modes is different, to give you an idea of what is possible.
For Dired mode, `mouse3-region-popup-entries' is used.  For Picture
mode, `mouse3-region-popup-x-popup-panes' is used, and both
`mouse3-popup-x-popup-panes-flag' and
`mouse3-region-popup-x-popup-panes' are made buffer-local.

The example code for Picture mode provides actions on the rectangle
defined by the region: items such as `Draw Rectangle' and `Clear
Rectangle'.

Let's look at the behavior for Dired mode in more detail.  The
vanilla Emacs behavior just raises an error, because Dired is
read-only.  Why not let a second `mouse-3' click at the same spot
do something wrt the selected file and dir names?  Two obvious
possibilities come to mind: toggle whether each file/dir is marked,
or pop up a menu that lets you act in various ways on each of the
selected files and dirs.

Option `mouse3-dired-function' lets you choose between these two
behaviors.  The default value is `mouse3-dired-use-menu', which
means to pop up a menu.  This is just like the default popup menu
except that it has an additional submenu, `Selected Files', that is
Dired-specific.  The first submenu is the global major-mode menu,
`Dired by name'.  The second, `Selected Files', has items that act
on the files and directories that are selected (in the region):

 `Mark'
 `Unmark'
 `Toggle Marked/Unmarked'
 `Flag for Deletion'
 `Stop Using Menu'

That last item just switches from having `mouse-3' pop up a menu to
having it toggle the markings of the selected files (the
alternative behavior of `mouse3-dired-function').

If you also use library Dired+ (`dired+.el'), which I recommend,
then that last menu item is not present, and when the region is
empty you get a different popup menu which pertains only to the
file where you clicked `mouse-3'.


Context-Specific Behavior (for Emacs-Lisp Programmers)
------------------------------------------------------

As mentioned above, Icicles provides an example of a program
imposing context-specific behavior for the second `mouse-3' click
at the same spot.  The context in this case is (a) completion and
(b) clicking in buffer `*Completions*'.  For that, Icicles sets a
buffer-local value of variable `mouse3-save-then-kill-command'.

If you write Emacs-Lisp code, note that this is an example where
the text is a set of entries in tabular format (columns).  Each
`*Completions*' entry (candidate) is defined by its `mouse-face'
property, not by its text.  For example, it is not delimited by
whitespace (completion candidates can contain spaces and newlines).
A context-specific function picks up the set of selected
completions as a list.

Similar opportunities can exist for other tabular or line-list
data: `*Buffer List*', compile/grep output, Info breadcrumbs,...
Use your imagination.  The Dired example is typical here: the
region sweeps out text linearly, but the only thing we are really
interested in are the file and subdirectory names that are inside
the region.

-------------------------------------------------------------------


User options defined here:

  `mouse3-dired-function', `mouse3-double-click-command',
  `mouse3-noregion-popup-entries',
  `mouse3-noregion-popup-x-popup-panes',
  `mouse3-picture-mode-x-popup-panes',
  `mouse3-popup-include-global-menus-flag',
  `mouse3-popup-x-popup-panes-flag', `mouse3-region-popup-entries',
  `mouse3-region-popup-x-popup-panes',

  `mouse3-second-click-default-command'.

Commands defined here:

  `mouse3-dired', `mouse3-dired-flag-region-files-for-deletion',
  `mouse3-dired-mark-region-files', `mouse3-dired-other-window',
  `mouse3-dired-toggle-marks-in-region',
  `mouse3-dired-toggle-marks-in-region-from-mouse',
  `mouse3-dired-unmark-region-files', `mouse3-dired-use-menu',
  `mouse3-dired-use-toggle-marks', `mouse3-kill/delete-region',
  `mouse3-popup-menu', `mouse3-pp-eval-sexp'.

Non-interactive functions defined here:

  `mouse3-dired-add-region-menu',
  `mouse3-dired-set-to-toggle-marks',
  `mouse3-dired-this-file-marked-p',
  `mouse3-dired-this-file-unmarked-p',
  `mouse3-dired-toggle-marks-in-region', `mouse3-ffap-guesser',
  `mouse3-file-or-dir', `mouse3-nonempty-region-p',
  `mouse3-region-popup-choice', `mouse3-region-popup-choice-1',
  `mouse3-region-popup-custom-entries',
  `mouse3-second-click-command'.

Internal variables defined here:

  `mouse3-ffap-max-region-size',
  `mouse3-noregion-popup-misc-submenu',
  `mouse3-region-popup-change-text-submenu',
  `mouse3-region-popup-check-convert-submenu',
  `mouse3-region-popup-copy-submenu',
  `mouse3-region-popup-highlight-submenu',
  `mouse3-region-popup-misc-submenu',
  `mouse3-region-popup-print-submenu',
  `mouse3-region-popup-rectangle-submenu',
  `mouse3-region-popup-register-submenu',
  `mouse3-region-popup-remove/replace-items',
  `mouse3-region-popup-remove/replace-rect-submenu',
  `mouse3-save-then-kill-command'.


 ***** NOTE: The following functions defined in `mouse.el' have
             been REDEFINED HERE:

 `mouse-save-then-kill' - Uses `mouse3-second-click-command' to
                          define second `mouse-3' click behavior.
