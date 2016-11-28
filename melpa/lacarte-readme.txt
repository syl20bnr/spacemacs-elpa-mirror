 Q. When is a menu not a menu?  A. When it's a la carte.

 Library La Carte lets you execute menu items as commands, with
 completion.  You can use it as an alternative to standard library
 `tmm.el'.

 Type a menu item.  Completion is available.  Completion candidates
 are of the form menu > submenu > subsubmenu > ... > menu item.
 For example:

   File > Open Recent > Cleanup list
   File > Open Recent > Edit list...

 When you choose a menu-item candidate, the corresponding command
 is executed.

 Put this in your init file (~/.emacs):

   (require 'lacarte)

 Suggested key bindings:

   (global-set-key [?\e ?\M-x] 'lacarte-execute-command)
   (global-set-key [?\M-`]     'lacarte-execute-menu-command)
   (global-set-key [f10]       'lacarte-execute-menu-command)

 (The latter two replace standard bindings for `tmm-menubar'.  On
 MS Windows, `f10' is normally bound to `menu-bar-open', which uses
 the Windows native keyboard access to menus.)

 To really take advantage of La Carte, use it together with
 Icicles.  Icicles is not required to be able to use La Carte, but
 it enhances the functionality of `lacarte.el' considerably.
 (Note: `lacarte.el' was originally called `icicles-menu.el'.)

 If you use MS Windows keyboard accelerators, consider using
 `lacarte-remove-w32-keybd-accelerators' as the value of
 `lacarte-convert-menu-item-function'.  It removes any unescaped
 `&' characters (indicating an accelerator) from the menu items.
 One library that adds keyboard accelerators to your menu items is
 `menuacc.el', by Lennart Borgman (< l e n n a r t . b o r g m a n
 @ g m a i l . c o m >).


 Commands defined here:

   `lacarte-execute-command', `lacarte-execute-menu-command'.

 User options defined here:

   `lacarte-convert-menu-item-function'.

 Faces defined here:

   `lacarte-shortcut'.

 Non-interactive functions defined here:

   `lacarte-add-if-menu-item', `lacarte-escape-w32-accel',
   `lacarte-get-a-menu-item-alist',
   `lacarte-get-a-menu-item-alist-1',
   `lacarte-get-a-menu-item-alist-22+',
   `lacarte-get-a-menu-item-alist-pre-22',
   `lacarte-get-overall-menu-item-alist',
   `lacarte-key-description', `lacarte-menu-first-p',
   `lacarte-propertize', `lacarte-remove-w32-keybd-accelerators',
   `lacarte-string-match-p'.

 Internal variables defined here:

   `lacarte-history', `lacarte-menu-items-alist'.


 Getting Started
 ---------------

 In your init file (`~/.emacs'), bind `ESC M-x' as suggested above:

   (global-set-key [?\e ?\M-x] 'lacarte-execute-command)

 Type `ESC M-x' (or `ESC ESC x', which is the same thing).  You are
 prompted for a command or menu command to execute.  Just start
 typing its name.  Each menu item's full name, for completion, has
 its parent menu names as prefixes.

 ESC M-x
 Command:
 Command: t [TAB]
 Command: Tools >
 Command: Tools > Compa [TAB]
 Command: Tools > Compare (Ediff) > Two F [TAB]
 Command: Tools > Compare (Ediff) > Two Files... [RET]


 Not Just for Wimps and Noobs Anymore
 ------------------------------------

 *You* don't use menus.  Nah, they're too slow!  Only newbies and
 wimps use menus.  Well not any more.  Use the keyboard to access
 any menu item, without knowing where it is or what its full name
 is.  Type just part of its name and use completion to get the
 rest: the complete path and item name.


 Commands and Menu Commands
 --------------------------

 You can bind either `lacarte-execute-menu-command' or
 `lacarte-execute-command' to a key such as `ESC M-x'.

 `lacarte-execute-menu-command' uses only menu commands.
 `lacarte-execute-command' lets you choose among ordinary Emacs
 commands, in addition to menu commands.  You can use a prefix arg
 with `lacarte-execute-command' to get the same effect as
 `lacarte-execute-menu-command'.

 Use `lacarte-execute-command' if you don't care whether a command
 is on a menu.  Then, if you want a command that affects a buffer,
 just type `buf'.  This is especially useful if you use Icicles -
 see below.

 You can use a prefix arg with `lacarte-execute-menu-command' to
 have it offer only items from specific keymaps: the local (major
 mode) keymap, the global keymap, or the minor-mode keymaps.

 By default, in Icicle mode, `ESC M-x' is bound to
 `lacarte-execute-command', and `M-`' is bound to
 `lacarte-execute-menu-command'.


 Icicles Enhances Dining A La Carte
 ----------------------------------

 Use Icicles with La Carte to get more power and convenience.

 It is Icicles that lets you choose menu items a la carte, in fact.
 That is, you can access them directly, wherever they might be in
 the menu hierachy.  Without Icicles, you are limited to choosing
 items by their menu-hierarchy prefixes, and you must complete the
 entire menu prefix to the item, from the top of the menu on down.
 With Icicles, you can directly match any parts of a menu item and
 its hierarchy path.  Icicles is here:
 http://www.emacswiki.org/Icicles.

 Type any part of a menu-item, then use the Page Up and Page Down
 keys (`prior' and `next') to cycle through all menu commands that
 contain the text you typed somewhere in their name.  You can match
 within any menu or within all menus; that is, you can match any
 part(s) of the menu-hierachy prefix.

 You can use `S-TAB' to show and choose from all such "apropos
 completions", just as you normally use `TAB' to show all prefix
 completions (that is, ordinary completions).  Vanilla, prefix
 completion is still available using `TAB', and you can cycle
 through the prefix completions using the arrow keys.

 You can use Icicles "progressive completion" to match multiple
 parts of a menu item separately, in any order.  For example, if
 you want a menu command that has to do with buffers and
 highlighting, type `buf M-SPC hig S-TAB'.

 Icicles apropos completion also lets you type a regular expression
 (regexp) - it is matched against all of the possible menu items.
 So, for instance, you could type `^e.+buff [next] [next]...' to
 quickly cycle to menu command `Edit > Go To > Goto End of Buffer'.
 Or type `.*print.*buf S-TAB' to choose from the list of all menu
 commands that match `print' followed somewhere by `buf'.

 If you know how to use regexps, you can easily and quickly get to
 a menu command you want, or at least narrow the list of candidates
 for completion and cycling.

 Additional benefits of using Icicles with La Carte:

 * When you cycle to a candidate menu item, or you complete to one
   (entirely), the Emacs command associated with the menu item is
   shown in the mode line of buffer `*Completions*'.

 * You can use `M-h' to complete your minibuffer input against
   commands, including menu-item commands, that you have entered
   previously.  You can also use the standard history keys
   (e.g. `M-p', `M-r') to access these commands.


 Menu Organization Helps You Find a Command
 ------------------------------------------

 Unlike commands listed in a flat `*Apropos*' page, menu items are
 organized, grouped logically by common area of application
 (`File', `Edit',...).  This grouping is also available when
 cycling completion candidates using Icicles, and you can take
 advantage of it to hasten your search for the right command.

 You want to execute a command that puts the cursor at the end of a
 buffer, but you don't remember its name, what menu it might be a
 part of, or where it might appear in that (possibly complex) menu.
 With Icicles and La Carte, you type `ESC M-x' and then type
 `buffer' at the prompt.  You use the Page Up and Page Down keys to
 cycle through all menu items that contain the word `buffer'.

 There are lots of such menu items.  But all items from the same
 menu (e.g. `File') are grouped together.  You cycle quickly (not
 reading) to the `Edit' menu, because you guess that moving the
 cursor has more to do with editing than with file operations, tool
 use, buffer choice, help, etc.  Then you cycle more slowly among
 the `buffer' menu items in the `Edit' menu.  You quickly find
 `Edit > Go To > Goto End of Buffer'.  QED.


 Learn About Menu Items By Exploring Them
 ----------------------------------------

 With Icicles, you can display the complete documentation (doc
 string) for the command corresponding to each menu item, as the
 item appears in the minibuffer.  To do this, just cycle menu-item
 candidates using `C-down' or `C-next', instead of `[down]' or
 `[next]'.  The documentation appears in buffer `*Help*'.

 In sum, if you use La Carte, you will want to use it with Icicles!

(@> "Index")

 If you have library `linkd.el' and Emacs 22 or later, load
 `linkd.el' and turn on `linkd-mode' now.  It lets you easily
 navigate around the sections of this doc.  Linkd mode will
 highlight this Index, as well as the cross-references and section
 headings throughout this file.  You can get `linkd.el' here:
 http://www.emacswiki.org/emacs/download/linkd.el.

 (@> "Change log")
 (@> "User Options")
 (@> "Internal Variables")
 (@> "Functions")
