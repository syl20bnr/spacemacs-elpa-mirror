   `Recent Commands' submenu for the menu-bar `Tools' menu.

 Global minor mode `recent-cmds-menu-mode' adds a `Recent Commands'
 submenu to the menu-bar `Tools' menu and updates
 `menu-bar-update-hook' so that this menu is automatically updated.
 Menu `Recent Commands' holds the most recent commands you have
 invoked using `M-x'.

 Put this in your init file (~/.emacs):

  (require 'cmds-menu)

 If you want to turn on the mode from the outset, add this:

  (recent-cmds-menu-mode 1)


 User options defined here:

   `recent-cmds-menu-max-size', `recent-cmds-menu-mode',
   `recent-cmds-name-length'.

 Commands defined here:

   `recent-cmds-menu-mode'.

 Non-interactive functions defined here:

   `recent-cmds-menu-bar-update'.

 Internal variables defined here:

   `recent-cmds-menu'.
