Adds a `Recent Commands' submenu to the menu-bar `Tools' menu.  It
holds the most recent commands you have invoked using `M-x'.

Put this in your init file (~/.emacs):

  (require 'cmds-menu)

Loading this file adds submenu `Recent Commands' and updates
`menu-bar-update-hook' so that this menu is automatically updated.


User options defined here:

   `recent-cmds-menu-max-size', `recent-cmds-name-length'.

Non-interactive functions defined here:

   `recent-cmds-menu-bar-update'.

Internal variables defined here:

   `recent-cmds-menu'.
