 Extensions to standard library tool-bar.el.

 New commands defined here:

   `show-tool-bar-for-one-command', `tool-bar-here-mode',
   `tool-bar-pop-up-mode'.


 New key bound here: [menu-bar pop-up-tool-bar]


 Usage:

   Load this library: (require 'tool-bar+).
   Turn on tool-bar pop-up mode: M-x tool-bar-pop-up-mode.

   Click "Buttons" in the menu-bar to access the tool-bar when you
   need it.  This displays the tool-bar buttons just long enough
   for one command: after you click a tool-bar button, the tool-bar
   disappears again.

   The advantage of `tool-bar-pop-up-mode' is that you do not lose
   frame real estate to the tool-bar -- you have it when you need
   it, at the cost of an extra click ("Buttons").

   In addition to defining minor mode `tool-bar-pop-up-mode', this
   library defines minor mode `tool-bar-here-mode', which is the
   same as the global `tool-bar-mode' except that it affects only
   the current frame.

   The advantage of `tool-bar-here-mode' is (again) that it saves
   real estate on frames other than the ones with the tool-bar.


 Put this in your initialization file (`~/.emacs'):

 (require 'tool-bar+) ; load this library
