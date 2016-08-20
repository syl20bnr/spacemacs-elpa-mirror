Commentary:

   Show key and mouse events and other events as you use them.

 There are two ways to show them:

 * Show the last key used, in a tooltip.  This is refreshed with
   each such event.  For this you use global minor mode
   `showkey-tooltip-mode'.

 * Show a log of such events, in a separate frame.  It is refreshed
   with each event, and it is kept on top of other frames without
   stealing the input focus.  For this you use global minor mode
   `showkey-log-mode'.

 Events that raise an error are not shown.

 Several user options control the behavior:

 * `showkey-log-frame-alist' is an alist of frame parameters for
   the logging frame.  (It is not used for `showkey-tooltip-mode'.)

 * `showkey-log-erase-keys' is a list of keys that will each
   restart logging, that is, erase the log and start it over.  (It
   is not used for `showkey-tooltip-mode'.)

 * `showkey-tooltip-height' is the height of the tooltip text, in
   units of 1/10 point.  The default value is 100, meaning 10pts.

 * `showkey-tooltip-ignored-events' and
   `showkey-log-ignored-events' are each a list of regexps to match
   against events that you do not want to show, for
   `showkey-tooltip-mode' and `showkey-log-mode', respectively.

 * `showkey-tooltip-key-only-flag' non-nil means show only the key
   used, not also its description.  The default value is nil.



 User options defined here:

   `showkey-log-erase-keys', `showkey-log-frame-alist',
   `showkey-tooltip-height', `showkey-log-ignored-events',
   `showkey-tooltip-ignored-events',
   `showkey-tooltip-key-only-flag'.

 Faces defined here:

    `showkey-log-latest'.

 Commands defined here:

   `showkey-log-mode', `showkey-tooltip-mode'.

 Non-interactive functions defined here:

   `showkey-log', `showkey-show-tooltip', `showkey-some'.

 Internal variables defined here:

   `showkey-insert-cmds', `showkey-last-key-desc',
   `showkey-log-frame', `showkey-log-overlay',
   `showkey-nb-consecutives'.
