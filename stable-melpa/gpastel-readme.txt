GPaste is a clipboard management system.  The Emacs package gpastel
makes sure that every copied text in GPaste is also in the Emacs
kill-ring.

Emacs has built-in support for synchronizing the system clipboard
with the `kill-ring' (see, `interprogram-paste-function', and
`save-interprogram-paste-before-kill').  This support is not
optimal because it makes the `kill-ring' only contain the last text
of consecutive copied texts.  In other words, a user cannot copy
multiple pieces of text from an external application without going
back to Emacs in between.

On the contrary, gpastel supports this scenario by hooking into the
GPaste clipboard manager.  This means that the `kill-ring' will
always contain everything the user copies in external applications,
not just the last piece of text.

Additionally, when using EXWM (the Emacs X Window Manager), gpastel
makes it possible for the user to use the `kill-ring' from external
applications.
