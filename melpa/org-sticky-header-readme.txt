This package displays in the header-line the Org heading for the
node that's at the top of the window.  This way, if the heading for
the text at the top of the window is beyond the top of the window,
you don't forget which heading the text belongs to.

The code is very simple and is based on `semantic-stickyfunc-mode'.

Installation:

Install from MELPA and run `org-sticky-header-mode'.

To install manually, put this file in your `load-path', require
`org-sticky-header' in your init file, and run the same command.

You probably want to add `org-sticky-func-mode' to your `org-mode-hook'.

If `org-startup-indented' is enabled, the
`org-sticky-header-prefix' will be automatically set to match the
`org-indent-mode' prefixes; otherwise you may wish to customize it.
