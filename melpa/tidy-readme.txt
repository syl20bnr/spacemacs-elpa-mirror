Commentary:

Provides a simple interface to the HTML Tidy program -- a free
utility that can fix common errors in your mark-up and clean up
sloppy editing automatically. See

      <http://tidy.sourceforge.net/>

for more details.  This package provides the following functions:

      `tidy-buffer',
      `tidy-parse-config-file',
      `tidy-save-settings', and
      `tidy-describe-options',

These can be invoked interactively (using M-x) or via the menu-bar.
The function `tidy-buffer' sends the current buffer to HTML Tidy,
replacing the existing contents with a "tidied" version.  If
`tidy-buffer' is given a prefix argument, tidy operates on the
current region, ignoring mark-up outside <BODY>...</BODY> tags
(useful for writhing cgi scripts in Pearl).  Warnings and errors
are presented in a compilation buffer to facilitate tracking down
necessary changes (e.g. C-x ` is bound to `next-error').

This package also provides menu-bar support for setting Tidy's many
options, and includes support for Tidy configuration files.  The
function `tidy-parse-config-file' will synchronise options
displayed in the menu-bar with the settings in `tidy-config-file'.
This is normally called by the load-hook for your HTML editing mode
(see installation instructions below).  The function
`tidy-save-settings' will save the current option settings to your
`tidy-config-file'.  Finally `tidy-describe-options' allows you to
browse the documentation strings associated with each option.



Installation:

This package assumes you have and up-to-date HTML Tidy program
installed on your system.  See the URL above for instructions on
how to do this.  To set up this support package, first place the
"tidy.el" file somewhere in your `load-path' and open it in Emacs.
Byte-compile and load this package using the command

M-x emacs-lisp-byte-compile-and-load <RET>

Next customise the variables `tidy-config-file', `tidy-temp-dir'
`tidy-shell-command', `tidy-menu-lock' and `tidy-menu-x-position'

M-x customize-group <RET> tidy <RET>

Now add the following autoloads to your ".emacs.el" file:

(autoload 'tidy-buffer "tidy" "Run Tidy HTML parser on current buffer" t)
(autoload 'tidy-parse-config-file "tidy" "Parse the `tidy-config-file'" t)
(autoload 'tidy-save-settings "tidy" "Save settings to `tidy-config-file'" t)
(autoload 'tidy-build-menu  "tidy" "Install an options menu for HTML Tidy." t)

If you use html-mode to edit HTML files then add something like
this as well

(defun my-html-mode-hook () "Customize my html-mode."
  (tidy-build-menu html-mode-map)
  (local-set-key [(control c) (control c)] 'tidy-buffer)
  (setq sgml-validate-command "tidy"))

(add-hook 'html-mode-hook 'my-html-mode-hook)

This will set up a "tidy" menu in the menu bar and bind the key
sequence "C-c C-c" to `tidy-buffer' in html-mode (normally bound to
`validate-buffer').

For other modes (like html-helper-mode) simple change the variables
`html-mode-hook' and `html-mode-map' to whatever is appropriate e.g.

(defun my-html-mode-hook () "Customize my html-helper-mode."
  (tidy-build-menu html-helper-mode-map)
  (local-set-key [(control c) (control c)] 'tidy-buffer)
  (setq sgml-validate-command "tidy"))

(add-hook 'html-helper-mode-hook 'my-html-mode-hook)

Finally, restart Emacs and open an HTML file to test-drive the tidy
package. For people new to HTML tidy check that the option "markup"
under the "Input/Output" sub menu is set. You can read the
documentation on this option via the menu item "Describe Options".

Enjoy!

New Features:

0. Now compatible with CVS version of Tidy as at 22 May 2003
1. Improved menu support to facillitate incorporting new options
2. Menu lock option makes menu stick when toggling options.
3. Now runs on XEmacs!!
4. Uses error file rather than std-error to retrieve errors (this
   fixes some odd pop up behaviour)
5. minor bug fix (empty config files)
6. handle buffer modified query in error buffer better
7. make it impossible to mark the error buffer as modified
8. Added the variable `tidy-temp-directory'.
9. Bugfix in tidy-buffer: call find-file-noselect with NOWARN
