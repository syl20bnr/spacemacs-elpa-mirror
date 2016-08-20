   Extensions to `help-fns.el'.  Also includes a redefinition of
   `describe-face', which is from `faces.el'.

   Note: As of Emacs 24.4, byte-compiling this file in one Emacs
   version and using the compiled file in another Emacs version
   does not work.


 Keys bound here:

   `C-h B'      `describe-buffer'
   `C-h c'      `describe-command'     (replaces `describe-key-briefly')
   `C-h o'      `describe-option'
   `C-h C-c'    `describe-key-briefly' (replaces `C-h c')
   `C-h C-o'    `describe-option-of-type'
   `C-h M-c'    `describe-copying'     (replaces `C-h C-c')
   `C-h M-f'    `describe-file'
   `C-h M-k'    `describe-keymap'
   `C-h M-l'    `find-function-on-key'

 Commands defined here:

   `describe-buffer', `describe-command', `describe-file',
   `describe-keymap', `describe-option', `describe-option-of-type'.

 User options defined here:

   `help-cross-reference-manuals' (Emacs 23.2+).

 Faces defined here:

   `describe-variable-value' (Emacs 24+).

 Non-interactive functions defined here:

   `describe-mode-1', `help-all-exif-data',
   `help-commands-to-key-buttons', `help-custom-type',
   `help-documentation', `help-documentation-property' (Emacs 23+),
   `help-key-button-string', `help-remove-duplicates',
   `help-substitute-command-keys', `help-value-satisfies-type-p',
   `help-var-inherits-type-p', `help-var-is-of-type-p',
   `help-var-matches-type-p', `help-var-val-satisfies-type-p',
   `Info-first-index-occurrence' (Emacs 23.2+),
   `Info-indexed-find-file' (Emacs 23.2+), `Info-indexed-find-node'
   (Emacs 23.2+), `Info-index-entries-across-manuals' (Emacs
   23.2+), `Info-index-occurrences' (Emacs 23.2+),
   `Info-make-manuals-xref' (Emacs 23.2+).

 Internal variables defined here:

   `Info-indexed-file' (Emacs 23.2+), `Info-indexed-nodes' (Emacs
   23.2+), `variable-name-history'.


 ***** NOTE: The following command defined in `faces.el'
             has been REDEFINED HERE:

 `describe-face'.


 ***** NOTE: The following command defined in `help.el'
             has been REDEFINED HERE:

 `describe-mode'.


 ***** NOTE: The following functions defined in `help-fns.el'
             have been REDEFINED HERE:

 `describe-function', `describe-function-1', `describe-variable',
 `help-fns--key-bindings', `help-fns--signature',


 ***** NOTE: The following command defined in `package.el'
             has been REDEFINED HERE:

 `describe-package'.


 Put this in your initialization file (`~/.emacs'):

   (require 'help-fns+)

 Acknowledgement: Passing text properties on doc strings to the
 *Help* buffer is an idea from Johan bockgard.  He sent it on
 2007-01-24 to emacs-devel@gnu.org, Subject
 "display-completion-list should not strip text properties".
