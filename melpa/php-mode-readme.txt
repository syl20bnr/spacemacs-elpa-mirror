PHP Mode is a major mode for editing PHP source code.  It's an
extension of C mode; thus it inherits all C mode's navigation
functionality.  But it colors according to the PHP grammar and
indents according to the PEAR coding guidelines.  It also includes
a couple handy IDE-type features such as documentation search and a
source and class browser.

## Usage

Put this file in your Emacs Lisp path (eg. site-lisp) and add to
your .emacs file:

  (require 'php-mode)

To use abbrev-mode, add lines like this:

  (add-hook 'php-mode-hook
    '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

To make php-mode compatible with html-mode, see http://php-mode.sf.net

Many options available under Help:Customize
Options specific to php-mode are in
 Programming/Languages/PHP
Since it inherits much functionality from c-mode, look there too
 Programming/Languages/C
