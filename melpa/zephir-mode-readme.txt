  GNU Emacs major mode for editing Zephir code.  Provides font-locking,
indentation, alignment and navigation support.

  It developed  as an extension of C mode; thus it inherits all C mode's
navigation functionality.  But it colors according to the Zephir grammar.

  Syntax checking: Flymake support is not provided.  See Flycheck at
http://www.flycheck.org for on-the-fly validation and liniting of Zephir
code.

  Zephir -- is a high level language that eases the creation and
maintainability of extensions for PHP.  Zephir extensions are
exported to C code that can be compiled and optimized by major C
compilers such as gcc/clang/vc++.  Functionality is exposed to the
PHP language.  For more information see https://zephir-lang.com

  Bug tracking is currently handled using the GitHub issue tracker at
https://github.com/sergeyklay/zephir-mode/issues

  Issues with this code are managed via the project issue management
on GitHub: https://github.com/sergeyklay/zephir-mode/issues?state=open

  History is tracked in the Git repository rather than in this file.
See https://github.com/sergeyklay/zephir-mode/commits/master

Usage:

  Put this file in your Emacs Lisp path (eg. site-lisp) and add to
your .emacs file:

  (require 'zephir-mode)

To use abbrev-mode, add lines like this:
  (add-hook 'zephir-mode-hook
    '(lambda () (define-abbrev zephir-mode-abbrev-table "ex" "extends")))

Note: The interface used in this file requires CC Mode 5.30 or
later.  You can check your cc-mode with the command M-x c-version.
You can get the latest version of cc-mode at http://cc-mode.sourceforge.net

Many options available under Help:Customize
Options specific to zephir-mode are in
 Programming/Languages/Php
Since it inherits much functionality from c-mode, look there too
 Programming/Languages/C
