  GNU Emacs major mode for editing Zephir code.  Provides font-locking,
indentation, alignment and navigation support.

  Zephir -- is a high level language that eases the creation and
maintainability of extensions for PHP.  Zephir extensions are
exported to C code that can be compiled and optimized by major C
compilers such as gcc/clang/vc++.  Functionality is exposed to the
PHP language.  For more information see https://zephir-lang.com

Syntax checking: Flymake support is not provided.  See Flycheck at
http://www.flycheck.org for on-the-fly validation and liniting of Zephir
code.

Movement: Move to the beginning or end of the current block with
`beginning-of-defun' (C-M-a) and `end-of-defun' (C-M-e) respectively.

Usage:  Put this file in your Emacs Lisp path (eg. site-lisp) and add to
your .emacs file:

  (require 'zephir-mode)

To use abbrev-mode, add lines like this:
  (add-hook 'zephir-mode-hook
    '(lambda () (define-abbrev zephir-mode-abbrev-table "ex" "extends")))

Many options available under Help:Customize
Options specific to zephir-mode are in Programming/Languages/Zephir

Bugs: Bug tracking is currently handled using the GitHub issue tracker at
https://github.com/sergeyklay/zephir-mode/issues

History: History is tracked in the Git repository rather than in this file.
See https://github.com/sergeyklay/zephir-mode/blob/master/CHANGELOG.md
