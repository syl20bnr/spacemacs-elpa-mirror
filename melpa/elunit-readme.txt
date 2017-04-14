THIS PACKAGE IS DEPRECATED.  For new packages you should use `ert'
instead.  The tests of some existing packages still use `elunit',
which is why it was made available on Melpa.

Inspired by regress.el by Wayne Mesard and Tom Breton, Test::Unit
by Nathaniel Talbott, and xUnit by Kent Beck

ElUnit exists to accomodate test-driven development of Emacs Lisp
programs.  Tests are divided up into suites.  Each test makes a
number of assertions to ensure that things are going according to
expected.

Tests are divided into suites for the purpose of hierarchical
structure and hooks.  The hierarchy allows suites to belong to
suites, in essence creating test trees.  The hooks are meant to
allow for extra setup that happens once per test, for both before
and after it runs.

You may use Emacs' built-in `assert' function for checking such
things, but the assertions at the bottom of this file provide much
better reporting if you use them.  Using `assert-that' is preferred
over built-in `assert'.
