key-leap-mode allows you to quickly jump to any visible line in a
window.  When key-leap-mode is enabled, it will populate the margin
of every line with an unique keyword.  By calling the interactive
command `key-leap-start-matching' the keywords become active.
Typing the keyword of a line in this state will move the point to
the beginning of that line.

You can change the way key-leap-mode generates its keys by setting
the variable `key-leap-key-strings'.  This is a list of strings
that specify what chars to use for each position in the keys.  For
example, adding this to your init file

(setq key-leap-key-strings '("htn" "ao" "ht"))

will make key-leap-mode generate and use the following keys:
hah hat hoh hot tah tat toh tot nah nat noh not

You are not restricted to just three-letter keywords.  By providing
4 different strings, for instance, key-leap will use 4 letters for
every keyword.

You should provide a large enough number of different characters
for key-leap to use.  The number of combinations of characters
should be bigger than the number of possible visible lines for your
setup, but not too much bigger than that.

By default, key-leap-mode will generate 125 keywords from the
home-row of a qwerty keyboard layout, in a right-left-right
fashion.

When calling `key-leap-start-matching' it will run the hooks
`key-leap-before-leap-hook' and `key-leap-after-leap-hook'. For
instance, to make key-leap-mode move to indentation after leaping,
add the following to your config:

(add-hook 'key-leap-after-leap-hook 'back-to-indentation)

When set to nil, `key-leap-upcase-active' will not make the active
parts of the keys upper-cased.  The default is t.

The faces for the active and inactive parts of the keys are
specified by the faces `key-leap-active' and `key-leap-inactive'
respectively.

Key-leap can be integrated with `evil-mode' by adding the function
`key-leap-create-evil-motion' to your Emacs config.  This creates
an evil motion called `key-leap-evil-motion'.  It works the same
way as `key-leap-start-matching', but it will also work with evil
features like operators, visual state and the jump list.  Please
see the documentation for `key-leap-create-evil-motion' for more
information.
