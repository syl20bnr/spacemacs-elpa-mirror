This package gives commands for sentence navigation and manipulation
that ignore abbreviations. For example, in a sentence that contains
"Mr. MacGyver", "MacGyver" will not be considered to be the start of
a sentence. One-spaced sentences are the target of this plugin, but
it will also work properly with two-spaced sentences.

This package is inspired by vim-textobj-sentence and uses a modified
version of its default regex list. Evil is an optional dependency
that is used for the text objects this package provides.

For more information see the README in the github repo.
