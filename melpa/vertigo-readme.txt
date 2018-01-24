This package is a port of the vim-vertigo plugin and gives commands that
allow the user to jump up or down a number of lines using the home row.
It will primarily be useful when relative line numbers are being used. To
jump down seven lines, for example, the user can press a key bound to
`vertigo-jump-down' and then press "j" since it is the seventh letter on the
home row. `vertigo-home-row' can be altered for non-QWERTY users. Since it is
unlikely that the user will want to use these commands to jump down one or
two lines, `vertigo-cut-off' can be set to determine that the first n keys
should accept another key afterwards. For example, if `vertigo-cut-off' is
set to its default value of 3, pressing "da" would jump 31 lines, pressing
"d;" would jump 30 lines, and pressing "f" would jump 4 lines.

A good alternative to this package is to use avy's `avy-goto-line'.

Additionally, vertigo provides commands to set the digit argument using the
same style of keypresses.

For more information see the README in the github repo.
