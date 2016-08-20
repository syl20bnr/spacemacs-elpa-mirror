Emacs provides `prog1', `prog2' and `progn' to group statements and
return the value of first, second or last form.  This package
extends this notion to allow arbitrary index.  The macro `prognth'
takes as a first argument the index of the form whose value is to
be returned.  If the index is 1, 2 or greater than length of the
body, the standard emacs forms are used.  Otherwise, this
translates to (prog1 (progn ... first INDEX forms) rest)

Additionally, `progX' for X from 3 to 9 are generated for easier
usage.
