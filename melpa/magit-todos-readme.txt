This package displays keyword entries from source code comments and Org files
in the Magit status buffer.  Activating an item jumps to it in its file.  By
default, it uses keywords from `hl-todo', minus a few (like "NOTE").

Installation:

MELPA

If you installed from MELPA, you're done.

Manual

Install these required packages:

a
anaphora
async
dash
f
hl-todo
magit
pcre2el
s

Then put this file in your load-path, and put this in your init file:

  (require 'magit-todos)
