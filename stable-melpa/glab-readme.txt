Glab is a library that provides basic support for using the Gitlab API
from Emacs packages.  It abstracts access to API resources using only
a handful of functions that are not resource-specific.

This library is implemented on top of Ghub.  Unlike Ghub, Glab does
not support the guided creation of tokens because Gitlab lacks the
features that would be necessary to implement that.  Users have to
create tokens through the web interface.
