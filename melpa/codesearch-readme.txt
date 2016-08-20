Description:

This extension allows you to use the codesearch code indexing
system in emacs.

For more details, see the project page at
https://github.com/abingham/codesearch.el.

For more details on codesearch, see its project page at
http://code.google.com/p/codesearch/

Installation:

The simple way is to use package.el:

  M-x package-install codesearch

Or, copy codesearch.el to some location in your emacs load
path. Then add "(require 'codesearch)" to your emacs initialization
(.emacs, init.el, or something).

Example config:

  (require 'codesearch)

This elisp extension assumes you've got the codesearch tools -
csearch and cindex - installed. See the codesearch-csearch and
codesearch-cindex variables for more information.
