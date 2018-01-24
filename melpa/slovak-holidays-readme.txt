This package adds [slovak](http://en.wikipedia.org/wiki/Slovakia)
holidays to the Emacs calendar.

If you have `org-agenda-include-diary` set to `t`, these will be
also listed in the `org-agenda` view.

Installation
============

If you install from MELPA, then you don't have to do anything as
the list will automatically autoload and be added to
`holiday-other-holidays` list.

If you install manually, add a call to `(slovak-holidays-add)`
somewhere into your `.emacs`.  Note that this must be called
*before* Emacs calendar is loaded.
