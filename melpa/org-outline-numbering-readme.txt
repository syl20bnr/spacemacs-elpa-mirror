This package defines a minor mode that displays an outline
numbering as overlays on Org mode headlines. The numbering matches
how it would appear when exporting the org file.

Activating ‘org-outline-numbering-mode’ displays the numbers and
deactivating it clears them. There is no facility for auto-updating
but the numbering can be recalculated by calling
‘org-outline-numbering-display’ and cleared by calling
‘org-outline-numbering-clear’.

By default trees that are commented, archived, tagged noexport or
other similar things that would exclude them from org export don’t
get a number to keep consistency with exports. If additional tags
are to be excluded from numbering they can be added to
‘org-outline-numbering-ignored-tags’.


Adapted from code posted by John Kitchin at:
https://emacs.stackexchange.com/a/32422
