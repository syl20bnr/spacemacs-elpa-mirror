This package is a front end for the command-line program djvused
from DjVuLibre, see http://djvu.sourceforge.net/.  It assumes you
have the programs djvused, djview, and ddjvu installed.

A normal work flow is as follows:

To visit a djvu file type M-x fjvu-find-file.  This command is the
only entry point to this package.  You may want to bind this
command to a key you like.  I use

  (global-set-key "\C-cd" 'djvu-find-file)

If you use this command to visit file foo.djvu, it puts you into
the (read-only) buffer foo@djvu.  Normally, this buffer is all you
need.

The menu bar of this buffer lists most of the commands with their
repsective key bindings.  For example, you can:

- Use `g' to go to the page you want. (Yes, this package operates on
  one page at a time. I guess that anything else would be too slow
  for large documents.)

- Use `v' to (re)start djview using the position in foo.djvu
  matching where point is in foo@djvu.  (I find djview fast enough
  for this, even for larger documents.)

- To highlight a region in foo.djvu mark the corresponding region
  in foo@djvu (as usual, `transient-mark-mode' comes handy for
  this).  Then type `h' and add a comment in the minibuffer if you
  like.  Type C-x C-s to save this editing.  Then type `v' to
  (re)start djview to show what you have done.

- Type i to enable `djvu-image-mode', a minor mode displaying the
  current page as an image.  Then
    drag-mouse-1 defines a region where to put a comment,
    C-drag-mouse-1 defines a region where to put a pushpin comment,
    S-drag-mouse-1 defines a region to highlight

- The editing of the text, annotation and outline (bookmark) layers
  really happens in the buffers foo@djvu-t.el, foo@djvu-a.el, and
  foo@djvu-o.el.  (The djvused syntax used in these buffers is so
  close to elisp that it was natural to give these buffers a
  djvu-edit-mode that is derived from emacs-lisp-mode.)

  You can check what is happening by switching to these buffers.
  The respective switching commands put point in these buffers such
  that it matches where you were in foo@djvu.

  In these buffers, the menu bar lists a few low-level commands
  available for editing these buffers directly.  If you know the
  djvused syntax, sometimes it can also be helpful to do such
  editing "by hand".

But wait: the syntax in the annotations buffer foo@djvu-a.el is a
slightly modified djvused syntax.  djvused can only highlight
rectangles.  So the highlighting of larger regions of text must use
multiple rectangles (i.e., multiple djvused "mapareas").  To make
editing easier, these are combined in the buffer foo@djvu-a.el.
(Before saving these things, they are converted using the proper
djvused syntax.)

When you visit a djvu file, djvu-mode recognizes mapareas belonging
together by checking that "everything else in these mapareas except
for the rects" is the same.  So if you entered a (unique) comment,
this allows djvu-mode to combine all the mapareas when you visit
such a file the second time.  Without a comment, this fails!

A second difference between what is displayed in the djvu buffers
and the input/output of djvused refers to nonascii characters.  I
am using djvused from DjVuLibre-3.5.22 which handles utf-8 by
backslash sequences.  So djvu mode converts these backslash
sequences into the corresponding utf-8 characters.  (More recent
versions of djvused can do this conversion, too.)



Djvu internals:
(see /usr/share/doc/libdjvulibre-dev/djvu3spec.djvu)

Supported area attributes             rect  oval  poly  line  text
(none)/(xor)/(border c)                X     X     X     X     X
(shadow_* t)                           X
(border_avis)                          X     X     X
(hilite color) / (opacity o)           X
(arrow) / (width w) / (lineclr c)                        X
(backclr c) / (textclr c) / (pushpin)                          X

c = #RRGGBB   t = thickness (1..32)
o = opacity = 0..100