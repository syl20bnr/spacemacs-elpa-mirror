Darcs is David's Advanced Revision Control System, available at
http://www.darcs.net/

This version of vc-darcs was tested with Emacs 23 and 24.  It might
still work on Emacs 22 with somewhat reduced functionality.

A few ideas for this file are directly taken from vc-svn.el.  Thanks to
Jim Blandy.

To install, put this file into your load-path and add the following to
your .emacs:

(add-to-list 'vc-handled-backends 'DARCS)
(autoload 'vc-darcs-find-file-hook "vc-darcs")
(add-hook 'find-file-hooks 'vc-darcs-find-file-hook)

There are a few reasons why vc is difficult to coerce into using darcs
as a backend.  By default, vc expects files (not trees) to be versioned
as nodes in an AND/OR tree, as is done by RCS and CVS.  Recent version
of vc allow some customisation of that, which allows smooth integration
with e.g. subversion.

Darcs doesn't version files at all; a darcs repository is a collection
of patches, and a particular file version is just the set of patches
that have been applied in order to build it.  While patches might be
reordered when moving between repositories, they usually remain ordered
(notable exceptions to that being unpull and optimize); hence,
a convenient mental shortcut is to identify a version by the latest
patch included in that version.  This is what we do.

Internally, darcs identifies a patch by its hash, which you may obtain
by using changes --xml.  We follow that approach in this code.  However,
as a hash might be difficult to remember at times (it's 65 characters
long), all commands that might take an interactive argument also accept
a regexp identifying a patch name.  See VC-DARCS-REV-TO-HASH.

The fit with vc is still not quite perfect.  A sore point is that vc
doesn't normalise versions; hence, if you have a patch called ``Initial
import'', you might end up with distinct but identical buffers called
vc-darcs.el~Init~, vc-darcs.el~Initial~ and so on.
