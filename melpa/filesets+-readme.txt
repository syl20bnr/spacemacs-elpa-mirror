 Extensions to `filesets.el'.

 This library provides some fixes to standard library
 `filesets.el'.  The reference version of that library is 1.8.4,
 but I believe the same fixes are appropriate for other versions
 (e.g. 2.2, named `filesets2.el', which is the latest version by
 the original author, at
 http://members.a1.net/t.link/CompEmacsFilesets.html).

 Put this in your initialization file (`~/.emacs'):

   (require 'filesets+)
   (filesets-init) ; Enable filesets

 You should also customize the following two options, to put the
 `Filesets' menu on the `File' menu just before item `Open
 File...':

  `filesets-menu-path'   - value should be ("file")
  `filesets-menu-before' - value should be "Open File..."
