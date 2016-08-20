   Extensions to `faces.el'.


 New functions defined here:

   `face-background-20+', `face-foreground-20+'.



 ***** NOTE: The following functions defined in `faces.el' have
             been REDEFINED HERE:

 `make-face' - Uses `completing-read' in the interactive spec,
               with, as default, `symbol-nearest-point'.

 `read-face-name' - `highlight' is the default (Emacs < 21 only).


 This file should be loaded after loading the standard GNU file
 `faces.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "faces" '(require 'faces+))
