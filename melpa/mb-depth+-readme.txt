 This library modifies library `mb-depth.el' slightly, to let you
 decide what depth indicator format to use, and which face to
 highlight it in.  It provides a minor tweak to function
 `minibuffer-depth-setup', which, in `mb-depth.el', hard-codes the
 face and indicator format.

 In addition, the default indicator format is simpler than that in
 `mb-depth.el', and the default face is `default' instead of
 `highlight'.

 Faces defined here:

   `minibuffer-depth-indicator'.

 User options defined here:

   `minibuffer-depth-indicator-format'.


 To use this library, put this in your init file (~/.emacs):

 ;; Use `condition-case' because if `mb-depth.el' can't be found,
 ;; then `mb-depth+.el' is not provided.
 (condition-case nil (require 'mb-depth+ nil t) (error nil))
