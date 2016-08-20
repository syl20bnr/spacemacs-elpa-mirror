   Enhancements of standard library `files.el'.

 Commands defined here:

   `dired-describe-listed-directory',
   `dired-mouse-describe-listed-directory',


 ***** NOTE: The following macro defined in `files.el' has been
             COPIED HERE:

   `minibuffer-with-setup-hook' (Emacs 22 & 23.1 only).

 ***** NOTE: The following functions defined in `files.el' have been
             REDEFINED HERE:

   `display-buffer-other-frame' - Use `read-buffer'.
                                  Do not select the buffer.
                                  (Emacs 20-23 only)
   `find-file-read-args' - In Dired, use file at cursor as default
                           (Emacs 22 & 23.1 only).
   `insert-directory' - Add file count in Dired for each dir.
   `switch-to-buffer-other-frame'  - Use `read-buffer'.
                                     Return the buffer switched to.
                                     (Emacs 20-23 only)
   `switch-to-buffer-other-window' -
      Use `read-buffer'.
      Raise frame of selected window (for non-nil `pop-up-frames').
      (Emacs 20-23 only)

 Load this library after loading the standard library `files.el'.
 However, if you use MS Windows, MS-DOS, or MacOS, then you will
 likely want to use library `ls-lisp+.el' together with
 `files+.el', to use an Emacs Lisp-only definition of
 `insert-directory'.

 In that case, do *NOT* load `files+.el' directly.  Instead, just
 load `ls-lisp+.el' - it will load `ls-lisp.el' and `files+.el'.
 That is, do only this in your init file:

  (require 'ls-lisp+)
