   Hide/show comments in code.

 Comments are hidden by giving them and `invisible' property of
 value `hide-comment'.


 Macros defined here:

   `with-comments-hidden'.

 Commands defined here:

   `hide/show-comments', `hide/show-comments-toggle'.

 User options defined here:

   `hide-whitespace-before-comment-flag', `ignore-comments-flag',
   `show-invisible-comments-shows-all'.

 Non-interactive functions defined here:

   `hide/show-comments-1'.


 Put this in your init file (`~/.emacs'):

  (require 'hide-comnt)


 Note for Emacs 20: The commands and option defined here DO NOTHING
 IN EMACS 20.  Nevertheless, the library can be byte-compiled in
 Emacs 20 and `hide-comnt.elc' can be loaded in later Emacs
 versions and used there.  This is the only real use of this
 library for Emacs 20: it provides macro `with-comments-hidden'.

 Note for Emacs 21: It lacks the `comment-forward' function, so we
 rely on the `comment-end' variable to determine the end of a
 comment. This means that only one type of comment terminator is
 supported.  For example, `c++-mode' sets `comment-end' to "",
 which is the convention for single-line comments ("// COMMENT").
 So "/* */" comments are treated as single-line commentsonly the
 first line of such comments is hidden.  The "*/" terminator is not
 taken into account.
