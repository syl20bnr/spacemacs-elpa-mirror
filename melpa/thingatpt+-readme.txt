   Extensions to `thingatpt.el'.


 Commands defined here:

   `find-fn-or-var-nearest-point', `forward-char-same-line',
   `forward-whitespace-&-newlines', `tap-put-thing-at-point-props',
   `tap-redefine-std-fns'.

 User options defined here:

   `tap-near-point-x-distance', `tap-near-point-y-distance'.

 Non-interactive functions defined here:

   `tap-bounds-of-color-at-point', `tap-bounds-of-form-at-point',
   `tap-bounds-of-form-nearest-point',
   `tap-bounds-of-list-at-point',
   `tap-bounds-of-list-contents-at-point',
   `tap-bounds-of-list-nearest-point',
   `tap-bounds-of-number-at-point',
   `tap-bounds-of-number-at-point-decimal',
   `tap-bounds-of-number-at-point-hex',
   `tap-bounds-of-sexp-at-point',
   `tap-bounds-of-sexp-nearest-point',
   `tap-bounds-of-string-at-point',
   `tap-bounds-of-string-contents-at-point',
   `tap-bounds-of-symbol-at-point',
   `tap-bounds-of-symbol-nearest-point',
   `tap-bounds-of-thing-nearest-point', `tap-color-at-point',
   `tap-color-nearest-point',
   `tap-color-nearest-point-with-bounds',
   `tap-define-aliases-wo-prefix', `tap-form-at-point-with-bounds',
   `tap-form-nearest-point', `tap-form-nearest-point-with-bounds',
   `tap-list-at/nearest-point-with-bounds',
   `tap-list-at-point-with-bounds', `tap-list-contents-at-point',
   `tap-list-contents-nearest-point', `tap-list-nearest-point',
   `tap-list-nearest-point-with-bounds',
   `tap-list-nearest-point-as-string', `tap-looking-at-p',
   `tap-looking-back-p', `tap-non-nil-symbol-name-at-point',
   `tap-non-nil-symbol-name-nearest-point',
   `tap-non-nil-symbol-nearest-point',
   `tap-number-at-point-decimal', `tap-number-at-point-hex',
   `tap-number-nearest-point', `tap-read-from-whole-string',
   `tap-region-or-word-at-point',
   `tap-region-or-word-nearest-point',
   `tap-region-or-non-nil-symbol-name-nearest-point',
   `tap-sentence-nearest-point', `tap-sexp-at-point-with-bounds',
   `tap-sexp-nearest-point', `tap-sexp-nearest-point-with-bounds',
   `tap-string-at-point', `tap-string-contents-at-point',
   `tap-string-contents-nearest-point', `tap-string-match-p',
   `tap-string-nearest-point', `tap-symbol-at-point-with-bounds',
   `tap-symbol-name-at-point', `tap-symbol-name-nearest-point',
   `tap-symbol-nearest-point',
   `tap-symbol-nearest-point-with-bounds',
   `tap-thing-at-point-with-bounds',
   `tap-thing/form-nearest-point-with-bounds',
   `tap-thing-nearest-point',
   `tap-thing-nearest-point-with-bounds',
   `tap-unquoted-list-at-point', `tap-unquoted-list-nearest-point',
   `tap-unquoted-list-nearest-point-as-string',
   `tap-word-nearest-point',

   plus the same functions without the prefix `tap-', if you invoke
   `tap-redefine-std-fns'.


 A REMINDER (the doc strings are not so great):

   These functions, defined in `thingatpt.el', all move point:
     `beginning-of-thing', `end-of-sexp', `end-of-thing',
     `forward-symbol', `forward-thing'.

 For older Emacs releases that do not have the following functions,
 they are defined here as no-ops:

 `constrain-to-field', `field-beginning', `field-end'.


 How To Use This Library
 =======================

 End Users
 ---------

 Load this library after loading the standard GNU file
 `thingatpt.el'.  You can put this in your init file (`~/.emacs'):

   (eval-after-load "thingatpt" '(require 'thingatpt+))

 That defines new functions and improved versions of some of the
 standard thing-at-point functions.  All such functions have the
 prefix `tap-', so they are not used by default in any way.

 Requiring library `thingatpt+.el' does not, however, make Emacs
 use the improved functions.  Merely loading it does not change the
 behavior of thing-at-point features.

 If you want functions defined here to be used for calls to
 standard Emacs functions that make use of the `thing-at-point' and
 `bounds-of-thing-at-point' symbol properties for standard thing
 types (e.g. `list'), then put this in your init file, instead:

   (eval-after-load "thingatpt"
     '(when (require 'thingatpt+)
        (tap-put-thing-at-point-props))

 Note that some of my other libraries, including Icicles,
 Bookmark+, `grep+.el', `replace+.el', and `strings.el', do exactly
 that.  Note too that `tap-put-thing-at-point-props' improves the
 behavior of (thing-at-point 'list) - see below.

 A further step, which I recommend, is to use the `tap-' versions
 of standard functions, defined here, everywhere in place of those
 standard functions.  In other words, redefine the standard
 functions as the `tap-' versions defined here.  For example,
 redefine `bounds-of-thing-at-point' to do what
 `tap-bounds-of-thing-at-point' does.

 (If you do that then you need not invoke
 `tap-put-thing-at-point-props' to pick up the versions defined
 here of standard functions.  The property values set by vanilla
 library `thingatpt.el' will be OK because the functions themselves
 will have been redefined in that case.)

 To get the most out of this library, I recommend that you put
 (only) the following in your init file:

   (eval-after-load "thingatpt"
     '(when (require 'thingatpt+)
        (tap-redefine-std-fns))

 That makes all Emacs code that uses the following standard
 functions use the their versions that are defined here, not the
 vanilla versions defined in `thingatpt.el'.

 `bounds-of-thing-at-point' - Better behavior.
                              Accept optional arg SYNTAX-TABLE.
 `form-at-point'            - Accept optional arg SYNTAX-TABLE.
 `list-at-point'            - Better behavior.
 `symbol-at-point'          - Use `emacs-lisp-mode-syntax-table'.
 `thing-at-point'           - Ensure it returns a string or nil.
                              Accept optional arg SYNTAX-TABLE.
 `thing-at-point-bounds-of-list-at-point'
                            - Better behavior.  Accept optional
                              args UP and UNQUOTEDP.


 Lisp Programmers
 ----------------

 If you write code that uses some of the functions defined here,
 this section is for you.

 You can use the functions defined in `thingatpt+.el' that have
 prefix `tap-' to obtain, for your code, the improvements they
 provide.  Doing only that has no effect on any code that calls
 vanilla thing-at-point functions (which have no prefix `tap-').

 For convenience you can invoke `tap-define-aliases-wo-prefix' to
 provide alias functions that have the same names but without the
 prefix `tap-'.  This affects only functions defined here that have
 no vanilla counterpart, so the aliases do not collide with any
 standard Emacs functions.  This is just a naming convenience.

 For example, you might do this:

   (when (require 'thingatpt+ nil t)  ; (no error if not found)
     (tap-define-aliases-wo-prefix))

 You can optionally enable the improvements defined here to have
 wider application, so that code that does not directly invoke the
 functions defined here nevertheless uses them indirectly.

 You can, for example, put `tap-' functions on THING-type symbols
 as property `thing-at-point' or property
 `bounds-of-thing-at-point'.  That has the effect of using those
 `tap-' functions for those THING types only.

 For example, to get the improvements for lists offered by
 `tap-list-at-point', you can do this:

   (put 'list 'bounds-of-thing-at-point
        'tap-bounds-of-list-at-point)
   (put 'list 'thing-at-point 'tap-list-at-point)

 That causes the vanilla thing-at-point functions to invoke those
 `tap-' functions when handling lists.  It has an effect only on
 lists, not on other THINGs.  This behavior happens because the
 generic vanilla functions `thing-at-point' and
 `bounds-of-thing-at-point' use those standard symbol properties.

 For even wider application, that is, if you want all of the
 improvements defined here to be available generally, then you will
 also need to do ONE of the following (#1 or #2):

 1. Call `tap-redefine-std-fns', to redefine standard functions.

 2. Do BOTH of these things:

   a. Call `tap-put-thing-at-point-props', to substitute `tap-'
      functions for standard functions as the values of symbol
      properties `thing-at-point' and `bounds-of-thing-at-point'.

   b. Call the individual `tap-*' functions explicitly for each of
      the standard functions that would be redefined by
      `tap-redefine-std-fns'.  Or call standard functions that make
      use of property `thing-at-point' or
      `bounds-of-thing-at-point'.

   This (#2) changes (improves) the behavior of things like
   (thing-at-point 'list), even though it does not redefine any
   standard functions.  Again, this is because functions
   `thing-at-point' and `bounds-of-thing-at-point' use symbol
   properties `thing-at-point' and `bounds-of-thing-at-point', and
   `tap-put-thing-at-point-props' changes those property values.
