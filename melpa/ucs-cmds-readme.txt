 This library defines three commands:

 * `ucsc-define-char-insert-cmd' - Define a command to insert a
                                   Unicode character.

 * `ucsc-make-commands' - Define such commands for all Unicode
                          chars whose names match a regexp.

 * `ucsc-insert' - Insert a Unicode character and possibly define a
                   command to insert it.

 You can also use `ucsc-define-char-insert-cmd' or
 `ucsc-make-commands' in Emacs-Lisp code (such as in your init
 file) to define character-inserting commands.

 * The names of the character-inserting commands created are the
   same as the char names, except that they are lowercase and any
   `SPC' chars in the character name are replaced by hyphens (`-').

 * You can use a numeric prefix argument with a character-inserting
   command to insert multiple copies of the given character.

 The character-inserting commands are tailor-made to insert a given
 Unicode character.  You can bind such a command to a key sequence,
 effectively adding Unicode characters to your keyboard.

 Command `ucsc-insert' is a replacement for vanilla command
 `insert-char' (called `ucs-insert' prior to Emacs 24), which Emacs
 binds by default to `C-x 8 RET' and which lets you type input to
 complete against a Unicode character name and then inserts that
 character.

 The behavior and code of `ucsc-insert' are identical to those of
 `insert-char' (`ucs-insert') except for what happens when you use
 a negative prefix argument:

 1. It acts as if the prefix-arg value was positive.  So a value of
    -3 inserts three copies of the character, just as 3 does.

 2. In addition to inserting the character, it uses
    `ucsc-define-char-insert-cmd' to define a command that you can
    use thereafter to insert that character.

 Whenever `insert-char' does anything (it does nothing for a
 negative prefix arg), `ucsc-insert' does the same thing.  Because
 of this, I recommend that you bind `ucsc-insert' to `C-x 8 RET' as
 a replacement for `insert-char'.  Put this in your init file:

   (define-key global-map [remap insert-char] 'ucsc-insert)

 If you need only a few such commands for inserting particular
 Unicode characters, and you do not want to look up their code
 points, then using `ucsc-insert' or `ucsc-define-char-insert-cmd'
 to define them interactively is sufficiently convenient.  But
 these commands, like `insert-char', can be a bit slow if you use
 completion, because there are many, *MANY* completion candidates.

 You can use `ucsc-make-commands' to quickly create a whole set of
 such commands for characters whose names are similar.  The list of
 commands (symbols) is returned.

 You provide a regexp as the argument to `ucsc-make-commands'.  It
 is matched against all Unicode character names (in `ucs-names').
 An insertion command is created for each of the characters whose
 name matches.

 For example, here are some tests to try.  You need a Unicode font.
 One of these fonts might help:

  (set-frame-font "DejaVu Sans Mono-10")
  (set-frame-font "DejaVu Sans 10")
  (set-frame-font "Arial Unicode MS")

 Sample command creations:

  (ucsc-make-commands "^math") ; Math symbols
  (ucsc-make-commands "latin") ; Latin alphabet characters
  (ucsc-make-commands "arabic")
  (ucsc-make-commands "^cjk")  ; Chinese, Japanese, Korean characters
  (ucsc-make-commands "^box drawings ")
  (ucsc-make-commands "^greek [a-z]+ letter") ; Greek characters
  (ucsc-make-commands "\\(^hangul\\|^circled hangul\\|^parenthesized hangul\\)")



 Icicles Can Help
 ----------------

 Use of the commands created using `ucsc-define-char-insert-cmd',
 `ucsc-make-commands', and `ucsc-insert' is enhanced by `Icicles'
 (http://www.emacswiki.org/icicles.el).

 When you enter the command name or code point interactively, you
 can take advantage of the more powerful completion of `Icicles',
 including regexp, substring (a subset of regexp), and various
 kinds of fuzzy matching.

 More importantly, you can use progressive completion, to match
 parts of a candidate name in any order.  And you can "chip away at
 the non-elephant", removing whole sets of candidates that match
 patterns that you are *not* interested in.

 With `Icicles', commands, such as `ucsc-define-char-insert-cmd'
 and `ucsc-insert', which use function `read-char-by-name' have the
 additional advantage that the characters themselves are displayed
 next to their names in buffer `*Completions*'.  So you see
 immediately what the names represent: WYSIWYG.


 Commands defined here:

   `ucsc-define-char-insert-cmd', `ucsc-insert',
   `ucsc-make-commands'.
