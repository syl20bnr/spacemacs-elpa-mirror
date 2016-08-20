 Lets you use key-sequence descriptions that do not bother with
 angle brackets: `<' and `>'.

 Prior to Emacs 21, vanilla GNU Emacs did not bother with angle
 brackets either, but someone around the turn of the century got
 the idea that Emacs could not do without them.  So instead of the
 `delete' key we now have the `<delete>' key.  And instead of`C-x
 M-delete' we now have `C-x M-<delete>'.  "On n'arrete pas le
 progres !"

 Angle brackets are not needed - why?  Because we already use
 spaces to separate keys in a key-sequence description (we use
 `SPC' to indicate the SPACE key).

 To be fair, it is true that sometimes people have taken the
 shortcut when writing about user input of writing, e.g., `M-x
 forward-char RET' instead of writing `M-x f o r w a r d - c h a
 r'. And if you write `forward' that way to stand for an input
 sequence of seven chars, then you cannot also expect `forward' to
 stand for a function key named `forward', can you?

 Well, yes you can, if the context makes things clear enough.  And
 I for one find `C-M-<insert>' butt-ugly (likewise `<C-M-insert>')
 - and, more importantly, insulting to Occam and his razor.

 So go ahead and go NaKeD - shed your angles.

 Here's what you need.  It won't completely purge Emacs from
 insulting you with the occasional pair of angle brackets, but it
 at least lets you DTRT in code that you write:

 * Use function `naked-key-description' instead of
   `key-description'.

   The former outputs naked key descriptions: no angle brackets
   around function keys.  E.g., if KEY is the sequence of events
   produced by holding the Shift key while hitting the Insert key,
   then `(naked-key-description KEY)' returns "S-insert" (and not
   "S-<insert>").  (Internally, this sequence of events is the
   vector [S-insert].)

 * Use function `naked' instead of `kbd' (which is a function
   starting with Emacs 24.3 and a macro before then).

   The former allows its argument key-sequence description to use
   naked keys, not clothed in angle brackets.  E.g., (naked
   "C-M-delete") has the same effect as (kbd "C-M-<delete>").

 * Use command `naked-read-kbd-macro' instead of `read-kbd-macro'.

 * Use function `naked-edmacro-parse-keys' instead of
   `edmacro-parse-keys' (if you happen to use that lower-level
   function).

 But you can also have it both ways if or when you might need to:
 All of these accept an optional argument ANGLES which, if
 non-`nil', returns the behavior to the vanilla one, expecting
 function keys to be fully clothed in angle brackets.  E.g.: (naked
 "C-M-<delete>" t).

 In addition, even without an non-nil ANGLES argument, function
 `naked' does the right thing for keys expressed using angle
 brackets.  IOW, (naked "M-<foobar>") returns the same thing that
 (naked "M-foobar") does: [M-foobar].

 Enjoy!
