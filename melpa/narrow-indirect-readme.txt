 Narrow using an indirect buffer that is a clone of the current
 buffer (which becomes the base buffer for the clone).

 Such an indirect buffer gives you a different view of a portion of
 the buffer, or even of the whole buffer (use `C-x h C-x 4 n n').
 It always has the same text and text properties, but otherwise it
 is pretty independent.

 In particular, you can kill an indirect buffer without affecting
 its base buffer.  You will likely want to kill indirect narrowed
 buffers rather than widening them.

 You can use indirect buffers for more than you might think.  You
 can use clones taken from portions of Dired buffers, for example,
 to give you useful (active) views into a directory listing.  There
 are only a few keys/commands (such as `g' to update the listing)
 that do not work, because they depend on a view of the whole Dired
 buffer.  Experiment, and you will no doubt find interesting new
 uses for indirect buffers.

 Note: Because an indirect clone shares text properties with its
 base buffer, if you give it a different major mode that uses
 different font-locking then the font-locking of the base buffer
 changes the same way.  However, you can restore the font-locking
 appropriate to the base buffer, by just toggling `font-lock-mode'
 off and on again there.

 See the Emacs manual, node `Indirect Buffers'.

 It is helpful to be able to easily distinguish indirect buffers
 from non-indirect buffers.  This library offers two ways to do
 this, for the indirect buffers it creates:

 * The buffer name of an indirect narrowed buffer starts with a
   prefix that you can set using option `ni-buf-name-prefix'.  The
   default value is `I-'.

 * The name of an indirect narrowed buffer is highlighted in the
   mode line using face `ni-mode-line-buffer-id' instead of face
   `mode-line-buffer-id'.  To turn this off, just customize the
   former to be the same as the latter.

 By default, the name of an indirect narrowed buffer reflects the
 name of its base buffer and the text of the narrowed region (or
 the name of the defined object, in the case of
 `ni-narrow-to-defun-indirect-other-window').  But you can control
 this in several ways.  See the command doc strings and user
 options `ni-buf-name-prefix', `ni-narrowed-buf-name-max', and
 `ni-buf-name-separator'.

 If you use Emacs 24.4 or later then invisible buffer text is
 filtered out from the name of the indirect buffer.  For example,
 if you invoke `ni-narrow-to-region-indirect-other-window' with an
 active region in a Dired buffer that is hiding details, then the
 (invisible) details will not be included in the indirect-buffer
 name.

 To customize the behavior of this library, do this:

   M-x customize-group Narrow-Indirect

 Suggested key bindings:

  (define-key ctl-x-4-map "nd" 'ni-narrow-to-defun-indirect-other-window)
  (define-key ctl-x-4-map "nn" 'ni-narrow-to-region-indirect-other-window)
  (define-key ctl-x-4-map "np" 'ni-narrow-to-page-indirect-other-window)


 User options defined here:

   `ni-buf-name-prefix', `ni-narrowed-buf-name-max',
   `ni-buf-name-separator'.

 Faces defined here:

   `ni-mode-line-buffer-id'.

 Commands defined here:

   `ni-narrow-to-defun-indirect-other-window',
   `ni-narrow-to-page-indirect-other-window',
   `ni-narrow-to-region-indirect-other-window'.

 Non-interactive functions defined here:

   `ni-buffer-substring-collapsed-visible'.

 Acknowledgments:

  The idea and original code for a command that combines narrowing
  with cloning a buffer as an indirect-buffer is due to Zane Ashby:
  http://demonastery.org/2013/04/emacs-narrow-to-region-indirect/.

  In Emacs bug thread #17401, Phil Sainty proposed adding three
  commands to Emacs based on this approach.  Lennart Borgman
  contributed code that uses, in the cloned buffer name, some text
  based on the narrowed region.

  The code in `narrow-indirect.el' extends this a bit and provides
  a couple of user options and some alternative (prefix-argument)
  behavior.  It is doubtful that Emacs Dev will ever adopt features
  such as those defined here, and if they do then this library can
  at least help for Emacs versions prior to their addition.
