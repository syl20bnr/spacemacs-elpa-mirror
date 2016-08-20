 You can use the commands defined here to select or move to
 different kinds of text entities ("things") that are at or near
 point.  They are especially useful in combination with Transient
 Mark mode.


 Commands defined here:

   `cycle-thing-region', `mark-enclosing-list',
   `mark-enclosing-list-backward', `mark-enclosing-list-forward',
   `mark-thing', `next-visible-thing', `next-visible-thing-repeat',
   `previous-visible-thing', `previous-visible-thing-repeat',
   `select-thing-near-point', `thgcmd-bind-keys', `thing-region'.

 User options defined here:

   `thing-types'.

 Non-interactive functions defined here:

   `thgcmd-bounds-of-thing-at-point', `thgcmd-invisible-p',
   `thgcmd-next-visible-thing-1', `thgcmd-next-visible-thing-2',
   `thgcmd-repeat-command', `thgcmd-things-alist'.

 Internal variables defined here:

   `thgcmd-defined-thing-p', `thgcmd-last-thing-type',
   `thgcmd-thing-region-index', `thgcmd-thing-region-point'.

 Put this in your init file (`~/.emacs'):

  (require 'thing-cmds)
  (thgcmd-bind-keys) ; Only if you want the key bindings it defines

 See also the doc strings of `next-visible-thing' and
 `thgcmd-bind-keys', for more information about thing navigation
 keys.
