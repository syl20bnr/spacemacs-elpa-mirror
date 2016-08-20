This is a generalization of an idea by Mickey Petersen of
masteringemacs fame: Use one keystroke to jump from a code buffer
to the corresponding repl buffer and back again.  This works even if
you do other stuff in between, as the last buffer used to jump to a
repl is stored in a buffer local variable in the repl buffer.

Currently this assumes that the stored command to start the repl
will switch to an already open repl buffer if it exists.

There are no repl/mode combinations preconfigured, put something
like the following in your Emacs setup for php and elisp repl:

    (require 'repl-toggle)
    (setq rtog/mode-repl-alist '((php-mode . php-boris) (emacs-lisp-mode . ielm)))

This defines a global minor mode, indicated at with 'rt' in the modeline, that
grabs "C-c C-z" as repl toggling keybinding.

I don't know with wich repl modes this actualy works.  If you use
this mode, please tell me your rtog/mode-repl-alist, so that I can
update the documentation.

Known to work:

- ~(php-mode . php-boris)~
- ~(emacs-lisp-mode . ielm)~
- ~(elixir-mode . elixir-mode-iex)~
- ~(ruby-mode . inf-ruby)~

If you supply the universal prefix argument you can

- C-u pass the current line
- C-u C-u pass the current defun
- C-u C-u C-u pass the the whole current buffer

to the repl buffer you switch to.

If you set rtog/fullscreen to true, prior to loading this module,
the repl-commands will be executed fullscreen, i.e. as single
frame, restoring the window-layout on stwitching back to the
buffer.

Emacs -- of course -- has more than one function to switch
between buffers. You can customize ~rtog/goto-buffer-fun~ to
accommodate your needs. The default is ~switch-to-buffer~; to
move focus to another frame that already shows the other buffer,
instead of switching the current frame to it, use
~pop-to-buffer~.

~(setq rtog/goto-buffer-fun 'pop-to-buffer)~

If the mode you want to use doesn't jump to an existing
repl-buffer, but always starts a new one, you can use
`rtog/switch-to-shell-buffer' in your configuration to get that
behaviour, e.g. for `octave-mode':

(rtog/add-repl 'octave-mode (rtog/switch-to-shell-buffer 'inferior-octave-buffer 'inferior-octave))
