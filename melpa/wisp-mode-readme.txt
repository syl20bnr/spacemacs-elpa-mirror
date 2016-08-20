To use, add wisp-mode.el to your emacs lisp path and add the following
to your ~/.emacs or ~/.emacs.d/init.el

(require 'wisp-mode)

For details on wisp, see
http://draketo.de/light/english/wisp-lisp-indentation-preprocessor

If you came here looking for wisp the lisp-to-javascript
compiler[1], have a look at wispjs-mode[2].

[1]: http://jeditoolkit.com/try-wisp

[2]: http://github.com/krisajenkins/wispjs-mode

ChangeLog:

 - 0.2.1: Disable electric-indent-local-mode in wisp-mode buffers.
 - 0.2: Fixed the regular expressions. Now org-mode HTML export works with wisp-code.
