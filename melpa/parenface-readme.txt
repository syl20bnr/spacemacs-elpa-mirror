Add a paren-face to emacs and add support for it to the various lisp modes.

Based on some code that Boris Schaefer <boris@uncommon-sense.net> posted
to comp.lang.scheme in message <87hf8g9nw5.fsf@qiwi.uncommon-sense.net>.

Log:
Modifications by Grant Rettke <grettke@acm.org>, November 2012 only to add support for editor and REPL modes for:
Clojure (and nREPL), IELM, Jess and inferior-jess.
2014-01-01 add support for [] {} by Zhao Wei <kaihaosw@gmail.com>

Usage:
(eval-after-load 'parenface
  (progn
    (set-face-foreground 'parenface-paren-face "SteelBlue4")
    (set-face-foreground 'parenface-bracket-face "SteelBlue4")
    (set-face-foreground 'parenface-curly-face "SteelBlue4")))
