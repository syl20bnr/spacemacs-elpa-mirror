This package adds Clojure syntax checker (via Joker) to flycheck.  To use it, add
to your init.el:

(require 'flycheck-joker)

Make sure Joker binary is on your path.
Joker installation instructions are here: https://github.com/candid82/joker#installation

Please read about Joker's linter mode to understand its capabilities and limitations:
https://github.com/candid82/joker#linter-mode
Specifically, it's important to configure Joker to reduce false positives:
https://github.com/candid82/joker#reducing-false-positives

Please see examples of the errors Joker can catch here:
https://github.com/candid82/SublimeLinter-contrib-joker#examples
