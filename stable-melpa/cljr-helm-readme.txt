A Helm wrapper for the cljr features.

Remembering key bindings for cljr is hard, especially the less
frequently used ones - this should help with that.

This package is available in melpa as `cljr-helm`. So doing `M-x
install-package RET cljr-helm` should get you what you need.

Then add `(require 'cljr-helm)` to your init file, bind `cljr-helm` to
a key (I'd suggest `C-c C-r`) in Clojure mode, and you're ready to go.
