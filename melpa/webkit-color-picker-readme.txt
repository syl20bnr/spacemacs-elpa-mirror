#+OPTIONS: toc:nil
* webkit-color-picker                                                :README:

Small experiment with embedded a Webkit widgets in a childframe. Requires Emacs 26 compiled with [[https://www.gnu.org/software/emacs/manual/html_node/emacs/Embedded-WebKit-Widgets.html][embedded Webkit Widget support]].

#+BEGIN_EXAMPLE
(require 'webkit-color-picker)
(global-set-key (kbd "C-c C-p") 'webkit-color-picker-show)
#+END_EXAMPLE

** Screenshot
[[./screenshots/webkit-color-picker.gif]]
