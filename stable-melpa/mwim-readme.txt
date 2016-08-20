MWIM stands for "Move Where I Mean".  This package is inspired by
<http://www.emacswiki.org/emacs/BackToIndentationOrBeginning>.  It
provides commands for moving to the beginning/end of code or line.

To install the package manually, add the following to your init file:

  (add-to-list 'load-path "/path/to/mwim-dir")
  (autoload 'mwim-beginning-of-code-or-line "mwim" nil t)
  (autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
  (autoload 'mwim-end-of-code-or-line "mwim" nil t)
  (autoload 'mwim-end-of-line-or-code "mwim" nil t)

Then you can bind some keys to some of those commands and start
moving.
