Major mode for editing emacs lisp.
This is alternative to GNU Emacs emacs-lisp-mode.

Major features different from emacs-lisp-mode:

• Syntax coloring of 99% statistically most frequently used elisp functions.
• Completion for function names with `ido-mode' interface. (press TAB after word)
• Function param template. (press space after function name.)
• 1 to 4 letters abbrevs for top 50 most used functions. e.g. “bsnp” → “buffer-substring-no-properties”
• Convenient formatting command that formats entire sexp expression unit. (press TAB before word.)

Call `xah-elisp-mode' to activate the mode.
Files ending in “.el” will also open in `xah-elisp-mode'.

Single letter abbrevs are:
d → defun
i → insert
l → let
m → message
p → point
s → setq

Call `list-abbrevs' to see the full list.

put this in your init to turn on abbrev
(abbrev-mode 1)

home page: http://ergoemacs.org/emacs/xah-elisp-mode.html

This mode is designed to be very different from the usual paredit/smartparens approach.
The focus of this mode is to eliminate any concept of {manual formatting, format “style”, “indentation”, “line of code”} from programer. Instead, pretty-print/rendering should be automated, as part of display system. ({Matematica, XML, HTML} are examples.)
The goal of this mode is for it to become 100% semantic lisp code editor, such that it is impossible to create mis-formed elisp expressions, yet being practical.

If you like the idea, please help fund the project. Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. See home page. Thanks.

2016-12-02 compatible with company-mode

equires emacs 24.3 because of using setq-local
