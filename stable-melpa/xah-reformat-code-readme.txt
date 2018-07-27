xah-reformat-code contains commands to reformat current paragraph into 1 long line or multiple short lines.

It is like emacs `fill-region', but designed for programing language source code.

it contain commands to strictly exchange whitespaces by newline, or vice versa. No adding other char or removing other char.

this is suitable for languages that strictly consider whitespaces equivalent except in string or comment. For example, XML, HTML, CSS, lisp, Wolfram Language.

2016-12-16 todo:
• auto skip strings and comments.
• cut lines at the proper logical locations, not just around 70 char.
• do proper indentation when changing to multi-line.
• cater to different languages. e.g. lisp, JavaScript, ruby, php, Java, etc.
• add commands to work on buffer, file, or all files in a directory.

--------------------------------------------------
MANUAL INSTALL

put the file xah-reformat-code.el in ~/.emacs.d/lisp/
create the dir if doesn't exist.

put the following in your emacs init file:

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'xah-reformat-code)

--------------------------------------------------
HOW TO USE

M-x xah-reformat-lines

don't use this in Python code!

If you like this project, Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. Thanks.

• Whitespace here is defined to be space, tab, and newline character.
• Repeated whitespace is considered equivalent to just 1 space or just 1 newline char.
• Whitespace is never created if it didn't exist before.
• Whitespace is never removed.
• No character other than whitespace are removed or inserted.

in the future, this package will expand to complete reformat. That is, given a singe very loooooooong line of code, it will reformat it into multiple lines in a pretty way, with proper place to insert newline, proper indentation, and skipping comment line or string.
