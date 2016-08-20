This module copies selected regions in org-mode as formatted text on the
clipboard that can be pasted into other applications. When not in org-mode,
the htmlize library is used instead.

For Windows the html-clip-w32.py script will be installed. It works pretty
well, but I noticed that the hyperlinks in the TOC to headings don't work,
and strike-through doesn't seem to work. I have no idea how to fix either
issue.

Mac OSX needs textutils and pbcopy, which should be part of the base install.

Linux needs a relatively modern xclip. https://github.com/astrand/xclip

There is one command: `ox-clip-formatted-copy' that should work across
Windows, Mac and Linux.

(require 'htmlize)
