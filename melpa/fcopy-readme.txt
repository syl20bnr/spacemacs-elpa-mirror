Funny Copy (Fcopy) is a minor mode to copy text; first, set the
paste point, and next look for the text to copy.  The past point is
the point where fcopy-mode start.  One stroke commands are prepared
to search and copy the text.  Copy commands automatically take the
cursor back to the past point, insert the text, and exit
fcopy-mode.

`q' and `C-g' is for exit fcopy-mode.  `?' for more help.

If you want to move (cut) text, not to copy, type `C-d' to toggle
on delete flag.  If the buffer is originally read only, deleting
and copying will fail.

If some scratches are in your copy text, you can modify it before
paste.  Before the copy command, type `m' to toggle on modify flag.
It prepares modify buffer, that you can modify the text with
replacement or overwrite.  See `fmodify-default-mode' or `C-h m' in
the modify buffer.  To insert modified text, type `C-cC-c'.
`C-cC-q' for quit modifying, and paste nothing.

forward   backward   Unit of Moving
--------  --------   --------
 C-f       C-b       character
  f         b        word
  a         e        line (beginning or end)
  n         p        line (next or previous)
  A         E        sentence (beginning or end)
  N         P        paragraph
  v         V        scroll (up or down)
[space]  [backspace] scroll (up or down)
  s         r        incremental search
  S         R        incremental search with regexp
  <         >        buffer (beginning or end)


Jump Commands
--------
 g  Go to line
 j  Jump to register
 o  To other window
 x  Exchange point and mark
 ,  Pop mark ring


Copy Commands
--------
 .        Set mark
 c        Copy character
 C        Copy block
 w        Copy word
 W        Copy word (before copy, back to the beginning of word).
 k        Copy line behind point like kill-line
[return]  Copy region (if there is), or kill whole line
 (        Copy text between pair (...) chars.
 )        Likewise, but not copy pair chars.
C-c (     Copy text between parens.
C-c )     Likewise, but not copy parens.

 m        Toggle modify flag.
C-d       Toggle delete flag.

The latest fcopy.el is available at:

  https://github.com/ataka/fcopy


The other simple copy command is distributed with Emacs.  See
misc.el in your lisp directory.  It copies characters from previous
non-blank line, starting just above point.  But remember, fcopy has
no influences from misc.el.

How to install:

To install, put this in your .emacs file:

 (autoload 'fcopy "fcopy" "Copy lines or region without editing." t)

And bind it to any key you like:

 (define-key mode-specific-map "k" 'fcopy)  ; C-c k for fcopy


Version and ChangeLog:

(defconst fcopy-version "7.0"
  "Version numbers of this version of Funny Copy.")

ver 7.0  2015/02/19
* New command fcopy; Do not call fcopy-mode directly.
* Drop fmodify-mode, see sugaryank.
* Available in melpa.

ver 6.0
* Hosted at github.
* Support utf-8 (unsupport EUC-JP).

ver 5.2.6  (Rev. 2.43)
* fcopy skips black lines in line moving commands.
  If mark is active, they does not skip blank lines.
* New function fmodify-query-replace-regexp is binded to [M-RET].
* Fixed problem for negative and zero argument in line moving commands.
* Fixed bug of misscounting paren when copying text between parens.

ver 5.2.5  (Rev. 2.35)  2003/07/09 01:45:26
* fcopy now go back to the correct paste point when deleting before
  fcopy-point.

ver 5.2.4  (Rev. 2.32)  2003/06/16 10:45:47
* Fix for XEmacs.
* Find proper paren pair even the point is in the multiple parens.

ver 5.2.3  (Rev. 2.26)  2002/12/02 14:20:41
* Support move mode; delete text and copy it.
* [rutern] is to replace command, in fmodify edit mode.

ver 5.2.2  (Rev. 2.20)  2002/11/17 21:56:09
* Fix problem that function regexp-opt in Emacs 20 caused error
  when strings contains Japanese character.

ver 5.2.1  (Rev. 2.16)  2002/11/12 01:27:27
* Change function of [space] to scroll-up.
  Use `.' to set mark.
* Change URL of Funny Copy support page.
* `(' now copy text between paired-strings like (...).
  `)' likewise, but does not copy the paired-strings.
* `;' copy comment text depending on major mode.

ver 5.2  2002/11/10 10:40:59  Masayuki Ataka
Minor version up of Funny Copy.  Merge fmodify.el.
Usage is in commentary section.

ver 5.1  2002/10/13 06:24:19  Masayuki Ataka
Minor version up of Funny Copy.  Use minor mode instead of major
mode.

ver 5.0  2002/09/06 05:54:06  Masayuki Ataka
The Fifth version of Funny Copy.  Remove tedious codes, like
`funny-copy-show-kill-ring', support of custom package, and a lot
of user options.  Make it sure to work with Emacs 19.

ver 4.0  2000/02/22 22:41:02  Masayuki Ataka
The Fourth version of Funny Copy.  Support Funny Modify mode.
New function name was `funny-copy-mode', binded to `C-c k'

ver 3.0  1999/04/24 00:17:33  Masayuki Ataka
Third version of Funny Copy.  Rewrite code from full scratch,
implemented with major mode.  New function name was
`MA-Special-copy-mode', means Masayuki Ataka special copy mode.
Also support seeing the inside of kill-ring.

ver 2.0  1998/09/??  Masayuki Ataka
Second version of Funny Copy.  Code name was `lupin'.  Copy text
interactively using cond.

ver 1.0  1998/08/??  Masayuki Ataka
First version of Funny Copy.  Just copy above line.  Command name
is `kill-above', and binded to `C-S-k'
