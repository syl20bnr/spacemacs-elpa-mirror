Introduction
------------

This package is a minor mode to visualize blanks (TAB, (HARD) SPACE
and NEWLINE).

blank-mode uses two ways to visualize blanks: faces and display
table.

* Faces are used to highlight the background with a color.
  blank-mode uses font-lock to highlight blank characters.

* Display table changes the way a character is displayed, that is,
  it provides a visual mark for characters, for example, at the end
  of line (?\xB6), at SPACEs (?\xB7) and at TABs (?\xBB).

The `blank-style' and `blank-chars' variables are used to select
which way should be used to visualize blanks.

Note that when blank-mode is turned on, blank-mode saves the
font-lock state, that is, if font-lock is on or off.  And
blank-mode restores the font-lock state when it is turned off.  So,
if blank-mode is turned on and font-lock is off, blank-mode also
turns on the font-lock to highlight blanks, but the font-lock will
be turned off when blank-mode is turned off.  Thus, turn on
font-lock before blank-mode is on, if you want that font-lock
continues on after blank-mode is turned off.

When blank-mode is on, it takes care of highlighting some special
characters over the default mechanism of `nobreak-char-display'
(which see) and `show-trailing-whitespace' (which see).

There are two ways of using blank-mode: local and global.

* Local blank-mode affects only the current buffer.

* Global blank-mode affects all current and future buffers.  That
  is, if you turn on global blank-mode and then create a new
  buffer, the new buffer will also have blank-mode on.  The
  `blank-global-modes' variable controls which major-mode will be
  automagically turned on.

You can mix the local and global usage without any conflict.  But
local blank-mode has priority over global blank-mode.  Blank mode
is active in a buffer if you have enabled it in that buffer or if
you have enabled it globally.

When global and local blank-mode are on:

* if local blank-mode is turned off, blank-mode is turned off for
  the current buffer only.

* if global blank-mode is turned off, blank-mode continues on only
  in the buffers in which local blank-mode is on.

To use blank-mode, insert in your ~/.emacs:

   (require 'blank-mode)

Or autoload at least one of the commands`blank-mode',
`blank-toggle-options', `global-blank-mode' or
`global-blank-toggle-options'.  For example:

   (autoload 'blank-mode                  "blank-mode"
     "Toggle blank visualization."          t)
   (autoload 'blank-toggle-options        "blank-mode"
     "Toggle local `blank-mode' options."   t)

blank-mode was inspired by:

   whitespace.el            Rajesh Vaidheeswarran <rv@gnu.org>
	Warn about and clean bogus whitespaces in the file
	(inspired the idea to warn and clean some blanks)

   show-whitespace-mode.el  Aurelien Tisne <aurelien.tisne@free.fr>
      Simple mode to highlight whitespaces
      (inspired the idea to use font-lock)

   whitespace-mode.el       Lawrence Mitchell <wence@gmx.li>
      Major mode for editing Whitespace
      (inspired the idea to use display table)

   visws.el                 Miles Bader <miles@gnu.org>
      Make whitespace visible
      (handle display table, his code was modified, but the main
      idea was kept)


Using blank-mode
----------------

There is no problem if you mix local and global minor mode usage.

* LOCAL blank-mode:
   + To toggle blank-mode options locally, type:

        M-x blank-toggle-options RET

   + To activate blank-mode locally, type:

        C-u 1 M-x blank-mode RET

   + To deactivate blank-mode locally, type:

        C-u 0 M-x blank-mode RET

   + To toggle blank-mode locally, type:

        M-x blank-mode RET

* GLOBAL blank-mode:
   + To toggle blank-mode options globally, type:

        M-x global-blank-toggle-options RET

   + To activate blank-mode globally, type:

        C-u 1 M-x global-blank-mode RET

   + To deactivate blank-mode globally, type:

        C-u 0 M-x global-blank-mode RET

   + To toggle blank-mode globally, type:

        M-x global-blank-mode RET

There are also the following useful commands:

`blank-cleanup'
   Cleanup some blank problems in all buffer or at region.

`blank-cleanup-region'
   Cleanup some blank problems at region.

The problems, which are cleaned up, are:

1. empty lines at beginning of buffer.
2. empty lines at end of buffer.
   If `blank-chars' has `empty' as an element, remove all empty
   lines at beginning and/or end of buffer.

3. 8 or more SPACEs at beginning of line.
   If `blank-chars' has `indentation' as an element, replace 8 or
   more SPACEs at beginning of line by TABs.

4. SPACEs before TAB.
   If `blank-chars' has `space-before-tab' as an element, replace
   SPACEs by TABs.

5. SPACEs or TABs at end of line.
   If `blank-chars' has `trailing' as an element, remove all
   SPACEs or TABs at end of line."

6. 8 or more SPACEs after TAB.
   If `blank-chars' has `space-after-tab' as an element, replace
   SPACEs by TABs.


Hooks
-----

blank-mode has the following hook variables:

`blank-mode-hook'
   It is evaluated always when blank-mode is turned on locally.

`global-blank-mode-hook'
   It is evaluated always when blank-mode is turned on globally.

`blank-load-hook'
   It is evaluated after blank-mode package is loaded.


Options
-------

Below it's shown a brief description of blank-mode options, please,
see the options declaration in the code for a long documentation.

`blank-style'		Specify the visualization style.

`blank-chars'		Specify which kind of blank is
				visualized.

`blank-space'		Face used to visualize SPACE.

`blank-hspace'		Face used to visualize HARD SPACE.

`blank-tab'			Face used to visualize TAB.

`blank-newline'		Face used to visualize NEWLINE char
				mapping.

`blank-trailing'		Face used to visualize trailing
				blanks.

`blank-line'			Face used to visualize "long" lines.

`blank-space-before-tab'	Face used to visualize SPACEs before
				TAB.

`blank-indentation'		Face used to visualize 8 or more
				SPACEs at beginning of line.

`blank-empty'		Face used to visualize empty lines at
				beginning and/or end of buffer.

`blank-space-after-tab'	Face used to visualize 8 or more
				SPACEs after TAB.

`blank-space-regexp'		Specify SPACE characters regexp.

`blank-hspace-regexp'	Specify HARD SPACE characters regexp.

`blank-tab-regexp'		Specify TAB characters regexp.

`blank-trailing-regexp'	Specify trailing characters regexp.

`blank-space-before-tab-regexp'	Specify SPACEs before TAB
					regexp.

`blank-indentation-regexp'	Specify regexp for 8 or more SPACEs at
				beginning of line.

`blank-empty-at-bob-regexp'	Specify regexp for empty lines at
				beginning of buffer.

`blank-empty-at-eob-regexp'	Specify regexp for empty lines at end
				of buffer.

`blank-space-after-tab-regexp'	Specify regexp for 8 or more
					SPACEs after TAB.

`blank-line-column'		Specify column beyond which the line
				is highlighted.

`blank-display-mappings'	Specify an alist of mappings for
				displaying characters.

`blank-global-modes'		Modes for which global `blank-mode' is
				automagically turned on.


Acknowledgements
----------------

Thanks to nschum (EmacsWiki) for the idea about highlight "long"
lines tail.  See EightyColumnRule (EmacsWiki).

Thanks to Juri Linkov <juri@jurta.org> for suggesting:
   * `define-minor-mode'.
   * `global-blank-*' name for global commands.

Thanks to Robert J. Chassell <bob@gnu.org> for doc fix and testing.

Thanks to Drew Adams <drew.adams@oracle.com> for toggle commands
suggestion.

Thanks to Antti Kaihola <antti.kaihola@linux-aktivaattori.org> for
helping to fix `find-file-hooks' reference.

Thanks to Andreas Roehler <andreas.roehler@easy-emacs.de> for
indicating defface byte-compilation warnings.

Thanks to TimOCallaghan (EmacsWiki) for the idea about highlight
"long" lines.  See EightyColumnRule (EmacsWiki).

Thanks to Yanghui Bian <yanghuibian@gmail.com> for indicating a new
newline character mapping.

Thanks to Pete Forman <pete.forman@westgeo.com> for indicating
whitespace-mode on XEmacs.

Thanks to Miles Bader <miles@gnu.org> for handling display table via
visws.el (his code was modified, but the main idea was kept).

Thanks to:
   Rajesh Vaidheeswarran <rv@gnu.org>	whitespace.el
   Aurelien Tisne <aurelien.tisne@free.fr>	show-whitespace-mode.el
   Lawrence Mitchell <wence@gmx.li>		whitespace-mode.el
   Miles Bader <miles@gnu.org>		visws.el
And to all people who contributed with them.
