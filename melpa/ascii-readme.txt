Introduction
------------

This package provides a way to display ASCII code on a window, that is,
display in another window an ASCII table highlighting the current character
code.

Well, maybe the name "ascii" is not a good name for this package, as this
package also displays non-ASCII code, that is, character code which is
greater than 255.  It also displays characters codified in HTML (&Aacute;),
quoted (=20), escaped (\xFF) and Emacs Lisp character (?\^A).

To use ascii, insert in your ~/.emacs:

   (require 'ascii)

Or:

   (autoload 'ascii-on        "ascii" "Turn on ASCII code display."   t)
   (autoload 'ascii-off       "ascii" "Turn off ASCII code display."  t)
   (autoload 'ascii-display   "ascii" "Toggle ASCII code display."    t)
   (autoload 'ascii-customize "ascii" "Customize ASCII code display." t)

For good performance, be sure to byte-compile ascii.el, e.g.

   M-x byte-compile-file <give the path to ascii.el when prompted>

This will generate ascii.elc, which will be loaded instead of ascii.el.

It runs on GNU Emacs 20.4.1, 21, 22 and 23.


Using ascii
-----------

To activate ascii, type:

   M-x ascii-on RET

Or:

   C-u 1 M-x ascii-display RET

To deactivate ascii, type:

   M-x ascii-off RET

Or:

   C-u 0 M-x ascii-display RET

To toggle ascii, type:

   M-x ascii-display RET

To customize ascii, type:

   M-x ascii-customize RET

You can also bind `ascii-display', `ascii-on', `ascii-off' and
`ascii-customize' to some key, like:

   (global-set-key "\C-c\C-a" 'ascii-on)
   (global-set-key "\C-c\C-e" 'ascii-off)
   (global-set-key "\C-c\C-t" 'ascii-display)
   (global-set-key "\C-c\C-c" 'ascii-customize)

If you're using `mule' package, a good usage example is to activate `ascii'
on emacs/etc/HELLO file.


Hooks
-----

ascii has the following hook variable:

`ascii-hook'
   It is evaluated once when ascii is turned on.


Options
-------

Below it's shown a brief description of ascii options, please, see the
options declaration in the code for a long documentation.

`ascii-code'				Specify list of character code to
					display.

`ascii-show-nonascii'		Non-nil means converts to unibyte and
					display the ascii code.

`ascii-show-nonascii-message'	Non-nil means show a message when
					character is above 255.

`ascii-window-size'			Specify initial ASCII window size.

`ascii-display-code'			Specify list of character range to be
					displayed.

`ascii-keep-window'			Non-nil means to keep ASCII window
					active.

`ascii-table-separator'		Specify string used to separate ASCII
					table columns.

`ascii-ascii-face'			Specify symbol face used to highlight
					ascii code.

`ascii-non-ascii-face'		Specify symbol face used to highlight
					non-ascii code.

To set the above options you may:

a) insert the code in your ~/.emacs, like:

	 (setq ascii-window-size 6)

   This way always keep your default settings when you enter a new Emacs
   session.

b) or use `set-variable' in your Emacs session, like:

	 M-x set-variable RET ascii-window-size RET 6 RET

   This way keep your settings only during the current Emacs session.

c) or use customization, for example:
	 click on menu-bar *Help* option,
	 then click on *Customize*,
	 then click on *Browse Customization Groups*,
	 expand *Data* group,
	 expand *Ascii* group
	 and then customize ascii options.
   Through this way, you may choose if the settings are kept or not when
   you leave out the current Emacs session.

d) or see the option value:

	 C-h v ascii-window-size RET

   and click the *customize* hypertext button.
   Through this way, you may choose if the settings are kept or not when
   you leave out the current Emacs session.

e) or invoke:

	 M-x ascii-customize RET

   and then customize ascii options.
   Through this way, you may choose if the settings are kept or not when
   you leave out the current Emacs session.


Acknowledgments
---------------

Thanks to Steven W. Orr <steveo@syslang.net> for patch to Emacs 23.

Thanks to Roman Belenov <roman@nstl.nnov.ru> for suggestion on dynamic ascii
table evaluation (depending on character encoding).

Thanks to Alex Schroeder <asc@bsiag.com> for suggestion on customization.
