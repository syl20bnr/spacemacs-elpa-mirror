Minor mode to highlight the line the cursor is in. You can change colors
of foreground (text) and background. The default behaviour is to set
only a background color, so that font-lock fontification colors remain
visible (syntax coloring). Enable a buffer using the command
`highlight-current-line-minor-mode' and customize via:

  M-x customize-group highlight-current-line <RET>.

You can select whether the whole line (from left to right window border)
is marked or only the really filled parts of the line (from left window
border to the last char in the line). The second behaviour is suitable
if it's important for you to see trailing spaces or tabs in a
line. Customize the variable `highlight-current-line-whole-line' (or use
the function `highlight-current-line-whole-line-on' retained for
compatibility with prior versions).

You may enable the minor-mode automatically for (almost) all buffers by
customizing the variable `highlight-current-line-globally' (or using the
compatibility command `highlight-current-line-on').  Buffers whose
buffer-name match the regular expression in the customizable variable
`highlight-current-line-ignore-regexp' do not highlighted.  You can
extend or redefine this regexp. This works together with the default
ignore function `highlight-current-line-ignore-function'. You can
redefine this function to implement your own criterias.

(The functions `highlight-current-line-on',
`highlight-current-line-set-fg-color' and
`highlight-current-line-set-bg-color' are retained for backward
compatibility. There's a special color "none" defined to set no color.)


People which made contributions or suggestions:

This list is ordered by time. Latest in time first.
- Peter S Galbraith   <psg@debian.org>
- Masatake Yamato     <jet@gyve.org>
- Hrvoje Niksic	 <hniksic@srce.hr>
- Jari Aalto		 <jari.aalto@ntc.nokia.com>
- Shawn Ostermann     <sdo@picard.cs.OhioU.Edu>
- Peter Ikier	 <p_ikier@infoac.rmi.de>
  Many thanks to him for the idea. He liked this behaviour in another
  editor ("Q").

Installation:

Put a copy of highlight-current-line.el/.elc into some path of
`load-path'. To show `load-path': <C-h v> load-path RET

Load the file, e.g. add in ~/.emacs

 (require 'highlight-current-line)

Enable it on a buffer using `M-x highlight-current-line-minor-mode'
or globally by customizing `highlight-current-line-globally'.

Previous versions of this code worked by adding other comamnds in
~/.emacs instead of using the custom interface.  This is still
supported:

 ;; If you want to mark only to the end of line:
 (highlight-current-line-whole-line-on nil)
 ;; switch highlighting on
 (highlight-current-line-on t)
 ;; Ignore no buffer
 (setq highlight-current-line-ignore-regexp nil) ; or set to ""
 ;; alternate way to ignore no buffers
 (fmakunbound 'highlight-current-line-ignore-function)
 ;; Ignore more buffers
 (setq highlight-current-line-ignore-regexp
      (concat "Dilberts-Buffer\\|"
	       highlight-current-line-ignore-regexp))

Troubleshooting:

- Q: I do not see matching parens from paren.el any more!
- A: Check the colors from highlight-current-line or from show-paren-face
  and choose some combination which works together.

