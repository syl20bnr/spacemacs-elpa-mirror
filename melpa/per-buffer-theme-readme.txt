`per-buffer-theme.el' is an Emacs library that automatically changes
the global theme according to buffer name or major mode.

If buffer name matches any of `per-buffer-theme/ignored-buffernames-regex'
no theme change occurs.

Customizable variable `per-buffer-theme/themes-alist' contains the
association between themes and buffer name or major modes.

Special `notheme' theme name can be used to make unload all themes and use
Emacs default theme.

If no theme matches then it will load the theme stored in
`per-buffer-theme/default-theme' variable.

There are two different methods in which buffer and theme can be checked.
It is controlled by customizable boolean `per-buffer-theme/use-timer':

- 't' will use a timer, triggered every `per-buffer-theme/timer-idle-delay'
  seconds.  This is the default as it works smoothly.
  If it slows down Emacs a bit choose a bigger delay value.

- 'nil' uses a function advice to `select-window' so it could introduce
  some Emacs windows flickering when switching buffers due to how
  `select-window' internally works.
