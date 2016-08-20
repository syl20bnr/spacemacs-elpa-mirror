   Frame configuration that uses one frame per window.

 This library is part of One-on-One Emacs, a collection of
 libraries that try to make Emacs more frame-oriented and less
 window-oriented.

 This library sets up Emacs to use multiple frames: individual
 frames are used, by default, instead of Emacs windows.  That is,
 the default is to use a frame for each Emacs window: one window on
 one frame.

 You can configure each of the frames defined here.

 Default properties are defined here for normal frames and
 "special" frames, which show "special-display buffers" (see Emacs
 manual for info on such frames).

 In addition, these user options control the creation of three
 separate, specialized frames:

   - `1on1-*Help*-frame-flag' - `*Help*' buffer frame
   - `1on1-*Completions*-frame-flag' - `*Completions*' buffer frame
   - `1on1-minibuffer-frame-flag' - minibuffer frame

 Buffers `*Help*' and `*Completions*' are always displayed in their
 own frames.  In addition, if `1on1-*Help*-frame-flag' or
 `1on1-*Completions*-frame-flag' is non-nil, then the `*Help*' or
 `*Completions*' frame has a special (customizable) appearance.

 If `1on1-minibuffer-frame-flag' is non-nil (the default value),
 then the minibuffer is shown in its own frame,
 `1on1-minibuffer-frame'; this is the only frame to have a
 minibuffer.  If you customize `1on1-minibuffer-frame-flag' to nil,
 then each frame will have its own minibuffer, as usual, and there
 will be no standalone minibuffer frame.

 By default, if you use a standalone minibuffer frame, it is
 automatically sized to the full width of your display and placed
 at the bottom of the display.

 If you use a standalone minibuffer frame then option
 `minibuffer-auto-raise' can make a difference.  This library does
 not change the option value.  A value of nil can make sense for
 some window managers that force the refocusing of a frame whenever
 it is raised.  If you use MS Windows then this is not a problem:
 command `1on1-emacs' sets the Windows-specific option
 `w32-grab-focus-on-raise' to nil, so that frame raising and
 focusing are decoupled.  So on MS Windows, at least, a non-nil
 value for `minibuffer-auto-raise' can make sense.

 If `1on1-fit-minibuffer-frame-flag' is non-nil,
 `1on1-minibuffer-frame-flag' is non-nil, and you also use library
 `fit-frame.el', then, whenever the minibuffer is active, the
 minibuffer frame height is automatically adjusted to fit its
 content after each command or user event (e.g. each key press).
 Options `1on1-fit-minibuffer-frame-max-height' and
 `1on1-fit-minibuffer-frame-max-height-percent' define the maximum
 possible height for this behavior.  In addition, if you bind
 `1on1-fit-minibuffer-frame' to a key (I use `C-o'), then you can
 use that key repeatedly to increase the height by one line, even
 beyond the maximum.

 To help you perceive changes to different minibuffer recursion
 levels, the background color of the minibuffer frame is changed
 slightly with each recursion-depth change.

 This library is especially useful if used in combination with
 One-on-One Emacs libraries `autofit-frame.el', which automatically
 fits frames to their sole window, and `fit-frame.el', which lets
 you fit a frame to its selected window manually.  Library
 `autofit-frame.el' uses library `fit-frame.el'.

 Because Emacs is not really designed to be frame-oriented, there
 are many built-in and standard functions that produce
 less-than-optimal results when frames, instead of windows, are the
 default.  In other One-on-One Emacs libraries, I have fixed most
 of these built-in functions to play well with frames.

 For more information on One-on-One Emacs see
 http://www.emacswiki.org/OneOnOneEmacs.

 To use this library, put the following at the *END* of your init
 file, `.emacs' (or `_emacs').  In particular, if your init file
 contains a `custom-set-variables' expression, then the following
 must appear *AFTER* that expression, in order for this to take
 into account your customizations of any `1on1-' user options.

   (require 'oneonone)
   (1on1-emacs)

 Initial frame: By default, the initial Emacs frame is like all
 other normal (non-special-display) frames; that is,
 `initial-frame-alist' effectively uses the frame properties
 defined in `default-frame-alist'.  If you would like the initial
 frame to be different, set `default-frame-alist' to nil after
 requiring `oneonone.el' but before executing `1on1-emacs':

   (require 'oneonone)
   (setq default-frame-alist  nil)
   (setq initial-frame-alist  '((background-color . "White"))); e.g.
   (1on1-emacs)

 If you want the text cursor to change to a box when Emacs is idle,
 then add this line also to your init file:

   (toggle-box-cursor-when-idle 1) ; Turn on box cursor when idle.

 Info and Customize frames: I recommend that you put the following
 code in your init file, so that Info and Customize buffers will
 display in their own frames.  Which code to use depends on your
 version of GNU Emacs.

   (cond ((< emacs-major-version 21)
          (remove-hook 'same-window-buffer-names "*info*"))
         ((= emacs-version 21)
          (remove-hook 'same-window-buffer-names "*info*")
          (remove-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'"))
         (t
          (remove-hook 'same-window-regexps "\\*info\\*\\(\\|<[0-9]+>\\)")
          (remove-hook 'same-window-regexps "\\`\\*Customiz.*\\*\\'")))

 Recommended key bindings (requires library `fit-frame.el'):

   (define-key minibuffer-local-map "\C-o"
               '1on1-fit-minibuffer-frame)
   (define-key minibuffer-local-must-match-map "\C-o"
               '1on1-fit-minibuffer-frame)
   (define-key minibuffer-local-completion-map "\C-o"
               '1on1-fit-minibuffer-frame)

 By default, `oneonone.el' sets the width of the bottom and right
 dividers, which separate Emacs windows, to 2 instead of 0.  This
 lets you more easily notice where to drag with your mouse, to
 resize windows.  If you use Emacs 24.4 or later then I also
 recommend that you consider customizing face `window-divider', to
 further highlight the dividers.


 Notes on user options defined here:
 ---------------------------------

 Some non-option variables are used here only as conveniences to
 define frame-parameter alists.  They are defined using `defvar',
 not `defcustom', because you cannot use Customize to define them
 independently of the alist user options they help to define.  The
 alists themselves are the variables to customize.  If you want to
 change the `defvar' variables individually and then use them to
 set the alist variables, then use `setq', not Customize, to change
 them, and restart Emacs for their changes to take effect.

 Changes to any user options defined here take effect as soon as
 `1on1-emacs' is executed, so you can do `M-x 1on1-emacs' to see
 their changes (no need to restart Emacs).

 User options `1on1-color-minibuffer-frame-on-setup-increment' and
 `1on1-color-minibuffer-frame-on-exit-increment' determine how much
 to change the color of the minibuffer frame when the minibuffer is
 entered and exited.  They are hue increments, and should be
 opposite in sign.

 They should cancel each other out, so that the color returns to
 what it was initially at any given minibuffer depth.  However,
 because of the way HSV and RGB color-component conversion works,
 the best cancellation does not necessarily occur when these
 options have the same absolute value.  And how much their absolute
 values should differ depends on that magnitude.  It is best to
 just set one of these to an increment you like, and then fiddle
 with the other until they more or less cancel.


 Commands defined here:

   `1on1-emacs', `1on1-fit-minibuffer-frame',
   `1on1-ORIG-abort-recursive-edit', `1on1-ORIG-top-level',
   `1on1-ORIG-y-or-n-p', `1on1-other-frame',
   `1on1-set-box-cursor-when-idle-interval',
   `1on1-set-cursor-type', `1on1-toggle-box-cursor-when-idle'.

 User options defined here:

   `1on1-*Completions*-frame-flag',
   `1on1-*Completions*-frame-at-right-flag',
   `1on1-*Help*-frame-flag',
   `1on1-active-minibuffer-frame-background',
   `1on1-active-mode-line-background',
   `1on1-change-cursor-on-input-method-flag',
   `1on1-change-cursor-on-overwrite/read-only-flag',
   `1on1-color-minibuffer-frame-on-exit-increment',
   `1on1-color-minibuffer-frame-on-setup-increment',
   `1on1-color-mode-line-flag',
   `1on1-completions-frame-background',
   `1on1-completions-frame-mouse+cursor-color',
   `1on1-completions-frame-width',
   `1on1-completions-frame-zoom-font-difference',
   `1on1-default-frame-cursor-color',
   `1on1-default-frame-cursor-color-input-method',
   `1on1-default-frame-cursor-type',
   `1on1-default-frame-cursor-type-overwrite/read-only',
   `1on1-default-frame-alist', `1on1-help-frame-background',
   `1on1-help-frame-mouse+cursor-color',
   `1on1-inactive-minibuffer-frame-background',
   `1on1-inactive-mode-line-background',
   `isearch-minibuffer-frame-background',
   `1on1-minibuffer-frame-alist', `1on1-minibuffer-frame-flag',
   `1on1-minibuffer-frame-left',
   `1on1-minibuffer-frame-top/bottom',
   `1on1-minibuffer-frame-width',
   `1on1-minibuffer-frame-width-percent',
   `1on1-remap-other-frame-command-flag',
   `1on1-special-display-frame-alist', `1on1-task-bar-height'
   (Emacs < 24.4).

 Non-interactive functions defined here:

   `1on1-box-cursor-when-idle',
   `1on1-change-cursor-on-input-method',
   `1on1-change-cursor-on-overwrite/read-only',
   `1on1-color-minibuffer-frame-on-exit',
   `1on1-color-minibuffer-frame-on-setup',
   `1on1-color-isearch-minibuffer-frame',
   `1on1-display-*Completions*-frame', `1on1-display-*Help*-frame',
   `1on1-filter-no-default-minibuffer',
   `1on1-flash-ding-minibuffer-frame',
   `1on1-minibuffer-prompt-end', `1on1-reset-minibuffer-frame',
   `1on1-remove-if', `1on1-set-minibuffer-frame-top/bottom',
   `1on1-set-minibuffer-frame-width',
   `1on1-setup-minibuffer-frame-coloring', `1on1-setup-mode-line'.

 Non-option variables defined here:

   `1on1-box-cursor-when-idle-p',
   `1on1-box-cursor-when-idle-interval',
   `1on1-box-cursor-when-idle-timer',
   `1on1-default-frame-background', `1on1-default-frame-font',
   `1on1-default-frame-foreground',
   `1on1-default-frame-menu-bar-lines',
   `1on1-default-frame-mouse-color', `1on1-default-frame-size',
   `1on1-default-frame-upper-left-corner', `1on1-divider-width',
   `1on1-last-cursor-type', `1on1-minibuffer-frame',
   `1on1-minibuffer-frame-background',
   `1on1-minibuffer-frame-bottom-offset',
   `1on1-minibuffer-frame-cursor-color',
   `1on1-minibuffer-frame-font',
   `1on1-minibuffer-frame-foreground',
   `1on1-minibuffer-frame-height',
   `1on1-minibuffer-frame-mouse-color',
   `1on1-special-frame-background',
   `1on1-special-frame-cursor-color', `1on1-special-frame-font',
   `1on1-special-frame-foreground',
   `1on1-special-frame-menu-bar-lines',
   `1on1-special-frame-mouse-color', `1on1-special-frame-size',
   `1on1-special-frame-upper-left-corner'.


 ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:

 `abort-recursive-edit', `top-level' -
              Reset color of minibuffer frame to "inactive" color.

 `y-or-n-p' - Temporarily color minibuffer frame to "active" color.


 Acknowledgements:

 The cursor-changing on input method and read-only was inspired by
 Juri Linkov <juri@jurta.org>.  Joe Casadonte <joc@netaxs.com>
 wrote a similar hook (`joc-cursor-type-set-hook'), which he got
 from Steve Kemp...
