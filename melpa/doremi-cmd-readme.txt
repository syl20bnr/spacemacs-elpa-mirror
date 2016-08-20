   Miscellaneous Do Re Mi commands.

 During Do Re Mi commands, you can press and hold an up/down arrow
 key, or rotate the mouse wheel, to change face attributes or frame
 parameters.  For more info, see file `doremi.el' and the
 doc-string for function `doremi' in particular.

 NOTE: Functions and variables in this library have the prefix
       `doremi-'.  In order to more easily distinguish commands
       that iterate in Do Re Mi fashion from other functions in the
       library, the iterative commands are suffixed with `+'.

 If you also use library `crosshairs.el' (which requires libraries
 `hl-line.el', `hl-line+.el', `vline.el', and `col-highlight.el'),
 then commands `doremi-marks+' and `doremi-global-marks+' use
 crosshairs to highlight the mark positions when you visit them.

 Note on saving changes made with the commands defined here:

   Some of the commands defined here change face and frame
   properties. You can save any changes you have made, by using
   Customize. To visit a Customize buffer of all unsaved changes
   you have made, use command `customize-customized'.

   Frame parameter changes, such as background color, can be saved
   for future use by all frames or all frames of a certain
   kind. For that, you must change the frame parameters of the
   correponding frame-alist variable.

   There is no single variable for saving changes to parameters of
   the current frame. Instead, there are several different
   frame-alist variables, which you can use to define different
   kinds of frames. These include: `default-frame-alist',
   `initial-frame-alist', and `special-display-frame-alist'. The
   complete list of such frame alist variables is available using
   function `frame-alist-var-names', defined in library
   `frame-cmds.el'.

   Example: Suppose you change the background color of a frame and
   want to make that the default background color for new frames in
   the future. You will need to update the value of variable
   `default-frame-alist' to use the `background-color' parameter
   setting of the changed frame.

   You can easily copy one or all parameter values from any given
   frame to any frame alist (such as `default-frame-alist'), by
   using the commands `set-frame-alist-parameter-from-frame' and
   `set-all-frame-alist-parameters-from-frame'. Those commands are
   defined in library `frame-cmds.el'.


 User options defined here:

   `doremi-color-themes', `doremi-custom-themes' (Emacs 24+),
   `doremi-custom-themes-accumulate-flag' (Emacs 24+),
   `doremi-themes-update-flag'.

 Commands defined here:

   `doremi-bookmarks+', `doremi-buffers+', `doremi-color-themes+',
   `doremi-custom-themes+' (Emacs 24+), `doremi-global-marks+',
   `doremi-marks+', `doremi-window-height+', `doremi-windows+'
   (Emacs 22+), `doremi-window-width+'.

 Non-interactive functions defined here:

   `doremi-buffers-1', `doremi-color-themes-1',
   `doremi-custom-themes-1' (Emacs 24+), `doremi-global-marks-1',
   `doremi-marks-1', `doremi-windows-1'.


 Add this to your initialization file (~/.emacs or ~/_emacs):

   (require 'doremi-cmd)


 See also these related Do Re Mi libraries:

   `doremi-frm.el' - Do Re Mi commands to adjust frame properties.

   `doremi-mac.el' - Macro to define Do Re Mi commands and
                     automatically add them to a Do Re Mi menu.


 Suggested bindings:

  (defalias 'doremi-prefix (make-sparse-keymap))
  (defvar doremi-map (symbol-function 'doremi-prefix)
    "Keymap for Do Re Mi commands.")
  (define-key global-map "\C-xt" 'doremi-prefix)
  (define-key doremi-map "b" 'doremi-buffers+)
  (define-key doremi-map "g" 'doremi-global-marks+)
  (define-key doremi-map "m" 'doremi-marks+)
  (define-key doremi-map "r" 'doremi-bookmarks+) ; reading books?
  (define-key doremi-map "s" 'doremi-custom-themes+) ; custom schemes
  (define-key doremi-map "w" 'doremi-window-height+)

 Customize the menu. Uncomment this to try it out.

  (defvar menu-bar-doremi-menu (make-sparse-keymap "Do Re Mi"))
  (define-key global-map [menu-bar doremi]
    (cons "Do Re Mi" menu-bar-doremi-menu))
  (define-key menu-bar-doremi-menu [doremi-custom-themes]
    '(menu-item "Custom Themes" . doremi-custom-themes+
      :help "Successively cycle among custom themes: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-global-marks]
    '(menu-item "Global Marks" . doremi-global-marks+
      :help "Successively cycle among global marks: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-marks]
    '(menu-item "Marks in Buffer" . doremi-marks+
      :help "Successively cycle among marks in this buffer: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-bookmarks]
    '(menu-item "Bookmarks" . doremi-bookmarks+
      :help "Successively cycle among bookmarks: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-buffers]
    '(menu-item "Buffers" . doremi-buffers+
      :help "Successively cycle among buffers: `up'/`down'"))
  (define-key menu-bar-doremi-menu [doremi-windows]
    '(menu-item "Windows" doremi-windows+
      :help "Successively cycle among windows: `up'/`down'"
      :enable (not (one-window-p))))
  (define-key menu-bar-doremi-menu [doremi-window-height]
    '(menu-item "Window Size" doremi-window-height+
      :help "Resize window incrementally: `up'/`down'/`left'/`right'"
      :enable (not (one-window-p))))
