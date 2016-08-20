   Extensions to `facemenu.el'.

 This library enhances the "Text Properties" menu.  It adds menu
 items to the menu, and provides two different versions of the
 menu: one for the menu-bar Edit menu (`facemenu-menu') and one for
 the mouse popup menu (`facemenu-mouse-menu').  In standard library
 `facemenu.el', these two menus are the same.

 Items are added to each of these menus to examine, copy, and
 change foreground and background colors in various ways.

 In the `C-mouse-2' popup version of the menu
 (`facemenu-mouse-menu'), menu items use the character under the
 mouse pointer, instead of the character after the text cursor
 (point).  For example, in the mouse menu, "Describe Properties"
 describes the text properties under the mouse pointer.  This makes
 the mouse menu generally more convenient than the menubar menu -
 just point and click.

 Menu items "Do Re Mi - *" make use of commands `doremi-face-fg+'
 `doremi-face-bg+', and `doremi-undo-last-face-change', which are
 defined in library `doremi-frm.el'.  They let you change the face
 color incrementally, using the arrow keys or the mouse wheel, and
 undo such changes.

 Menu items "Palette - *" make use of library `palette.el' (which
 is available only for Emacs 22 and later).  They let you use a
 color palette to change the color.  You can restore the face color
 before as it was before palette editing, by using command
 `facemenup-face-bg-restore' or `facemenup-face-fg-restore'.

 If option `facemenup-palette-update-while-editing-flag' is nil,
 then quitting the palette using `q' cancels any color change you
 made there, and exiting the palette using `x' effects the color
 change.  If the option is non-nil (the default), then each time
 you change the palette color the face color changes as well.  This
 lets you see the effect immediately, without exiting the palette,
 but you cannot then use `q' in the palette to cancel the edit.
 But you can still restore the color as it was before the edit.

 Both Do Re Mi and the color palette let you change colors by
 changing color components, whether RGB (red, green, blue) or HSV
 (hue, saturation, value).

 In addition, standard commands `facemenu-set-face' (`M-o o') and
 `list-faces-display' have been enhanced to let you pick a face to
 apply from the *Faces* display.  The face sample text is an action
 button that does this.  The face name is a link to the face
 description, which also has a link to customize the face.  If the
 region is active when you call either of these functions, then
 clicking a face's sample text applies the face to the region;
 otherwise, it applies the face to newly entered text.  To use this
 feature with `facemenu-set-face', use a numeric prefix argument,
 such as `C-8'.

 The reason for this enhancement to `facemenu-set-face' and
 `list-faces-display' is to let you see what face you are choosing:
 its appearance, not just its name.  If you use Icicles, then you
 do not need this enhancement, because face names are displayed in
 *Completions* with their own faces.

 Similarly, standard command `list-colors-display' has been
 enhanced to open the color palette on a color when you click it.
 (Using the palette to edit the color does not change the original
 color's definition.)  The palette is useful in this context mainly
 to show you the color in context.  In addition, the tooltip is
 improved, showing more precise HSV values and decimal RGB.

 Standard commands `facemenu-set-face' (`M-o o') and
 `facemenu-add-face' have also been enhanced here, so that they
 prevent the highlighting that you add from being erased by font
 lock.  To take advantage of this, you must use Emacs version 22 or
 later, and you must also load library `font-lock+.el'.  I strongly
 recommend that you do that.  Otherwise, you cannot use facemenu
 commands in a font-locked buffer.

 If you load library `highlight.el' before you load `facemenu+.el',
 then the commands in that library are also added to the Text
 Properties menu, as a Highlight submenu.

 If you also use library `zones.el' then narrowing and other
 commands record buffer zones (including narrowings) in
 buffer-local variable `zz-izones' (by default).  You can use
 command `facemenup-add-face-to-regions' to add a face to these
 zones, and you can use command
 `facemenup-add-face-to-regions-in-buffers' to add a face to all
 zones recorded for a given set of buffers.

 Commands defined here:

   `facemenu-mouse-menu', `facemenup-add-face-to-regions',
   `facemenup-add-face-to-regions-in-buffers',
   `facemenup-change-bg-of-face-at-mouse+',
   `facemenup-change-bg-of-face-at-point+',
   `facemenup-change-fg-of-face-at-mouse+',
   `facemenup-change-fg-of-face-at-point+',
   `facemenup-customize-face-at-mouse',
   `facemenup-customize-face-at-point',
   `facemenup-describe-text-properties-at-mouse',
   `facemenup-face-bg-restore', `facemenup-face-fg-restore',
   `facemenup-palette-face-bg-at-mouse',
   `facemenup-palette-face-bg-at-point',
   `facemenup-palette-face-fg-at-mouse',
   `facemenup-palette-face-fg-at-point',
   `facemenup-paste-to-face-bg-at-mouse',
   `facemenup-paste-to-face-bg-at-point',
   `facemenup-paste-to-face-fg-at-mouse',
   `facemenup-paste-to-face-fg-at-point',
   `facemenu-rgb-format-for-display',
   `facemenup-set-face-attribute',
   `facemenup-set-face-attribute-at-mouse',
   `facemenup-set-face-attribute-at-point',
   `facemenup-set-face-bg-RGB-at-mouse',
   `facemenup-set-face-bg-RGB-at-point',
   `facemenup-set-face-bg-RGB-hex-at-mouse',
   `facemenup-set-face-bg-RGB-hex-at-point',
   `facemenup-set-face-fg-RGB-at-mouse',
   `facemenup-set-face-fg-RGB-at-point',
   `facemenup-set-face-fg-RGB-hex-at-mouse',
   `facemenup-set-face-fg-RGB-hex-at-point',
   `palette-for-background-at-point',
   `palette-for-foreground-at-point'.

 User options defined here:

   `facemenup-palette-update-while-editing-flag'.

 Non-interactive functions defined here:

   `facemenup-copy-tree' (Emacs 20-21), `facemenup-face-bg',
   `facemenup-face-fg', `facemenup-nonempty-region-p',
   `facemenup-set-face-attribute-at--1',
   `facemenup-set-face-from-list'.

 Internal variables defined here:

   `facemenu-mouse-menu', `facemenup-err-mouse',
   `facemenup-err-point', `facemenup-highlight-menu',
   `facemenup-last-face-bg', `facemenup-last-face-changed',
   `facemenup-last-face-fg'.

 Button types defined here:

   `help-facemenu-edit-color', `help-facemenu-set-face'.

 Macros defined here:

   `facemenu+-with-help-window'.


 ***** NOTE: The following functions defined in `facemenu.el'
             have been REDEFINED HERE:

   `facemenu-add-face' (Emacs 22+),
   `facemenu-post-self-insert-function' (Emacs 24+),
   `facemenu-read-color', `facemenu-set-face' (Emacs 22+),
   `list-colors-print' (Emacs 22+).


 ***** NOTE: The following function defined in `faces.el'
             has been REDEFINED HERE (Emacs 22+):

   `list-faces-display'.

 Add this to your init file (~/.emacs):

   (require 'facemenu+)


 Suggestions, if you use Emacs 22 or later:

 1. Load library `font-lock+.el' also, to prevent font lock from
    erasing the highlighting you add using `facemenu+.el'.

 2. (This is unrelated to `facemenu+.el'.)  Customize option
    `facemenu-listed-faces' to t, so that any faces you use are
    automatically added to the face menu.  That way, to use one
    again, you need not choose `Other...' in the menu each time.
