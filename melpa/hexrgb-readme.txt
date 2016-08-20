 Functions to manipulate colors, including RGB hex strings.

 This library provides functions for converting between RGB (red,
 green, blue) color components and HSV (hue, saturation, value)
 color components.  It helps you convert among Emacs color
 components (whole numbers from 0 through 65535), RGB and HSV
 floating-point components (0.0 through 1.0), Emacs color-name
 strings (such as "blue"), and hex RGB color strings (such as
 "#FC43A7912").

 An RGB hex string, such as used as a frame `background-color'
 property, is a string of 1 + (3 * n) characters, the first of
 which is "#".  The other characters are hexadecimal digits, in
 three groups representing (from the left): red, green, and blue
 hex codes.

 Constants defined here:

   `hexrgb-defined-colors', `hexrgb-defined-colors-alist',
   `hexrgb-defined-colors-no-dups',
   `hexrgb-defined-colors-no-dups-alist'.

 Options defined here:

   `hexrgb-canonicalize-defined-colors-flag'.

 Commands defined here:

   `hexrgb-blue', `hexrgb-complement', `hexrgb-green',
   `hexrgb-hue', `hexrgb-read-color', `hexrgb-red',
   `hexrgb-saturation', `hexrgb-value'.

 Non-interactive functions defined here:

   `hexrgb-approx-equal', `hexrgb-canonicalize-defined-colors',
   `hexrgb-color-name-to-hex', `hexrgb-color-values-to-hex',
   `hexrgb-color-value-to-float', `hexrgb-defined-colors',
   `hexrgb-defined-colors-alist',
   `hexrgb-delete-whitespace-from-string',
   `hexrgb-float-to-color-value', `hexrgb-hex-char-to-integer',
   `hexrgb-hex-to-color-values', `hexrgb-hex-to-hex',
   `hexrgb-hex-to-hsv', `hexrgb-hex-to-rgb', `hexrgb-hsv-to-hex',
   `hexrgb-hex-to-int', `hexrgb-hsv-to-rgb',
   `hexrgb-increment-blue', `hexrgb-increment-equal-rgb',
   `hexrgb-increment-green', `hexrgb-increment-hex',
   `hexrgb-increment-hue', `hexrgb-increment-red',
   `hexrgb-increment-saturation', `hexrgb-increment-value',
   `hexrgb-int-to-hex', `hexrgb-blue-hex', `hexrgb-green-hex',
   `hexrgb-red-hex', `hexrgb-rgb-hex-string-p',
   `hexrgb-rgb-hex-to-rgb-hex', `hexrgb-rgb-to-hex',
   `hexrgb-rgb-to-hsv'.


 Add this to your initialization file (~/.emacs or ~/_emacs):

   (require 'hexrgb)

 Do not try to use this library without a window manager.
 That is, do not use this with `emacs -nw'.
