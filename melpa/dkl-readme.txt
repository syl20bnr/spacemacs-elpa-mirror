`dkl' provides an ASCII-art representation of a keyboard layout, within an Emacs buffer.

<img src="screencap.png">

## Table of Contents

- [Installation](#installation)
- [Usage](#usage)
- [Layout file format](#layout)
- [TODO](#todo)
- [Issues](#issues)
- [License](#license)

## Installation

Install [dkl from MELPA](http://melpa.org/#/ewmctrl), or put the `dkl' directory in your load-path and do a `(require 'display-keyboard-layout)'.

## Usage

Create an `dkl-layout' buffer with `M-x dkl-display'.

Within the `dkl-layout' buffer, the default keybindings are:

* s - Toggle display of shifted and unshifted layouts.

* q - Close the `dkl-layout' buffer and window.

Customisation options, including how `dkl' highlights typed keys, are available via the `dkl' customize-group.

<a name="layout"></a>

## Layout file format

A layout file contains Emacs Lisp which ensures the layout is used with the correct keyboard, followed by the setting of the `dkl--current-layout` variable:

```elisp
(if (not (string= dkl-keyboard-name "standard"))
    (user-error "Layout `qwerty-us' must be used with `dkl-keyboard-name' set to \"standard\"")
  (setq dkl--current-layout
        ;; Top row
        '((57 . ("`" "~"))
        ...
```

The layout data structure consists of an alist, where the `car` of each entry indicates a character position in the relevant keyboard file, and the `cdr` contains a list of the unshifted and shifted glyphs to display at that position in a `*dkl-layout*` buffer.

## TODO

* `devanagari-inscript` layout:

  * Fix failure to highlight certain keys during composition.

## Issues / bugs

If you discover an issue or bug in `dkl' not already noted:

* as a TODO item, or

* in [the project's "Issues" section on GitHub](https://github.com/flexibeast/dkl/issues),

please create a new issue with as much detail as possible, including:

* which version of Emacs you're running on which operating system, and

* how you installed `dkl'.

## License

[GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.
