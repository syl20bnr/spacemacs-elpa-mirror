`picolisp-mode' provides a major mode for PicoLisp programming.

This package is not based on, nor connected with, the PicoLisp support for Emacs provided in [the PicoLisp distribution](http://software-lab.de/down.html), or the more recently [updated version of that support](https://github.com/tj64/picolisp-mode). At this stage, the main advantages provided by this package are:

* an actively maintained and supported system;

* access to the PicoLisp reference documentation, including via ElDoc;

* basic Imenu support;

* ease of customisability; and

* a cleaner codebase.

## Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
 - [Syntax highlighting](#usage-highlighting)
 - [REPL](#repl)
 - [Documentation](#documentation)
 - [Commenting](#commenting)
 - [Indentation](#indentation)
 - [Miscellaneous](#miscellanous)
- [TODO](#todo)
- [Issues](#issues)
- [License](#license)

## Features

* Syntax highlighting of PicoLisp code. (But please read the below [note on syntax highlighting](#note-highlighting).)

* Comint-based `pil' REPL buffers.

* Quick access to documentation for symbol at point.

## Installation

Install [picolisp-mode from MELPA](http://melpa.org/#/picolisp-mode), or put the `picolisp-mode' folder in your load-path and do a `(require 'picolisp-mode)'.

## Usage

<a name='usage-highlighting'></a>

### Syntax highlighting

Enable syntax highlighting for a PicoLisp source buffer with `M-x picolisp-mode'.

### REPL

Start a `pil' REPL session with `M-x picolisp-repl' or, from a `picolisp-mode' buffer, with `C-c C-r' (`picolisp-repl').

### Documentation

Access documentation for the function at point with `C-c C-d' (`picolisp-describe-symbol'). By default, documentation will be displayed via the `lynx' HTML browser. However, one can set the value of `picolisp-documentation-method' to either a string containing the absolute path to an alternative browser, or - for users of Emacs 24.4 and above - to the symbol `picolisp--shr-documentation'; this function uses the `shr' library to display the documentation in an Emacs buffer. The absolute path to the documentation is specified via `picolisp-documentation-directory', and defaults to `/usr/share/picolisp/doc/'.

ElDoc support is available; note, however, that documentation is not yet accessible for some symbols - in particular, the `c[ad]*ar' functions - due to edge-cases in the reference documentation structure.

### Commenting

Comment a region in a `picolisp-mode' buffer with `C-c C-;' (`picolisp-comment-region'); uncomment a region in a `picolisp-mode' buffer with `C-c C-:' (`picolisp-uncomment-region'). By default one '#' character is added/removed; to specify more, supply a numeric prefix argument to either command.

### Indentation

Indent a region in a `picolisp-mode' buffer with `C-c M-q' (`picolisp-indent-region'). Indentation is done via the `pilIndent' script provided with the current PicoLisp distribution; the path to the script is specified via the `picolisp-pilindent-executable' variable.

### Miscellaneous

SLIME users should read the below [note on SLIME](#note-slime).

The various customisation options, including the faces used for syntax highlighting, are available via the `picolisp' customize-group.

<a name="note-highlighting"></a>

### A note on syntax highlighting

PicoLisp's creator is opposed to syntax highlighting of symbols in PicoLisp, for [good reasons](http://www.mail-archive.com/picolisp@software-lab.de/msg05019.html). However, some - such as the author of this package! - feel that, even taking such issues into consideration, the benefits can outweigh the costs. (For example, when learning PicoLisp, it can be useful to get immediate visual feedback about unintentionally redefining a PicoLisp 'builtin'.) To accommodate both views, syntax highlighting can be enabled or disabled via the `picolisp-syntax-highlighting-p' variable; by default, it is set to `t' (enabled).

<a name="note-slime"></a>

### A note on [SLIME](https://github.com/slime/slime)

The design of SLIME is such that it can override `picolisp-mode' functionality. (The documentation for `picolisp--disable-slime-modes' provides details.) The user-customisable variable `picolisp-disable-slime-p' specifies whether to override these overrides, and defaults to `t'.

## TODO

* Implement `pilIndent' in Emacs Lisp, make available as a fallback when `pilIndent' is not available.

* Fix misalignment of single-'#' comments upon newline.

* Handle edge-cases in reference documentation structure:

 * `picolisp-describe-symbol' failures;

 * `picolisp--eldoc-function' fails on e.g. `c[ad]*ar'.

<a name="issues"></a>

## Issues / bugs

If you discover an issue or bug in `picolisp-mode' not already noted:

* as a TODO item, or

* in [the project's "Issues" section on GitHub](https://github.com/flexibeast/picolisp-mode/issues),

please create a new issue with as much detail as possible, including:

* which version of Emacs you're running on which operating system, and

* how you installed `picolisp-mode'.

## License

[GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.
