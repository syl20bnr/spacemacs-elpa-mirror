Faustine allows the edition of Faust (http://faust.grame.fr) code.

## Features

- Project-based (inter-linked Faust files)
- Faust code syntax hightlighting, indentation and keyword completion
- Build/compile with output window
- Graphic diagrams generation and vizualisation in the (default) browser
- Browse generated C++ code inside Emacs
- Inter-linked files/buffers :
    - From "component" to Faust file
    - From "include" to Faust library file
- From error to file:line number
- From function name to online documentation
- Fully configurable (build type/target/architecture/toolkit, keyboard shortcuts, etc.)
- Automatic keyword completion
- Modeline indicator of the state of the code

## Installation

### Simple

- Install it from [MELPA](https://melpa.org).

### Hard

- Copy/clone this repository in `load-path`
- Add `  (require 'faustine)` to you init file

Optionally, add something like this to your init file:
```
(add-to-list 'auto-mode-alist
             '("\\.dsp\\'" . faustine-mode))
```
to put any new Faust file in the mode.
