Faustine allows the edition of Faust (http://faust.grame.fr) code.

## Features

- Project-based (inter-linked Faust files)
- Faust code syntax hightlighting, indentation and keyword completion (using faust-mode)
- Build/compile with output window
- Graphic diagrams generation and vizualisation in the (default) browser
- Browse generated C++ code inside Emacs
- Inter-linked files/buffers :
    - From "component" to Faust file
    - From "include" to Faust library file
- From error to file:line number
- From function name to online documentation
- Fully configurable (build type/target/architecture/toolkit, keyboard shortcuts, etc.)
- Automatic keyword completion (if [Auto-Complete](https://github.com/auto-complete/auto-complete) is installed)
- Automatic objets (functions, operators, etc.) template insertion with default sensible values (if [Yasnippet](https://github.com/joaotavora/yasnippet) is installed)
- Modeline indicator of the state of the code

## Installation

### Easy

- Install it from [MELPA](https://melpa.org).

### Hard

- Copy/clone this repository in `load-path`
- Copy/clone [Faust-mode](https://github.com/rukano/emacs-faust-mode) in `load-path`
- Add
```elisp
(require 'faustine)
(require 'faust-mode)
```
to your init file

### Faust
Oh, and of course install [the latest Faust](http://faust.grame.fr/download/) and ensure it's in the PATH.

## Usage

### Enter the mode

Use `faustine-mode` ; Optionally, add something like this to your init file:
```elisp
(add-to-list 'auto-mode-alist
             '("\\.dsp\\'" . faustine-mode))
```
to put any new Faust file in the mode.

### Commands
Every interactive command is documented in [the README](https://bitbucket.org/yassinphilip/faustine/src/master/README.md) file.
