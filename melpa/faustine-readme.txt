Faustine allows the edition of Faust (http://faust.grame.fr) code.

## Features

- Project-based (inter-linked Faust files)
- Faust code syntax hightlighting, indentation and keyword completion
- Build/compile with configurable output window
- Graphic diagrams generation and vizualisation in the browser
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

Put it in `load-path` ; optionally add your usual Faust file
extension to the `auto-mode-alist`:
```
(add-to-list 'auto-mode-alist
             '("\\.dsp\\'" . faustine-mode))
```
to put any new Faust file in the mode.
