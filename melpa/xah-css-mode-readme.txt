Major mode for editing CSS code. Alternative to GNU emacs's builtin `css-mode'.

Features:

• Correct Syntax coloring ALL CSS words. Does not color typos.

• Color coded words by semantics. Each type of CSS words are colored distinctly. e.g. HTML tag names, CSS attribute names, predefined CSS value names, CSS unit names, pseudo selector names, media keywords, etc.

• ID selector string and class name in bold for easy identification.

• Keyword completion with `ido-mode' interface. Press Tab ↹ after a word to complete. All CSS words are supported: {html5 tags, property names, property value keywords, units, colors, pseudo selectors, “at keywords”, …}.

• Single Key Prettify Code Format. Press Tab ↹ before word to reformat current block of code. That is, all lines enclosed by curly brackets {}.

• Syntax coloring of hexadecimal color format #rrggbb , #rgb , and HSL Color format hsl(0,68%,42%).

• Call `xah-css-hex-to-hsl-color' to convert #rrggbb color format under cursor to HSL Color format.

• Call `xah-css-minify' to compact region.

• Call `xah-css-expand-to-multi-lines' to expand minified CSS code to multi-lines format.

• Call `describe-function' on `xah-css-mode' for detail.
