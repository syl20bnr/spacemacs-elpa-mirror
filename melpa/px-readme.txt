Provides functions to preview LaTeX codes like $x^2$ in any
buffer/mode.

Use `px-preview-region' to preview LaTeX codes delimited by $ pairs
in the region.
Use `px-preview' to process the whole buffer.
Use `px-remove' to remove all images and restore the text back.
Use `px-toggle' to toggle between images and text on the whole
buffer.

Most of this code comes from weechat-latex.el which in turn uses
org-mode previewer.
