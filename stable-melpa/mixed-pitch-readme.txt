`mixed-pitch-mode' is a minor mode that enables mixing variable-pitch and
fixed-pitch fonts in the same buffer.  The list
`mixed-pitch-fixed-pitch-faces' defines the faces that are kept fixed-pitch,
everything else becomes variable-pitch.

Original idea came from https://ogbe.net/blog/toggle-serif.html
Shared with permission.

Usage:
(require 'mixed-pitch)
(mixed-pitch-mode)
Or, to apply mixed-pitch-mode in all text modes:
(add-hook 'text-mode-hook #'mixed-pitch-mode)
