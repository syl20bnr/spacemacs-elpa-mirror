Variable-pitch support for org-mode.  This minor mode enables
‘variable-pitch-mode’ in the current Org-mode buffer, and sets some
particular faces up so that they are are rendered in fixed-width
font.  Also, indentation, list bullets and checkboxes are displayed
in monospace, in order to keep the shape of the outline.

Installation:

Have this file somewhere in the load path, then:

  (require 'org-variable-pitch)
  (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)

Configurables:

  - ‘org-variable-pitch-fixed-font’: The font used for parts of the
    buffer to be kept in fixed-width font.

  - ‘org-variable-pitch-fixed-faces’: List of org-mode faces to
    keep monospace.



