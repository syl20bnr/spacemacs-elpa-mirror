An EditorConfig extension that defines a property to specify which
Emacs major-mode to use for files.

To enable this plugin, add `editorconfig-custom-majormode' to
`editorconfig-custom-hooks':

(add-hook 'editorconfig-custom-hooks
          'editorconfig-custom-majormode)
