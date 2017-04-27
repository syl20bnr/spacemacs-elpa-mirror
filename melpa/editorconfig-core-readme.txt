This library is one implementation of EditorConfig Core, which parses
.editorconfig files and returns properties for given files.
This can be used in place of, for example, editorconfig-core-c.


Use from EditorConfig Emacs Plugin

Emacs plugin (v0.5 or later) can utilize this implementation.
By default, the plugin first search for any EditorConfig executable,
and fallback to this library if not found.
If you always want to use this library, add following lines to your init.el:

    (setq editorconfig-get-properties-function
          'editorconfig-core-get-properties-hash)


Functions

editorconfig-core-get-properties (&optional file confname confversion)

Get EditorConfig properties for FILE.

If FILE is not given, use currently visiting file.
Give CONFNAME for basename of config file other than .editorconfig.
If need to specify config format version, give CONFVERSION.

This functions returns alist of properties. Each element will look like
(KEY . VALUE) .


editorconfig-core-get-properties-hash (&optional file confname confversion)

Get EditorConfig properties for FILE.

This function is almost same as `editorconfig-core-get-properties', but
returns hash object instead.
