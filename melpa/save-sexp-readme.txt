Persistently save the value of variables by replacing the textual
representations of the setter S-expression (e.g. `setq') used to
set it's value in some file.

This is similar to how Custom saves options and faces; but instead
of using exactly one instance of `custom-set-variables' to set the
values of all customized variables, this package saves each
variable separatly using an instance of some form that sets the
value of a variable, like e.g. `setq', `defvar' or `defconst'.

  (save-sexp-save-setq "~/file.el" 'variable)

This removes all top-level (setq variable ...) forms and inserts a
new `setq' form where the first match was removed, setting VARIABLE
to it's current value.

`save-sexp-save' can be used to replace any setter of the form:

  (SETTER '?VARIABLE VALUE [DOC-STRING])

See `save-sexp-save's doc-string for how ways to control
intentation etc. `save-sexp-save-setq' and similar functions can
also be used interactively.
