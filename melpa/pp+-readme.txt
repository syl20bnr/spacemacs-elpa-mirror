   Extensions to `pp.el'.

 Features:

  * You can optionally show the result of pretty-printing in a
    tooltip at point, by customizing option `pp-max-tooltip-size'
    (Emacs 24.4+).

  * You can use a zero prefix argument (e.g. `M-o') with
    `pp-eval-last-sexp' (`C-x C-e') or `pp-eval-expression', to
    swap the use of a tooltip defined by option
    `pp-max-tooltip-size'.

  * There are additional commands that are versions of
    `pp-eval-last-sexp' and `pp-eval-expression' that always or
    never use a tooltip.

  * Pretty-printing respects options
    `pp-eval-expression-print-length' and
    `pp-eval-expression-print-level', which act like `print-length'
    and `print-level', but only for pretty-printing.

  * The buffer displaying the pretty-printed result is in
    `emacs-lisp-mode' (and is fontified accordingly), but without
    having run `emacs-lisp-mode-hook' or `change-major-mode-hook'.

  * Command `pp-eval-expression' is enhanced in these ways:

     - Option `eval-expression-debug-on-error' is respected.

     - With no prefix argument, option `pp-max-tooltip-size' is
       respected. If a tooltip is not used then if the value fits
       on one line (frame width) it is shown in the echo
       area. Otherwise, it is shown in buffer *Pp Eval Output*'.

     - With a zero prefix arg, the use of a tooltip according to
       `pp-max-tooltip-size' is swapped: if that option is `nil'
       then a tooltip is used, and if non-`nil' a tooltip is not
       used.

     - With non-zero prefix argument, the value is inserted into
       the current buffer at point. With a negative prefix arg, if
       the value is a string, then it is inserted without being
       enclosed in double-quotes (").

     - Completion is available, using keymap
       `pp-read-expression-map', which is like
       `read-expression-map' but with some Emacs-Lisp key bindings.

     - With a prefix arg, the value is inserted into the current
       buffer at point.  With a negative prefix arg, if the value
       is a string, then it is inserted without being enclosed in
       double-quotes (`"').

  * Command `pp-eval-last-sexp' is enhanced in these ways:

     - With a zero prefix arg, the use of a tooltip according to
       `pp-max-tooltip-size' is swapped: if that option is `nil'
       then a tooltip is used, and if non-`nil' a tooltip is not
       used.

     - With a non-zero prefix arg, the value is inserted into the
       current buffer at point.

  * Alternative commands are defined that use a tooltip whenever
    possible, or that never use a tooltip (they ignore option
    `pp-max-tooltip-size'): `pp-eval-expression-with-tooltip',
    `pp-eval-expression-without-tooltip',
    `pp-eval-last-sexp-with-tooltip', and
    `pp-eval-last-sexp-without-tooltip' (Emacs 24.4+).



 Suggested binding: use `M-:' for `pp-eval-expression'.

   (global-set-key [remap eval-expression] 'pp-eval-expression)


 User options defined here:

   `pp-eval-expression-print-length',
   `pp-eval-expression-print-level', `pp-max-tooltip-size' (Emacs
   24+).

 Faces defined here:

   `pp-tooltip' (Emacs 24+).

 Commands defined here:

   `pp-eval-expression-with-tooltip',
   `pp-eval-expression-without-tooltip',
   `pp-eval-last-sexp-with-tooltip',
   `pp-eval-last-sexp-without-tooltip'.

 Non-interactive functions defined here:

   `pp-expression-size', `pp-show-tooltip', `pp-tooltip-show'.

 Variables defined here:

   `pp-read-expression-map'.


 ***** NOTE: The following functions defined in `pp.el' have
             been REDEFINED HERE:

   `pp-display-expression', `pp-eval-expression',
   `pp-eval-last-sexp'.
