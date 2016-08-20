Emacs 24 introduced lexical scope for variables to Emacs
Lisp.  However, rather than provide it via a new form, say llet or
whatever, the buffer local variable `lexical-binding' was
introduced to switch the behaviour of binding forms.  This is an
unfortunate situation because the semantics of a piece of code
depend on the value of a buffer local variable at the time of
evaluation.

This minor mode is intended to make it plain what the value of
`lexical-binding' is in the buffers used to evaluate Lisp forms.
It does this by adding to the mode line the string "(LEX)" to
indicate lexical binding is enabled, and "(DYN)" to indicate that
lexical binding is disabled and that dynamic binding is in effect.

Other lexical scope specific utilities such as lexbind-lexscratch
may also find a home here.

To install, once lexbind-mode.el is located somewhere in your
load-path, you can add this to your initialization file:

(require 'lexbind-mode)
(add-hook 'emacs-lisp-mode-hook 'lexbind-mode)
