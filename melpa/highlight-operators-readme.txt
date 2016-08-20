This library defines a face named `highlight-operators-face' used
for operators (e.g. '+' and '&') in programming modes.
`highlight-operators-mode' is a minor mode that enables this extra
highlighting.  Add `highlight-operators-mode' to your favorite
programming major mode hooks (e.g. (add-hook 'python-mode
'highlight-operator-mode)) or add it to `prog-mode-hook'.

This mode doesn't work well with lisp major modes.
