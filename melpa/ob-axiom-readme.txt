The ``ob-axiom'' package is an org-babel extension that integrates
the axiom-environment into org-mode, allowing a literate
development & presentation style with easy publishing to HTML, PDF,
etc.

To enable this extension put

 (require 'ob-axiom)

in your ~/.emacs initialisation file.

There are two language names defined in this extension: ``axiom''
and ``spad''.  The former is for running arbitrary code in a
running axiom process, but both are also declared as tangle'able
languages.  They correspond to ``axiom-input-mode'' and
``axiom-spad-mode'' in the axiom-environment system, respectively.

There are two extra header options (non-standard org-babel options)
for org-babel ``#+BEGIN_SRC axiom'' source code blocks:-

  :block-read <yes/no>      (defaults to no)
  :show-prompt <yes/no>     (defaults to yes)

The block-read option forces ob-axiom to send the entire code block
to the running axiom process via a temporary file.  This allows
``pile mode'' axiom source code to be handled correctly.  Otherwise
ob-axiom sends each line of the code block individually to the
axiom process for interpretation.

The show-prompt option allows to enable or inhibit the display of
the axiom REPL prompt on a block-by-block basis.
