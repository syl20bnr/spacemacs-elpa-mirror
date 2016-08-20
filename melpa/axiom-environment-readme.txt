The ``axiom-environment'' package is intended to make it easier to
work with, and understand, the Axiom, OpenAxiom and FriCAS computer
algebra systems.  It implements four different major modes for the
Emacs text editor:

  1. axiom-process-mode: for interaction with a running Axiom
  process.

  2. axiom-help-mode: for displaying help information about the
     Axiom system.

  3. axiom-input-mode: for editing a .input (Axiom script) file.

  4. axiom-spad-mode: for editing Axiom library code written in the
     SPAD language.

The main features of these modes (so far) are syntax highlighting
to display package, domain & category names (and their
abbreviations) in distinct colours, and to give quick access to
popup buffers displaying summary information about these types and
their operations.  The syntax highlighting feature allows to see
at a glance which aspect of the type system we are concerned with
(domains or categories), and the popup buffer feature allows to
examine (and re-examine) these types without interupting the
workflow (i.e. interaction in the Axiom REPL).
