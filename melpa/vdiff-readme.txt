A tool like vimdiff for Emacs

* Introduction

vdiff is a diff tool for Emacs that is made to behave like vimdiff, meaning
diff information is displayed in buffers as you edit them. There are commands
for cycling through the hunks detected by =diff= and applying changes from
one buffer to the other. The main features are

  1. Synchronized scrolling of the buffers with lines matching between the
     two
  2. Commands to transmit (send/receive) changes between buffers
  3. Automatic folding of lines that are unchanged in both buffers
  4. Commands to jump easily between hunks
  5. Everything done through overlays, meaning vdiff doesn't alter the actual
     text in the buffer (unless you are transmit changes of course)
  6. Unlike ediff, remain in buffers instead of having to use a third "control
     buffer"
  7. Cool hydra (see below)

Contributions and suggestions are very welcome.

See https://github.com/justbur/emacs-vdiff for more information
