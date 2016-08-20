> **NOTE**: By default `flycheck-purescript' compiles the project sources and
> writes it to an "output" directory relative to a project root, you can
> change it with the variable `flycheck-purescript-compile-output-dir'.

## Setup

    (eval-after-load 'flycheck
      '(flycheck-purescript-setup))
