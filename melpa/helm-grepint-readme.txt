### Description

This package solves the following problem for me:
- A single function call interface to grep and therefore keybinding.
- Selects the grep based on context: Inside a git-repository, runs
  git-grep, otherwise runs ag.
- Uses helm to select candidates and jumps to the given line with RET.

The following enables the aforementioned:

        (require 'helm-grepint)
        (helm-grepint-set-default-config)
        (global-set-key (kbd "C-c g") #'helm-grepint-grep)

### Key bindings within helm

- RET selects an item and closes the helm session.
- Right arrow selects the item, but does not close the helm session.  This
  is similar as `helm-occur'.

### Additional features

This has a second interactive function `helm-grepint-grep-root'.  This runs the
grepping inside a root directory.  By default this has been defined for the
git-grep where it greps from the git root directory.

### Customization

Look into the function `helm-grepint-set-default-config' to see how the default
cases are configured.  Also look into `helm-grepint-add-grep-config' for more
details on what is required for a new grep to be defined.

### Changes

Version 1.1.1

- Add `--ignore-case' argument for `git-grep' to make it consistent with
  `ag' in the `helm-grepint-set-default-config'.

Version 1.1.0

- Fix incompatibilities with recent helm versions.
- Add `helm-grepint-candidate-number-limit' variable to control the number
  of candidates instead of hard-coding 500.
- Create a new example configuration which adds the ag-presearch
  functionality.  The example configurations are now versioned:
  `helm-grepint-set-default-config-v1.0.0' and
  `helm-grepint-set-default-config-v1.1.0'.
- Change the `helm-grepint-set-default-config' function to an alias of
  `helm-grepint-set-default-config-v1.0.0'.  Add new alias
  `helm-grepint-set-default-config-latest' which points to
  `helm-grepint-set-default-config-v1.1.0'.

Version 1.0.0

- Add action to create a `grep-mode' buffer from the helm-buffer.
- Add universal-argument to manually ask the used grep configuration.

Version 0.5.5

- Fix swooping into multiple files within a helm session.  Previously it
  would change default-directory every swoop.
- Add action to open the helm buffer in grep-mode.  This enables the use of
  e.g. `wgrep'.
- Add `helm-grepint-grep-ask-root' and set it as default for ag.
