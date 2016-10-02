### Description

This package solves the following problems for me:
- A single function call interface to grep and therefore keybinding.
- Selects the grep based on context: Inside a git-repository, runs
  `git-grep', otherwise runs `ag'.
- Uses helm to select candidates and jumps to the given line with RET.

And the following additional problems (as of version 1.2.0):
- A second interactive function `helm-grepint-grep-root'.  This runs the
  grepping inside a root directory.  By default this has been defined for
  the git-grep where it greps from the git root directory.
- Inside a huge git repository one can create a file defined in the
  variable `helm-grepint-default-config-ag-presearch-marker-file' and it
  will set that directory as the root directory for grepping.  It uses `ag'
  instead of `git-grep' as the grep.
- The grepping is case-insensitive by default, but if an upper-case letter
  is given case-sensitive grepping is done.

The following enables the aforementioned:

        (require 'helm-grepint)
        (helm-grepint-set-default-config-latest)
        (global-set-key (kbd "C-c g") #'helm-grepint-grep)
        (global-set-key (kbd "C-c G") #'helm-grepint-grep-root)

The original configuration (i.e. without the above additional features) is
available with the following:

        (helm-grepint-set-default-config)

### Key bindings within helm

- `RET'/`F1' selects an item and closes the helm session.
- `F2' displays the grep results in a `grep-mode' buffer.
- `Right-arrow' selects the item, but does not close the helm session.  This
  is similar as `helm-occur'.  Default helmkeybindings for this feature are
  also available (`C-j' and `C-z').
- `M-c' cycles case sensitiveness.

### Customization

Look into the function `helm-grepint-set-default-config' to see how the default
cases are configured.  Also look into `helm-grepint-add-grep-config' for more
details on what is required for a new grep to be defined.

### Changes

Version 1.2.0

- Obsoleted `helm-grepint-get-grep-config' in favor of
  `helm-grepint-grep-config'.
- Make the ignore-case a separate argument in the grep configuration.  This
  way it can be toggled on and off easily.
- Add case-fold-search support (case-(in)sensitiveness).  Add Helm
  keybinding `M-c' to control it.
- Add smart case-sensitiveness checking.
- Add a new configuration `helm-grepint-set-default-config-v1.2.0' which
  makes the smart cases-sensitiveness as the default.  The configuration is
  now the `helm-grepint-set-default-config-latest'.

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
