Large parts of this code is shamelessly stolen from python.el and
adapted to Nim


This package provide a major-mode future (syntax highlight and
indentation) and some necessity futures (jump-to-definition,
linting, el-doc) if you install nimsuggest.  See below for the detail.

Configuration:

If you installed from MELPA, pretty much all configuration are set
by default to just use this major-mode.  (syntax highlight and indentation)
But if you want some editor integration like jump-to-definition or
auto-completion, you need to install/build (or just include it in
the PATH if you already have it) `nimsuggest' which is Nim
language's an editor agnostic tool.

## Nimsuggest
(if you are impatient, skip until `install nimsuggest` to install it)

Nimsuggest is an editor agnostic tool for Nim and nim-mode provides:

1. Completion feature -- `C-M-i` and `M-TAB` keys and auto-complete feature if
   you install [company-mode](https://github.com/company-mode/company-mode)
2. Asynchronous linting -- nimsuggest take care .nims files as
   configuration file, so it's smarter than `nim check` command (1)
3. Showing info under the cursor in minibuffer -- (1)
4. Jump to definition feature -- `M-.` for go to def and `M-,` for
   back to before the go to def position

(1): those are automatically turned on if you turned on `nimsuggest-mode`

### Install nimsuggest

1. Use stable version:
   See [official download instruction](http://nim-lang.org/download.html) at
   "Installation based on generated C code" section.

2. Use latest version:
   This way may or may not work (depending on Nim or nimsuggest's
   state and we can't support all the way), so use above way
   if you prefer stable.
   ```sh
   #  assuming you already installed Nim
   cd /path/to/Nim_repository
   ./koch tools
   ```

After you installed nimsuggest, you may need following configuration in
your Emacs configuration file (e.g, ~/.emacs.d/init.el):

```el
can be optional.  See below note
(setq nim-nimsuggest-path "path/to/nimsuggest")

Currently nimsuggest doesn't support nimscript files, so only nim-mode...
(add-hook 'nim-mode-hook 'nimsuggest-mode)

if you installed company-mode (optional)
(add-hook 'nim-mode-hook 'company-mode)
(add-hook 'nimscript-mode-hook 'company-mode)
or use below instead if you want to activate `company-mode` on all programming
related modes.
(add-hook 'prog-mode-hook 'company-mode)
```

Note that above `nim-nimsuggest-path` variable is automatically set
the result of `(executable-find "nimsuggest")`, so if you can get
value from the `executable-find`, you may not need that
configuration unless you want to set specific version of nimsuggest.

TODO:
- find reference
