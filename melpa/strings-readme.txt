       Miscellaneous string functions.

 You may want to put this in your `~/.emacs' file, to erase the
 minibuffer when it is inactive and `minibuffer-empty-p':

  (require 'strings)
  (add-hook 'pre-command-hook 'erase-nonempty-inactive-minibuffer))


 Macros defined here:

   `empty-name-p', `non-empty-name-p'.

 Functions defined here:

   `absdiff', `buffer-alist', `concat-w-faces',
   `current-d-m-y-string', `current-line-string',
   `display-in-minibuffer', `display-lines-containing',
   `echo-in-buffer', `empty-name-p', `erase-inactive-minibuffer',
   `erase-nonempty-inactive-minibuffer', `fill-string',
   `frame-alist', `insert-in-minibuffer', `minibuffer-empty-p',
   `non-empty-name-p', `ordinal-suffix', `pick-some-words',
   `read-any-variable', `read-number', `region-description',
   `set-minibuffer-empty-p', `string-w-face',
   `symbol-name-before-point', `word-before-point'.

 Variables defined here:

   `minibuffer-empty-p'.


 ***** NOTE: These EMACS PRIMITIVES have been REDEFINED HERE:

 `read-buffer'   - Uses `completing-read'.
 `read-variable' - Uses `symbol-nearest-point' & `completing-read'
                   to get the default.
