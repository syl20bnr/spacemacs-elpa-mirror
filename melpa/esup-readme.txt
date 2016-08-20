The most recent code is always at http://github.com/jschaf/esup

`esup' profiles your Emacs startup time by examining all top-level
S-expressions (sexps).  `esup' starts a new Emacs process from Emacs to
profile each SEXP.  After the profiled Emacs is complete, it will exit and
your Emacs will display the results.

`esup' will step into `require' and `load' forms at the top level of a file,
but not if they're enclosed in any other statement.

(require 'eieio)

(defvar esup-load-path
  ;; Emacs doesn't visit a file when loading it, meaning
  ;; `buffer-file-name' returns nil.
  (file-name-directory (file-truename
                        (if load-in-progress
                            load-file-name
                          buffer-file-name)))
  "Full directory path to esup.el and esup-child.el.")

We need to load esup-child to access `esup-result'.  `esup-child' may not be
on the path, so lets add it here.
(let ((load-path (append load-path (list esup-load-path))))
  (require 'esup-child))


On Emacs 24.3 and below, the `with-slots' macro expands to `symbol-macrolet'
instead of `cl-symbol-macrolet'.
(eval-when-compile
  (if (and (<= emacs-major-version 24)
           (<= emacs-minor-version 3))
      (require 'cl)
    (require 'cl-lib)))
