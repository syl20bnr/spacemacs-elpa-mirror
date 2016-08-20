Here we provide functions which facilitate writing multi-file Emacs
packages and facilitate running from the source tree without having
to "install" code or fiddle with evil `load-path'. See
https://github.com/rocky/emacs-load-relative/wiki/NYC-Lisp-talk for
the the rationale behind this.

The functions we add are relative versions of `load', and `require'
and versions which take list arguments. We also add a `__FILE__'
function and a `provide-me' macro.

The latest version of this code is at:
    http://github.com/rocky/emacs-load-relative/

`__FILE__' returns the file name that that the calling program is
running.  If you are `eval''ing a buffer then the file name of that
buffer is used. The name was selected to be analogous to the name
used in C, Perl, Python, and Ruby.

`load-relative' loads an Emacs Lisp file relative to another
(presumably currently running) Emacs Lisp file. For example suppose
you have Emacs Lisp files "foo.el" and "bar.el" in the same
directory. To load "bar.el" from inside Emacs lisp file "foo.el":

    (require 'load-relative)
    (load-relative "baz")

The above `load-relative' line could above have also been written as:

    (load-relative "./baz")
or:
    (load-relative "baz.el")  # if you want to exclude any byte-compiled files

Use `require-relative' if you want to `require' the file instead of
`load'ing it:

   (require-relative "baz")

or:

   (require-relative "./baz")

The above not only does a `require' on 'baz', but makes sure you
get that from the same file as you would have if you had issued
`load_relative'.

Use `require-relative-list' when you have a list of files you want
to `require'. To `require-relative' them all in one shot:

    (require-relative-list '("dbgr-init" "dbgr-fringe"))

Finally, macro `provide-me' saves you the trouble of adding a
symbol after `provide' using the file basename (without directory
or file extension) as the name of the thing you want to
provide.

Using this constrains the `provide' name to be the same as
the filename, but I consider that a good thing.
