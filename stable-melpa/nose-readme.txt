This gives a bunch of functions that handle running nosetests on a
particular buffer or part of a buffer.

Installation

In your emacs config:

(require 'nose)
next line only for people with non-eco non-global test runners
(add-to-list 'nose-project-names "my/crazy/runner")

Note that if your global nose isn't called "nosetests", then you'll want to
redefine nose-global-name to be the command that should be used.

By default, the root of a project is found by looking for any of the files
'setup.py', '.hg' and '.git'. You can add files to check for to the file
list:

(add-to-list 'nose-project-root-files "something")

or you can change the project root test to detect in some other way
whether a directory is the project root:

(setq nose-project-root-test (lambda (dirname) (equal dirname "foo")))

If you want dots as output, rather than the verbose output:
(defvar nose-use-verbose nil) ; default is t

nose.el adds a minor mode called 'nose' that's currently only used to
manage keybindings and provide a hook for changing the behaviour of
the nose output buffer.

This is the recommended way to activate nose keybindings when viewing
Python files:

(add-hook 'python-mode-hook (lambda () (nose-mode t)))

nose-mode is also activated when nose displays the buffer that shows
the output of nosetests.

Code like that given below can be used to change nose.el's keybindings.
The bindings shown are nose.el's default bindings. If you wish to use
these bindings, then you don't need to include this code in .emacs

(define-key nose-mode-map "\C-ca" 'nosetests-all)
(define-key nose-mode-map "\C-cm" 'nosetests-module)
(define-key nose-mode-map "\C-c." 'nosetests-one)
(define-key nose-mode-map "\C-cc" 'nosetests-again)
(define-key nose-mode-map "\C-cpa" 'nosetests-pdb-all)
(define-key nose-mode-map "\C-cpm" 'nosetests-pdb-module)
(define-key nose-mode-map "\C-cp." 'nosetests-pdb-one)
