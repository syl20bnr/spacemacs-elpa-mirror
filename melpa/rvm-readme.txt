M-x rvm-use-default prepares the current Emacs session to use
the default ruby configured with rvm.

M-x rvm-use allows you to switch the current session to the ruby
implementation of your choice. You can also change the active gemset.

Compiler support:

(eval-when-compile (require 'cl))
(defvar eshell-path-env)
(defvar persp-mode)
(defvar perspectives-hash)
(declare-function persp-switch "perspective" (name))
