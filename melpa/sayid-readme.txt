Sayid is a debugger for clojure.  This package, sayid.el, is a client
for the sayid nrepl middleware.

To enable, use something like this:

(eval-after-load 'clojure-mode
  '(sayid-setup-package))
