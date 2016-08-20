This is a package for searching Packagist.org, a PHP library repository, from
within emacs.

To run a search, simply execute M-x emacsagist-search

TAB/Shift-TAB, n/p, and j/k all jump between links.

(require 'cl-lib)
(require 'cl)
(require 'json)
(require 'url)
