inf-mongo.el provides a REPL buffer connected to a MongoDB shell
(mongo) subprocess.

Install

$ cd ~/.emacs.d/vendor
$ git clone git://github.com/endofunky/inf-mongo.git

In your emacs config:

(add-to-list 'load-path "~/.emacs.d/vendor/inf-mongo")
(require 'inf-mongo)

Usage

Run with `M-x inf-mongo'
