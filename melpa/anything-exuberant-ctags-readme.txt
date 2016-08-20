This package use `anything' as a interface to find tag with Exuberant ctags.

Follow command is `anything' interface of find ctags.

`anything-exuberant-ctags-select'
`anything-exuberant-ctags-select-from-here'

If you want to use tag file in cache directory
instead current directory, example you have a
tag file in directory "~/MyEmacs/", and you want
always search tag in this directory.
You can setup like below:

(setq anything-exuberant-ctags-enable-tag-file-dir-cache t)
(setq anything-exuberant-ctags-cache-tag-file-dir "~/MyEmacs/")

Installation:

Put anything-exuberant-ctags.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'anything-exuberant-ctags)

It is good to use anything-match-plugin.el to narrow candidates.
http://www.emacswiki.org/cgi-bin/wiki/download/anything-match-plugin

In your project root directory, do follow command to make tags file.

ctags --verbose -R --fields="+afikKlmnsSzt"

No need more.
