View CHM file.

This package is view CHM file in Emacs.
This package use `archmage' decompress
CHM file and view in browser.

Below are commands you can use:

     `chm-view-file'         View CHM file.
     `chm-view-dired'        View dired marked CHM files.


Installation:

This package is base on `archmage', so make sure
`archmage' have install in your system, like me
(I use Debian), install `archmage':

     sudo aptitude install archmage -y

And then put chm-view.el to your load-path.
The load-path is usually ~/elisp/.
It's set in your ~/.emacs like this:
(add-to-list 'load-path (expand-file-name "~/elisp"))

And the following to your ~/.emacs startup file.

(require 'chm-view)

That's all, enjoy! :)
