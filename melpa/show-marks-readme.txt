Commentary:

* Commentary
This library provides the commands to navigate and display the mark ring.
The `show-marks' command displays a buffer listing the marks in the buffer from which it was called.
You can then press enter on one of the listed marks to jump to it, or press d to delete it from the
mark ring. You can also use the `forward-mark' and `backward-mark' commands to navigate the marks in
the mark ring.

Installation:

Put show-marks.el in a directory in your load-path, e.g. ~/.emacs.d/
You can add a directory to your load-path with the following line in ~/.emacs
(add-to-list 'load-path (expand-file-name "~/elisp"))
where ~/elisp is the directory you want to add
(you don't need to do this for ~/.emacs.d - it's added by default).

Add the following to your ~/.emacs startup file.

(require 'show-marks)

 I recommend also binding the commands to global keys, e.g:

   (global-set-key (kbd "<C-s-right>") 'forward-mark)
   (global-set-key (kbd "<C-s-left>") 'backward-mark)
   (global-set-key (kbd "<C-s-down>") 'show-marks)
