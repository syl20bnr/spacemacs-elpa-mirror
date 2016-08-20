* Usage

   (require 'switch-window)
   (global-set-key (kbd "C-x o") 'switch-window)

* Changelog

0.11 - 2013-09-14

 - restore point to end-of-buffer for windows where it was the case after
   switching, fixing an anoying bug.

0.10 - 2011-06-19

 - implement M-x delete-other-window (thanks developernotes on github)

0.9 - 2010-11-11 - emacs22 called, it wants some support

 - implement a propertize based hack to support emacs22

0.8 - 2010-09-13 - 999

 - Suport more than 9 windows (with a single key to type)
 - Use quail-keyboard-layout to choose single key labels for windows

0.7 - 2010-08-23 - window-dedicated-p

 - temporarily unset the window dedicated flag for displaying the
   numbers, patch from René Kyllingstad <Rene@Kyllingstad.com>
 - fix timeout and RET handling wrt to not changing window selection

0.6 - 2010-08-12 - *Minibuf-1*

 - add support for selecting the minibuffer when it's active
 - some try at a better horizontal centering
 - assorted cleanup

0.5 - 2010-08-08 - Polishing

 - dim:switch-window-increase is now a maximum value
