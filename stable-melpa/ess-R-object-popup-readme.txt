I have defined a function, ess-R-object-popup, that when
invoked, will return a popup with some information about
the object at point.  The information returned is
determined by which R function is called.  This is controlled
by an alist, called ess-R-object-popup-alist.  The default is
given below.  The keys are the classes of R object that will
use the associated function.  For example, when the function
is called while point is on a factor object, a table of that
factor will be shown in the popup.  The objects must of course
exist in the associated inferior R process for this to work.
The special key "other" in the alist defines which function
to call when the class is not mached in the alist.  By default,
the str function is called, which is actually a fairly useful
default for data.frame and function objects.

simply save this file in a directory in my load-path
and then
place
   (require 'ess-R-object-popup)
   (define-key ess-mode-map "\C-c\C-g" 'ess-R-object-popup)

in yout init file.
