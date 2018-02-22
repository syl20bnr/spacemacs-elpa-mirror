* ivy-posframe README                                :README:

** What is ivy-posframe
ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

NOTE: ivy-posframe requires Emacs 26

** How to enable ivy-posframe
1. Global mode
   #+BEGIN_EXAMPLE
   (require 'ivy-posframe)
   (setq ivy-display-function #'ivy-posframe-display)
   #+END_EXAMPLE
2. Per-command mode.
   #+BEGIN_EXAMPLE
   (require 'ivy-posframe)
   (push '(counsel-M-x . ivy-posframe-display) ivy-display-functions-alist)
   #+END_EXAMPLE
3. Fallback mode
   #+BEGIN_EXAMPLE
   (require 'ivy-posframe)
   (push '(t . ivy-posframe-display) ivy-display-functions-alist)
   #+END_EXAMPLE

** How to set the style of ivy-posframe
1. window-buttom-left style
   #+BEGIN_EXAMPLE
   (setq ivy-posframe-style 'window-buttom-left)
   #+END_EXAMPLE
   [[./snapshots/ivy-posframe1.gif]]
2. Window-center style
   #+BEGIN_EXAMPLE
   (setq ivy-posframe-style 'window-center)
   #+END_EXAMPLE
   [[./snapshots/ivy-posframe2.gif]]
3. Point style
   #+BEGIN_EXAMPLE
   (setq ivy-posframe-style 'point)
   #+END_EXAMPLE
   [[./snapshots/ivy-posframe3.gif]]
