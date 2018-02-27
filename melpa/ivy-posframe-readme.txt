* ivy-posframe README                                :README:

** What is ivy-posframe
ivy-posframe is a ivy extension, which let ivy use posframe
to show its candidate menu.

NOTE: ivy-posframe requires Emacs 26

** Display functions

1. ivy-posframe-display
2. ivy-posframe-display-at-frame-center
3. ivy-posframe-display-at-window-center
4. ivy-posframe-display-at-frame-buttom-left
5. ivy-posframe-display-at-window-buttom-left
6. ivy-posframe-display-at-point

** How to enable ivy-posframe
1. Global mode
   #+BEGIN_EXAMPLE
   (require 'ivy-posframe)
   (setq ivy-display-function #'ivy-posframe-display)
   ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-center)
   ;; (setq ivy-display-function #'ivy-posframe-display-at-window-center)
   ;; (setq ivy-display-function #'ivy-posframe-display-at-frame-buttom-left)
   ;; (setq ivy-display-function #'ivy-posframe-display-at-window-buttom-left)
   ;; (setq ivy-display-function #'ivy-posframe-display-at-point)
   #+END_EXAMPLE
2. Per-command mode.
   #+BEGIN_EXAMPLE
   (require 'ivy-posframe)
   ;; Different command can use different display function.
   (push '(counsel-M-x . ivy-posframe-display-at-window-buttom-left) ivy-display-functions-alist)
   (push '(complete-symbol . ivy-posframe-display-at-point) ivy-display-functions-alist)
   #+END_EXAMPLE
3. Fallback mode
   #+BEGIN_EXAMPLE
   (require 'ivy-posframe)
   (push '(t . ivy-posframe-display) ivy-display-functions-alist)
   #+END_EXAMPLE

If you use `ivy-posframe-display', you can use `ivy-posframe-style'
to set show style.

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
