  When making demonstrations of new products, technologies and other
  geekery, I love the versatility of using Emacs to demonstrate the
  trifecta of sprint reviews, including:

  - Presentations explaining the technologies
  - Source code ... correctly highlighted
  - Executing the code in Eshell ... or similar demonstration
  - Test a new feature

  However, I don't want to fat-finger, mentally burp, or even delay
  the gratification while I type, so I predefine each "step" as an
  Elisp function or keyboard macro, and then have =demo-it= execute
  each function when I hit either the SPACE key or the F12 key
  (advanced minor mode).

  Using the library is a three step process:

  1. Load the library in your own Elisp source code file
  2. Create a collection of functions that "do things".
  3. Call the =demo-it-start= function with the ordered list of
     functions.

  For instance:

  (load-library "demo-it")   ;; Load this library of functions

  (defun my-demo/step-1 ()
    (delete-other-windows)
    (demo/org-presentation "~/presentations/my-demo/demo-start.org"))

  (defun my-demo/step-2 ()
    (demo-it-load-file "~/Work/my-proj/src/my-proj.py")
    (demo-it-presentation-return))

  (defun my-demo ()
     "My fabulous demonstration."
     (interactive)
     (demo-it-start (list
                     'my-demo/step-1
                     'my-demo/step-2
                     ;; ...
                   )))

  (my-demo) ;; Optionally start the demo when file is loaded.

  Each "step" is a series of Elisp functions that "do things".
  While this package has a collection of helping functions, the steps
  can use any Elisp command to show off a feature.

  I recommend installing these other Emacs packages:

  - https://github.com/takaxp/org-tree-slide
  - https://github.com/sabof/org-bullets
  - https://github.com/magnars/expand-region.el
  - https://github.com/Bruce-Connor/fancy-narrow

  See http://github.com/howardabrams/demo-it for more details and
  better examples.  You will want to walk through the source code
  for all the utility functions.
