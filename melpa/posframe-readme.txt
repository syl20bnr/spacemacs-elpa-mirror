* Posframe README                                :README:
** What is posframe
Posframe can pop a posframe at point, this *posframe* is a
child-frame with its root window's buffer.

The main advantages are:
1. It is fast enough for daily usage :-)
2. It works well with CJK language.

NOTE: For MacOS users, posframe need Emacs (version >= 26.0.91)

[[./snapshots/posframe-1.png]]

** Installation

#+BEGIN_EXAMPLE
(require 'posframe)
#+END_EXAMPLE

** Usage

*** Create a posframe

#+BEGIN_EXAMPLE
(posframe-show " *my-posframe-buffer*"
               :string "This is a test"
               :position (point))
#+END_EXAMPLE

Arguments documents:
#+BEGIN_EXAMPLE
C-h f posframe-show
#+END_EXAMPLE

*** Hide a posframe
#+BEGIN_EXAMPLE
(posframe-hide " *my-posframe-buffer*")
#+END_EXAMPLE

*** Hide all posframes
#+BEGIN_EXAMPLE
M-x posframe-hide-all
#+END_EXAMPLE

*** Delete a posframe
1. Delete posframe and its buffer
   #+BEGIN_EXAMPLE
   (posframe-delete " *my-posframe-buffer*")
   #+END_EXAMPLE
2. Only delete posframe's frame
   #+BEGIN_EXAMPLE
   (posframe-delete-frame " *my-posframe-buffer*")
   #+END_EXAMPLE
*** Delete all posframes
#+BEGIN_EXAMPLE
M-x posframe-delete-all
#+END_EXAMPLE

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.
