* Posframe README                                :README:
** What is posframe
Posframe can pop a posframe at point, this *posframe* is a
child-frame with its root window's buffer.

The main advantages are:
1. It is very fast, `posframe-show' is faster than `popup-tip'
   of popup.el.
2. It works well with CJK language.

NOTE: For MacOS users, posframe need emacs (version >= 26.0.91)

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

Addition arguments:
1. :position, set the position when posframe is poped up.
2. :background-color, set posframe's background color.
3. :foreground-color, set posframe's foreground color.
4. :margin-left, set posframe's left margin width.
5. :margin-right, set posframe's right margin width.
6. :override-parameters, User can use it to override
   *all* the frame parameters of posframe's child-frame.

*** Hide a posframe
#+BEGIN_EXAMPLE
(posframe-hide " *my-posframe-buffer*")
#+END_EXAMPLE

*** Hide all posframes
#+BEGIN_EXAMPLE
M-x posframe-hide-all
#+END_EXAMPLE

*** Delete a posframe
#+BEGIN_EXAMPLE
(posframe-delete " *my-posframe-buffer*")
#+END_EXAMPLE

*** Delete all posframes
#+BEGIN_EXAMPLE
M-x posframe-delete-all
#+END_EXAMPLE

Note: this command will delete all posframe buffers,
suggest not run this command if you are sharing a buffer
between posframe and other packages.
