Malinka is a project management Emacs package for C/C++

It uses rtags to help the user jump around the code easily and without the
mistaken tag jumping that other taggers frequently have with C/C++ code.
The main functionality of malinka is to properly populate and communicate the
compiler commands to the rtags daemons depending on the project you are working
on.

Optionally and if you also have flycheck with the clang syntax-checker activated
malinka will communicate to flycheck's clang syntax checker the appropriate
cpp-defines and include paths so that flycheck can do its syntax checking.

The way to define a project is by using `malinka-define-project' and to provide
the basic attributes that a project needs.  For more information you can read
the function's docstring and the readme file.  For a quick introduction you can
visit this blog post http://blog.refu.co/?p=1311
