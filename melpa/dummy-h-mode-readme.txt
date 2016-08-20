The `dummy-h-mode' is a major mode for header files (.h)
of C, C++ and Objective-C.  It detects the suitable major mode
that is one of `c-mode', `c++-mode' and `objc-mode',
and then switches the buffer major mode to the detected major mode.
The detecting process is described as follows:

1. Check the existance of the corresponding source file (.c, .cc, etc.)
2. Search keywords in the file.
3. Check the filename extensions of all files in the directory.
