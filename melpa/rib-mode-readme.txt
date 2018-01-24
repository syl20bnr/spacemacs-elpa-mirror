rib-mode is intended to be an (X)Emacs plugin in order to add
syntax highlighting and indentation automatically to the text
editing of RenderMan® Interface Bytestream (RIB) files that are in
ASCII.  The plugin code is in Lisp which is interpreted by the
Emacs environment upon application startup.

I was unable to find such a plugin on the web, yet I do know that
those using RenderMan tools also use Emacs editors.  For example,
there is a plugin for the RenderMan Shading Language (very similar
to the C language), but none for the scene description interface
known as RIB (a pascal-like language).  RIB is intended to be human
readable and editable to allow for the highest level of control of
your scenes without resorting to C programming.

References:
[1] Pixar, RenderMan Interface Specification 3.2.1
[2] Borton, S.A., "An Emacs language mode creation tutorial"
