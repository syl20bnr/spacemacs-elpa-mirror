Linked-buffers enables simultaneous editing and viewing of the same (or
closely related) text in two or more buffers, potentially in different modes.

Sometimes, it would be nice to edit a file in two ways at once. For instance,
you might have a source file in a computational language with richly marked
documentation. As Emacs is a modal editor, it would be nice to edit this file
both in a mode for the computational language and for the marked up
documentation.

One solution to this is to use a single-mode which supports both types of
editing. The problem with this is that it is fundamentally difficult to
support two types of editing at the same time; more over, you need a new mode
for environment.

Another solution is to use one of the multiple-mode tools which are available.
The problem with this is that they generally need some support from the modes
in question. And, again, the difficulty is supporting both forms of editing in
the same environment.

Linked buffers provide an alternative solution. Two linked buffers, by
default, the two share content but are otherwise independent. Therefore,
you can have two buffers open, each showing the content in different modes;
to switch modes, you simply switch buffers. The content, location of point,
and view are shared.

However, linked-buffers can also perform a bi-directional transformation
between the two. If this is done, then the two can have different but related
text. It is possible to configure the transformation for any two buffers in a
extensible way, although mostly we have concentrated on mode-specific
configuration.

The main user entry point is through `global-linked-buffer-start-mode' which
provides tools to create new a new linked-buffer.

Configuration:

linked-buffers are configurable in a large number of ways. It is possible
to control the nature of the transformation, the default buffer name that a
linked-buffer takes, and the file location (or not) of the linked-buffer.
For this release of linked-buffer currently, each buffer can only be linked
to a single buffer, although this restriction will be removed in later
versions.

Configuration of a buffer happens in one of two places. First,
`linked-buffer-init' is run when a linked-buffer is first created. This
function should set the actual configuration `linked-buffer-config', and is
mostly designed for use as a file-local or dir-local variable. All subsequent
configuration happens through `linked-buffer-config' which is an EIEIO object
and associated methods.

There are now a number of different configurations, which can be used for
general-purpose use as well as an extension points for subclass
configurations. The two most general configurations are:

 - default: this copies all text exactly, but does not transfer
   text-properties (which is the behaviour of indirect buffers). It is
   possible to configure the default file or mode on a per-object basis.
 - block: this is designed for programmatic syntaxes where blocks of code are
   demarcated by start and end tags, and everything else is commented by
   line-start comments. Comments are added or removed between the two buffers.

The second of these is extended in linked-buffer-org.el to provide the
configuration for this file: there is a normal emacs-lisp file in one buffer
and an org-mode version in another. Other programmatic and documentation modes
are supported in other files.
