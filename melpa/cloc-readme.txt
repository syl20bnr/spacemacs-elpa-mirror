This is a small attempt at cloc integration for Emacs. The functionality is
exposed through two functions: cloc, an interactive function which performs
a search through all buffers whose filepaths match the given regex (or the
current buffer, as desired), and counts lines of code in them. It also
exposes cloc-get-results-as-plists, a non-interactive function which does
the same thing, but parses and organizes it all into a list of plists for
easier analysis.

cloc will search over all buffers, including those which do not visit files,
and tramp buffers, but if the buffer is not visiting a file (and therefore
does not have a pathname), cloc will only be able to match the regex to the
buffer's buffer-name.

Example searches include: "\.cpp$", for all C++ sources files, or "/foo/",
where "/foo/" is the name of a project directory; cloc will then count all
code over all open buffers visiting files within a directory named foo.
