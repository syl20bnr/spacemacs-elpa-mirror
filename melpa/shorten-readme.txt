This is a component-wise string shortener, meaning that, given a list
of strings, it breaks each string into parts, then computes shortest
prefix of each part with respect to others of the same 'depth', such
that when joined back together, the shortened form of the whole string
remains unique within the resulting list.  Many styles of shortening
are made possible via three functions that the caller may provide: the
split function, the join function, and the validate-component function.

Strings are broken with the value of `shorten-split-function' (a
procedure string->list), and shortened components are rejoined with the
value of `shorten-join-function' (a procedure list->string[*]).  The
default split and join functions break the string on word boundaries,
and rejoin on the empty string.  Potential shortened forms of
components are tested with `shorten-validate-component-function'; its
default value passes only if its argument contains at least one
word-constituent character (regexp \w), meaning that by default,
components consisting entirely of non-word characters will not be
shortened, and components that start with non-word characters will only
be shortened so much that they have at least one word-constituent
character in them.

The main entry point is `shorten-strings', which takes a list of strings
as its argument and returns an alist ((STRING . SHORTENED-STRING) ...).

[*] Also takes a second argument; see docstring of
`shorten-join-function'.
