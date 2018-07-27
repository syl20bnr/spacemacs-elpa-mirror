Dumb indentation mode is appropriate for editing buffers that Emacs
does not fully understand syntactically, such as HTML/PHP
(typically involving multiple languages with different indentation
rules in the same buffer).  The default indentation level is 2
(customize `stupid-indent-level').

Key bindings:

TAB       -- indent current line by the value of `stupid-indent-level'
S-TAB     -- outdent current line
C-c TAB   -- indent region
C-c S-TAB -- outdent region
RET       -- newline and indent
C-c C-TAB -- indent according to mode
