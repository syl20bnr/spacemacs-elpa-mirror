Major mode for editing Apache Aurora configuration files.  It's derived from
Python mode adding Aurora and Pystachio helpers, structs and types for
syntax coloring along with some Aurora inspection commands and key bindings.

Provides side effect free Aurora client commands (currently `inspect' and
`diff') to test the results of changes.

- C-c a i  runs `aurora inspect <jobpath> <config>' (`aurora-config-inspect')
- C-c a d  runs `aurora diff <jobpath> <config>' (`aurora-config-inspect')
