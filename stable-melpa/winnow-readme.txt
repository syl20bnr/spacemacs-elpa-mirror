Winnow `compilation-mode' results by matching or excluding lines from the
results. Normally these buffers are `read-only-mode', preventing the use of
editing commands, but `winnow-mode' commands inhibits this to apply
`flush-line' or `keep-lines' on the command output.

As the edits are to the buffer, `recompile' is the recommended way to
regenerate the search.

The main use case for this is filtering `ag-mode' search results in the
buffer to separate the wheat from the chaff.
