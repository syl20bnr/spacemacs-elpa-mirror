Add syntax-based indentation when editing SQL code: TAB indents the current
line based on the syntax of the SQL code on previous lines.
`sqlind-minor-mode' is a minor mode that enables/disables this
functionality. To setup syntax-based indentation for every SQL buffer, add
`sqlind-minor-mode' to `sql-mode-hook'.

The package also defines align rules so that the `align' function works for
SQL statements, see `sqlind-align-rules'.

Indentation rules can be customized, for more information, see the
"README.md" file or the doc strings for `sqlind-basic-offset',
`sqlind-default-indentation-offsets-alist' and
`sqlind-indentation-syntax-symbols'.