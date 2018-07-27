This library provides a method for quickly calculating how many
lines in a given project. It is inspired by `find-file-in-project'.

It depends on the command `find', `wc', `rev' and `sort'.
find . -type f \( -name "*.el" -or -name "*.elc" \) -not -regex ".*/elpa/.*"

Installation
It is recommended installed by the ELPA package system.
You could install it by M-x: with
package-install: how-many-lines-in-project.

Usage
M-x: how-many-lines-in-project

There are some variables you may need to config.
`hm-lines-file-extensions'
`hm-lines-sort-by-type'
`hm-lines-find-regex'
