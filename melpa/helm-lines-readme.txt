A helm interface for completing by lines in project.

Run `helm-lines' to complete the current line by other lines that
exist in the current git project.

Enable `helm-follow-mode' to replace in place as you jump around
the suggestions.

Requires `ag' [1] to be installed.

  [1]: https://github.com/ggreer/the_silver_searcher

The project root is defined by the results of
`helm-lines-project-root-function', and by default it is the root
of the current version-control tree.  Projectile users might like
to set this variable to `projectile-root'.
