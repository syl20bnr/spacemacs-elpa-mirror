Major mode for editing docker-compose files, providing context-aware
completion of docker-compose keys through completion-at-point-functions.

The completions can be used with the completion system shipped with vanilla
Emacs, and 3rd-party frontends like company-mode, autocomplete, and
ido-at-point.

By default, the keyword completion function detects the docker-compose
version of the current buffer and suggests the appropriate keywords.

See the README for more details.
