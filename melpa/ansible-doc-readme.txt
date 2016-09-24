Ansible documentation for GNU Emacs.

Provide `ansible-doc' to view the documentation of an Ansible module in
Emacs.

Additionally provide `ansible-doc-mode' minor mode to add documentation
lookup to YAML Mode.  Enable with:

(add-hook 'yaml-mode-hook #'ansible-doc-mode)
