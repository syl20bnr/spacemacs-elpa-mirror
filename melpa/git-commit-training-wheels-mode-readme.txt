Helps you craft well formed commit messages with git-commit-mode
Directives for what makes a well formed commit come from
tpope: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html

Based in part on https://github.com/re5et/magit-commit-training-wheels

Usage:

  (require 'git-commit-training-wheels-mode) ;; Not necessary if using ELPA package
  (add-hook 'git-commit-mode-hook 'git-commit-training-wheels-mode)
