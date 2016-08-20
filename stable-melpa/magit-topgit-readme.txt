This package provides very basic support for TopGit.

  TopGit is a patch queue manager that aims to make handling
  of large amounts of interdependent topic branches easier.

For information about TopGit see https://github.com/greenrd/topgit.

When `magit-topgit-mode' is turned on then the list of TopGit
topics is displayed in the status buffer.  While point is on such
a topic it can checked out using `RET' and discarded using `k'.
Other TopGit commands are available from the TopGit popup on `T'.

To enable the mode in a particular repository use:

  cd /path/to/repository
  git config --add magit.extension topgit

To enable the mode for all repositories use:

  git config --global --add magit.extension topgit

To enable the mode globally without dropping to a shell:

  (add-hook 'magit-mode-hook 'magit-topgit-mode)
