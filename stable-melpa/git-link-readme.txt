Create URLs for files and commits in GitHub/Bitbucket/GitLab/...
repositories.  `git-link' returns the URL for the current buffer's file
location at the current line number or active region.  `git-link-commit'
returns the URL for a commit.  URLs are added to the kill ring.

With a prefix argument prompt for the remote's name.  Defaults to "origin".
