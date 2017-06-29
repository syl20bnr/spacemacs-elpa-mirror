A helm interface for git hunks - browsing, staging, unstaging and killing.

Enable `helm-follow-mode' and trigger `helm-hunks' and jump around
unstaged hunks like never before with `C-n' and `C-p'. Stage with `C-s'.

Run `helm-hunks-current-buffer' to jump around the current buffer only.

Run `helm-hunks-staged' to jump around staged hunks, unstage with `C-u'.

Run `helm-hunks-staged-current-buffer' to jump around staged hunks in
the current buffer only.

Kill hunks you wish undone with `C-k'.

Preview hunk changes with `C-c C-p', and jump to hunks in "other window"
or "other frame" with `C-c o' and `C-c C-o', respectively.

Commit with `C-c C-c`, amend with `C-c C-a`.

Quit with `C-c C-k'.

Credits/inspiration: git-gutter+ - https://github.com/nonsequitur/git-gutter-plus/
