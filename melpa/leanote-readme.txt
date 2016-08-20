The leanote package is a minor mode for writing note in markdown file.
Before use this package, please spend a few minutes to learn leanote, which
is an open-source platform https://github.com/leanote/leanote. The leanote
office provide note server, android & ios apps.

The leanote package provides follwoing features:
* M-x leanote-login ------ login to server.
* M-x leanote-sync  ------ sync all notes from server to local.
* M-x leanote-push  ------ push current note to remote server (include create new).
* M-x leanote-pull  ------ pull(update) current note from server.
* M-x leanote-find  ------ find all notes for current account.
* M-x leanote-delete ----- delete current note

Here is hot-keys
C-x C-l u --- push/create note to remote server.
C-x C-l r --- rename note
C-x C-l f --- leanote-find
C-x C-l o --- leanote pull, force update from remote

Add hook to markdown file
(add-hook 'markdown-mode-hook 'leanote)

Optional

Install leanote status in mode line if you have installed spaceline
M-x leanote-spaceline-status
