In this file we define `mu4e-conversation' (+ helper functions), which is
used for viewing all e-mail messages of a thread in a single buffer.

From the headers view, run the command `mu4e-conversation'.  Call
`mu4e-conversation-toggle-view' (bound to "V" by default) to switch between
linear and tree view.

To fully replace `mu4e-view' with `mu4e-conversation' from any other command
(e.g. `mu4e-headers-next', `helm-mu'), call

  (setq mu4e-view-func 'mu4e-conversation)
