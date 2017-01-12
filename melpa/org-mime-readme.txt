WYSWYG, html mime composition using org-mode

For mail composed using the orgstruct-mode minor mode, this
provides a function for converting all or part of your mail buffer
to embedded html as exported by org-mode.  Call `org-mime-htmlize'
in a message buffer to convert either the active region or the
entire buffer to html.

Similarly the `org-mime-org-buffer-htmlize' function can be called
from within an org-mode buffer to convert the buffer to html, and
package the results into an email handling with appropriate MIME
encoding.

Quick start:
Write mail in message-mode, make sure the mail body follows org format.
Before sending mail, `M-x org-mime-htmlize'

Setup (OPTIONAL):
you might want to bind this to a key with something like the
following message-mode binding

  (add-hook 'message-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)))

and the following org-mode binding

  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)))
