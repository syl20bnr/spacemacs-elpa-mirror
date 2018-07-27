WYSIWYG, html mime composition using org-mode

For mail composed using the orgstruct-mode minor mode, this
provides a function for converting all or part of your mail buffer
to embedded html as exported by org-mode.  Call `org-mime-htmlize'
in a message buffer to convert either the active region or the
entire buffer to html.

Similarly the `org-mime-org-buffer-htmlize' function can be called
from within an org-mode buffer to convert the buffer to html, and
package the results into an email handling with appropriate MIME
encoding.

`org-mime-org-subtree-htmlize' is similar to `org-mime-org-buffer-htmlize'
but works on a subtree. It can also read the following subtree properties:
MAIL_SUBJECT, MAIL_TO, MAIL_CC, and MAIL_BCC. Note the behavior of this is
controlled by `org-mime-up-subtree-heading'. The default is to go up to the
heading containing the current subtree.

Here is the sample of a subtree:

* mail one
  :PROPERTIES:
  :MAIL_SUBJECT: mail title
  :MAIL_TO: person1@gmail.com
  :MAIL_CC: person2@gmail.com
  :MAIL_BCC: person3@gmail.com
  :END:

To avoid exporting the table of contents, you can setup
`org-mime-export-options':
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))

Or just setup your export options in the org buffer/subtree. These are
overridden by `org-mime-export-options' when it is non-nil.


Quick start:
Write a message in message-mode, make sure the mail body follows
org format. Before sending mail, run `M-x org-mime-htmlize' to convert the
body to html, then send the mail as usual.

Setup (OPTIONAL):
You might want to bind this to a key with something like the
following message-mode binding

  (add-hook 'message-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)))

and the following org-mode binding

  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)))

Extra Tips:
1. In order to embed images into your mail, use the syntax below,
[[/full/path/to/your.jpg]]

2. It's easy to add your own emphasis markup. For example, to render text
between "@" in a red color, you can add a function to `org-mime-html-hook':

  (add-hook 'org-mime-html-hook
            (lambda ()
              (while (re-search-forward "@\\([^@]*\\)@" nil t)
                (replace-match "<span style=\"color:red\">\\1</span>"))))

3. Now the quoted mail uses a modern style (like Gmail), so mail replies
looks clean and modern. If you prefer the old style, please set
`org-mime-beautify-quoted-mail' to nil.
