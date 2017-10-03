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

`org-mime-org-subtree-htmlize' is similar to `org-mime-org-buffer-htmlize'
but works on subtree. It can also read subtree properties MAIL_SUBJECT,
MAIL_TO, MAIL_CC, and MAIL_BCC. Here is the sample of subtree:

* mail one
  :PROPERTIES:
  :MAIL_SUBJECT: mail title
  :MAIL_TO: person1@gmail.com
  :MAIL_CC: person2@gmail.com
  :MAIL_BCC: person3@gmail.com
  :END:

To avoid exporting TOC, you can setup `org-mime-export-options',
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil))
Or just setup your export options in org buffer/subtree which is overrided
by `org-mime-export-options' when it's NOT nil.

You can change `org-mime-up-subtree-heading' before exporting subtree.
heck its documentation.

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

Extra Tips:
1. In order to embed image into your mail, use below org syntax,
  [[/full/path/to/your.jpg]]

2. It's easy to add your own emphasis symbol.  For example, in order to render
text between "@" in red color, you can use `org-mime-html-hook':
  (add-hook 'org-mime-html-hook
            (lambda ()
              (while (re-search-forward "@\\([^@]*\\)@" nil t)
                (replace-match "<span style=\"color:red\">\\1</span>"))))

3. Since v0.0.8, the quoted mail use modern style (like Gmail). If you prefer
   the original style, please set `org-mime-beautify-quoted-mail' to nil.
