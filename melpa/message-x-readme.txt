The most recent version of this can always be fetched from the
following FTP site:
ls6-ftp.cs.uni-dortmund.de:/pub/src/emacs

Installation:

You must be using Gnus 5 or higher for this to work.  Installation
is simple: just put this file somewhere in your load-path, run M-x
byte-compile-file RET, and put the following line in your .gnus file:

     (require 'message-x)

Customization is possible through the two variables
message-x-body-function and message-x-completion-alist, which see.

Purpose:

This assigns a context-sensitive function to the TAB key in message
mode of Gnus.  When in a header line, this performs completion
based on which header we're in (for example, newsgroup name
completion makes sense in the Newsgroups header whereas mail alias
expansion makes sense in the To and Cc headers).  When in the
message body, this executes a different function, by default it is
indent-relative.

To be more precise, the mechanism is as follows.  When point is in
a known header (a header mentioned in
`message-x-completion-alist'), then the completion function thus
specified is executed.  For the To and Cc headers, this could be
`bbdb-complete-name', for example.  Then we look if the completion
function has done anything.  If the completion function has NOT
done anything, then we invoke the function specified by
`message-x-unknown-header-function'.

When point is in an unknown header (not mentioned in
`message-x-completion-alist'), then we invoke the function
specified by `message-x-unknown-header-function'.  This function
could advance point to the next header, for example.  (In fact,
that's the default behavior.)

When point is not in a header (but in the body), then we invoke the
function specified by `message-x-body-function'.  By default, this
is `indent-relative' -- the default indentation function for text
mode.
