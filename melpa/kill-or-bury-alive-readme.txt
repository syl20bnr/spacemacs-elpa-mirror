Have you ever killed some buffer that you might want to leave alive?
Motivation for killing is usually “get out of my way for now”, and
killing may be not the best choice in many cases unless your RAM is
very-very limited.  This package allows to teach Emacs which buffers we
want to kill and which ones we prefer to bury alive.

When we really want to kill a buffer, it turns out that not all buffers
would like to die the same way.  The package allows to specify *how* to
kill various kinds of buffers.  This may be especially useful when you're
working with some buffer that has an associated process, for example.

But sometimes you may want to get rid of most buffers and bring Emacs to
some more or less virgin state.  You probably don't want to kill scratch
buffer and maybe ERC-related buffers too.  You can specify which buffers
to purge.
