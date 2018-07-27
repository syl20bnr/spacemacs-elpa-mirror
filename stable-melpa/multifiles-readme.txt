Bind a key to `mf/mirror-region-in-multifile`, let's say `C-!`. Now
mark a part of the buffer and press it. A new \*multifile\* buffer pops
up. Mark some other part of another file, and press `C-!` again. This
is added to the \*multifile\*.

You can now edit the \*multifile\* buffer, and watch the original files change.
Or you can edit the original files and watch the \*multifile\* buffer change.

**Warning** This API and functionality is highly volatile.
