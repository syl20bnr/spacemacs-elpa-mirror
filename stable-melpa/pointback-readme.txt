When you have two windows X and Y showing different sections of the
same buffer B, then switch to a different buffer in X, and then
show B in X again, the new point in X will be the same as in Y.
With pointback-mode, window points are preserved instead, and point
will be where it originally was in X for B when you return to B.

Use M-x pointback-mode RET to enable pointback-mode for a buffer.
Use M-x global-pointback-mode RET to enable it for all buffers.
