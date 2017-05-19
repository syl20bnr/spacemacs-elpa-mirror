C-x 7 <command> causes a buffer displayed by <command>
to appear in another window in the same frame; a window
is created if necessary.

C-x 9 <command> causes a buffer displayed by <command>
to appear in another frame; a frame is created if necessary.

C-x W moves the current buffer to another window in the same frame.

C-x F moves the current buffer to another frame.

In addition, C-x 7 and C-x 9 can be followed by these keys:

0 - deletes the current window.

1 - deletes the other windows/frames.

2 - shows another view of the current buffer in a new
    window/frame.

a - creates a commit log entry for the current defun in
    another window/frame.

b - switches to another buffer in another window/frame.

d - start dired in another window/frame.

f - find-file in another window/frame.

m - compose mail in another window/frame.

o - move to another window/frame.

r - find-file-read-only in another window/frame.

To extend this list, add key bindings to ‘ofw-transient-map’.