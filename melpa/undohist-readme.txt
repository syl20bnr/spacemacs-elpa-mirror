This extension provides a way to use undo history of
individual file buffers persistently.

Write the following code to your .emacs:

(require 'undohist)
(undohist-initialize)

Now you can record and recover undohist by typing
C-x C-s (save-buffer) an C-x C-f (find-file).
And then type C-/ (undo).
