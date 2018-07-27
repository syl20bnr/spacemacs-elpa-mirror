This file provides a minor mode to broadcast edits and navigation in
one buffer to one or more other buffers.  This is similar to the idea
of multiple cursors, but takes place across multiple buffers.  To use
it, place two or more buffers in broadcast-mode with

M-x broadcast-mode

This links those buffers together so that edits, cursor navigation,
and even kill-ring operations made in one of the broadcast mode
buffers are replicated in the other buffers.  If a broadcast-mode
buffer is not visible, that is to say "buried," and not currently
displayed by any window, then it is not affected by actions performed
in other broadcast mode buffers.  Thus you can only edit what you can
see.

The Kill Ring
-------------
The kill rings in broadcast-mode buffers are independent, so each
buffer can kill and yank independent text.  At the same time, kill ring
operations are shared among buffers where it makes sense, allowing you
to kill in a non-broadcast buffer and yank into a broadcast buffer and
vice versa.  When killing in a broadcast buffer, each broadcast buffer
may place something different on their kill ring.  The text that is killed
in the primary broadcast buffer, (then one with focus), will also be placed
on the main kill ring for non-broadcast buffers.

Undo Boundaries
---------------
In order to synchronize undo behavior between linked buffers, an undo boundary
is placed after every command.  This can mean undoing can take a little longer
if you're going back very far.
