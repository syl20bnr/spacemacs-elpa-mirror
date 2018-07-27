This program is an implementation of minesweeper for Emacs.

This game consists of a minefield.  There are squares with mines, and squares without mines.
The goal is to reveal all squares that do not have a mine.  If you reveal a mine, it explodes,
and the game is over!

To begin playing, call `M-x minesweeper`.

Use the arrow keys, p/b/n/f, or C-p/C-n/C-b/C-f to move around in the minefield.

To reveal squares, either left-click on them, or move point and press space, enter, or `x`.

When a square is revealed, if it is not a mine, there will be a number indicating the count of mines in the neighboring eight squares.
If a square with no neighboring mines is revealed, all its neighbors will also be revealed.

You can mark squares that you think have mines in them.  Marked squares are protected
from being revealed by any means.
To mark a square, right-click on it, or press `m`.  Marked squares can be unmarked the same way.

You can choose to reveal all the neighbors of a square by middle-clicking on a square, or moving point there and pressing `c`.

Keywords: game fun minesweeper inane diversion
