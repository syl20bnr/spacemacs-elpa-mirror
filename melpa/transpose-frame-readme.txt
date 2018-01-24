This program provides some interactive functions which allows users
to transpose windows arrangement in currently selected frame:

`transpose-frame'  ...  Swap x-direction and y-direction

       +------------+------------+      +----------------+--------+
       |            |     B      |      |        A       |        |
       |     A      +------------+      |                |        |
       |            |     C      |  =>  +--------+-------+   D    |
       +------------+------------+      |   B    |   C   |        |
       |            D            |      |        |       |        |
       +-------------------------+      +--------+-------+--------+

`flip-frame'  ...  Flip vertically

       +------------+------------+      +------------+------------+
       |            |     B      |      |            D            |
       |     A      +------------+      +------------+------------+
       |            |     C      |  =>  |            |     C      |
       +------------+------------+      |     A      +------------+
       |            D            |      |            |     B      |
       +-------------------------+      +------------+------------+

`flop-frame'  ...  Flop horizontally

       +------------+------------+      +------------+------------+
       |            |     B      |      |     B      |            |
       |     A      +------------+      +------------+     A      |
       |            |     C      |  =>  |     C      |            |
       +------------+------------+      +------------+------------+
       |            D            |      |            D            |
       +-------------------------+      +-------------------------+

`rotate-frame'  ...  Rotate 180 degrees

       +------------+------------+      +-------------------------+
       |            |     B      |      |            D            |
       |     A      +------------+      +------------+------------+
       |            |     C      |  =>  |     C      |            |
       +------------+------------+      +------------+     A      |
       |            D            |      |     B      |            |
       +-------------------------+      +------------+------------+

`rotate-frame-clockwise'  ...  Rotate 90 degrees clockwise

       +------------+------------+      +-------+-----------------+
       |            |     B      |      |       |        A        |
       |     A      +------------+      |       |                 |
       |            |     C      |  =>  |   D   +--------+--------+
       +------------+------------+      |       |   B    |   C    |
       |            D            |      |       |        |        |
       +-------------------------+      +-------+--------+--------+

`rotate-frame-anticlockwise'  ...  Rotate 90 degrees anti-clockwise

       +------------+------------+      +--------+--------+-------+
       |            |     B      |      |   B    |   C    |       |
       |     A      +------------+      |        |        |       |
       |            |     C      |  =>  +--------+--------+   D   |
       +------------+------------+      |        A        |       |
       |            D            |      |                 |       |
       +-------------------------+      +-----------------+-------+

This program is tested on GNU Emacs 22, 23.


Installation:

First, save this file as transpose-frame.el and byte-compile in a directory
that is listed in load-path.

Put the following in your .emacs file:

  (require 'transpose-frame)

To swap x-direction and y-direction of windows arrangement, for example,
just type as:

  M-x transpose-frame

Have fun!
