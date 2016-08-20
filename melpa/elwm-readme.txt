See github readme at https://github.com/Fuco1/elwm

This package works great in combination with `golden-ratio' package:
https://github.com/roman/golden-ratio.el

Layouts:

- tile, vertical, left

+-----------+-------------+
|           |      1      |
|           +-------------+
|  master   |      2      |
|           +-------------+
|           |      3      |
+-----------+-------------+

- tile, horizontal, top

+-------+---------+-------+
|   1   |    2    |   3   |
|       |         |       |
+-------+---------+-------+
|                         |
|         master          |
+-------------------------+

Windows in the master area can be again split in the same direction
as the stack windows, but only in one column.
