Soon after finding a file, the `judge-indent-mode' detects indent style
of the file and then it changes the behavior of your Emacs to follow
the detected indent style.  It makes it possible to write your own code
into another person's or your team's program without breaking the existing
indent style.

The detection method is described as follows.  First, one-tab, two-space,
four-space and eight-space indents at the beginning of all the lines are
counted.  Second, the numbers of counted indents are compared.
Finally, the indent style is chosen among the following pairs of indent
and tab widths.

      \  Indent
       \  2 4 8
    Tab \------
      2 | U
      4 | X U
      8 | X X U <- It cannot distinguish three `U's.
      - | X X X
