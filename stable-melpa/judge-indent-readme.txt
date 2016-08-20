The `judge-indent-mode' detects, soon after finding a file,
indent and tab widths as one of the following 9 (strictly 7) patterns.

      \  indent
       \  2 4 8
    tab \------
      2 | U
      4 | X U
      8 | X X U <- It cannot distinguish between three `U's.
      - | X X X

The detection method is counting 2-space, 4-space, 8-space and 1-tab
at the beginning of every line and comparing among them.  The behavior
of your Emacs is then switched to go along with the detected indent style.
You can easily write your own code into another person's or your team's
code without breaking the existing indent style.
