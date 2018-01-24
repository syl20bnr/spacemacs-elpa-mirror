Simple wrappers for running Unison to sync things; handy for
putting into midnight-hook's or similar.

To use, first set `unison-args' to the arguments you'd normally
call unison with (Unison won't do ), then call M-x unison (or put
#'unison into a hook).
