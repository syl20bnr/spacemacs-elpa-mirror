This code runs top from within emacs (using top's batch mode), and
provides functionality to operate on processes.

In order to run it, just execute M-x top.  Unlike real top, the
resulting buffer doesn't refresh automatically (yet, it's a feature
I'd like to add someday).  You can refresh the buffer manually by
pressing 'g'.  If you'd like to mark processes for later actions,
use 'm' to mark or 'u' to unmark.  If no processes are marked, the
default action will apply to the process at the current line.  At
the time of this writing, the valid actions are:

    -strace a process
    -kill processes
    -renice processes

You can also toggle showing processes of a specific user by
pressing 'U'.

NOTE:  tested only on Debian GNU/Linux unstable and solaris 8.

Should work out of the box for Debian's version of 'top', however
for solaris 8's version of top, I found the following settings to
be useful:

    (defun top-mode-solaris-generate-top-command (user)
      (if (not user)
          "top -b"
        (format "top -b -U%s" user)))
    (setq top-mode-generate-top-command-function
          'top-mode-solaris-generate-top-command)
    (setq top-mode-strace-command "truss")
