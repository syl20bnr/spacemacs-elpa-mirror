Open recentf immediately after Emacs is started.
If files are opend, does nothing.  Open recentf otherwise.
(For example, it is when execute by specifying the file from command line.)
This script uses only advice function for startup.  Not privede interactive functions.
(This approach's dirty hack, but the hook to be the alternative does not exist.)

put into your own .emacs file (~/.emacs.d/init.el)

  (init-open-recentf)

`init-open-recentf' Support helm, ido, anything (or nothing).
Determine from your environment, but it is also possible that you explicitly.

  (setq init-open-recentf-interface 'ido)
  (init-open-recentf)

If you want to do another thing, you can specify an arbitrary function.

  (setq init-open-recentf-function #'awesome-open-recentf)
  (init-open-recentf)
