M-x hack-time-mode RET -1 12:05 RET

sets current-time back to yesterday 12:05 PM.

M-x hack-time-mode RET

disables hack-time-mode and brings back time to normal.

See https://gitlab.com/marcowahl/hack-time-mode for the source.

Use cases:

- View Org agenda as if today was another day.  Achieve this by
  hacking the time to the desired date and open the agenda.

- Mark Org-todo-items done at another day conveniently.  Achieve
  this by hacking the time to the desired date and change the
  todo-state of the item in question.

Limitations:

'hack-time-mode' has actually limitted control over time.  There
are time sources in Emacs _not_ controlled by 'hack-time-mode'.
Watch out!
