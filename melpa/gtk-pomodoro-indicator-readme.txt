Integrate with `org-pomodoro-start' e.g. like this:

(gtk-pomodoro-indicator
 (cl-case state
   (:pomodoro "p 25")
   (:short-break "b 5")
   (:long-break "b 20")
   (t (error "unexpected"))))
