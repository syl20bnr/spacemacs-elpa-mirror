Use `ivy-mode' to input email address from BBDB efficiently.
Smart to know some ethic groups display family name before given name

`M-x counsel-bbdb-complete-mail' to input email address.
`M-x counsel-bbdb-reload' to reload contacts from BBDB database.

You can also customize `counsel-bbdb-customized-insert' to insert
email in your own way:
  (setq counsel-bbdb-customized-insert
        (lambda (r append-comma)
          (let* ((family-name (nth 1 r))
                 (given-name (nth 2 r))
                 (display-name (nth 3 r))
                 (mail (nth 4 r))))
          (insert (format "%s:%s:%s:%s%s"
                          given-name
                          family-name
                          display-name
                          mail
                          (if append-comma ", " " ")))))

BTW, `ivy-resume' is fully supported.
