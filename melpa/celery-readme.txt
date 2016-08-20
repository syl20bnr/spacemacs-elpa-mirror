o.s. pre-requisite:
- either an accessible remote celery ready machine
- either your local celery ready machine

For example, either the local machine with:
- celery installed on machine (apt-get install -y celeryd)
- A ssh ready machine (cf. README.org for detailed example)

If using ssh, configure this mode to know it:
(custom-set-variables '(celery-command "ssh remote-node celery"))
and you should be good to go.

You can order or filter the celery data outputed per worker using
`celery-workers-list':
(custom-set-variables '(celery-workers-list '(aw01 aw02)))
