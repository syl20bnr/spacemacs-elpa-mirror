Django project management package with the goodies you would expect
and then some.  The project buffer workings is pretty much inspired
by the good ol' `magit-status' buffer.

This package relies heavily in fgallina's `python.el' available in
stock Emacs>=24.3 (or https://github.com/fgallina/python.el).

Implements File navigation (per app, STATIC_ROOT, MEDIA_ROOT and
TEMPLATE_DIRS), Etag building, Grep in project, Quick jump (to
settings, project root, virtualenv and docs), Management commands
and Quick management commands.

File navigation: After opening a project, a directory tree for each
installed app, the STATIC_ROOT, the MEDIA_ROOT and each
TEMPLATE_DIRS is created.  Several commands are provided to work
with the current directory at point.

Etags building: Provides a simple wrapper to create etags for
current opened project.

Grep in project: Provides a simple way to grep relevant project
directories using `rgrep'.  You can override the use of `rgrep' by
tweaking the `python-django-cmd-grep-function'.

Quick jump: fast key bindings to jump to the settings module, the
project root, the current virtualenv and Django official web docs
are provided.

Management commands: You can run any management command from the
project buffer via `python-django-mgmt-run-command' or via the
quick management commands accesible from the Django menu.
Completion is provided for all arguments and you can cycle through
opened management command process buffers very easily.  Another
cool feature is that comint processes are spiced up with special
processing, for instance if are using runserver and get a
breakpoint via pdb or ipdb the pdb-tracking provided by
`python-mode' will trigger or if you enter dbshell the proper
`sql-mode' will be used.

Quick management commands: This mode provides quick management
commands (management commands with sane defaults, smart prompt
completion and process extra processing) defined to work with the
most used Django built-in management commands like syncdb, shell,
runserver, test; several good ones from `django-extensions' like
shell_plus, clean_pyc; and `south' ones like convert_to_south,
migrate, schemamigration.  You can define new quick commands via
the `python-django-qmgmt-define' and define ways to handle when
it's finished by defining a callback function.
