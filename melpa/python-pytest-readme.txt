This package provides helpers to run pytest.

The main command is python-pytest-popup, which will show a
dispatcher menu, making it easy to change various options and
switches, and then run pytest using one of the actions, which can
also be run directly as commands:

- python-pytest (run all tests)
- python-pytest-file (current file)
- python-pytest-file-dwim (‘do what i mean’ for current file)
- python-pytest-function (current function)
- python-pytest-function-dwim (‘do what i mean’ for current function)
- python-pytest-last-failed (rerun previous failures)
- python-pytest-repeat (repeat last invocation)

A prefix argument causes the generated command line to be offered
for editing, and various customization options influence how some
of the commands work. See the README.rst for detailed information.
