This is a simple global minor mode which will replicate the changes
done by virtualenv activation inside Emacs.

The main entry points are `pyvenv-activate', which queries the user
for a virtual environment directory to activate, and
`pyvenv-workon', which queries for a virtual environment in
$WORKON_HOME (from virtualenvwrapper.sh).

If you want your inferior Python processes to be restarted
automatically when you switch your virtual environment, add
`pyvenv-restart-python' to `pyvenv-post-activate-hooks'.
