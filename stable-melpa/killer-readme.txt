This package defines additional commands to kill and delete text.
Most notably it defines smarter variants of the built-in commands
which delete text.  Where the built-in command always deletes text
the variants defined here instead kill the text if the previous
command was a kill command.

Not that this package is not namespace-save and the author does not
it any longer.  However because the function definitions in this
library are all quite simple you might still want to give it a try
if you often some command which deleted some text had instead
killed it.
