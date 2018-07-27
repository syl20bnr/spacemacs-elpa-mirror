How many times have you wanted to fire off a quick command, such as M-!,
but the directory you want to run the command in isn't the same as the
directory of the current buffer?  In those situations, you want a quick way
to change the default-directory *for only the next command*.  That is what
Ido-Springboard aims to solve.

It overrides command keys in ido-mode so that they use the
default-directory of the current viable completion, rather than the
default-directory of the buffer where you started the ido command.
