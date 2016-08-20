A simple package to disconnect from the online world for a while,
possibly reconnecting later. Any interruptions or distractions
which occur once the command is run are guaranteed to be local.

'M-x quiet' will disconnect from the network, optionally
reconnecting after a certain time.

the function 'quiet' can be used anywhere in emacs where a lack of
network access could be seen as a feature, e.g. as a mode-hook (or
with defadvice) to your preferred distraction free writing
environment.

you may need to customize or setq quiet-disconnect and
quiet-connect to the appropriate shell commands to turn your
network (interface) on or off
