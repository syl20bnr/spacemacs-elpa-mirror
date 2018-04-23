This package helps you control your GNU/Linux desktop from Emacs.
With desktop-environment, you can control the brightness and volume
as well as take screenshots and lock your screen.  The package
depends on the availability of shell commands to do the hard work
for us.  These commands can be changed by customizing the
appropriate variables.

The global minor mode `desktop-environment-mode' binds standard
keys to provided commands: e.g., <XF86AudioRaiseVolume> to raise
the volume, <print> to take a screenshot, and <s-l> to lock the
screen.
