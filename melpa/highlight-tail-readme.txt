=== WHAT IS IT?

This minor-mode draws a tail in real time, when you write.  It
changes the background color of some last typed characters and
smoothly fade them out to the background color.

So from now on, your Emacs will be even more sexy! ;o )

If you do not understand what I mean, check the animation:
http://nic-nac-project.net/~necui/img/htshow.gif

=== INSTALLATION

Place this file in your load-path and add

(require 'highlight-tail)
(message "Highlight-tail loaded - now your Emacs will be even more sexy!")

[ here some setq of variables - see CONFIGURATION section below ]

(highlight-tail-mode)

to your ~/.emacs

=== CONFIGURATION

The default configuration is quite good, but you could and should
customize it to your own needs.  Here are some examples.  It's hard
for me to explain what you will see when use them, so just give
them a try.  Place these setq in your .emacs.el file *before* the
(highlight-tail-mode) function call.

1. -----

(setq highlight-tail-colors '(("black" . 0)
                              ("#bc2525" . 25)
                              ("black" . 66)))

2. -----

(setq highlight-tail-steps 14
      highlight-tail-timer 1)

3. -----

(setq highlight-tail-posterior-type 'const)

These are all customizable variables.  I think you get the idea
how to customize this mode for best fit.

ATTENTION

You will often need to run (highlight-tail-reload) function to make
changes work :).

=== SPEED

From version 0.8 this mode doesn't use much CPU power.  There were
problems with this in earlier versions.  Now it takes about 2-8% of
my Athlon XP 2000+ power in normal speed typing (maybe it is still
too much? - if you have suggestions how to make it eat less - mail
me).  When I press a key and hold it down it eats approximately 15%
(in comparison to prior version (100%) it is a very good result).
The CPU eaten depends mainly on two variables:
`highlight-tail-steps' and `highlight-tail-timer'.  So combine with
these two to achieve satisfactory performance.

=== Terminals

Highlight-tail doesn't work on terminals (only in window-systems
like X11)
