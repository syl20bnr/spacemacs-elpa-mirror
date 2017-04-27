evil-jumper is an add-on for older versions of evil-mode (prior
to Feb 2016) which replaces the implementation the jump list such
that it mimics more closely with Vim's behavior. Specifically, it
will jump across buffer boundaries and revive dead buffers if
necessary. The jump list can also be persisted to history file
using `savehist' and restored
between sessions.

Install:

(require 'evil-jumper)

Usage:

(evil-jumper-mode t)
