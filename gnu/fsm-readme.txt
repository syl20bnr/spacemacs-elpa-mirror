fsm.el is an exercise in metaprogramming inspired by gen_fsm of
Erlang/OTP.  It aims to make asynchronous programming in Emacs Lisp
easy and fun.  By "asynchronous" I mean that long-lasting tasks
don't interfer with normal editing.

Some people say that it would be nice if Emacs Lisp had threads
and/or continuations.  They are probably right, but there are few
things that can't be made to run in the background using facilities
already available: timers, filters and sentinels.  As the code can
become a bit messy when using such means, with callbacks everywhere
and such things, it can be useful to structure the program as a
state machine.

In this model, a state machine passes between different "states",
which are actually only different event handler functions.  The
state machine receives "events" (from timers, filters, user
requests, etc) and reacts to them, possibly entering another state,
possibly returning a value.

The essential macros/functions are:

define-state-machine  - create start-FOO function
define-state          - event handler for each state (required)
define-enter-state    - called when entering a state (optional)
define-fsm            - encapsulates the above three (more sugar!)
fsm-send              - send an event to a state machine
fsm-call              - send an event and wait for reply

fsm.el is similar to but different from Distel:
<URL:http://fresh.homeunix.net/~luke/distel/>
Emacs' tq library is a similar idea.

Here is a simple (not using all the features of fsm.el) example:

;; -*- lexical-binding: t; -*-
(require 'fsm)
(cl-labels ((hey (n ev)
                 (message "%d (%s)\tp%sn%s!" n ev
                          (if (zerop (% n 4)) "o" "i")
                          (make-string (max 1 (abs n)) ?g))))
  (cl-macrolet ((zow (next timeout)
                     `(progn (hey (cl-incf count) event)
                             (list ,next count ,timeout))))
    (define-fsm pingpong
      :start ((init) "Start a pingpong fsm."
              (interactive "nInit (number, negative to auto-terminate): ")
              (list :ping (ash (ash init -2) 2) ; 4 is death
                    (when (interactive-p) 0)))
      :state-data-name count
      :states
      ((:ping
        (:event (zow :pingg 0.1)))
       (:pingg
        (:event (zow :pinggg 0.1)))
       (:pinggg
        (:event (zow :pong 1)))
       (:pong
        (:event (zow :ping (if (= 0 count)
                               (fsm-goodbye-cruel-world 'pingpong)
                             3))))))))
(fsm-send (start-pingpong -16) t)

Copy into a buffer, uncomment, and type M-x eval-buffer RET.
Alternatively, you can replace the `fsm-goodbye-cruel-world'
form with `nil', eval just the `cl-labels' form and then type
M-x start-pingpong RET -16 RET.

Version 0.2:
-- Delete trailing whitespace.
-- Fix formatting.
-- Use lexical binding.
-- Port to cl-lib.
-- Remove unnecessary fsm-debug-output message.
-- Add FSM name to fsm-debug-output messages that were not including it.
-- Fix checkdoc errors.
-- Change FSMs from plists to uninterned symbols.

NOTE: This is version 0.1ttn4 of fsm.el, with the following
mods (an exercise in meta-meta-programming ;-) by ttn:
-- Refill for easy (traditional 80-column) perusal.
-- New var `fsm-debug-timestamp-format'.
-- Make variables satisfy `user-variable-p'.
-- Use `format' instead of `concat'.
-- New func `fsm-goodbye-cruel-world'.
-- Make start-function respect `interactive' spec.
-- Make enter-/event-functions anonymous.
-- New macro `define-fsm'.
-- Example usage in Commentary.