We call a lightweight coroutine (or `lcr' for short) a function
which does not return its result directly, but instead passes it to
an extra *continuation* argument (often called 'continue' or
'cont').  Codinig with explicit contination arguments is a
well-known technique, called continuation-passing style (CPS).

CPS allows inversion of control to take place.  Indeed the
continuation may be installed as a callback, rather than being
called directly.  In general, any CPS function may yield control,
and thus implement a lightweight form of concurrency.

This module
provides:

- marcros which can translate direct-style code into cps
- a library of lcr's to read from processes, wait some time, etc.

Why use this module, instead of Emacs' built-in concurrency support?

- for better control over context switch and/or scheduling
- for versions of Emacs which do not provide concurrency
