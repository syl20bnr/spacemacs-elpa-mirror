An alternative matcher for ido-mode.

Gotchas

The matcher does not apply sub-metrics to strings longer than `512`
characters. That is, within a single class, all strings over `512`
characters are going to give the same score.

The matcher relies on heavy caching and might take up a lot of memory.

You might need to bump your GC threshold.

Usage

To try it out simply run:

`M-x ido-clever-match-enable RET`

You can turn it off with:

`M-x ido-clever-match-disable RET`
