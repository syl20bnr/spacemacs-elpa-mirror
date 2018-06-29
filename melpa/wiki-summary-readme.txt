It's often the case when reading some document in Emacs (be it
code text or prose) that I come across a word or phrase that I
don't know. In order to simplify my feedback loop when wiki-summary
lets me look up something in a couple seconds.

To use this package, simply call M-x wiki-summary (or bind it to a key).
This will prompt you for an article title to search. For convience,
this will default to the word under the point. When you hit enter
this will query Wikipedia and if an article is found, bring up the
title in a separate window. Spaces will be properly escaped so
something like "Haskell (programming language)" will bring up the
intended page.

I'm not sure exactly what else people would want out of this package.
Feature request issues are welcome.
