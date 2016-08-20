This program is based on the swank-fuzzy.lisp.
Thanks the CL implementation authors for that useful software.

I love the SLIME/swank's FUZZY-COMPLETIONS. It provides fairly sane
completion results neary invariably. So I translated the original CL
implementation into Emacs Lisp. Thankfully, the core part of the scoring
algorithm in this package is almost identical that in the CL implementation.
(Thanks Emacs Lisp cl package!). Visit the SLIME site
http://common-lisp.net/project/slime/

This file provides a single function, `el-swank-fuzzy-completions',
which you can use for the SLIME/swank's FUZZY-COMPLETIONS alike completion
function for Emacs Lisp.
