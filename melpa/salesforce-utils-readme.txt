This is a tiny package that facilitates one Salesforce-related
task: converting a fifteen-character Salesforce object ID to an
eighteen-character object-ID-with-checksum.

This project is not associated with Salesforce (the company) in any
way.

MOTIVATION

At my job, I occasionally need to convert a fifteen-digit
Salesforce ID into the eighteen-character version.  I was advised by
co-workers to use a Chrome plugin for this purpose, but I don't use
Chrome and am generally loathe to leave Emacs when it can be
avoided.  So I researched the algorithm for generating the three
checksum characters and implemented it in Emacs Lisp.
