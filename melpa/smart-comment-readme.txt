Code gets rewritten when refactoring to accomidate for new features or
when fixing bugs. An approach used by many developers is to disable a
piece of code with comments, then rewrite an improved version below,
test that is works and then delete the commented code. smart-comment
makes this and similair workflows much swifter.

smart-comment is implemented on top of the commenting functions built
in to Emacs. It is meant to replace `comment-dwim` as the function you
bind to `M-;`.

It triggers different comment actions taking the current location of
the point into acount. Commenting out lines of code is faster.
Commented lines can be marked for later deletion and then all removed
with a single command.
