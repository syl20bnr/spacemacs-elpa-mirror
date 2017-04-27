Commentary:

Bitcoin donations gratefully accepted: 1D6meUBuHXLxQNiBfaNKYRfWVVTTYU2okM

This library changes keyboard macro query points into decision points or conditional
branches. A query point can be created by typing C-x q or entering the `kbd-macro-query'
command when entering a keyboard macro.
When the macro is replayed and the query point is reached the user will be prompted with
options to either quit the macro, continue the rest of the macro, enter recursive edit and
store a new macro, add a conditional branch (explained next), or replay a previously saved
(named) macro. The user may also recenter the window by pressing C-l.
If the user chooses to add a conditional branch they will be prompted for a condition form,
and an action to perform if that condition evaluates to non-nil. The action can be to quit the macro,
continue the macro, create a new macro for that condition, execute an elisp form or command, or replay
a previously saved macro.
If the condition evaluates to non-nil the next time the macro is replayed then the corresponding
action will be performed. If several conditions-action pairs are created for a given query point
then the conditions will be evaluated in the order in which they where created until one of them evaluates
to non-nil. If they all evaluate to nil then the user will be prompted as before to either quit, continue,
create/replay a macro, or add another condition-action pair.

By adding query points to the end of each newly created macro, macro decision trees can be built up
and complex automated operations performed.

NOTES:

If you are creating a complex macro with several layers of conditional branching you should build it up
layer at a time from the bottom up, naming the constituent macros as you go. You need to make sure all
conditions of a constituent macro are defined before using that macro in another one as you will not be
able to edit the constituent macro while running the parent macro. It may be safer to just write a program
to do the task instead.
If you want to see what macros have been named (and maybe run one) you can use `kbd-macro-query' (C-x C-k q).
Also note that when prompted for a condition you can scroll forward through the input history using M-n to get
conditions for searching for strings/regexps. You can add to this list by customizing `kmacro-decision-conditions'.

Customizable Options:

Below are customizable option list:

 `kmacro-decision-conditions'
   A list of conditions to be made available in the history list in calls to `kmacro-decision'
   default = (quote ("(search-forward \"??\" nil nil)" "(re-search-forward \"??\" nil nil)"))

Installation:

Put kmacro-decision.el in a directory in your load-path, e.g. ~/.emacs.d/
You can add a directory to your load-path with the following line in ~/.emacs
(add-to-list 'load-path (expand-file-name "~/elisp"))
where ~/elisp is the directory you want to add
(you don't need to do this for ~/.emacs.d - it's added by default).

Add the following to your ~/.emacs startup file.

(require 'kmacro-decision)
