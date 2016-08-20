The `minibuffer-complete' command, bound by default to TAB in the
minibuffer completion keymaps, displays the list of possible
completions when no additional characters can be completed.
Subsequent invocations of this command cause the window displaying
the *Completions* buffer to scroll, if necessary.

This library advises the `minibuffer-complete' command so that
subsequent invocations instead select each of the possible
completions in turn, inserting it into the minibuffer and
highlighting it in the *Completions* buffer.  As before, the window
displaying the possible completions is scrolled if necessary.  This
feature is enabled by loading this file and setting the
`minibuffer-complete-cycle' option to t with `M-x customize-variable'
or `M-x set-variable'; it is disabled by unsetting the option (to
nil).  Besides t, the special value `auto' enables the feature and
also causes the first completion to be selected immediately.

You can also customize the `minibuffer-complete-cycle' face, which is
used to highlight the selected completion, with `M-x customize-face'
or any of the `M-x set-face-' commands.

The technique of deleting the minibuffer contents, then (for file
name completion) inserting the directory component of the initial
input, and then inserting the completion string itself is based on
cycle-mini.el (1.03) by Joe Reiss <jreiss@vt.edu>.

Emacs 24 introduced `completion-cycle-threshold' which achieves a
similar goal.  This extension allows you to see the completion
window while cycling, and to cycle backward with <backtab>.
