Extension to Edebug to make it a little nicer to work with.

Breakpoints can now be toggled from an Elisp buffer without first
running Edebug with `edebug-x-modify-breakpoint-wrapper', bound to
`C-x SPC'. If the function isn't instrumented already then it will
instrument it and then set the breakpoint. Conditional breakpoints
can also be set by calling the previous command with a prefix
argument.

The list of current break points can be viewed with
`edebug-x-show-breakpoints', bound to `C-c C-x b'. From the
tabulated list buffer the following commands are available:

`edebug-x-kill-breakpoint' bound to `K': clear breakpoint
`edebug-x-visit-breakpoint' bound to `RET': visit breakpoint location

To view a list of instrumented functions execute `C-c C-x i',
`edebug-x-show-instrumented'. The instrumented functions buffer has
these commands:

`edebug-x-evaluate-function' bound to `E': evaluate function,
clearing breakpoints within it.
`edebug-x-find-function' bound to `RET': jump to function.

There is also a convenience command, `edebug-x-show-data' (bound to
`C-c C-x s') which will split the window into two showing both the
breakpoints and instrumented function buffers. Executing `Q' will
remove both these buffers.
