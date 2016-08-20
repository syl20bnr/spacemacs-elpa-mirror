This package defines mapping macros that report progress.

For many of the standard and CL mapping functions like `mapc' macros
like `mapc-with-progress-reporter' are defined here.  The arguments
have the same meaning as the respective arguments of the mapping
function or of `make-progress-reporter', which ever has an argument by
the same name.

Even when the original mapping function supports multiple sequences the
macros defined here only support one.  All of `make-progress-reporter's
arguments except for MESSAGE are optional.  This includes the starting
and final state arguments.

All standard mapping function with exactly two mandatory arguments that
call the function applied to each element with exactly one argument are
supported by `map-with-progress-reporter', which can be used when no
progress reporting variant of that function has been defined here.  But
any optional arguments the original might have are not supported.
