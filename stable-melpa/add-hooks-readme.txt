Typically, you would need to call `add-hook' multiple times with
similar arguments to declare multiple functions for one hook, or
vice versa.  `add-hooks-pair' is a variant that takes multiple
hooks or functions that apply to each other.  The `add-hooks'
function tidies up duplicate hook and function names further into a
single declarative call (inspired by the `bind-key' package).

If you're using `use-package', use its `:hook' keyword instead, which has a
similar syntax while establishing autoload definitions and supporting
deferred loading.
