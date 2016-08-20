The only entry point is `describe-unbound-keys'; it prompts for the maximum
complexity to allow, which should probably be at least 5 to find enough
keys to be worthwhile.  Lisp may call just `unbound-keys' to get a list of
key representations suitable for `define-key'.
