This package adds support for erlang projects using rebar3 to
flycheck.  For more on rebar3: https://rebar3.org

To use it, add the following to wherever you configure Emacs.

(require 'flycheck-rebar3)
(flycheck-rebar3-setup)

There's currently an open PR to include this checker with flycheck,
https://github.com/flycheck/flycheck/pull/1144
Once it's merged, this package will be sunset.
