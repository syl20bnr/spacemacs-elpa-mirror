Simple mode to shorten URLs from Emacs.

Use (bitly-shorten URL) from an Emacs Lisp program, or
M-x bitly-url-at-point to replace the URL at point (or the region)
with a shortened version.

To use, go to https://bitly.com/a/oauth_apps to generate your personal
API access token. Then customize `bitly-access-token' and set it to
the token you just got.
