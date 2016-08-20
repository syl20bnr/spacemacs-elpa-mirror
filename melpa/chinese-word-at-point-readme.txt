This file provides two additional `chinese-word' and `chinese-or-other-word'
things to `thing-at-point' function.

Using:

1. Use (thing-at-point 'chinese-word) to get Chinese word at point
2. Use (thing-at-point 'chinese-or-other-word) to get any possible word
(including Chinese) at point

You can also use (chinese-word-at-point) and (chinese-or-other-word-at-point)
if you prefer.

3. use (chinese-word-chinese-string-p string) to test whether a string consists
of pure Chinese characters.
