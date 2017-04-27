editorconfig-fnmatch.el provides a fnmatch implementation with a few
extensions.
The main usage of this library is glob pattern matching for EditorConfig, but
it can also act solely.

editorconfig-fnmatch-p (name pattern)

Test whether NAME match PATTERN.

PATTERN should be a shell glob pattern, and some zsh-like wildcard matchings
can be used:

*           Matches any string of characters, except path separators (/)
**          Matches any string of characters
?           Matches any single character
[name]      Matches any single character in name
[^name]     Matches any single character not in name
{s1,s2,s3}  Matches any of the strings given (separated by commas)
{min..max}  Matches any number between min and max


This library is a port from editorconfig-core-py library.
https://github.com/editorconfig/editorconfig-core-py/blob/master/editorconfig/fnmatch.py
