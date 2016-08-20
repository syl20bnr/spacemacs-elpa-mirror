Mark special chars, by default nonascii, non-IDN chars, in modes
where they may be confused with regular chars. See `markchars-mode'
and `markchars-what'.  There are two modes: confusable detection
(where we look for mixed scripts within a word, without using the
http://www.unicode.org/reports/tr39/ confusable tables) and pattern
detection (where any regular expressions can be matched).

The marked text will have the 'markchars property set to either
'confusable or 'pattern and the face set to either
`markchars-face-confusable' or `markchars-face-pattern'
respectively.