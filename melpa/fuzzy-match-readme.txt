Purpose:

Fuzzy-match is a package of functions to provide non-exact comparison
between strings.  Since I am no expert on such things, and certain criteria
for non-exact comparison had to be dropped for reasons of efficiency (e.g.,
transposition), and the non-exact nature of non-exact comparison, this
package may or may not provide what you want.

Caveat:

This is fuzzy software.  Use it at your own risk.

The fuzzy-matcher deals with comparing strings.  For a programmer wishing to
use the fuzzy-match library, the front-end functions are likely to be
`FM-matchiness' (and corresponding `FM-closeness'), `FM-all-fuzzy-matches'
(and corresponding `FM-all-close-matches'), and `FM-fuzzy-sort'.  These can
be thought to mirror `string-match', `all-completions' and `sort'.

The function `FM-matchiness' returns an integer which is the number of
matching characters from STRING1 in STRING2.  What denotes "the number of
matching characters" is arbitrary.

The fuzziness between two strings, STRING1 and STRING2, is calculated by
finding the position in STRING2 of a prefix of STRING1.  The first character
of STRING1 is found in STRING2.  If we find it, we continue matching
successive characters from STRING1 at successive STRING2 positions.  When we
have found the longest prefix of STRING1 in STRING2, we decide whether it is
a match.  It is considered a match if the length of the prefix is greater or
equal to the offset of the beginning of the prefix of STRING1 in STRING2.
This means that "food" will match "barfoo" because "foo" (the prefix)
matches "foo" in "barfoo" with an offset and length of 3.  However, "food"
will not be considered to match "barfu", since the length is 1 while the
offset is 3.  The fuzz value of the match is the length of the prefix.  If
we find a match, we take the prefix off STRING1 and the string upto the end
of the match in STRING2.  If we do not find a match, we take off the first
character in STRING1.  Then we try and find the next prefix.

So, to walk through an example:

(FM-matchiness "pigface" "pigsfly"):

STRING1              STRING2         MATCH LENGTH    OFFSET          FUZZ
pigface              pigsfly         3               0               3
face                 sfly            1               1               1
ace                  ly              0               0               0
ce                   ly              0               0               0
c                    ly              0               0               0

     => 4

(FM-matchiness "begining-of-l" "beginning-of-l"):

STRING1              STRING2         MATCH LENGTH    OFFSET          FUZZ
begining-of-l        beginning-of-l  5               0               5
ing-of-l             ning-of-l       8               1               8

     => 13

Another function of interest is `FM-all-fuzzy-matches'.  This returns a list
of those strings that have the highest fuzzy match with a given string.
Those strings are sorted so that there is a preference for strings with the
same number of characters, and sharing the longest prefix with the given
string:

(FM-all-fuzzy-matches "abc" '("xxabcxx" "xabcxxx" "xabx"))
     => ("xabcxxx" "xxabcxx")

(FM-all-fuzzy-matches "point-mx" (all-completions "point" obarray))
     => ("point-max" "point-max-marker")

(FM-all-fuzzy-matches "begining-of-l" (all-completions "begin" obarray))
     => ("beginning-of-line")

Corresponding to `FM-matchiness' and `FM-all-fuzzy-matches' are
`FM-closeness' and `FM-all-close-matches'.  They differ from the former
functions in that they take into account the difference in length between
the target string and candidate string:

(FM-closeness "begining-of-l" "beginning-of-l")
     => 12

Note from above that the matchiness is 13 and the difference in length of
the two strings is 1.

(FM-all-close-matches "point-mx" (all-completions "point" obarray))
     => ("point-max")

Note from above that although the matchiness is equal between the target
"point-mx" and the candidates "point-max" and "point-max-marker", the former
candidate has less of a difference in length from the target.

Other functions that may be of use to package writers using this package are
`FM-map-fuzzy-matches' (and corresponding `FM-map-close-matches') and
`FM-max-matchiness' (and corresponding `FM-max-closeness').  The mapping
functions map matchiness or closeness to a list, while the max functions
return the maximum matchiness or closeness from a list.

Also provided are some interface functions for user packages.  These
functions are `FM-offer-corrections' and `FM-list-candidates'.  To
demonstrate the usefulness of this package, `lisp-spell-symbol' (analogous
to `lisp-complete-symbol') is provided.  Without an arg, the command uses
`FM-all-close-matches' to find spelling corrections:

(goto-char (point-mx M-x lisp-spell-symbol RET
     -| Replaced point-mx with point-max
(goto-char (point-max

With a double prefix arg, the command uses `FM-all-fuzzy-matches' to find
spelling corrections:

(goto-char (point-mx C-u C-u M-x lisp-spell-symbol RET
     -| Possible candidates are:
     -| point-max                       point-max-marker

Any number of prefix args means that the user is prompted when replacing
with the single correction candidate.

Installation:

Put this file where your Emacs can find it and byte compile it.

To use, put in your package that uses these functions:

(require 'fuzzy-match)

To use the interactive package, put the following in your ~/.emacs file:

(autoload 'lisp-spell-symbol "fuzzy-match"
  "Perform spell checking on Lisp symbol preceding point." t)
(define-key esc-map "#" 'lisp-spell-symbol)

This will define the key M-# (ESC #) to call `lisp-spell-symbol'.
For Emacs-19 users you can also add an entry to the "ispell" menu-bar:

(define-key-after ispell-menu-map [ispell-symbol]
  '("Check Symbol" . lisp-spell-symbol) 'ispell-word))


 If you like `fuzzy-match.el', you might also be interested in
 Icicles, which lets you use the same fuzzy matching for minibuffer
 input completion: http://www.emacswiki.org/Icicles.
