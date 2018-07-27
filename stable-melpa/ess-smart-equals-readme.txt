 Assignment in R is syntactically complicated by three features: 1. the
 historical role of '_' (underscore) as an assignment character in
 the S language (SPlus may still allow this); 2. the somewhat
 inconvenient-to-type, if conceptually pure, '<-' operator as the
 preferred assignment operator; and 3. the ability to use either
 an '=' or an '<-' for assignment.

 ESS uses '_' as a (default) smart assignment character which expands
 to the '<-' with one invokation and gives an underscore on two.
 This makes it rather painful to use underscores in variable, field,
 and function names. Moreover, _ no longer has any association with
 assignment in R, so the mnemonic is strained.

 It is possible to reassign the special underscore to another character,
 such as '=', but that raises other inconviences because of the multiple
 roles that '=' can play (assignment and default argument setting).

 This package gives an alternative smart assignment for R and S code
 that is tied to the '=' key instead of the '_' key. It intelligently
 handles the various ways that '=' is used in R (and S) by examining
 the preceding context. It works under the assumption that '=' used
 for default arguments to functions *will not* be surrounded by
 spaces but that binary operators involving '=' /should be/. When
 this is enabled, underscore is completely divorced from assignment
 and thus can be used directly in names.

 This package defines a global minor mode `ess-smart-equals-mode', that
 when enabled for S-language modes causes the '=' key to use the
 preceding character to determine the intended construct (assignment,
 comparison, default argument). Loosely speaking, an '=' preceded by a
 space is converted to an assignment, an '=' preceded by a comparison
 character (<>!=) becomes a space-padded comparison operator, and
 otherwise just an '=' is inserted. The specific rules are as follows:

  1. In a string or comment or with a non-S language, just insert '='.
  2. If a space (or tab) preceeds the '=', insert a version of `ess-S-assign'
     with no leading space (e.g., "<- "). (Other preceeding spaces are
     left alone.)
  3. If any of =<>! preceed the current '=', insert an '= ', but
     if no space preceeds the preceeding character, insert a space
     so that the resulting binary operator is surrounded by spaces.
  4. If the `ess-S-assign' string (e.g., "<- ") precedes point,
     insert '== ' (a double *not* a single equals).
  5. Otherwise, just insert an '='.

 With a prefix argument, '=' always just inserts an '='.

 These insertions ensure that binary operators have a space on either
 end but they do not otherwise adjust spacing on either side. Note that
 in #4 above, the second '=' key is assumed to be intended as an equality
 comparison because the assignment would have been produced by an '='
 following a space.

 Examples: In the left column below, ^ marks the location at which an '='
 key is pressed, and in the right column it marks the resulting
 position of point.

    Before '='         After '='
    ----------         ---------
    foo ^              foo <- ^
    foo     ^          foo     <- ^
    foo(a^             foo(a=^
    foo=^              foo == ^
    foo<^              foo <= ^
    "foo ^             "foo =^
    #...foo ^          #...foo =^
    foo <- ^           foo == ^


 Installation
 ------------
 Either put this file on your load path
 Disabling the minor mode restores (as well as possible) the previous
 ESS assignment setup.
