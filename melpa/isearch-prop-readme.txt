   Search text-property or overlay-property contexts.

 Such contexts are either zones of text that have certain text
 properties or overlays that have certain overlay properties.

 This file is part of package Isearch+, which includes also file
 `isearch+.el'.  You can use either of the files without the other,
 if you like, but I recommend that you use them together.

 Some of the features of library `isearch+.el' need library
 `zones.el'.  There is no error if you do not have that library,
 but those features will be unavailable without it.

 Some of the features provided by this library are based on similar
 features introduced by Icicles (http://www.emacswiki.org/Icicles).

 More description below - see Overview of Features.


 Index
 -----

 If you have library `linkd.el' and Emacs 22 or later, load
 `linkd.el' and turn on `linkd-mode' now.  It lets you easily
 navigate around the sections of this doc.  Linkd mode will
 highlight this Index, as well as the cross-references and section
 headings throughout this file.  You can get `linkd.el' here:
 http://dto.freeshell.org/notebook/Linkd.html.

 (@> "Overview of Features")
 (@> "Macros")
 (@> "Variables")
 (@> "Keys")
 (@> "General Search-Property Commands")
 (@> "General Non-Interactive Functions")
 (@> "Search-Zones Commands and Functions")
 (@> "Imenu Commands and Functions")
 (@> "THING Commands and Functions")
 (@> "Character-Property Search")


 Macros defined here:

   `isearchp-with-comments-hidden'.

 Commands defined here:

   `isearchp-add-prop-to-lazy-highlights',
   `isearchp-add-prop-to-other-prop-zones', `isearchp-cleanup',
   `isearchp-hide/show-comments', `isearchp-imenu',
   `isearchp-imenu-command', `isearchp-imenu-macro',
   `isearchp-imenu-non-interactive-function',
   `isearchp-lazy-highlights-forward',
   `isearchp-lazy-highlights-forward-regexp',
   `isearchp-mark-lazy-highlights',
   `isearchp-narrow-to-lazy-highlights',
   `isearchp-narrow-to-matching-zones',
   `isearchp-next-visible-thing',
   `isearchp-previous-visible-thing', `isearchp-property-backward',
   `isearchp-property-backward-regexp',
   `isearchp-property-forward', `isearchp-property-forward-regexp',
   `isearchp-put-prop-on-region',
   `isearchp-regexp-context-regexp-search',
   `isearchp-regexp-context-search',
   `isearchp-regexp-define-contexts',
   `isearchp-remove-all-properties', `isearchp-remove-dimming',
   `isearchp-remove-property', `isearchp-thing',
   `isearchp-thing-define-contexts', `isearchp-thing-regexp',
   `isearchp-toggle-complementing-domain',
   `isearchp-toggle-dimming-outside-search-area',
   `isearchp-toggle-ignoring-comments',
   `isearchp-toggle-hiding-comments', `isearchp-zones-backward',
   `isearchp-zones-backward-regexp', `isearchp-zones-forward',
   `isearchp-zones-forward-regexp'.

 User options defined here:

   `isearchp-dimming-color',
   `isearchp-dim-outside-search-area-flag',
   `isearchp-hide-whitespace-before-comment-flag',
   `isearchp-ignore-comments-flag',
   `isearchp-query-replace-zones-flag'.

 Non-interactive functions defined here:

   `isearchp-add-regexp-as-property',
   `isearchp-add/remove-dim-overlay',
   `isearchp-bounds-of-thing-at-point',
   `isearchp-complement-dimming', `isearchp-defined-thing-p',
   `isearchp-dim-color', `isearchp-dim-face-spec',
   `isearchp-exclude-zones-w-no-lazy-highlight',
   `isearchp-lazy-highlights-forward-1',
   `isearchp-lazy-highlights-present-p', `isearchp-message-prefix',
   `isearchp-next-visible-thing-1',
   `isearchp-next-visible-thing-2',
   `isearchp-next-visible-thing-and-bounds',
   `isearchp-properties-in-buffer', `isearchp-property-1',
   `isearchp-property-default-match-fn',
   `isearchp-property-filter-pred', `isearchp-property-matches-p',
   `isearchp-read-context-regexp', `isearchp-read-face-names',
   `isearchp-read-face-names--read', `isearchp-read-sexps',
   `isearchp-regexp-read-args', `isearchp-regexp-scan',
   `isearchp-remove-duplicates',
   `isearchp-restore-pred-and-remove-dimming', `isearchp-some',
   `isearchp-thing-read-args', `isearchp-text-prop-present-p',
   `isearchp-thing-scan', `isearchp-things-alist',
   `isearchp-zones-1', `isearchp-zones-filter-pred',
   `isearchp-zone-limits-izones', `isearchp-zones-read-args'.

 Internal variables defined here:

   `isearchp-dimmed-overlays', `isearchp-excluded-zones',
   `isearchp-property-prop', `isearchp-property-prop-prefix',
   `isearchp-property-type', `isearchp-property-values',
   `isearchp-complement-domain-p', `isearchp-context-level',
   `isearchp-filter-predicate-orig', `isearchp-last-thing-type',
   `isearchp-zone-limits-function'.


 Keys bound in `isearch-mode-map' here:

   `C-t'        `isearchp-property-forward'
   `C-M-t'      `isearchp-property-forward-regexp'
   `C-M-;'      `isearchp-toggle-ignoring-comments'
   `C-M-~'      `isearchp-toggle-complementing-domain'


 This file should be loaded *AFTER* loading the standard GNU file
 `isearch.el'.  So, in your `~/.emacs' file, do this:

 (eval-after-load "isearch" '(require 'isearch-prop))

(@* "Overview of Features")

Overview of Features ---------------------------------------------

 Library `isearch-prop.el' lets you search within contexts.  You
 can limit incremental search to a set of zones of buffer text -
 search contexts that in effect constitute a multi-region.  These
 zones can be defined in various ways, including some ways provided
 specially by this library.  You can search zones defined by either
 their limits (positions) or text or overlay properties on their
 text.  (For the former, you also need library `zones.el'.)

 You can put text or overlay properties on zones of text that are
 defined by matching a regexp or by corresponding to a type of
 THING (e.g. comment, sexp, paragraph).

 As one example of searching zones that have a given overlay
 property, you can search the matches of a previous Isearch, the
 lazy-highlight text.  If you also use library `isearch+.el' then
 you can use `M-s h l' during isearch to toggle the automatic
 removal of lazy-highlighting.  Toggle it off, to keep the
 highlighting from the last search.

 While you are searching lazy-highlight zones from a previous
 search, if you hit `C-S-SPC' then you start searching the current
 search hits.  You can repeat this.  This is a kind of progressive
 searching: narrowing of search results.

 Another kind of progressive searching of zones of text is
 available with `S-SPC'.  It removes zones from among the areas
 being searched if the zones do not currently contain a search hit
 (lazy highlight).  This means you can search using multiple search
 patterns, effectively AND'ing them.  For example, you can search
 zones for both `fox' and `hen' by searching for one and then the
 other, using `S-SPC' in between.  You need library `zones.el' for
 this, and this feature is currently limited to removing zones
 defined by their limits, not text or overlay properties.

 Features:

 * If you use library `zones.el' then you can search a set of
   buffer zones that are defined by their limits (markers or
   numbers) -- like multiple regions, using commands
   `isearchp-zones-forward' and `isearchp-zones-forward-regexp'.
   You can use different such zone sets.

   Library `zones.el' gives you an easy, interactive way to define
   them.  A prefix arg to the commands that search a set of zones
   prompts you for a variable whose value is such a set.  By
   default the variable is `zz-izones'.

   If option `isearchp-query-replace-zones-flag' is non-nil then
   replacement commands, such as `query-replace', limit replacement
   to the current set of zones (value of the current value of
   `zz-izones-var').

 * You can search within text-property or overlay-property zones of
   the buffer or active region.  Example: search within zones
   having a `face' text property with a value of
   `font-lock-comment-face' or `font-lock-string-face'.

 * The basic commands for searching propertied zones are
   `isearchp-property-forward' and
   `isearchp-property-forward-regexp', and their backward
   counterparts.  By default, you are prompted for the property
   type (`text' or `overlay'), the property, and the property
   values (e.g., a list of faces, for property `face').  These
   commands are bound to `C-t' and `C-M-t', respectively, during
   Isearch.

 * Besides relying on other code to set `face' and other text
   properties for use with `C-t', you can use command
   `isearchp-put-prop-on-region' (outside of Isearch) to add a text
   property to a zone of text.  By default, it applies the last
   property and value whose zones you searched using `C-t', but a
   prefix arg lets you specify the property and value to apply.
   This gives you an interactive way to set up zones for
   text-property search (`C-t').  For property `face', empty input
   removes all faces from the region.

 * You can use command `isearchp-mark-lazy-highlights' to put
   property `isearchp-lazy-highlight' on the text that has lazy
   highlighting (an overlay with face `lazy-highlight').  To use
   this, you will likely want to first set option
   `lazy-highlight-cleanup' to nil, so this highlighting is not
   removed when you exit Isearch.  If you use library `isearch+.el'
   then you can use `M-s h l' during Isearch to toggle option
   `lazy-highlight-cleanup'.  (You can also remove lazy-highlight
   highlighting manually anytime using `M-x
   lazy-highlight-cleanup'.)

 * You can use command `isearchp-lazy-highlights-forward' `(or
   `isearchp-lazy-highlights-forward-regexp') to search the zones
   of text that have text property `isearchp-lazy-highlight', that
   is, the text that you have marked using
   `isearchp-mark-lazy-highlights'.  This lets you search within
   the hits from a previous search.

 * You can use `C-S-SPC' (command
   `isearchp-narrow-to-lazy-highlights') to narrow a lazy-highlight
   search.  What this means is that while you are searching the
   marked lazy-highlight zones, if you hit `C-S-SPC' then the
   current lazy-highlight areas (from the current search of the
   marked zones) are marked and replace the previously marked
   zones.  The effect is that you are now searching only the areas
   that were lazy-highlighted before you hit `C-S-SPC'.

 * You can search zones of text/overlays that have a given
   property, as described above, or you can search the
   *COMPLEMENT*: the zones that do *NOT* have a given property.
   You can toggle this search-domain complementing at any time
   during Isearch, using `C-M-~' (command
   `isearchp-toggle-complementing-domain').

 * When you search propertied zones, the non-searchable zones are
   sometimes dimmed, to make the searchable areas stand out.
   Option `isearchp-dim-outside-search-area-flag' controls whether
   such dimming occurs.  You can toggle it anytime during Isearch,
   using `C-M-D' (aka `C-M-S-d').  Option `isearchp-dimming-color'
   defines the dimming behavior.  It specifies a given background
   color to use always, or it specifies that the current background
   color is to be dimmed a given amount.

 * You can use `M-S-delete' to clean up any property-searching
   artifacts (properties, dimming), as well as remove any
   lazy-highlighting.

 * You can search the zones of text that match a given regexp,
   using command `isearchp-regexp-context-search' or
   `isearchp-regexp-context-regexp-search'.  This is equivalent to
   using command `isearchp-regexp-define-contexts', which marks
   such zones with a text property, and then using command
   `isearchp-property-forward' or
   `isearchp-property-forward-regexp' (`C-t' or `C-M-t' during
   Isearch, respectively).

 * You can search the text of THINGS of various kind (sexps, lists,
   defuns, lines, pages, sentences, filenames, strings, comments,
   xml/html elements, symbols,...), using command `isearchp-thing'
   or `isearchp-thing-regexp'.  This is equivalent to using command
   `isearchp-thing-define-contexts', which marks such zones with a
   text property, and then using `isearchp-property-forward' or
   `isearchp-property-forward-regexp' (`C-t' or `C-M-t' during
   Isearch, respectively).

 * Not related to searching, but you can also move forward and
   backward among things of a given kind, using the repeatable
   commands `isearchp-next-visible-thing' and
   `isearchp-previous-visible-thing'.  For best results I strongly
   recommend that you also use library `thingatpt+.el'.  It
   enhances the vanilla treatment of THINGS and fixes various
   vanilla thing-at-point bugs.

 * You can search the text of Emacs-Lisp definitions of different
   kinds, using commands `isearchp-imenu',
   `isearchp-imenu-command', `isearchp-imenu-macro', and
   `isearchp-imenu-non-interactive-function'.  Since Imenu is based
   on regexps that recognize definitions, these commands are based
   on the behavior of `isearchp-regexp-context-search'.

 * You can remove properties from text or overlays using commands
   `isearchp-remove-property' and `isearchp-remove-all-properties'.
   By default, the latter removes only properties whose names begin
   with `isearchp-'.  These are the properties inserted
   automatically by the commands of this library, when you do not
   specify a property.

 * You can hide or show code comments during Isearch, using `M-;'
   (command `isearchp-toggle-hiding-comments').  You can toggle
   ignoring comments during Isearch, using `C-M-;' (command
   `isearchp-toggle-ignoring-comments').
