
   An Emacs overlay demarcates a region of text in a buffer, often
giving it a different face or changing other properties for that
region. There are many circumstance in which it might be useful to
create, update, and delete overlays automatically when text matches
some criterion, specified for example by regular expressions. This is
what the auto-overlays package addresses. It is intended as an Elisp
library, providing functions to be used by other Elisp packages, so
does not itself define any new interactive commands or minor modes.

This documentation is an extract from the extensive Auto Overlays
Manual that comes with the package. For more detailed information and
examples, please read the manual.


1 Overview
**********

The auto-overlays package automatically creates, updates and destroys
overlays based on regular expression matches in the buffer text. The
overlay is created when text is typed that matches an auto-overlay
regexp, and is destroyed if and when the matching text is changed so
that it no longer matches.

   The regexps are grouped into sets, and any number of different sets
of regexps can be active in the same buffer simultaneously. Regexps in
different sets are completely independent, and each set can be activated
and deactivated independently (*note Defining Regexps::). This allows
different Emacs modes to simultaneously make use of auto-overlays in the
same buffer.

   There are different "classes" of auto-overlay, used to define
different kinds of overlay behaviour. Some classes only require a single
regexp, others require separate regexps to define the start and end of
the overlay (*note Defining Regexps::). Any additional regexps, beyond
the minimum requirements, act as alternatives; if more than one of the
regexps matches overlapping regions of text, the one that appears
earlier in the list will take precedence. The predefined regexp classes
are: `word', `line', `self', `nested' and `flat', but the auto-overlay
package can easily be extended with new classes.

`word'
     These are used to define overlays that cover the text matched by
     the regexp itself, so require a single regexp. An example use
     would be to create overlays covering single words.

`line'
     These are used to define overlays that stretch from the text
     matching the regexp to the end of the line, and require a single
     regexp to define the start of the overlay. An example use would be
     to create overlays covering single-line comments in programming
     languages such as c.

`self'
     These are used to define overlays that stretch from one regexp
     match to the next match for the same regexp, so naturally require
     a single regexp. An example use would be to create overlays
     covering strings delimited by `""'.

     Note that for efficiency reasons, `self' overlays are _not_ fully
     updated when a new match is found. Instead, when a modification is
     subsequently made at any position in the buffer after the new
     match, the overlays are updated _up to_ that position. The update
     occurs just _before_ the modification is made. Therefore, the
     overlays at a given buffer position will not necessarily be
     correct until a modification is made at or after that position
     (*note To-Do::).

`nested'
     These are used to define overlays that start and end at different
     regexp matches, and that can be nested one inside another. This
     class requires separate start and end regexps. An example use
     would be to create overlays between matching braces `{}'.

`flat'
     These are used to define overlays that start and end at different
     regexp matches, but that can not be nested. Extra start matches
     within one of these overlays are ignored. This class requires
     separate start and end regexps. An example use would be to create
     overlays covering multi-line comments in code, e.g. c++ block
     comments delimited by `/*' and `*/'.

   By default, the entire text matching a regexp acts as the
"delimeter". For example, a `word' overlay will cover all the text
matching its regexp, and a `nested' overlay will start at the end of
the text matching its start regexp. Sometimes it is useful to be able
to have only part of the regexp match act as the delimeter. This can be
done by grouping that part of the regexp (*note Defining Regexps::).
Overlays will then start and end at the text matching the group,
instead of the text matching the entire regexp.

   Of course, automatically creating overlays isn't much use without
some way of setting their properties too. Overlay properties can be
defined along with the regexp, and are applied to any overlays created
by a match to that regexp. Certain properties have implications for
auto-overlay behaviour.

`priority'
     This is a standard Emacs overlay property (*note Overlay
     Properties: (elisp)Overlay Properties.), but it is also used to
     determine which regexp takes precedence when two or more regexps
     in the same auto-overlay definition match overlapping regions of
     text. It is also used to determine which regexp's properties take
     precedence for overlays that are defined by separate start and end
     matches.

`exclusive'
     Normally, different auto-overlay regexps coexist, and act
     completely independently of one-another. However, if an
     auto-overlay has non-nil `exclusive' and `priority' properties,
     regexp matches within the overlay are ignored if they have lower
     priority. An example use is ignoring other regexp matches within
     comments in code.


2 Auto-Overlay Functions
************************

To use auto-overlays in an Elisp package, you must load the overlay
classes that you require by including lines of the form
     (require 'auto-overlay-CLASS)
   near the beginning of your package, where CLASS is the class name.
The standard classes are: `word', `line', `self', `nested' and `flat'
(*note Overview::), though new classes can easily be added (*note
Extending the Auto-Overlays Package::).

   Sometimes it is useful for a package to make use of auto-overlays if
any are defined, without necessarily requiring them. To facilitate
this, the relevant functions can be loaded separately from the rest of
the auto-overlays package with the line
     (require 'auto-overlay-common)
   This provides all the functions related to searching for overlays and
retrieving overlay properties. *Note Searching for Overlays::. Note that
there is no need to include this line if any auto-overlay classes are
`require'd, though it will do no harm.

   This section describes the functions that are needed in order to make
use of auto-overlays in an Elisp package. It does _not_ describe
functions related to extending the auto-overlays package. *Note
Extending the Auto-Overlays Package::.

2.1 Defining Regexps
====================

An auto-overlay definition is a list of the form:
     (CLASS &optional :id ENTRY-ID REGEXP1 REGEXP2 ...)
   CLASS is one of the regexp classes described in the previous section
(*note Overview::). The optional `:id' property should be a symbol that
can be used to uniquely identify the auto-overlay definition.

   Each REGEXP defines one of the regexps that make up the auto-overlay
definition. It should be a list of the form
     (RGXP &optional :edge EDGE :id SUBENTRY-ID @rest PROPERTY1 PROPERTY2 ...)
   The `:edge' property should be one of the symbols `'start' or
`'end', and determines which edge of the auto-overlay this regexp
corresponds to. If `:edge' is not specified, it is assumed to be
`'start'. Auto-overlay classes that do not require separate `start' and
`end' regexps ignore this property. The `:id' property should be a
symbol that can be used to uniquely identify the regexp. Any further
elements in the list are cons cells of the form `(property . value)',
where PROPERTY is an overlay property name (a symbol) and VALUE its
value. In its simplest form, RGXP is a single regular expression.

   If only part of the regexp should act as the delimeter (*note
Overview::), RGXP should instead be a cons cell:
     (RX . GROUP)
   where RX is a regexp that contains at least one group (*note Regular
Expressions: (elisp)Regular Expressions.), and GROUP is an integer
identifying which group should act as the delimeter.

   If the overlay class requires additional groups to be specified,
RGXP should instead be a list:
     (RX GROUP0 GROUP1 ...)
   where RX is a regexp. The first GROUP0 still specifies the part that
acts as the delimeter, as before. If the entire regexp should act as
the delimeter, GROUP0 must still be supplied but should be set to 0
(meaning the entire regexp). None of the standard classes make use of
any additional groups, but extensions to the auto-overlays package that
define new classes may. *Note Extending the Auto-Overlays Package::.

   The following functions are used to load and unload regexp
definitions: 

`(auto-overlay-load-definition SET-ID DEFINITION &optional POS)'
     Load a new auto-overlay DEFINITION, which should be a list of the
     form described above, into the set identified by the symbol
     SET-ID. The optional parameter POS determines where in the set's
     regexp list the new regexp is inserted. If it is `nil', the regexp
     is added at the end. If it is `t', the regexp is added at the
     beginning. If it is an integer, the regexp is added at that
     position in the list. Whilst the position in the list has no
     effect on overlay behaviour, it does determine the order in which
     regexps are checked, so can affect efficiency.

`(auto-overlay-load-regexp SET-ID ENTRY-ID REGEXP &optional POS)'
     Load a new REGEXP, which should be a list of the form described
     above, into the auto-overlay definition identified by the symbol
     ENTRY-ID, in the set identified by the symbol SET-ID. REGEXP
     should be a list of the form described above.  The optional POS
     determines the position of the regexp in the list of regexps
     defining the auto-overlay, which can be significant for overlay
     behaviour since it determines which regexp takes precedence when
     two match the same text.

`(auto-overlay-unload-set SET-ID)'
     Unload the entire regexp set identified by the symbol SET-ID.

`(auto-overlay-unload-definition SET-ID ENTRY-ID)'
     Unload the auto-overlay definition identified by the symbol
     ENTRY-ID from the set identified by the symbol SET-ID.

`(auto-overlay-unload-regexp SET-ID ENTRY-ID SUBENTRY-ID)'
     Unload the auto-overlay regexp identified by the symbol
     SUBENTRY-ID from the auto-overlay definition identified by the
     symbol ENTRY-ID in the set identified by the symbol SET-ID.

`(auto-overlay-share-regexp-set SET-ID FROM-BUFFER @optional TO-BUFFER)'
     Share the set of regexp definitions identified by the symbol
     SET-ID in buffer `from-buffer' with the buffer TO-BUFFER, or the
     current buffer if TO-BUFFER is null. The regexp set becomes common
     to both buffers, and any changes made to it in one buffer, such as
     loading and unloading regexp definitions, are also reflected in
     the other buffer. However, the regexp set can still be enabled and
     disabled independently in both buffers. The same regexp set can be
     shared between any number of buffers. To remove a shared regexp
     set from one of the buffers, simply unload the entire set from that
     buffer using `auto-overlay-unload-regexp'. The regexp set will
     remain defined in all the other buffers it was shared with.

2.2 Starting and Stopping Auto-Overlays
=======================================

A set of regexps is not active until it has been "started", and can be
deactivated by "stopping" it. When a regexp set is activated, the
entire buffer is scanned for regexp matches, and the corresponding
overlays created. Similarly, when a set is deactivated, all the overlays
are deleted. Note that regexp definitions can be loaded and unloaded
whether the regexp set is active or inactive, and that deactivating a
regexp set does _not_ delete its regexp definitions.

   Since scanning the whole buffer for regexp matches can take some
time, especially for large buffers, auto-overlay data can be saved to an
auxiliary file so that the overlays can be restored more quickly if the
same regexp set is subsequently re-activated. Of course, if the text in
the buffer is modified whilst the regexp set is disabled, or the regexp
definitions differ from those that were active when the overlay data was
saved, the saved data will be out of date. Auto-overlays automatically
checks if the text has been modified and, if it has, ignores the saved
data and re-scans the buffer. However, no check is made to ensure the
regexp definitions used in the buffer and saved data are consistent
(*note To-Do::); the saved data will be used even if the definitions
have changed.

   The usual time to save and restore overlay data is when a regexp set
is deactivated or activated. The auxilliary file name is then
constructed automatically from the buffer name and the set-id. However,
auto-overlays can also be saved and restored manually.

`(auto-overlay-start SET-ID @optional BUFFER SAVE-FILE NO-REGEXP-CHECK)'
     Activate the auto-overlay regexp set identified by the symbol
     SET-ID in BUFFER, or the current buffer if the latter is `nil'. If
     there is an file called `auto-overlay-'BUFFER-NAME`-'SET-ID in the
     containing up-to-date overlay data, it will be used to restore the
     auto-overlays (BUFFER-NAME is the name of the file visited by the
     buffer, or the buffer name itself if there is none). Otherwise, the
     entire buffer will be scanned for regexp matches.

     The string SAVE-FILE specifies the where to look for the file of
     saved overlay data. If it is nil, it defaults to the current
     directory. If it is a string specifying a relative path, then it is
     relative to the current directory, whereas an absolute path
     specifies exactly where to look. If it is a string specifying a
     file name (with or without a full path, relative or absolute),
     then it overrides the default file name and/or location. Any other
     value of SAVE-FILE will cause the file of overlay data to be
     ignored, even if it exists.

     If the overlays are being loaded from a file, but optional argument
     no-regexp-check is non-nil, the file of saved overlays will be
     used, but no check will be made to ensure regexp refinitions are
     the same as when the overlays were saved.

`(auto-overlay-stop SET-ID @optional BUFFER SAVE-FILE LEAVE-OVERLAYS)'
     Deactivate the auto-overlay regexp set identified by the symbol
     SET-ID in BUFFER, or the current buffer if the latter is `nil'.
     All corresponding overlays will be deleted (unless the
     LEAVE-OVERLAYS option is non-nil, which should only be used if the
     buffer is about to be killed), but the regexp definitions are
     preserved and can be reactivated later.

     If SAVE-FILE is non-nil, overlay data will be saved in an
     auxilliary file called `auto-overlay-'BUFFER-NAME`-'SET-ID in the
     current directory, to speed up subsequent reactivation of the
     regexp set in the same buffer (BUFFER-NAME is the name of the file
     visited by the buffer, or the buffer name itself if there is
     none). If SAVE-FILE is a string, it overrides the default save
     location, overriding either the directory if it only specifies a
     path (relative paths are relative to the current directory), or
     the file name if it only specifies a file name, or both.

`(auto-overlay-save-overlays SET-ID @optional BUFFER FILE)'
     Save auto-overlay data for the regexp set identified by the symbol
     SET-ID in BUFFER, or the current buffer if `nil', to an auxilliary
     file called FILE. If FILE is nil, the overlay data are saved to a
     file called `auto-overlay-'BUFFER-NAME`-'SET-ID in the current
     directory (BUFFER-NAME is the name of the file visited by the
     buffer, or the buffer name itself if there is none). Note that
     this is the only name that will be recognized by
     `auto-overlay-start'.

`(auto-overlay-load-overlays SET-ID @optional BUFFER FILE NO-REGEXP-CHECK)'
     Load auto-overlay data for the regexp set identified by the symbol
     SET-ID into BUFFER, or the current buffer if `nil', from an
     auxilliary file called FILE. If FILE is nil, it attempts to load
     the overlay data from a file called
     `auto-overlay-'BUFFER-NAME`-'SET-ID in the current directory
     (BUFFER-NAME is the name of the file visited by the buffer, or the
     buffer name itself if there is none). If NO-REGEXP-CHECK is
     no-nil, the saved overlays will be loaded even if different regexp
     definitions were active when the overlays were saved. Returns `t'
     if the overlays were successfully loaded, `nil' otherwise.

2.3 Searching for Overlays
==========================

Auto-overlays are just normal Emacs overlays, so any of the standard
Emacs functions can be used to search for overlays and retrieve overlay
properties. The auto-overlays package provides some additional
functions.

`(auto-overlays-at-point @optional POINT PROP-TEST INACTIVE)'
     Return a list of overlays overlapping POINT, or the point if POINT
     is null. The list includes _all_ overlays, not just auto-overlays
     (but see below). The list can be filtered to only return overlays
     with properties matching criteria specified by PROP-TEST. This
     should be a list defining a property test, with one of the
     following forms (or a list of such lists, if more than one
     property test is required):
          (FUNCTION PROPERTY)
          (FUNCTION PROPERTY VALUE)
          (FUNCTION (PROPERTY1 PROPERTY2 ...) (VALUE1 VALUE2 ...))
     where FUNCTION is a function, PROPERTY is an overlay property name
     (a symbol), and VALUE can be any value or lisp expression. For
     each overlay, first the values corresponding to the PROPERTY names
     are retrieved from the overlay and any VALUEs that are lisp
     expressions are evaluated. Then FUNCTION is called with the
     property values followed by the other values as its arguments. The
     test is satisfied if the result is non-nil, otherwise it fails.
     Tests are evaluated in order, but only up to the first failure.
     Only overlays that satisfy all property tests are returned.

     All auto-overlays are given a non-nil `auto-overlay' property, so
     to restrict the list to auto-overlays, PROP-TEST should include
     the following property test:
          ('identity 'auto-overlay)
     For efficiency reasons, the auto-overlays package sometimes leaves
     overlays hanging around in the buffer even when they should have
     been deleted. These are marked with a non-nil `inactive' property.
     By default, `auto-overlays-at-point' ignores these. A non-nil
     INACTIVE will override this, causing inactive overlays to be
     included in the returned list (assuming they pass all property
     tests).

`(auto-overlays-in START END @optional PROP-TEST WITHIN INACTIVE)'
     Return a list of overlays overlapping the region between START and
     END. The PROP-TEST and INACTIVE arguments have the same behaviour
     as in `auto-overlays-at-point', above. If WITHIN is non-nil, only
     overlays that are entirely within the region from START to END
     will be returned, not overlays that extend outside that region.

`(auto-overlay-highest-priority-at-point @optional POINT PROP-TEST)'
     Return the highest priority overlay at POINT (or the point, of
     POINT is null). The PROP-TEST argument has the same behaviour as
     in `auto-overlays-at-point', above. An overlay's priority is
     determined by the value of its `priority' property (*note Overlay
     Properties: (elisp)Overlay Properties.). If two overlays have the
     same priority, the innermost one takes precedence (i.e. the one
     that begins later in the buffer, or if they begin at the same
     point the one that ends earlier; if two overlays have the same
     priority and extend over the same region, there is no way to
     predict which will be returned).

`(auto-overlay-local-binding SYMBOL @optional POINT)'
     Return the "overlay-local" binding of SYMBOL at POINT (or the
     point if POINT is null), or the current local binding if there is
     no overlay binding. An "overlay-local" binding for SYMBOL is the
     value of the overlay property called SYMBOL. If more than one
     overlay at POINT has a non-nil SYMBOL property, the value from the
     highest priority overlay is returned (see
     `auto-overlay-highest-priority-at-point', above, for an
     explanation of "highest priority").
