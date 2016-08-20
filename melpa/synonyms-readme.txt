 Look up synonyms for a word or phrase in a thesaurus.


 Getting Started
 ---------------

 To use library Synonyms, you will need the Moby Thesaurus II file,
 `mthesaur.txt', available here:

   https://archive.org/details/mobythesauruslis03202gut

 Put this in your initialization file (~/.emacs):

   ;; The file names are absolute, not relative, locations
   ;;     - e.g. /foobar/mthesaur.txt.cache, not mthesaur.txt.cache
   (setq synonyms-file        <name & location of mthesaur.txt>)
   (setq synonyms-cache-file  <name & location of your cache file>)
   (require 'synonyms)

 As an alternative to the first two lines, you can use Customize to
 set `synonyms-file' and `synonyms-cache-file' persistently.  The
 second of these files is created by this library, to serve as a
 synonym cache for completion.

 The main command is `synonyms'.  It prompts you for a word or
 phrase to look up in the thesaurus.  The synonyms found are then
 displayed in buffer *Synonyms*.  For example, `M-x synonyms RET
 democracy' displays synonyms for `democracy'.

 If you do not define `synonyms-file' and `synonyms-cache-file'
 prior to using command `synonyms', that command will prompt you to
 define them.  If you want to use the same values during subsequent
 Emacs sessions, then you should use `M-x customize-option' to save
 those newly defined values.


 Some Definitions
 ----------------

 The thesaurus is divided into "entries", which are like glossary
 entries: each entry is followed by associated words and phrases,
 which, for lack of a better word, I refer to as "synonyms".  For
 example, `democracy' is an entry, and it is followed by its
 synonyms.  Some synonyms are not also entries.  For example,
 `patriarchy' is in the thesaurus as a synonym but not as an entry.

 Note: What I call "synonyms" here are not necessarily synonyms, in
 the sense of having the same or even similar meanings.  They are
 simply terms collected together with the same thesaurus entry
 because they are related in some way - the grouping is what
 defines their relation.

 In Moby Thesaurus II, the meanings of synonyms in the same group
 do have something in common, but this might be simply the fact
 that they are terms of a similar kind.  For example, the
 "synonyms" following the `democracy' thesaurus entry are words
 such as `dictatorship' and `autocracy'.  These are different forms
 of the same general thing: government - they are certainly not
 synonymous with each other or with the entry `democracy'.


 Searching the Thesaurus
 -----------------------

 The default input value for command `synonyms' is the word under
 the cursor.  Alternatively, if a region is active and you are in
 Transient Mark mode (recommended), then it is the text in the
 region (selection).

 Your input is actually treated as a regular expression (regexp),
 so you can also input patterns like `for.*ion', which will match
 thesaurus entries `formation', `formulation', `fornication',
 `fortification', and `forward motion'.  Note that the last of
 these is a phrase rather than a single word.

 Using a regexp as input is a powerful way to search, but be aware
 that it can be costly in CPU time and computer memory if the
 regexp is not appropriate.  The regexp `.*' will, for example,
 likely use up available memory before being able to return the
 entire thesaurus (it's very large).  You can always use `C-g' to
 interrupt a thesaurus search if you mistakenly use an inefficient
 regexp.


 Using a Prefix Argument To Do More
 ----------------------------------

 You can use a prefix argument to modify searching and the
 presentation of search results, as follows:

   `C-u'     - Search for additional synonyms, in two senses:

               1) Return also synonyms that are matched partially
                  by the input.

               2) Search the entire thesaurus for input matches,
                  even if the input matches a thesaurus entry.

   `M--'     - Append the search results to any previous search
               results, in buffer *Synonyms*.  (Normally, the new
               results replace any previous results.)

   `C-u C-u' - `C-u' plus `M--': Search more and append results.

 If you find yourself often using a particular prefix argument (for
 example, to append results), then you might want to instead change
 the default behavior to reflect this preference.  Options
 `synonyms-match-more-flag' and `synonyms-append-result-flag'
 correspond to using `C-u' and `M--', respectively.  In fact, a
 prefix argument simply toggles the value of the corresponding
 option for the duration of the command.  So, for example, if
 `synonyms-append-result-flag' is t and you use `M--', then results
 will not be appended.

 When partially matching input (`C-u', sense #1), complete synonyms
 are matched against your input.  This means that you generally
 need not add a preceding or trailing `.*' to try to match a
 complete synonym.  For example, input `format' will match the
 complete synonyms `conformation', `efformation', `format',
 `formation', `formative', `formational', `information',
 `informative', `informational', `malformation', `deformation',
 `reformation', `transformation', `reformatory', and so on - there
 is no need to input `.*format.*' to match the same synonyms.

 To better understand the meaning of #2 above for `C-u' (to
 continue the search even if your input matches an entry), try, for
 example, `C-u M-x synonyms RET widespread'.  You'll see not only
 the main synonyms listed for `widespread' as an entry, but also
 lots of different meanings of `widespread', judging by the entries
 for which it is listed as a synonym:

   `accepted', `ample', `broad', `broadcast', `capacious',
   `catholic', `commodious', `commonness', `conventional',
   `currency', `current', `customary', `deep', `deltoid',
   `diffuse', `discrete', `dispersed', `disseminated',
   `dissipated', `distributed', `epidemic', `established',
   `everyday', `expansive', `extended', `extensive', `familiar',
   `fan shaped', `far flung', `far reaching', `flaring', `full',
   `general', `indiscriminate', `infinite', `large scale',
   `liberal', `normal', `normality', `open', `ordinary',
   `outstretched', `pervasive', `popular', `prescribed',
   `prescriptive', `prevailing', `prevalence', `prevalent',
   `public', `rampant', `received', `regnant', `regular',
   `regulation', `reign', `rife', `roomy', `ruling', `run',
   `scattered', `set', spacious`', `sparse', `splay', `sporadic',
   `sprawling', `spread', `standard', `stock', `straggling',
   `stretched out', `sweeping', `time-honored', `traditional',
   `universal', `usual', `vast', `voluminous', `wholesale', `wide
   open', `wide', and `wonted'.

 These are just the entries! Each of these is of course followed by
 its own synonyms - perhaps 100 or 300, including `widespread'.

 This list of entries is not the same list as the synonyms for
 entry `widespread'.  There are words and phrases here that are not
 in the latter list, and vice versa.  For example, the former (but
 not the latter) list includes `full'; the latter (but not the
 former) list includes `wide-reaching'.

 The latter are the words most closely related to `widespread'.
 The list above are the other thesaurus entries (corresponding to
 main categories) to which `widespread' is most closely related.
 Looking at all of the synonym groups in which `widespread' appears
 can tell you additional information about its meanings - and it
 can provide additional synonyms for `widespread'.


 Using Completion with Synonyms
 ------------------------------

 You can complete words and phrases in the minibuffer, as input to
 command `synonyms'.  You can use library Synonyms together with
 library `Icicles to complete a partial word in a text buffer into a
 word or phrase in the thesaurus.  If you use both libraries then
 load Icicles after Synonyms.  For more information on Icicles, see
 `http://www.emacswiki.org/Icicles'.

 ** Minibuffer Input Completion **

 You can enter any text to match against thesaurus synonyms.  When
 you are prompted by command `synonyms' to enter this text, you can
 also use input completion to complete to a thesaurus synonym.
 That is, even though you can enter any text (including a regexp),
 completion will only complete to synonyms in the thesaurus.

 If you load library Icicles, then a more powerful version of
 command `synonyms' is used.  In particular, it lets you:

  - Use `S-TAB' during completion to see the list of all synonyms
    (thesaurus terms) that match your minibuffer input so far.

  - Use `next' (or repeated `S-TAB'), and `prior' (usually keys
    `Page Down' and `Page Up') during completion to cycle through
    the completion candidates (synonyms) that match your input.

  - Use `C-next' and `C-prior' during completion to display the
    synonyms of the current completion candidate.

 ** Completing Buffer Text Using the Thesaurus **

 Icicles also provides two commands for using completion to insert
 thesaurus entries in a buffer:

  - `icicle-complete-thesaurus-entry' completes a word in a text
    buffer to any word or phrase in the thesaurus.  I bind it to
    `C-c /'.

  - `icicle-insert-thesaurus-entry' inserts thesaurus words and
    phrases in a text buffer.  It is a multi-command, which means
    that, within a single call to it, you can insert any number of
    thesaurus entries, in succession.  If you want to, you can
    write an entire book using a single call to
    `icicle-insert-thesaurus-entry'!


 Browsing the Thesaurus
 ----------------------

 Besides using command `synonyms' to search for synonyms, you can
 use Synonyms to browse the thesaurus.  This is really just the
 same thing, but key and mouse bindings are provided in buffer
 *Synonyms*, so you need not input anything - just point and click
 the hyperlinks.  Buffer *Synonyms* is in Synonyms major mode,
 which provides a few additional features.

 You can still choose to search for additional synonyms or append
 search results, without bothering with a prefix argument, by using
 modifier keys (Control, Meta) with a mouse click.

 Another way of browsing is to revisit previous search-result
 pages.  You can do this using commands `synonyms-history-backward'
 and `synonyms-history-forward'.  In buffer *Synonyms*, these are
 bound to the following key sequences, for convenience:

   `l', `p', `mouse-4' - `synonyms-history-backward'
   `r', `n', `mouse-5' - `synonyms-history-forward'

 The `l' and `r' bindings correspond to the history bindings in
 Info.  The `p' and `n' bindings stand for "previous" and "next".
 The bindings to additional mouse buttons correspond to typical
 bindings for Back and Forward in Web browsers.

 In addition to these bindings, the same history commands can be
 accessed by clicking links [Back] and [Forward] with `mouse-2'.

 If you have previously used the append option (via, for example,
 `M-mouse2'), so that there are multiple search results in buffer
 *Synonyms*, then using a history command simply takes you to the
 preceding (for [Back]) or following (for [Forward]) result in the
 buffer, measured from the current cursor position.  Depending on
 the cursor position, this might be different from the previous or
 next search made previously.

 This is for convenience, but it is also more efficient in the case
 of a regexp search that takes a long time.  Except for this
 special treatment of appended results, whenever you navigate the
 search-results history you are actually searching again for a
 synonym you sought previously.  The case of appended results is
 analogous to accessing a Web browser cache when navigating the
 history.

 You can of course use modifier keys (Control, Meta) while you
 click links [Back] and [Forward], to impose their usual behavior:
 search for additional synonyms or append search results, or both.

 Finally, some people prefer menus, so there is a Synonyms menu-bar
 menu when you are in Synonyms mode, complete with all of the
 functionalities described above.

 For more information on the browsing possibilities in buffer
 *Synonyms*, use `?' in Synonyms mode.


 Dictionary Definitions, Antonyms, etc.
 --------------------------------------

 Synonyms works with a large but simple database of groups of words
 and phrases that are synonyms of each other.  This database does
 not provide definitions of words or phrases; it simply groups
 them.  Command `synonym-definition' (aka `dictionary-definition')
 lets you look up a word or phrase (or a regexp) using one or more
 dictionaries on the Web.  That is usually the best source for this
 kind of information, but you obviously need an Internet connection
 to use this command.

 Options (variables) `synonyms-dictionary-url' and
 `synonyms-dictionary-alternate-url' are URLs you can set to point
 to the dictionaries of your choice.  The default value of
 `synonyms-dictionary-alternate-url' looks up the search term in
 multiple dictionaries, and it lets you use wildcards.  Use `C-h v
 synonyms-dictionary-alternate-url' for more information.  The
 default value of `synonyms-dictionary-url' usually provides a
 quicker answer.  Both of these URLs also give you access to
 additional information about the search term (antonyms, etymology,
 even pronunciation).

 In buffer *Synonyms*, you can simply hit `d' followed by `RET' or
 `mouse-2' to look up a term that is in the buffer.  Just as for
 looking up a synonym by clicking `mouse-2', if you select text
 (region), then that text is looked up.


 A Cache File of Synonyms
 ------------------------

 The very first time you use Synonyms, a large list of synonyms
 will be compiled and written to a cache file.  This is slow - it
 takes 2-3 minutes - but it is only a one-time cost.  From then on,
 whenever you first use Synonyms during an Emacs session, the cache
 file will be read (quickly), to create the list of synonyms that
 are used for minibuffer completion.


 Using Other Thesauri, Dictionaries, and so on - CSV data
 --------------------------------------------------------

 There is nothing in library Synonyms that ties it to the Moby
 Thesaurus II thesaurus.  All of its functionality will work with
 any file of comma-separated values.  Each line of such a file is
 interpreted as a synonym group, as understood here, and the first
 word or phrase on each line is interpreted as a thesaurus entry,
 as understood here.  This means only that search results are
 organized into sections with entry headers.

 If, for example, you had a CSV file of personal contacts, where
 the first term in each line was a last name or a company name,
 then you could use library Synonyms to query it, producing the
 same kind of output as for the thesaurus.

 One thing to keep in mind if you try to use library Synonyms with
 a different CSV file is that there are several different CSV-file
 syntaxes.  The one that Synonyms is built to use is a simple one,
 with no quote marks around entries and no embedded quote marks
 within entries.

 Similarly, there is nothing here that limits the functionality to
 English.  If you had a thesaurus in another language, it should
 work as well.

 Currently, Synonyms works with a single raw synonyms file
 (thesaurus) and a corresponding single cache file (for
 completion).  However, it would be easy to extend the
 functionality to use multiple thesauri or, in general, multiple
 CSV files.  Suggestions of requirements (e.g. ways to select a
 thesaurus for particular passages of text) are welcome.



 Things Defined Here
 -------------------

 Faces defined here -

   `synonyms-heading', `synonyms-search-text',
   `synonyms-mouse-face'.


 User options (variables) defined here -

   `synonyms-append-result-flag', `synonyms-cache-file',
   `synonyms-file', `synonyms-fill-column',
   `synonyms-match-more-flag', `synonyms-mode-hook',
   `synonyms-use-cygwin-flag'.

 Commands defined here -

   `dictionary-definition', `synonyms', `synonyms-append-result',
   `synonyms-append-result-no-read', `synonyms-definition',
   `synonyms-definition-mouse', `synonyms-definition-no-read',
   `synonyms-ensure-synonyms-read-from-cache',
   `synonyms-history-backward', `synonyms-history-forward',
   `synonyms-make-obarray', `synonyms-match-more',
   `synonyms-match-more-no-read',
   `synonyms-match-more+append-result',
   `synonyms-match-more+append-result-no-read', `synonyms-mode',
   `synonyms-mouse', `synonyms-mouse-append-result',
   `synonyms-mouse-match-more',
   `synonyms-mouse-match-more+append-result', `synonyms-no-read',
   `synonyms-write-synonyms-to-cache'.

 Non-interactive functions defined here -

   `synonyms-action', `synonyms-add-history-links',
   `synonyms-default-regexp', `synonyms-define-cache-file',
   `synonyms-define-synonyms-file', `synonyms-format-entries',
   `synonyms-format-entry', `synonyms-format-finish',
   `synonyms-format-synonyms',
   `synonyms-hack-backslashes-if-cygwin', `synonyms-lookup',
   `synonyms-nearest-word', `synonyms-file-readable-p',
   `synonyms-search-entries', `synonyms-search-synonyms',
   `synonyms-show-synonyms', `synonyms-file-writable-p'.

 Internal variables defined here -

   `synonyms-history', `synonyms-history-forward',
   `synonyms-list-for-obarray', `synonyms-mode-map',
   `synonyms-obarray', `synonyms-search-text'.

 Key bindings made here - see `synonyms-mode'.  All key bindings
 are local to Synonyms mode; no global bindings are made here.



 Acknowledgements
 ----------------

 The basic functionality provided here was derived from library
 `mthesaur.el', by Tad Ashlock <taashlo@cyberdude.com>.  That
 library, in turn, was inspired by library `thesaurus.el', by Ray
 Nickson.  Thanks also to those who sent helpful bug reports.


 Note on MS Windows Emacs 20 and Cygwin `grep'
 ---------------------------------------------

 There is apparently a bug in the Emacs (at least versions 20-22) C
 code that implements function `call-process' on MS Windows.  When
 using native Windows Emacs with Cygwin commands, such as `grep',
 the C code removes a level of backslashes in some cases, so string
 arguments supplied to `call-process' need to have twice as many
 backslashes as they should need in those cases.  It is for this
 reason that option `synonyms-use-cygwin-flag' is supplied here.
 When that option is non-nil, backslashes in regexps are hacked to
 do the right thing.  (In Emacs 20, this means doubling the
 backslashes; in Emacs 21-22, this means doubling them unless there
 are spaces in the search string.)


 Maybe To Do?
 ------------

 1. It would be ideal to have not only synonym information but also
    definitions, antonyms, more general and more specific terms,
    filtering by part of speech (verb vs adjective etc.), and so
    on.  A good example of what I'd really like to have is provided
    by the free Windows program WordWeb (available here:
    http://wordweb.info/).  Combining that functionality with
    Icicles completion features would provide a great tool, IMO.

    `synonyms-definition*' goes a long way toward providing this,
    and perhaps it is the best way to go, since there is so much
    more definitional info on the Web.
