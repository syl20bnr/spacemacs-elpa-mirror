   Extensions to `isearch.el' (incremental search).

 The Isearch+ libraries are these:

 `isearch+.el' (this file)    - Various extensions to `isearch.el'.
                                Can be used with Emacs 20 or later.
 `isearch-prop.el' (optional) - Commands to search within contexts,
                                which are character-property zones:
                                spans of text that have certain
                                text or overlay properties.  Can be
                                Used with Emacs 23 or later.

 You can use either of the Isearch+ files without the other, but I
 recommend that you use them together.


 This file should be loaded *AFTER* loading the standard GNU file
 `isearch.el'.  So in your `~/.emacs' file, do this:

 (eval-after-load "isearch" '(require 'isearch+))

 Library `isearch-prop.el' is optional.  If you do not want to use
 it then do not put it in your `load-path'.  If it is in your
 `load-path' then it will automatically be loaded when you load
 library `isearch+.el'.

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
 (@> "Change log")
 (@> "Faces and Variables")
 (@> "Keys and Hooks")
 (@> "Macros")
 (@> "Commands")
 (@> "Non-Interactive Functions")


 Commands defined here:

   `isearchp-act-on-demand' (Emacs 22+),
   `isearchp-append-register', `isearch-char-by-name' (Emacs
   23-24.3), `isearchp-complete',
   `isearchp-cycle-mismatch-removal',
   `isearchp-eval-sexp-and-insert' (Emacs 22+),
   `isearchp-fontify-buffer-now', `isearchp-init-edit',
   `isearchp-open-recursive-edit' (Emacs 22+),
   `isearchp-remove-failed-part' (Emacs 22+),
   `isearchp-remove-failed-part-or-last-char' (Emacs 22+),
   `isearchp-retrieve-last-quit-search',
   `isearchp-set-region-around-search-target',
   `isearchp-toggle-lazy-highlight-cleanup' (Emacs 22+),
   `isearchp-toggle-lazy-highlighting' (Emacs 22+),
   `isearchp-toggle-literal-replacement' (Emacs 22+),
   `isearchp-toggle-option-toggle',
   `isearchp-toggle-regexp-quote-yank',
   `isearchp-toggle-repeat-search-if-fail' (Emacs 22+),
   `isearchp-toggle-search-invisible',
   `isearchp-toggle-set-region',
   `isearchp-toggle-symmetric-char-fold' (Emacs 25+),
   `isearchp-yank-char' (Emacs 22+), `isearchp-yank-line' (Emacs
   22+), `isearchp-yank-sexp-symbol-or-char' (Emacs 22+),
   `isearchp-yank-sexp-symbol-or-char-1' (Emacs 22+),
   `isearchp-yank-symbol-or-char' (Emacs 22+),
   `isearchp-yank-symbol-or-char-1' (Emacs 22+),
   `isearchp-yank-word-or-char' (Emacs 22+).

 User options defined here:

   `isearchp-case-fold', `isearchp-deactivate-region-flag' (Emacs
   24.3+), `isearchp-drop-mismatch',
   `isearchp-drop-mismatch-regexp-flag',
   `isearchp-initiate-edit-commands' (Emacs 22+),
   `isearchp-mouse-2-flag', `isearchp-on-demand-action-function'
   (Emacs 22+), `isearchp-regexp-quote-yank-flag',
   `isearchp-repeat-search-if-fail-flag' (Emacs 22+),
   `isearchp-restrict-to-region-flag' (Emacs 24.3+),
   `isearchp-resume-with-last-when-empty-flag' (Emacs 22+),
   `isearchp-ring-bell-function', `isearchp-set-region-flag',
   `isearchp-toggle-option-flag'.

 Faces defined here:

   `isearch-fail', `isearchp-multi', `isearchp-overwrapped',
   `isearchp-regexp', `isearchp-word', `isearchp-wrapped'.

 Macros defined here:

   `isearchp-user-error'.

 Non-interactive functions defined here:

   `isearchp-barf-if-use-minibuffer',
   `isearchp-complete-past-string', `isearchp-fail-pos',
   `isearchp-highlight-lighter', `isearchp-message-prefix',
   `isearchp-message-suffix', `isearchp-read-face-names',
   `isearchp-read-face-names--read', `isearchp-read-sexps',
   `isearchp-remove-duplicates', `isearchp-remove-mismatch',
   `isearchp-repeat-command', `isearchp-repeat-search-if-fail'
   (Emacs 22+), `isearchp-replace-fixed-case-p' (Emacs 22+),
   `isearchp-replace-match' (Emacs 22+),
   `isearchp-replace-multiple' (Emacs 22+),
   `isearchp-replace-on-demand' (Emacs 22+),
   `isearchp-reset-noprompt-action-fn', `isearchp-set-region',
   `isearchp-set-sel-and-yank',
   `isearchp-update-edit-init-commands' (Emacs 22+).

 Internal variables defined here:

   `isearchp-last-non-nil-invisible',
   `isearchp-last-quit-regexp-search', `isearchp-last-quit-search',
   `isearchp-nomodify-action-hook' (Emacs 22+),
   `isearchp-noprompt-action-function',
   `isearchp-orig-ring-bell-fn', `isearchp-pref-arg',
   `isearchp-reg-beg', `isearchp-reg-end',
   `isearchp-replace-literally' (Emacs 22+), `isearchp-replacement'
   (Emacs 22+), `isearchp--replacing-on-demand' (Emacs 22+),
   `isearchp-win-pt-line', `isearch-update-post-hook' (Emacs
   20-21).


 ***** NOTE: The following macros and functions defined in
             `isearch.el' have been REDEFINED OR ADVISED HERE:

 `isearch-abort'       - Save search string when `C-g'.
 `isearch-backward', `isearch-backward-regexp' -
                         Prefix arg can  `multi-isearch-buffers'.
 `isearch-cancel'      - Restore cursor position relative to window.
 `isearch-dehighlight' - Remove unused arg, for Emacs 20.
 `isearch--describe-word-mode' - Face `isearchp-word' on string.
 `isearch-edit-string' - Put point at mismatch position.
 `isearch-forward', `isearch-forward-regexp' -
                         Prefix arg can  `multi-isearch-buffers'.
 `isearch-lazy-highlight-search' - Can limit to region (24.3+)
 `isearch-lazy-highlight-update' - Can limit to region (24.3+)
 `isearch-mode'        - Save cursor position relative to window.
 `isearch-mode-help'   - End isearch.  List bindings.
 `isearch-message'     - Highlight failed part of search string in
                         echo area, in face `isearch-fail'.
 `isearch-message-prefix' - Highlight prompt keywords:
                            wrapped, regexp, word, multi
 `isearch-mouse-2'     - Respect `isearchp-mouse-2-flag'(Emacs 21+)
 `isearch-search'      - Can limit to active region (Emacs 24.3+)
 `isearch-repeat'      - Can limit to active region (Emacs 24.3+)
 `isearch-printing-char' - Respect option `isearchp-drop-mismatch'
 `isearch-toggle-case-fold' - Respect `isearchp-toggle-option-flag'
                              Show case sensitivity in mode-line.
                              Message.
 `isearch-toggle-invisible' - Respect `isearchp-toggle-option-flag'
                              Message.
 `isearch-toggle-word' - Message, and turn off regexp search.
 `isearch-update' - Run `isearch-update-post-hook' (Emacs 20-21).
                  - Run `isearchp-noprompt-action-function' and
                    `isearchp-nomodify-action-hook' (Emacs 22+).
 `isearch-yank-string' - Respect `isearchp-regexp-quote-yank-flag'.
 `with-isearch-suspended' - Add `catch': update `isearch-success'.


 ***** NOTE: The following internal variables defined in
             `isearch.el' have been REDEFINED HERE:

 `isearch-invisible'   - defined for Emacs<24.4 & added doc string.


 ***** NOTE: The following function defined in `misearch.el' has
             been ADVISED HERE:

 `multi-isearch-end'    - Fix for bug #20234: reset buffer list.


 Keys bound in `isearch-mode-map' here:

   `C-`'        `isearchp-toggle-regexp-quote-yank'
   `C-+'        `isearchp-toggle-search-invisible'
   `C-_'        `isearchp-yank-symbol-or-char' (Emacs 22+)
   `C-('        `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
   `C-end'      `goto-longest-line' (requires `misc-cmds.el')
   `C-h'        `isearch-mode-help'
   `C-x n'      `isearchp-toggle-region-restriction' (Emacs 24.3+)
   `C-x o'      `isearchp-open-recursive-edit' (Emacs 22+)
   `C-x r g'    `isearchp-append-register'
   `C-x 8 RET'  `isearch-char-by-name' (Emacs 23-24.3)
   `C-y C-_'    `isearchp-yank-symbol-or-char' (Emacs 22+)
   `C-y C-('    `isearchp-yank-sexp-symbol-or-char' (Emacs 22+)
   `C-y C-2'    `isearch-yank-secondary'
   `C-y C-c'    `isearchp-yank-char' (Emacs 22+)
   `C-y C-e'    `isearchp-yank-line'
   `C-y C-w'    `isearchp-yank-word-or-char' (Emacs 22+)
   `C-y C-y'    `isearch-yank-kill'
   `C-y M-y'    `isearch-yank-pop' (Emacs 24+)
   `C-z'        `isearchp-yank-char' (Emacs 22+)
   `M-c'        `isearch-toggle-case-fold'
   `M-e'        `isearch-edit-string'
   `M-g'        `isearchp-retrieve-last-quit-search'
   `M-k'        `isearchp-cycle-mismatch-removal'
   `M-r'        `isearch-toggle-regexp'
   `M-s ='      `isearchp-toggle-symmetric-char-fold' (Emacs 25+)
   `M-s i'      `isearch-toggle-invisible'
   `M-s h l'    `isearchp-toggle-lazy-highlight-cleanup'(Emacs 22+)
   `M-s h L'    `isearchp-toggle-lazy-highlighting'(Emacs 22+)
   `M-s w'      `isearch-toggle-word'
   `M-TAB'      `isearchp-complete'
   `M-s M-SPC'  `isearchp-toggle-set-region'
   `M-s M-k'    `isearchp-toggle-repeat-search-if-fail'
   `M-w'        `isearchp-kill-ring-save'
   `C-M-i'      `isearchp-complete'
   `C-M-l'      `isearchp-remove-failed-part' (Emacs 22+)
   `C-M-y'      `isearch-yank-secondary'
   `C-M-RET'    `isearchp-act-on-demand' (Emacs 22+)
   `C-M-tab'    `isearchp-complete' (on MS Windows)
   `next'       `isearch-repeat-forward'
   `prior'      `isearch-repeat-backward'


 User option `isearchp-initiate-edit-commands' causes certain keys
 not to exit Isearch but rather to edit the search string.
 Customize it to `nil' if you do not want this behavior at all.


 The following bindings are made here for incremental search edit
 mode:

   `C-x 8 RET'  `insert-char' (Emacs 23+)
   `C-M-tab'    `isearch-complete-edit' (MS Windows only)

(@* "Overview of Features")

Overview of Features ---------------------------------------------

 * Case-sensitivity is indicated in the mode line minor-mode
   lighter: `ISEARCH' for case-insensitive; `Isearch' for
   case-sensitive.

 * Whether search is literal or regexp is indicated in the mode
   line minor-mode lighter: `R*SEARCH' or `R*search', for regexp.

 * Highlighting of the mode-line minor-mode lighter when search has
   wrapped around or overwrapped.

 * Highlighting of parts of the prompt, to indicate the type of
   search: regexp, word, multiple-buffer, and whether searching has
   wrapped around the buffer (Emacs 22+ only).

 * Optional limiting of search to the active region, controlled by
   option `isearchp-restrict-to-region-flag'.  Deactivation of the
   active region is controlled by option
   `isearchp-deactivate-region-flag'.  Both of these are available
   for Emacs 24.3 and later.  You can use `C-x n' (command
   `isearchp-toggle-region-restriction') during search to toggle
   `isearchp-restrict-to-region-flag'.

   NOTE: For search to be limited to the active region in Info, you
   must also use library `info+.el'.

 * Option and commands to let you select the last target occurrence
   (set the region around it):

   - Option `isearchp-set-region-flag' - Non-`nil' means
     automatically set the region around the last search target.
   - Command `isearchp-toggle-set-region', bound to `M-s M-SPC'
     during isearch - toggle `isearchp-set-region-flag'.
   - Command `isearchp-set-region-around-search-target' - manually
     set the region around the last search target.

 * If you also use library `character-fold+.el' then you can use
   `M-s =' (command `isearchp-toggle-symmetric-char-fold') to
   toggle whether character folding is symmetric.  Note that lazy
   highlighting can slow down symmetric char folding considerably,
   so you might also want to use `M-s h L' to turn off such
   highlighting.

 * When you visit a search hit, you can perform an action on it.
   Use `C-M-RET' (command `isearchp-act-on-demand' - Emacs 22+
   only) to invoke the action function that is the value of option
   `isearchp-on-demand-action-function'.  That function is passed
   the current search-hit string and its start and end positions in
   the buffer.  Search moves to the next hit in the same search
   direction, so just repeating `C-M-RET' carries out the action on
   subsequent hits.

 * The default value of `isearchp-on-demand-action-function' is
   function `isearchp-replace-on-demand', which replaces the search
   hit.  This means that you can replace (or delete) chosen search
   hits on demand.

   By default, the replacement string is empty, so with no prefix
   argument the action is to delete the search hit (replace it with
   nothing).

   With a prefix arg, `isearchp-replace-on-demand' prompts for the
   replacement, which is used thereafter until you again use a
   prefix arg.  Since you can use a prefix arg at any time, you can
   provide different replacements for different search hits.  When
   prompted, if you clear the minibuffer and hit `RET', hit
   replacement just becomes search-hit deletion.

   . With a plain prefix arg (`C-u') or a numeric prefix arg of
     value 1 (e.g. `C-1'), `isearchp-replace-on-demand' replaces
     only the current search hit.

   . With a negative prefix arg (e.g. `M--' or `C--'),
     `isearchp-replace-on-demand' toggles automatic replacement by
     just searching.  Automatic replacement means that each time
     you use a search key (e.g. `C-s') to visit a search hit, the
     hit is automatically replaced, without your needing to hit
     `C-M-RET'.  Using a prefix arg again with `C-M-RET' cancels
     this (as does quitting and restarting Isearch).

   . With a positive prefix arg N (e.g. `C-8' or `C-u 200'),
     `isearchp-replace-on-demand' replaces N search hits (but it
     stops at the search limit, if reached).

   . With a zero prefix arg (e.g. `C-0),
     `isearchp-replace-on-demand' replaces *all* remaining search
     hits (up to the search limit).

   (NOTE: To use a prefix arg within Isearch, you must set
   `isearch-allow-prefix' (if available) or `isearch-allow-scroll'
   to non-`nil'.)

 * When you use on-demand replacement (with `C-M-RET') the
   replacement text can be either inserted literally, as is, or
   interpreted as in `query-replace-regexp'.  In the latter case,
   you can use `\&', `\=\N', `\#', `\,' and `\?'.

   For example, suppose you use a regexp-search pattern of
   `\(e\)\|a' and a replacement pattern of `\,(if \1 "a" "e")'.
   Each `C-M-RET' will then swap `e' for `a' and vice versa.

   See the doc for `query-replace-regexp' and node `Regexp Replace'
   of the Emacs manual for more information.

   (Note that `\?' is supported, but it is not very useful in this
   context, because it prompts you to edit the result each time you
   hit `C-M-RET'.  Instead, use `C-u C-M-RET' whenever you want to
   change (edit) the replacement pattern.)

 * You can use `C-M-`' (`isearchp-toggle-literal-replacement')
   anytime during Isearch to toggle whether replacement text is
   used literally or interpreted per the special regexp-replacement
   constructs.

   Note that the use of the special regexp replacement patterns is
   unrelated to the kind of incremental search: literal string
   search or regexp search.  Just remember that the way to switch
   on/off the special behavior of `\&' and so on is to use `C-M-`'.

 * The value of variable `isearchp-noprompt-action-function' is a
   function that is invoked automatically, after you visit each
   search hit.  The function is called with no arguments.  It
   cannot use the minibuffer, but it can modify buffer contents.
   The variable is reset to `nil' when you quit Isearch.  As an
   example of use, command `isearchp-replace-on-demand' with a
   negative prefix arg sets this to `isearchp-replace-match', which
   causes automatic replacement each time you visit a search hit.

 * Hook `isearchp-nomodify-action-hook' (Emacs 22+ only) is also
   run after each search visit.  Its functions also must accept the
   same arguments as `isearchp-act-on-demand'.  The functions can
   use the minibuffer, but they must not update the buffer text (in
   a way noticeable by Isearch), or else that will likely lead to a
   call-stack overflow.  This is because they are called with
   Isearch suspended during `isearch-update' (which can itself be
   invoked by the action...).

 * Option (`isearchp-regexp-quote-yank-flag') and command
   (`isearchp-toggle-regexp-quote-yank', bound to `C-`') to toggle
   quoting (escaping) of regexp special characters.  With escaping
   turned off, you can yank text such as `^\*.*' without it being
   transformed to `\^\\\*\.\*'.

 * `M-:' (`isearchp-eval-sexp-and-insert') prompts you for a Lisp
   sexp, evaluates it, and appends the value to the search string.
   This is useful, for example, to use `rx' or another
   regexp-creation helper to create a regexp search pattern.

   For example: `C-M-s M-: (rx (and line-start (1+ (in "("))))'
   searches using the result of that `rx' sexp, which is "^(+".
   (The double-quote chars are removed.)

   Remember too that you can use `C-u M-:' after `M-e'.  That
   inserts the sexp value into the minibuffer, where you are
   editing the search string.  Use this when you do not want to
   simply append the sexp value to the search string, but instead
   you want to do some editing of it or the rest of the search
   string.

 * `M-g' (`isearchp-retrieve-last-quit-search') yanks the last
   successful search string (regexp or plain) from when you last
   hit `C-g' in Isearch.  Sometimes you search for something but
   abandon the search - you just want to check the locations of
   something, without staying at any of them.  Afterward, if you
   want to find them again, use `M-g'.  This yanks that search
   string, so you can append it to whatever you are already
   searching for.

 * `C-x r g' (`isearchp-append-register') appends the contents of a
   register to the search string.  You are prompted for the
   register to use.  This is the same key that is bound globally to
   `insert-register'.  If you want this key to instead exit Isearch
   and insert the register in the buffer, then define this key in
   `isearch-mode-map' as `nil' (i.e., unbind it), and optionally
   bind `isearchp-append-register' to a different key in
   `isearch-mode-map'.

 * `C-M-y' (`isearch-yank-secondary') yanks the secondary selection
   into the search string, if you also use library `second-sel.el'.

 * `C-z' (`isearchp-yank-char') yanks successive characters onto
   the search string.  This command is also bound to `C-y C-c'.

 * `C-_' (`isearchp-yank-symbol-or-char') yanks successive symbols
   (or words or subwords or chars) into the search string.

 * `C-(' (`isearchp-yank-sexp-symbol-or-char') yanks successive
   sexps (or symbols or words or subwords or chars) into the search
   string.

 * `M-w' (`isearchp-kill-ring-save') copies the current search
   string to the kill ring.  You can then, for example, use `C-s
   M-y' to search for the same thing in another Emacs session.

   (I use this all the time, but you might not use multiple Emacs
   sessions.)  Note that if you did not have this feature then you
   would need to select the search-string text (in the text buffer
   or in the `M-e' Isearch edit buffer) and copy it to the kill
   ring. (Note: `M-w' used to toggle word search, but
   `isearch-toggle-word' is now `M-s w'.)

 * All commands that yank text onto the search string are bound to
   keys with prefix `C-y' (in addition to any other Isearch
   bindings):

     `C-y C-_'   isearchp-yank-symbol-or-char
     `C-y C-('   isearchp-yank-sexp-symbol-or-char
     `C-y C-2'   isearch-yank-secondary
     `C-y C-c'   isearchp-yank-char
     `C-y C-e'   isearchp-yank-line
     `C-y C-w'   isearchp-yank-word-or-char
     `C-y C-y'   isearch-yank-kill
     `C-y M-y'   isearch-yank-pop

   You can repeat any of these for which it makes sense (i.e., all
   except `isearch-yank-secondary', `isearch-yank-kill', and
   `isearch-yank-pop') by just repeating the last key.  For
   example: `C-y C-e C-e C-e' adds the text up to the end of three
   lines.

 * `C-x 8 RET' (`isearch-char-by-name') reads the name of a Unicode
   character with completion and appends it to the search string.
   Same thing when editing the search string (i.e., after `M-e').
   This is part of GNU Emacs starting with Emacs 24.4.

 * `C-x o' (`isearchp-open-recursive-edit') opens a recursive
   editing session, where you can do anything you like (including
   search for something different).  Using `C-M-c' closes the
   recursive editing session and resumes the search (from the
   current position where you hit `C-M-c').

 * Option `isearchp-resume-with-last-when-empty-flag' non-`nil'
   (the default) means that if Isearch is resumed with an empty
   search string, after being suspended, the previous search string
   is used.  If `nil', it is resumed with an empty search string,
   as if starting over from the resumed location.

 * `C-g' after successfully finding matches restores not only the
   original position but also its relative position in the window.
   IOW, you get back to what you saw before searching.  Fixes Emacs
   bug #12253 for Isearch.

 * Highlighting of the mismatched portion of your search string in
   the minibuffer.  This is the portion that is removed if you do
   `C-g', or removed/replaced manually if you use `C-M-l' (see
   next) or automatically if you use `M-k' (see below).  I added
   this feature to GNU Emacs 23.1.

 * `C-M-l' (`isearchp-remove-failed-part') removes the failed part
    of the search string, if any.  `C-g' does this as well, but
    `C-g' also has an effect when search is successful.

 * `C-<backspace>' (`isearchp-remove-failed-part-or-last-char')
   also removes the failed part, if any.  If there is none then it
   removes the last character.  You might prefer to bind this to
   `DEL' (Backspace), in place of `isearch-delete-char'.

 * `M-k' (`isearchp-cycle-mismatch-removal') cycles automatic
   removal or replacement of the input portion that does not match.
   The behavior is controlled by the value of option
   `isearchp-drop-mismatch':

   `replace-last' - Your current input replaces the last mismatched
                    text.  You can always see your last input, even
                    if it is a mismatch.  And it is available for
                    editing using `M-e'.
   `nil'          - Your current input is appended, even if the
                    previous input has a mismatched portion.
   anything else  - Your current input is ignored (removed) if it
                    causes a mismatch.  The search string always
                    has successful matches.

 * Option `isearchp-drop-mismatch-regexp-flag' controls whether
   regexp search respects option `isearchp-drop-mismatch'.  If
   `nil' (the default value) then regexp search acts as if
   `isearchp-drop-mismatch' were `nil'.  This is because typing a
   regexp such as `[a-w]' can be problematic when mismatches are
   automatically replaced.  There is no problem for many regexp
   patterns however, so you might prefer customizing this to
   non-`nil' and using `M-k' to turn `isearchp-drop-mismatch' off
   only temporarily, when needed.

 * Non-nil option `isearchp-repeat-search-if-fail-flag' means that
   Isearch fails only when there are no search hits within the
   search limits.  If there are search hits, and if there are no
   more hits in the current search direction, then search restarts
   automatically at the limit.  You can toggle this behavior using
   `M-s M-k' anytime during Isearch.

 * You can use option `isearchp-ring-bell-function' to suppress or
   replace bell ringing (`ding') during Isearch (but not for
   quitting with `C-g').  Set it to `ignore', for example, to
   suppress any indication of an error.  Set it to a function such
   as `echo-bell' (from library `echo-bell.el'), to indicate errors
   only visually.

 * Non-`nil' option `isearchp-toggle-option-flag', which you can
   toggle using `M-s v' (`isearchp-toggle-option-toggle'),
   determines whether commands that toggle behavior also toggle an
   associated user option.  For such commands, a prefix argument
   flips the behavior, as if `isearchp-toggle-option-flag' were
   toggled temporarily.  Currently this feature applies to toggles
   `M-c' (case-sensitivity) and `M-s i' (matching hidden text).

 * `M-c' (`isearch-toggle-case-fold') toggles case sensitivity.  If
   option `isearchp-toggle-option-flag' is non-`nil' then it
   toggles option `isearchp-case-fold' to change the sensitivity
   from now on.  Otherwise, the option value is not changed, so the
   effect is for the current search only.

 * `M-s i' (`isearch-toggle-invisible') toggles invisible-text
   sensitivity.  If option `isearchp-toggle-option-flag' is
   non-`nil' then it toggles option `search-invisible' to change
   the sensitivity from now on.  Otherwise, the option value is not
   changed, so the effect is for the current search only.

 * `C-+' (`isearchp-toggle-search-invisible') toggles the value of
   option `search-invisible'.  The effect is like that of `M-s i'
   with no prefix argument and with non-`nil'
   `isearchp-toggle-option-flag'.

 * `M-s h l' (`isearchp-toggle-lazy-highlight-cleanup') toggles the
    value of option `lazy-highlight-cleanup'.  When the option
    value is `nil' you can continue to see the search hits
    highlighted from the last search.  Toggle the option off, or
    use command `isearch-lazy-highlight-cleanup', to remove the
    highlighting.  See also option `lazy-highlight-max-at-a-time'.

 * `M-s h L' (`isearchp-toggle-lazy-highlighting') toggles the
    value of option `isearch-lazy-highlight'.  Turning this
    highlighting off can sometimes speed up searching considerably,
    in particular for symmetric character folding.

 * Other bindings during Isearch:

   - `next', `prior' repeat the last Isearch forward and backward
     (easier than using the chords `C-s', `C-r').
   - `C-end' - go to the longest line.  Repeat to go to the longest
     line following that one in the buffer.  As usual, `C-g' puts
     you back where you started.  This binding is made only if you
     also use `misc-cmds.el'.
   - `C-h' provides help on Isearch while searsching.  This library
     also redefines `isearch-mode-help' so that it lists all
     Isearch bindings and ends Isearch properly.

 * `M-e' (`isearch-edit-string') automatically puts the cursor at
   the first mismatch position in the search string, for easy
   editing.  Whereas `C-g' (see also `M-k') removes all of the
   mismatch, this feature lets you change or insert a character or
   two, without losing the rest of the search string.

 * A user option, `isearchp-initiate-edit-commands', that specifies
   commands whose keys will not exit Isearch but will instead
   initiate editing of the search string.  For example, if
   `backward-char' is included in the list then `C-b' and `left'
   will just move the cursor backward over the search string so you
   can change, delete, or insert chars in the middle somewhere.
   This makes the search string more minibuffer-like.

 * You can, by default, select text with the mouse, then hit `C-s'
   etc. to search for it.  This is controlled by user option
   `isearchp-mouse-2-flag'.

 If you have Emacs 23 or later then I recommend that you also use
 the companion library, `isearch-prop.el'.  If it is in your
 `load-path' then it will be loaded by `isearch+.el'.  It lets you
 limit incremental searching to contexts that you define.

 Example: search within zones having a `face' text property with a
 value of `font-lock-comment-face' or `font-lock-string-face'.
 Search overlays or text properties.

 Besides relying on existing text properties such as `face' for
 contexts to search, you can use command
 `isearchp-put-prop-on-region' to add any text property to the
 region.  This gives you an easy way to set up contexts for
 text-property search.  For property `face', empty input to
 `isearchp-put-prop-on-region' removes all faces from the region.

 If you use library `highlight.el' then you can highlight and
 unhighlight Isearch matches in different faces, including for
 multiple-buffer searches.  That library binds keys `M-s h h' and
 `M-s h u' for this highlighting and unhighlighting.
