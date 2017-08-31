   Extensions to `info.el'.

 More description below.

 If you use Emacs 20, 21, or 22 then use library `info+20.el'
 instead of `info+.el'.

(@> "Index")

 Index
 -----

 If you have library `linkd.el', load `linkd.el' and turn on
 `linkd-mode' now.  It lets you easily navigate around the sections
 of this doc.  Linkd mode will highlight this Index, as well as the
 cross-references and section headings throughout this file.  You
 can get `linkd.el' here:
 http://www.emacswiki.org/emacs/download/linkd.el.

 (@> "Things Defined Here")
 (@> "Documentation")
 (@> "Macros")
 (@> "Faces (Customizable)")
 (@> "User Options (Customizable)")
 (@> "Internal Variables")
 (@> "New Commands")
 (@> "Replacements for Existing Functions")
 (@> "Non-Interactive Functions")

(@* "Things Defined Here")

 Things Defined Here
 -------------------

 Commands defined here:

   `Info-breadcrumbs-in-mode-line-mode',
   `Info-change-visited-status' (Emacs 24+),
   `Info-describe-bookmark' (Emacs 24.2+),
   `Info-follow-nearest-node-new-window', `Info-goto-node-web',
   `Info-history-clear', `Info-make-node-unvisited', `info-manual',
   `Info-merge-subnodes',
   `Info-mouse-follow-nearest-node-new-window',
   `Info-outline-demote', `Info-outline-promote',
   `Info-persist-history-mode' (Emacs 24.4+),
   `Info-save-current-node', `Info-set-breadcrumbs-depth',
   `Info-set-face-for-bookmarked-xref' (Emacs 24.2+),
   `Info-toggle-breadcrumbs-in-header',
   `Info-toggle-fontify-angle-bracketed',
   `Info-toggle-fontify-bookmarked-xrefs' (Emacs 24.2+),
   `Info-toggle-fontify-emphasis',
   `Info-toggle-fontify-quotations',
   `Info-toggle-fontify-single-quote',
   `Info-toggle-node-access-invokes-bookmark' (Emacs 24.4+),
   `Info-toc-outline', `Info-toc-outline-refontify-region',
   `Info-url-for-node', `Info-virtual-book'.

 Faces defined here:

   `info-command-ref-item', `info-constant-ref-item',
   `info-double-quoted-name', `info-emphasis', `info-file',
   `info-function-ref-item',`info-macro-ref-item', `info-menu',
   `info-node', `info-quoted-name', `info-reference-item',
   `info-single-quote', `info-special-form-ref-item',
   `info-string', `info-syntax-class-item',
   `info-user-option-ref-item', `info-variable-ref-item',
   `info-xref-bookmarked' (Emacs 24.2+).

 Options (user variables) defined here:

   `Info-bookmarked-node-xref-faces' (Emacs 24.2+),
   `Info-breadcrumbs-in-header-flag',
   `Info-display-node-header-fn', `Info-emphasis-regexp',
   `Info-fit-frame-flag', `Info-fontify-angle-bracketed-flag',
   `Info-fontify-bookmarked-xrefs-flag' (Emacs 24.2+),
   `Info-fontify-emphasis-flag', `Info-fontify-quotations-flag',
   `Info-fontify-reference-items-flag',
   `Info-fontify-single-quote-flag',
   `Info-node-access-invokes-bookmark-flag' (Emacs 24.4+),
   `Info-saved-history-file' (Emacs 24.4+), `Info-saved-nodes',
   `Info-subtree-separator', `Info-toc-outline-no-redundancy-flag'.

 Macros defined here:

   `info-user-error'.

 Non-interactive functions defined here:

   `Info-bookmark-for-node', `Info-bookmark-name-at-point',
   `Info-bookmark-named-at-point', `Info-bookmark-name-for-node',
   `Info-display-node-default-header', `info-fontify-quotations',
   `info-fontify-reference-items',
   `Info-insert-breadcrumbs-in-mode-line', `Info-isearch-search-p',
   `Info-node-name-at-point', `Info-read-bookmarked-node-name',
   `Info-restore-history-list' (Emacs 24.4+),
   `Info-save-history-list' (Emacs 24.4+), `Info-search-beg',
   `Info-search-end', `Info-toc-outline-find-node',
   `Info-toc-outline-refontify-links'.

 Internal variables defined here:

   `Info-breadcrumbs-depth-internal', `info-fontify-emphasis',
   `Info-merged-map', `Info-mode-syntax-table',
   `info-quotation-regexp', `info-quoted+<>-regexp',
   `Info-toc-outline-map'.


 ***** NOTE: The following standard faces defined in `info.el'
             have been REDEFINED HERE:

 `info-title-1', `info-title-2', `info-title-3', `info-title-4'.


 ***** NOTE: The following standard functions defined in `info.el'
             have been REDEFINED or ADVISED HERE:

 `info-display-manual' - Use completion to input manual name.
 `Info-find-emacs-command-nodes' - Added arg MSGP and message.
 `Info-find-file' - Handle virtual books.
 `Info-find-node', `Info-find-node-2' -
    Call `fit-frame' if `Info-fit-frame-flag'.
    Added optional arg NOMSG.
 `Info-fontify-node' -
    1. Show breadcrumbs in header line and/or mode line.
    2. File name in face `info-file'.
    3. Node names in face `info-node'.
    4. Menu items in face `info-menu'.
    5. Only 5th and 9th menu items have their `*' colored.
    6. Notes in face `info-xref'.
    7. If `Info-fontify-emphasis-flag', then fontify _..._.
    8. If `Info-fontify-quotations-flag', then fontify ‘...’ or
       `...' in face `info-quoted-name', “...” in face
       `info-double-quoted-name',  and "..." in face `info-string'.
    9. If `Info-fontify-angle-bracketed-flag' and
       `Info-fontify-quotations-flag' then fontify <...> in face
       `info-quoted-name'.
   10. If `Info-fontify-single-quote-flag' and
       `Info-fontify-quotations-flag', then fontify ' in face
       `info-single-quote'.
 `Info-goto-emacs-command-node' -
    1. Uses `completing-read' in interactive spec, with,
       as default, `symbol-nearest-point'.
    2. Added optional arg MSGP.
    3. Message if single node found.
    4. Returns `num-matches' if found; nil if not.
 `Info-goto-emacs-key-command-node' -
    1. Added optional arg MSGP.
    2. If key's command not found, then `Info-search's for key
       sequence in text and displays message about repeating.
 `Info-goto-node' - Respect option
    `Info-node-access-invokes-bookmark-flag' (Emacs 24.4+).
 `Info-history' - A prefix arg clears the history.
 `Info-insert-dir' -
    Added optional arg NOMSG to inhibit showing progress msgs.
 `Info-mode' - Doc string shows all bindings.
 `Info-read-node-name'   - Added optional arg DEFAULT.
 `Info-search' - 1. Fits frame.
                 2. Highlights found regexp if `search-highlight'.
 `Info-set-mode-line' - Handles breadcrumbs in the mode line.
 `Info-mouse-follow-nearest-node' - With prefix arg, show node in
                                    a new Info buffer.
 `Info-isearch-search' - Respect restriction to active region.
 `Info-isearch-wrap' - Respect restriction to active region.


 ***** NOTE: The following standard function
             has been REDEFINED HERE:

 `outline-invisible-p' - Fixes Emacs bug #28080.

(@* "Documentation")

 Documentation
 -------------

 Library `info+.el' extends the standard Emacs library `info.el' in
 several ways.  It provides:

 * Association of additional information (metadata) with Info
   nodes.  You do this by bookmarking the nodes.  Library Bookmark+
   gives you the following features in combination with `info+.el'.
   In many ways an Info node and its default bookmark can be
   thought of as the same animal.

   - Rich node metadata.  In particular, you can tag nodes with any
     number of arbitrary tags, to classify them in different and
     overlapping ways.  You can also annotate them (in Org mode, by
     default).

   - You can use `C-h C-b' to show the metadata for a (bookmarked)
     node.  This is all of the associated bookmark information,
     including the annotation and tags for that node and the number
     of times you have visited it.  If invoked with point on a
     link, the targeted node is described; otherwise, you are
     prompted for the node name.

   - Links for bookmarked nodes can have a different face, to let
     you know that those nodes have associated metadata.  Option
     `Info-fontify-bookmarked-xrefs-flag' controls whether this is
     done.

   - The face for this is `info-xref-bookmarked' by default, but
     you can set the face to use for a given Info bookmark using
     `C-x f' (command `Info-set-face-for-bookmarked-xref').  This
     gives you an easy way to classify nodes and show the class of
     a node by its links.  Uses faces to make clear which nodes are
     most important to you, or which are related to this or that
     general topic.

   - If option `Info-node-access-invokes-bookmark-flag' is non-nil
     then going to a bookmarked Info node invokes its bookmark, so
     that the node metadata (such as number of visits) gets
     updated.  Command `Info-toggle-node-access-invokes-bookmark'
     toggles the option value.

   - You can automatically bookmark nodes you visit, by enabling
     mode `bmkp-info-auto-bookmark-mode'.  Toggle the mode off
     anytime you do not want to record Info visits.

   - In the bookmark-list display (from `C-x r l') you can sort
     bookmarks by the time of last visit (`s d') or by the number
     of visits (`s v').  This gives you an easy way to see which
     parts of which Info manuals you have visited most recently and
     how much you have visited them.

 * Editable, outline-enabled tables of contents (TOCs).  Command
   `Info-toc-outline' (bound to `O') opens a separate Info buffer
   showing the table of contents (TOC).  This is similar to the
   standard command `Info-toc' (bound to `T'), but the buffer is
   cloned from the manual and is in `outline-minor-mode'.  Also,
   there is no redundancy, by default: each TOC entry is listed
   only once, not multiple times.  (This is controlled by option
   `Info-toc-outline-no-redundancy-flag'.)

   You can have any number of such TOCs, for the same manual or for
   different manuals.

   Outline minor mode lets you hide and show, and promote and
   demote, various parts of the TOC tree for a manual.  And since
   the TOC is editable you can make other changes to it: sort parts
   of it, delete parts of it, duplicate parts of it, move parts
   aroundin an ad hoc way, and so on.  Info+ makes the outlining
   commands behave, so that hidden Info text (e.g. markup text such
   as `*note'...`::' surrounding links) is kept hidden.

   Especially when combined with `Info-persist-history-mode',
   command `Info-change-visited-status' (`C-x DEL', see below), and
   the Info+ bookmarking enhancements (e.g., special link
   highlighting and persistently tracking the number of visits per
   node), `Info-toc-outline' gives you a way to organize access and
   visibility of a manual's nodes, to reflect how you use it.

 * Additional, finer-grained Info highlighting.  This can make a
   big difference in readability.

   - Quoted names, like this: `name-stands-out' or
     `name-stands-out', and strings, like this: "string-stands-out"
     are highlighted if `Info-fontify-quotations-flag' is
     non-`nil'.

   - Angle-bracketed names, like this: <tab>, are highlighted if
     `Info-fontify-angle-bracketed-flag' and
     `Info-fontify-quotations-flag' are non-`nil'.

   - Isolated single quotes, like this: 'foobar, are highlighted if
     `Info-fontify-single-quote-flag' and
     `Info-fontify-quotations-flag' are non-`nil'.

   - Emphasized text, that is, text enclosed in underscore
     characters, like this: _this is emphasized text_, is
     highlighted if `Info-fontify-emphasis-flag' is non-`nil'.
     (But if internal variable `info-fontify-emphasis' is `nil'
     then there is no such highlighting, and that option has no
     effect.)

   - In the Emacs Lisp manual, reference items are highlighted, so
     they stand out.  This means: constants, commands, functions,
     macros, special forms, syntax classes, user options, and other
     variables.

   Be aware that such highlighting is not 100% foolproof.
   Especially for a manual such as Emacs or Elisp, where arbitrary
   keys and characters can be present anywhere, the highlighting
   can be thrown off.

   You can toggle each of the `Info-fontify-*-flag' options from
   the `Info' menu or using an `Info-toggle-fontify-*' command.
   For example, command `Info-toggle-fontify-emphasis' toggles
   option `Info-fontify-emphasis-flag'.

 * You can show breadcrumbs in the mode line or the header line, or
   both. See where you are in the Info hierarchy, and access higher
   nodes directly.

   - In the mode line.  Turned on by default.

     See ‘Toggle Breadcrumbs’ in the `mouse-3' mode-line menu and
     `Toggle Breadcrumbs in Mode Line' in the `Info' menu (in the
     menu-bar or in the minor-mode indicator). You can customize
     option `Info-breadcrumbs-in-mode-line-mode' if you want to
     turn this off by default. (Available for Emacs 23+ only.)

   - In the header (just below the header line).

     (I also added this to vanilla Emacs 23.)  This is OFF by
     default in `Info+'.  See `Toggle Breadcrumbs in Header Line'
     in `Info' menu.  Be aware that unlike breadcrumbs in the mode
     line, this can occasionally throw off the destination accuracy
     of cross references and searches slightly.

 * Some of the commands defined here:

   - `Info-virtual-book' (bound to `v') – Open a virtual Info
     manual of saved nodes from any number of manuals.  The nodes
     are those saved in option `Info-virtual-book'.  With `C-u',
     bookmarked Info nodes are also included.  (If you use Icicles,
     see also `icicle-Info-virtual-book'.)

   - `Info-persist-history-mode' - Enabling this minor mode saves
     the list of your visited Info nodes between Emacs sessions.
     Together with command `Info-history' (bound to `L' by
     default), this gives you a persistent virtual manual of the
     nodes you have visited in the past.  If the mode is enabled
     then the list of visited nodes is saved to the file named by
     option `Info-saved-history-file' when you quit Emacs (not
     Info) or when you kill an Info buffer.

     (If you also use library Bookmark+ then you can bookmark Info
     nodes, including automatically.  This records how many times
     you have visited each node and when you last did so.)

   - `Info-change-visited-status' (bound to `C-x DEL') - Toggle or
     set the visited status of the node at point or the nodes in
     the active region.  Useful if you use
     `Info-fontify-visited-nodes' to show you which nodes you have
     visited.  No prefix arg: toggle.  Non-negative prefix arg: set
     to visited.  Negative prefix arg: set to unvisited.

   - `Info-save-current-node' (bound to `.') – Save the name of the
     current node to list `Info-saved-nodes', for use by `v'
     (`Info-virtual-book').

   - `Info-merge-subnodes' – Integrate the current Info node with
     its subnodes (the nodes in its Menu), perhaps recursively.

     Use `Info-merge-subnodes' to extract a self-contained report
     (possibly the whole manual) from an Info manual.  The report
     is itself an Info buffer, with hyperlinks and normal Info
     behavior.

     There are various prefix-argument possibilities that govern
     just how subnodes are treated (recursively or not, for
     instance).  There are a few user options that let you
     customize the report appearance.


 The following bindings are made here for Info-mode:

   `?'              `describe-mode' (replaces `Info-summary')
   `+'              `Info-merge-subnodes'
   `.'              `Info-save-current-node'
   `a'              `info-apropos'
   `G'              `Info-goto-node-web'
   `O'              `Info-toc-outline'
   `v'              `Info-virtual-book'
   `mouse-4'        `Info-history-back'
   `mouse-5'        `Info-history-forward'
   `S-down-mouse-2' `Info-mouse-follow-nearest-node-new-window'
   `S-RET'          `Info-follow-nearest-node-new-window'

 The following bindings are made here for merged Info buffers:

   `.'              `beginning-of-buffer'
   `b'              `beginning-of-buffer'
   `q'              `quit-window'
   `s'              `nonincremental-re-search-forward'
   `M-s'            `nonincremental-re-search-forward'
   `TAB'            `Info-next-reference'
   `ESC TAB'        `Info-prev-reference'

 The global binding `C-h r' is changed from `info-emacs-manual' to
 `info-manual', which behaves the same except if you use a prefix
 arg.  With a prefix arg you can open any manual, choosing either
 from all installed manuals or from those that are already shown in
 Info buffers.

 The following behavior defined in `info.el' has been changed:
  "*info" has been removed from `same-window-buffer-names', so that
  a separate window can be used if you so choose.

 Suggestion: Use a medium-dark background for Info.  Try, for
 example, setting the background to "LightSteelBlue" in your
 `~/.emacs' file.  You can do this as follows:

        (setq special-display-buffer-names
              (cons '("*info*" (background-color . "LightSteelBlue"))
                    special-display-buffer-names))

 Alternatively, you can change the background value of
 `special-display-frame-alist' and set `special-display-regexps' to
 something matching "*info*":

        (setq special-display-frame-alist
              (cons '(background-color . "LightSteelBlue")
                    special-display-frame-alist))
        (setq special-display-regexps '("[ ]?[*][^*]+[*]"))

 If you do use a medium-dark background for Info, consider
 customizing face to a lighter foreground color - I use "Yellow".

 Also, consider customizing face `link' to remove its underline
 attribute.

 This file should be loaded after loading the standard GNU file
 `info.el'.  So, in your `~/.emacs' file, do this:
 (eval-after-load "info" '(require 'info+))
