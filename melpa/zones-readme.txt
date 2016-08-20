    Zones of text - like multiple regions.

   More description below.

   Bug reports etc.: (concat "drew" ".adams" "@" "oracle" ".com")


(@> "Index")

 Index
 -----

 If you have library `linkd.el' and Emacs 22 or later, load
 `linkd.el' and turn on `linkd-mode' now.  It lets you easily
 navigate around the sections of this doc.  Linkd mode will
 highlight this Index, as well as the cross-references and section
 headings throughout this file.  You can get `linkd.el' here:
 http://dto.freeshell.org/notebook/Linkd.html.

 (@> "Things Defined Here")
 (@> "Documentation")
   (@> "Compatibility")
   (@> "Coalesced (United) Zones")
   (@> "Izone Commands")
   (@> "Izone List Variables")
   (@> "Keys")
   (@> "Command `zz-narrow-repeat'")
   (@> "Define Your Own Commands")
 (@> "Change log")

(@* "Things Defined Here")

 Things Defined Here
 -------------------

 Commands defined here:

   `zz-add-zone', `zz-add-zone-and-coalesce',
   `zz-add-zone-and-unite', `zz-clone-and-coalesce-zones',
   `zz-clone-and-unite-zones', `zz-clone-zones',
   `zz-coalesce-zones', `zz-delete-zone', `zz-narrow',
   `zz-narrow-repeat', `zz-select-region',
   `zz-select-region-repeat', `zz-set-izones-var',
   `zz-unite-zones'.

 Non-interactive functions defined here:

   `zz-buffer-of-markers', `zz-car-<', `zz-every',
   `zz-izone-has-other-buffer-marker-p', `zz-izone-limits',
   `zz-izone-limits-in-bufs', `zz-izones', `zz-izones-from-zones',
   `zz-izones-p', `zz-izones-renumber', `zz-marker-from-object',
   `zz-markerize', `zz-max', `zz-min', `zz-narrowing-lighter',
   `zz-number-or-marker-p', `zz-rassoc-delete-all',
   `zz-readable-marker', `zz-readable-marker-p',
   `zz-read-any-variable', `zz-read-bufs', `zz-regexp-car-member',
   `zz-remove-if', `zz-remove-if-not',
   `zz-remove-izones-w-other-buffer-markers',
   `zz-remove-zones-w-other-buffer-markers', `zz-repeat-command',
   `zz-set-intersection', `zz-set-union', `zz-some',
   `zz-string-match-p', `zz-two-zone-intersection',
   `zz-two-zone-union', `zz-zones-complement',
   `zz-zone-has-other-buffer-marker-p', `zz-zone-intersection',
   `zz-zone-intersection-1', `zz-zone-ordered',
   `zz-zones-overlap-p', `zz-zones-same-buffer-p',
   `zz-zone-union', `zz-zone-union-1'.

 Internal variables defined here:

   `zz-izones', `zz-izones-var', `zz-lighter-narrowing-part',
   `zz-add-zone-anyway-p'.


 ***** NOTE: This EMACS PRIMITIVE has been ADVISED HERE:

   `narrow-to-region'.


 ***** NOTE: The following functions defined in `lisp.el' and
             `page.el' have been REDEFINED here:

   `narrow-to-defun', `narrow-to-page'.

(@* "Documentation")

 Documentation
 -------------

 Library `zones.el' lets you easily define and subsequently act on
 multiple zones of buffer text.  You can think of this as enlarging
 the notion of "region".  In effect, it can remove the requirement
 of target text being a contiguous sequence of characters.  A set
 of buffer zones is, in effect, a (typically) noncontiguous
 "region" of text.


(@* "Compatibility")
 ** Compatibility **

 Some of the functions defined here are not available for Emacs
 versions prior to Emacs 22.  Others are not available for versions
 prior to Emacs 23.  This is mentioned where applicable.


(@* "Zones")
 ** Zones **

 A "zone" is a basic zone or an izone.  A zone represents the text
 between its two positions, just as an Emacs region is the text
 between point and mark.

 A "basic zone" is a list of two buffer positions followed by a
 possibly empty list of extra information: (POS1 POS2 . EXTRA).

 An "izone" is a list whose first element is an identifier that is
 is a natural number (1, 2, 3,...)  and whose cdr is a basic zone:
 (ID POS1 POS2 . EXTRA).

 The positions of a zone can be natural numbers (1, 2, 3,...),
 markers for the same buffer, or readable markers for the same
 buffer.  (Behavior is undefined if a zone has markers for
 different buffers.)  Each position of a given zone can take any of
 these forms.

 A "readable marker" is a list (marker BUFFER POSITION), where
 BUFFER is a buffer name (string) and where POSITION is a buffer
 position (number only).

 The positions of a zone can be in either numeric order.  The
 positions are also called the zone "limits".  The lower limit is
 called the zone "beginning"; the upper limit is called its "end".


(@* "Coalesced (United) Zones")
 ** Coalesced (United) Zones **

 A list of zones can contain zones that overlap or are adjacent
 (the end of one is one less than the beginning of the other).

 Basic-zone union and intersection operations (`zz-zone-union',
 `zz-zone-intersection') each act on a list of zones, returning
 another such list, but which has POS1 <= POS2 in each of its
 zones, and which lists its zones in ascending order of their cars.
 For basic-zone union, the resulting zones are said to be
 "coalesced", or "united".

 The extra info in the zones that result from zone union or
 intersection is just the set union or set intersection of the
 extra info in the zones that are combined.

 After a list of zones has been altered by `zz-zone-union' or
 `zz-zone-intersection':

 * Each zone in the result list is ordered so that its first
   element is smaller than its second.

 * The zones in the result list have been sorted in ascending order
   by their first elements.

 * The zones in the result list are not adjacent and do not
   overlap: there is some other buffer text (i.e., not in any zone)
   between any two zones in the result.


(@* "Izone Commands")
 ** Izone Commands **

 Commands that manipulate lists of zones generally use izones,
 because they make use of the zone identifiers.

 Things you can do with zones:

 * Sort them.

 * Unite (coalesce) adjacent or overlapping zones (which includes
   sorting them).

 * Intersect them.

 * Narrow the buffer to any of them.  Cycle among narrowings.  If
   you use library `icicles.el' then you can also navigate among
   them in any order, and using completion against BEG-END range
   names.

 * Select any of them as the active region.  Cycle among regions.

 * Search them (they are automatically coalesced first).  For this
   you need library `isearch-prop.el'.

 * Highlight and unhighlight them.  For this you need library
   `highlight.el' or library `facemenu+.el' (different kinds of
   highlighting).

 * Add the active region to a list of zones.

 * Add the region to a list of zones, and then unite (coalesce) the
   zones.

 * Delete an izone from a list of zones.

 * Clone a zones variable to another one, so the clone has the same
   zones.

 * Clone a zones variable and then unite the zones of the clone.

 * Make an izone variable persistent, in a bookmark.  Use the
   bookmark to restore it in a subsequent Emacs session.  For this
   you need library `bookmark+.el'.


(@* "Izone List Variables")
 ** Izone List Variables **

 Commands that use izones generally use a variable that holds a
 list of them.  By default, this is the buffer-local variable
 `zz-izones'.  But such a variable can be buffer-local or global.
 If it is global then it can use markers and readable markers for
 different buffers.

 The value of variable `zz-izones-var' is the variable currently
 being used by default for izone commands.  The default value is
 `zz-izones'.

 You can have any number of izones variables, and they can be
 buffer-local or global variables.

 You can use `C-x n v' (command `zz-set-izones-var') anytime to set
 `zz-izones-var' to a variable whose name you enter.  With a prefix
 argument, the variable is made automatically buffer-local.  Use
 `C-x n v' to switch among various zone variables for the current
 buffer (if buffer-local) or globally.

 Sometimes another zone command prompts you for the izones variable
 to use, if you give it a prefix argument.  The particular prefix
 arg determines whether the variable, if not yet bound, is made
 buffer-local, and whether `zz-izones-var' is set to the variable
 symbol:

  prefix arg         buffer-local   set `zz-izones-var'
  ----------         ------------   -------------------
   Plain `C-u'        yes            yes
   > 0 (e.g. `C-1')   yes            no
   = 0 (e.g. `C-0')   no             yes
   < 0 (e.g. `C--')   no             no

 For example, `C-u C-x n a' (`zz-add-zone') prompts you for a
 different variable to use, in place of the current value of
 `zz-izones-var'.  The variable you enter is made buffer-local and
 it becomes the new default izones variable for the buffer; that
 is, `zz-izones-var' is set to the variable symbol.

 As another example, suppose that `zz-izones-var' is `zz-izones',
 the default value and buffer-local by design.  If you then use
 `C-- C-x n s' and enter a variable name at the prompt, that
 variable is not made buffer-local, and `zz-izones-var' is not set
 to that variable.  The active region is pushed to the variable,
 but because `zz-izones-var' is unchanged, a subsequent `C-x n s'
 (no prefix arg) pushes to `zz-izones'.


(@* "Keys")
 ** Keys **

 Most of the commands that manipulate izones are bound on keymap
 `narrow-map'.  They are available on prefix key `C-x n', along
 with the narrowing/widening keys `C-x n d', `C-x n n', `C-x n p',
 and `C-x n w':

 C-x n a   `zz-add-zone' - Add to current izones variable
 C-x n A   `zz-add-zone-and-unite' - Add izone, then unite izones
 C-x n c   `zz-clone-zones' - Clone zones from one var to another
 C-x n C   `zz-clone-and-unite-zones' - Clone then unite zones
 C-x n d   `narrow-to-defun'
 C-x n C-d `zz-delete-zone' - Delete an izone from current var
 C-x n h   `hlt-highlight-regions' - Highlight izones
 C-x n H   `hlt-highlight-regions-in-buffers' - in multiple buffers
 C-x n n   `narrow-to-region'
 C-x n p   `narrow-to-page'
 C-x n r   `zz-select-region-repeat' - Cycle as active regions
 C-x n u   `zz-unite-zones' - Unite (coalesce) izones
 C-x n v   `zz-set-izones-var' - Set `zz-izones-var' to a variable
 C-x n w   `widen'
 C-x n x   `zz-narrow-repeat' - Cycle as buffer narrowings


(@* "Command `zz-narrow-repeat'")
 ** Command `zz-narrow-repeat' **

 Library `zones.el' modifies commands `narrow-to-region',
 `narrow-to-defun', and `narrow-to-page' (`C-x n n', `C-x n d',
 and `C-x n p') so that the current buffer restriction
 (narrowing) is added to the izone list the current buffer (by
 default, buffer-local variable `zz-izones').

 You can then use `C-x n x' to cycle among previous buffer
 narrowings.  Repeating `x' repeats the action: `C-x n x x x x'
 etc.  Each time you hit `x' a different narrowing is made current.
 This gives you an easy way to browse your past narrowings.

 If the izone variable is not buffer-local then `zz-narrow-repeat'
 can cycle among the narrowings in different buffers, switching the
 buffer accordingly.

 Invoking `C-x n x' with a prefix argument changes the behavior
 as follows:

 * A plain prefix arg (`C-u') widens the buffer completely.

 * A zero numeric prefix arg (e.g `C-0') widens completely and
   resets (empties) the current izone variable.

 * A numeric prefix arg N takes you directly to the abs(N)th
   previous buffer narrowing.  That is, it widens abs(N) times.
   Positive and negative args work the same, except that a negative
   arg also pops entries off the ring: it removes the ring entries
   from the most recent back through the (-)Nth one.

 By default, `C-x n x' is bound to command `zz-narrow-repeat'.
 (For Emacs versions prior to 22 it is bound by default to
 `zz-narrow', which is a non-repeatable version.  Repeatability is
 not available before Emacs 22.)

 The mode-line lighter `Narrow' is still used for the ordinary
 Emacs narrowing commands.  But for `zz-narrow-repeat' (`C-x n x')
 the current narrowing is indicated in the lighter by an
 identifying number: `Narrow-1', `Narrow-2', and so on.  `mouse-2'
 on the `Narrow' part still widens completely, but `mouse-2' on the
 `-NUM' part uses `zz-narrow-repeat' to cycle to the next
 narrowing.


(@* "Define Your Own Commands")
 ** Define Your Own Commands **

 Pretty much anything you can do with the Emacs region you can do
 with a set of zones (i.e., with a non-contiguous "region").  But
 existing Emacs commands that act on the region do not know about
 non-contiguous regions.  What you will need to do is define new
 commands that take these into account.

 You can define your own commands that iterate over a list of
 izones in a given buffer, or over such lists in a set of buffers.
 Utility functions `zz-izone-limits', `zz-izone-limits-in-bufs',
 and `zz-read-bufs' can help with this.

 As examples of such commands, if you use library `highlight.el'
 then you can use `C-x n h' (command `hlt-highlight-regions') to
 highlight the izones recorded for the current buffer.  You can use
 `C-x n H' (command `hlt-highlight-regions-in-buffers') to do the
 same across a set of buffers that you specify (or across all
 visible buffers).  If option `hlt-auto-faces-flag' is non-nil then
 each region gets a different face.  Otherwise, all of the regions
 are highlighted with the same face.  Complementary (unbound)
 commands `hlt-unhighlight-regions' and
 `hlt-unhighlight-regions-in-buffers' unhighlight.

 Defining your own command can be simple or somewhat complex,
 depending on how the region is used in the code for the
 corresponding region-action Emacs command.  The definition of
 `hlt-highlight-regions' just calls existing function
 `hlt-highlight-region' once for each recorded region:

(defun hlt-highlight-regions (&optional regions face msgp mousep
                                        buffers)
  "Apply `hlt-highlight-region' to regions in `zz-izones'."
  (interactive (list (zz-izone-limits) nil t current-prefix-arg))
  (dolist (start+end  regions)
    (hlt-highlight-region (nth 0 start+end) (nth 1 start+end)
                          face msgp mousep buffers)))

 That's it - just iterate over `zz-izones' with a function that
 takes the region as an argument.  What `zones.el' offers in this
 regard is a way to easily define a set of buffer zones.
