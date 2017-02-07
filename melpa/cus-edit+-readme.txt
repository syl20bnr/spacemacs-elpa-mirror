Enhancements to `cus-edit.el'.

 Terminology
 -----------

 Most of the enhancements here basically try to make Customize play
 a little better with Emacs.  Customize deals with user
 preferences, which are faces and variables.  User variables are
 also called "user options"; they are defined by `user-variable-p'.
 All faces are user preferences.

 Note: The Customize source code sometimes refers to both faces and
 user variables (what I am calling "user preferences") as "user
 options".  The latter term has traditionally been used in Emacs for
 user variables only.

 Contents
 --------

 The enhancements to standard library `cus-edit.el' provided by
 `cus-edit+.el' include miscellaneous enhancements, such as
 formatting the Customize buffer better, and enhancements to make
 Customize play better with the rest of Emacs.  Most of the
 description here is about the latter.

 Customizing Options of a Particular Type
 ----------------------------------------

 Command `customize-apropos-options-of-type' is similar to the
 standard command `customize-apropos-options', but it also lets you
 specify the type of the options to find.

 To use `customize-apropos-options-of-type', you specify a
 `defcustom' type and a regexp.  The command opens a Customize
 buffer for all options that are defined with the specified type
 and whose names match the specified regexp.

 With a prefix argument, the matching options need not be defined
 with exactly the same `defcustom' type.  They need only have their
 current value be compatible with the type you specify.

 Enhancements to Make Customize Play Better with Emacs
 -----------------------------------------------------

 There are various ways to change preferences outside of Customize,
 and Customize unfortunately doesn't recognize these changes as
 preference settings - it considers them to be "changed outside the
 customize buffer", which really means that it just throws up its
 hands.

 The main idea here is to let Customize know about changes made
 outside it, to make Customize think that they were made inside
 Customize.  In this way, there is no such outside/inside
 distinction (for user preferences), and it doesn't matter where or
 how a preference is changed.

 When this is the case, you can use command `customize-unsaved' to
 check all preferences that have been changed from their standard
 or saved settings, and it will take all changes into account, no
 matter how or where the changes were made.

 Because this is useful, `customize-unsaved' has been modified to
 be used as a hook in `kill-emacs-query-functions'.  That way,
 before you quit Emacs, you are notified of all preference changes
 you have made and given a chance to save them (individually or
 collectively).  This is analogous to Emacs asking you about files
 you've changed but not saved, before letting you exit.

 If you also use another hook function to confirm exit from Emacs,
 and it is invoked after `customize-unsaved', then you will likely
 want to also customize `customize-unsaved-confirm-exits-flag' to
 non-`nil'.  That stops `customize-unsaved' from canceling
 quitting.  In this scenario, `C-x C-c' becomes, in effect, a key
 binding for `customize-unsaved': Say `y' to see your unsaved
 changes and then `y' or `n' to the next prompt to quit Emacs or
 not, depending on whether you want to save some changes before
 quitting.

 Updating Customize with External Changes
 ----------------------------------------

 In order to keep Customize synched with changes made outside it,
 command `customize-update-all' can be used automatically during
 idle moments to tell Customize that all user options and faces
 that have been changed outside it are now set - just as if they
 were set in Customize.  You can turn this automatic updating on
 and off with command `customize-toggle-outside-change-updates'.

 Read This If You Use Frame-Specific Faces
 -----------------------------------------

 The automatic updating that you turn on (or off) with
 `customize-toggle-outside-change-updates' uses internal variable
 `custom-update-timer' to periodically run command
 `customize-update-all'.  That command in turn uses commands
 `customize-update-all-vars' to update variables and
 `customize-update-all-faces' to updated faces.  It thus updates
 Customize with respect to face changes as well as variable
 changes.

 Unlike user options (variables), faces can be specific to
 individual frames.  The face updating code used here assumes that
 you do not use different versions of a face on different frames,
 that, for example, face `foo' has the same attributes on all
 frames.  If that is not the way you use faces and frames, then you
 might not want to automatically update Customize to recognize face
 changes made outside Customize.  In that case, change the value of
 `custom-update-timer' in your init file (~/.emacs) to use
 `customize-update-all-vars' instead of `customize-update-all':

 (defvar custom-update-timer
   (run-with-idle-timer custom-check-for-changes-interval
                        t 'customize-update-all-vars) ; <======
   "Timer used to automatically tell Customize of outside changes
 to preferences when idle.")

 The automatic face updating works like this: The current-frame
 definition of each face is compared with the face's definition
 according to Customize.  If these are different, then the
 Customize definition is updated to the current-frame definition.
 Since there is no way to know which frame will be current when
 updating takes place, if you 1) use automatic updating and 2) use
 different definitions of a given face on different frames, then
 you probably do not want to update faces automatically.

 Dealing with Spurious Changes, 1: Save
 --------------------------------------

 Even if you don't change any preferences yourself, when you quit
 Emacs the first time you are informed that there are lots of
 changed preferences, and you are given a chance to save those
 changes.  What are those changes? They represent all of the user
 preferences that Emacs and various Emacs libraries have changed
 behind Customize's back - even before you did anything.

 You'll see user options like `baud-rate' that are set in Emacs C
 code without informing Customize to mark their settings as
 `standard' (= installed).  There shouldn't be any such apparent
 "changes", since this is part of standard Emacs, but that's the
 way it is, for now.  Customize is still fairly new, and lots of
 Emacs libraries still define and change user preferences without
 going through Customize and, in effect, telling it not to consider
 such preference changes as changes.

 If you choose to save these preference changes, you will never
 again be bothered by being informed that they have changed (unless
 you change them).  So, that's one solution to this bother, which
 makes it a one-time only nuisance: just say "save all".

 Dealing with Spurious Changes, 2: Ignore
 ----------------------------------------

 Another solution is also possible.  Some user preferences, like
 `case-fold-search' and `debug-on-error' are really the kind of
 thing that you change often and temporarily - you don't really
 care about saving their changes, and you certainly don't want to
 be asked whether or not you want to save them each you quit Emacs.

 To deal with that, a list of ignored preferences,
 `customize-unsaved-ignore', is defined here.  Its preferences
 (symbols) are not used by `customize-unsaved' at all (you can
 override that interactively with a prefix arg).  So, the other way
 to deal with the legacy Emacs preferences, besides just saving
 them in your custom file, is to add them to
 `customize-unsaved-ignore' so `customize-unsaved' will ignore
 them.

 To make it easy for you to add preferences to this ignore list,
 `Ignore Unsaved Changes' menu items and buttons have been added.
 You can choose to ignore specific preferences or all preferences
 in a Customize buffer - in particular, all preferences in the
 Customize buffer from `customize-unsaved' (all changed
 preferences).

 Dealing with Spurious Changes, 3: Consider Unchanged
 ----------------------------------------------------

 There is also a third way to treat preference changes that you are
 not responsible for, as an alternative to saving them to your
 custom file or having Customize always ignore them: tell Customize
 to consider the current changes as unchanged.  This essentially
 treats them as having been saved, but without saving them.  You can
 do this using the `Consider Unchanged' menu items and buttons
 added here.

 For instance, after starting Emacs, you can examine the current
 preference changes (using `customize-unsaved') from Emacs itself
 and loaded libraries, and choose `Consider Unchanged' to let
 Customize know that the current values are to be treated as if
 they were saved, but without actually saving them to your custom
 file.  That way, your custom file is not polluted with things that
 you are not really concerned with, yet you are not bothered by
 seeing such fictitious changes show up each time you check for
 changes.

 However, unlike ignoring changes to certain preferences, and
 really saving current preference values, `Consider Unchanged' is
 not a persistent change.  You can use it at any time to "reset"
 the change counter for given preferences, so that the current
 change is considered the new base value (as if it were saved), and
 any further changes you make to them will show up as changes,
 using `customize-unsaved'.

 Updating, Revisited
 -------------------

 Finally, there is one more additional menu item and button
 provided here, `Set from External Changes', which just executes
 the appropriate `customize-update-all*' command (depending on the
 menu/button location).  It is useful if you do not choose to use
 automatic updating of preferences that are changed outside of
 Customize.

 Conclusion & Future
 -------------------

 The changes in Customize behavior provided by this library are
 important, because they can encourage library authors to explore
 other ways of changing preference values, and still let users save
 the changes using Customize.  For instance in my library
 `doremi-frm.el' I have defined several commands that let you
 directly manipulate frame and face properties (e.g. incremental
 color changes).

 By making Customize aware of such "outside" changes, you can
 easily save them in Customize.  There are lots of preferences that
 would be amenable to such "direct-manipulation", which I think
 would be an improvement in ease of customization.

 In sum, this library in effect gets rid of the useless "changed
 outside Customize" state.  User preferences have only the
 `standard', `saved', or `set' (= unsaved) state, regardless of how
 they are set.  Someday, Customize will be more integrated with the
 rest of Emacs, and "changed outside Customize" will make no more
 sense than "changed outside of Emacs".  Customize will then be
 just one possible way to access the space of user preferences, the
 same preference space that other parts of Emacs can access
 differently.  If we're writing on the same blackboard, you can see
 what I erase, and I can see what you write.

 That's the goal.  This tries to be a step on the way.

 How To Use
 ----------

 To use this library, put this in your init file (.emacs):

   (require cus-edit+)

 To update Customize once, manually, so that it learns about stuff
 that was customized outside it, use any of these commands:
 `customize-update-all-faces', `customize-update-all-vars',
 `customize-update-all' (which is both faces and variables).

 To do the same updating automatically, add this to your init file
 (automatic updating is off by default):

   (customize-toggle-outside-change-updates 99)

 To turn automatic updating on/off, use command
 `customize-toggle-outside-change-updates'.

 To change the number of idle seconds before automatically updating
 Customize, use command `customize-set-auto-update-timer-period'.

 To *not* have `customize-unsaved' check for unsaved preference
 changes when you quit Emacs, add this also:

   (remove-hook 'kill-emacs-query-functions 'customize-unsaved)


 Options (variables) defined here:

   `customp-buffer-create-hook', `custom-buffer-verbose-help'
   (Emacs 20, 21 only), `customize-unsaved-confirm-exits-flag',
   `customize-unsaved-ignore'.


 Commands defined here:

   `Custom-consider-unchanged', `Custom-ignore-unsaved',
   `customize-apropos-options-of-type',
   `customize-consider-all-faces-unchanged',
   `customize-consider-all-unchanged',
   `customize-consider-all-vars-unchanged',
   `customize-other-window',
   `customize-set-auto-update-timer-period',
   `customize-toggle-outside-change-updates',
   `customize-update-all', `customize-update-all-faces',
   `customize-update-all-vars'.

 Functions defined here:

   `custom-consider-face-unchanged',
   `custom-consider-variable-unchanged',
   `custom-ignore-unsaved-preference', `custom-type',
   `custom-update-face', `custom-update-variable',
   `custom-value-satisfies-type-p', `custom-var-inherits-type-p',
   `custom-var-is-of-type-p', `custom-var-matches-type-p',
   `custom-var-val-satisfies-type-p'.

 Internal variables defined here:

   `custom-check-for-changes-interval',
   `custom-check-for-changes-when-idle-p', `custom-update-timer'.


 ***** NOTE: The following variables defined in `cus-edit.el' have
             been REDEFINED HERE:

   `custom-commands', `custom-face-menu', `Custom-mode-menu',
   `custom-variable-menu'.


 ***** NOTE: The following functions defined in `cus-edit.el' have
             been REDEFINED HERE:

   `custom-add-parent-links', `custom-buffer-create-internal',
   `customize-group-other-window', `customize-mode',
   `customize-unsaved'.
