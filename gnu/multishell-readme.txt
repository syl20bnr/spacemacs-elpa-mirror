Easily use and navigate multiple shell buffers, including remote shells.
Fundamentally, multishell is the function `multishell-pop-to-shell' -
a la `pop-to-buffer' - plus a keybinding. Together, they enable you to:

* Get to the input point from wherever you are in a shell buffer,
  ... or to any of your shell buffers, from anywhere inside emacs.

* Use universal arguments to launch and choose among alternate shell buffers,
  ... and change which is the current default.

* Easily restart disconnected shells, or shells from prior sessions
  ... the latter from Emacs builtin savehist minibuf history persistence

* Append a path to a new shell name to launch a shell in that directory,
  ... and use a path with Emacs tramp syntax to launch a remote shell -
  for example:

  * `#root/sudo:root@localhost:/etc` for a buffer named "*#root*" with a
    root shell starting in /etc.

  * `/ssh:example.net:` for a shell buffer in your homedir on example.net.
    The buffer will be named "*example.net*".

  * `#ex/ssh:example.net|sudo:root@example.net:/var/log` for a root shell
    starting in /var/log on example.net named "*#ex*".

  * 'interior/ssh:gateway.corp.com|ssh:interior.corp.com:' to go via
    gateway.corp.com to your homedir on interior.corp.com.  The buffer
    will be named "*interior*". You could append a sudo hop, and so on.

* Thanks to tramp, file visits from the shell will seamlessly be in
  the auspices of the target account, and relative to the current
  directory, on the host where the shell is running.

* Manage your list of shells, current and past, as a collection.

See the `multishell-pop-to-shell` docstring for details.

Customize-group `multishell' to select and activate a keybinding and set
various behaviors. Customize-group `savehist' to preserve buffer
names/paths across emacs restarts.

Please use
[the multishell repository](https://github.com/kenmanheimer/EmacsMultishell)
issue tracker to report problems, suggestions, etc, and see that
repository for a bit more documentation.

Change Log:

* 2016-02-11 1.1.5 Ken Manheimer:
  - Rectify multishell list sorting to preserve recentness
  - Increment the actual multishell-version setting, neglected for 1.1.4.
* 2016-02-11 1.1.4 Ken Manheimer:
  - hookup multishell-list as completion help buffer.
    Mouse and keyboard selections from help listing properly exits
    minibuffer.
* 2016-02-09 1.1.3 Ken Manheimer:
  multishell-list:
  - add some handy operations, like cloning new entry from existing
  - add optional behaviors to existing operations for returning to
    stopped shells without restarting them.
  - solidify maintaining focus on current entry
  - fix miscellaneous.
* 2016-01-31 1.1.2 Ken Manheimer:
  - Settle puzzling instability of multishell-all-entries
    - The accumulations was putting items going from more to less active
      categories to be put at the end, not beginning.
    - Also, using history for prompting changes history - implement
      no-record option to avoid this when needed.
  - Implement simple edit-in-place multishell-replace-entry and use in
    multishell-list-edit-entry.
  - Remove now unnecessary multishell-list-revert-buffer-kludge.
  - Rectify byte compiler offenses, and other fixes - thanks to Stefan
    Monnier for pointing out many of the corrections.
  - Avoid directly calling tramp functions unnecessarily.
* 2016-01-30 1.1.1 Ken Manheimer:
  - shake out initial multishell-list glitches:
    - (Offer to) delete shell buffer, if present, when deleting entry.
    - Set recency (numeric rank) as initial sort field
    - Recompute list on most operations that affect the order, and try to
      preserve stability. (Kludgy solution, needs work.)
  - Set version to 1.1.1 - multishell-list addition should have been 1.1.0.
* 2016-01-30 1.0.9 Ken Manheimer:
  - Add multishell-list for managing the collection of current and
    history-registered shells: edit, delete, and switch/pop to entries.
    Easy access by invoking `multishell-pop-to-shell' from in the
    `multishell-pop-to-shell' universal arg prompts.
  - Duplicate existing shell buffer names in completions, for distinction.
  - Add paths to buffers started without one, when multishell history dir
    tracking is enabled.
  - Major code cleanup:
    - Simplify multishell-start-shell-in-buffer, in particular using
      shell function, rather than unnecessarily going underneath it.
    - Establish multishell-name-from-entry as canonical name resolver.
    - Fallback to eval-after-load in emacs versions that lack
      with-eval-after-load (eg, emacs 23).
    - save-match-data, where match-string is used
    - resituate some helpers
* 2016-01-24 1.0.8 Ken Manheimer:
  - Work around the shell/tramp mishandling of remote+sudo+homedir problem!
    The work around is clean and simple, basically using high-level `cd'
    API and not messing with the low-level default-directory setting.
    (Turns out the problem was not in my local config. Good riddance to the
    awkward failure handler!)
  - Clean up code resolving the destination shell, starting to document the
    decision tree in the process. See getting-to-a-shell.md in the
    multishell repository, https://github.com/kenmanheimer/EmacsMultishell
  - There may be some shake-out on resolving the destination shell, but
    this release gets the fundamental functionality soundly in place.
* 2016-01-23 1.0.7 Ken Manheimer:
  - Remove notes about tramp remote+sudo+homedir problem. Apparently it's
    due to something in my local site configuration (happens with -q but
    not -Q).
* 2016-01-22 1.0.6 Ken Manheimer:
  - Add multishell-version function.
  - Tweak commentary/comments/docstrings.
  - Null old multishell-buffer-name-history var, if present.
* 2016-01-16 1.0.5 Ken Manheimer:
  - History now includes paths, when designated.
  - Actively track current directory in history entries that have a path.
    Custom control: multishell-history-entry-tracks-current-directory
  - Offer to remove shell's history entry when buffer is killed.
    (Currently the only UI mechanism to remove history entries.)
  - Fix - prevent duplicate entries for same name but different paths
  - Fix - recognize and respect tramp path syntax to start in home dir
  - Simplify history var name, migrate existing history if any from old name
* 2016-01-04 1.0.4 Ken Manheimer - Released to ELPA
* 2016-01-02 Ken Manheimer - working on this in public, but not yet released.

TODO and Known Issues:

* Add custom shell launch prep actions
  - for, eg, port knocking, interface activations
  - shell commands to execute when shell name or path matches a regexp
  - list of (regexp, which - name, path, or both, command)
* Investigate whether we can recognize and provide for failed hops.
  - Tramp doesn't provide useful reactions for any hop but the first
  - Might be stuff we can do to detect and convey failures?
  - Might be no recourse but to seek tramp changes.
* Try minibuffer field boundary at beginning of tramp path, to see whether
  the field boundary magically enables tramp path completion.