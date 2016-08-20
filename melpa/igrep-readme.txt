The `igrep' command is like `grep' except that it takes three
required arguments (PROGRAM, REGEX, and FILES) and an optional
argument (OPTIONS) instead of just one argument (COMMAND).  The
analogous `egrep' and `fgrep' commands are also defined for
convenience.

The `igrep-find' command is like `igrep' except that it uses `find`
to recursively `grep` a directory.  The analogous `egrep-find' and
`fgrep-find' commands are also defined for convenience.

When called interactively, `igrep' and `igrep-find' (and their
analogues) provide defaults for the REGEX and FILES arguments based
on the current word and the visited file name (if the `igrep-regex-
default' and `igrep-files-default' options are set, respectively).
The `igrep-insert-default-key' option allows the default value to be
inserted into the minibuffer for editing; since Emacs 20 provides
that via the minibuffer history, it's only enabled for older
versions by default. Other options that control the user interface
are `igrep-insert-default-directory', `igrep-read-options', `igrep-
read-multiple-files', `igrep-verbose-prompts', `igrep-save-buffers',
and `igrep-menu-bar'.

Besides the basic `igrep-program' and `igrep-find-program' global
variables, other variables control the syntax of the `grep` and
`find` shell commands that are executed: `igrep-options', `igrep-
regex-option', `igrep-case-fold-search', `igrep-find-prune-clause',
`igrep-find-file-clause', and `igrep-find-use-xargs'.

The `igrep-use-zgrep' user option controls whether the corresponding
GNU (gzip) "zPROGRAM" script is used, to `grep` compressed files.
Special minibuffer history lists are maintained for the REGEX and
FILES arguments.

The `agrep' and `agrep-find' commands are interfaces to the
approximate `grep` utility, which is distributed with the `glimpse'
indexing and query tool (available from http://www.tgries.de/agrep/).

`grep' itself can be advised to provide the `igrep' interface when
it is invoked interactively (but still use the original argument
list when it is called from Emacs Lisp), via the `igrep-insinuate'
command.  `igrep-insinuate' also defines `grep-find' as an alias for
`igrep-find', `dired-do-grep' and `dired-do-grep-find' as aliases
for `dired-do-igrep' and `dired-do-igrep-find', and `Buffer-menu-
grep' as an alias for `Buffer-menu-igrep'.

When run interactively from Dired mode, the various `igrep' commands
provide defaults for the REGEX and FILES arguments that are based on
the visited directory (including any inserted subdirectories) and
the current file.  The alternative `dired-do-igrep' and `dired-do-
igrep-find' commands respect the `dired-do-*' command conventions: a
prefix argument is interpreted as the number of succeeding files to
`grep`, otherwise all the marked files are `grep`ed.

The `igrep-visited-files' command provides a simple way to `grep`
just those files that are being visited in buffers.  The `Buffer-
menu-igrep' command does the same thing, for buffers marked for
selection in Buffer Menu mode.

Installation:

1. Put this file in a directory that is a member of load-path, and
   byte-compile it (e.g. with `M-x byte-compile-file') for better
   performance.  You can ignore any warnings about references to free
   variables and "not known to be defined" functions.
2. Put these forms in default.el or ~/.emacs:
   (autoload 'igrep "igrep"
      "*Run `grep` PROGRAM to match REGEX in FILES..." t)
   (autoload 'igrep-find "igrep"
      "*Run `grep` via `find`..." t)
   (autoload 'igrep-visited-files "igrep"
      "*Run `grep` ... on all visited files." t)
   (autoload 'dired-do-igrep "igrep"
      "*Run `grep` on the marked (or next prefix ARG) files." t)
   (autoload 'dired-do-igrep-find "igrep"
      "*Run `grep` via `find` on the marked (or next prefix ARG) directories." t)
   (autoload 'Buffer-menu-igrep "igrep"
     "*Run `grep` on the files visited in buffers marked with '>'." t)
   (autoload 'igrep-insinuate "igrep"
     "Define `grep' aliases for the corresponding `igrep' commands." t)
2. a. For completeness, you can add these forms as well:
   (autoload 'grep "igrep"
      "*Run `grep` PROGRAM to match REGEX in FILES..." t)
   (autoload 'egrep "igrep"
      "*Run `egrep`..." t)
   (autoload 'fgrep "igrep"
      "*Run `fgrep`..." t)
   (autoload 'agrep "igrep"
      "*Run `agrep`..." t)
   (autoload 'grep-find "igrep"
      "*Run `grep` via `find`..." t)
   (autoload 'egrep-find "igrep"
      "*Run `egrep` via `find`..." t)
   (autoload 'fgrep-find "igrep"
      "*Run `fgrep` via `find`..." t)
   (autoload 'agrep-find "igrep"
      "*Run `agrep` via `find`..." t)
3. If you are running Windows 95/NT, you should install findutils
   and grep from release 17.1 (or higher) of the Cygnus GNU-Win32
   distribution (http://www.cygnus.com/misc/gnu-win32/).

Usage:

These igrep commands accept 1, 2, or 3 `C-u' prefix arguments:
	M-x igrep		M-x igrep-find
	M-x  grep		M-x  grep-find	[after `M-x igrep-insinuate']
	M-x egrep		M-x egrep-find
	M-x fgrep		M-x fgrep-find
	M-x agrep		M-x agrep-find

These igrep commands accept a single `C-u' prefix argument:
	M-x igrep-visited-files
	M-x Buffer-menu-igrep	[in the *Buffer List* buffer]

These igrep commands interpret a prefix argument like the Emacs
`dired-do-*' commands:
	M-x dired-do-igrep	M-x dired-do-igrep-find
	M-x  dired-do-grep	M-x  dired-do-grep-find	[after `M-x
							 igrep-insinuate']

These Emacs commands can be used after any igrep command:
	C-x ` (M-x next-error)
	C-c C-c (M-x compile-goto-error)	[in the *igrep* buffer]

Customization examples:

To ignore case by default:
	(setq igrep-options "-i")
or:
	(setq igrep-case-fold-search t)
To search subdirectories by default:
	(setq igrep-find t)
To search files with the GNU (gzip) zgrep script:
	(setq igrep-use-zgrep t)
or define new igrep commands (this works for zegrep and zfgrep as well):
	(igrep-define zgrep)		; M-x zgrep
	(igrep-find-define zgrep)	; M-x zgrep-find
To search "*.[ch]" files by default in C mode:
	(put 'igrep-files-default 'c-mode
	     (lambda () "*.[ch]"))
To disable the default search regex and/or files pattern, except for
specific modes:
	(setq igrep-regex-default 'ignore)
	(setq igrep-files-default 'ignore)
To avoid exceeding some shells' limit on command argument length
(this only searches files in the current directory):
	(setq igrep-find t
	      igrep-find-prune-clause "-type d \\! -name .")

To do:
1. Replace igrep-options with a table that maps igrep-program
   to the appropriate options, and/or support POSIX (egrep -> `grep -E`).
2. Generalize support for the -prune find clause (e.g. -fstype nfs).
3. Provide support for `glimpse`.

