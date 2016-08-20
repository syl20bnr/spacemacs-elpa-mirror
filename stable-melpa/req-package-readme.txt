							━━━━━━━━━━━━━━━━
							  REQ-PACKAGE


							 Edward Knyshov
							━━━━━━━━━━━━━━━━


Table of Contents
─────────────────

1 req-package
.. 1.1 Description
.. 1.2 Usage
.. 1.3 El Get
.. 1.4 More?
.. 1.5 Migrate from use-package
.. 1.6 Note
.. 1.7 Logging
.. 1.8 Contribute
.. 1.9 Things to be done
..... 1.9.1 TODO take package dependencies from it's meta data
..... 1.9.2 TODO el-get/elpa packages must be in priority over builtin ones
.. 1.10 Changelog
..... 1.10.1 v0.9
..... 1.10.2 v0.8
..... 1.10.3 v0.7
..... 1.10.4 v0.6
..... 1.10.5 v0.5
..... 1.10.6 v0.4.2
..... 1.10.7 v0.4.1
..... 1.10.8 v0.4-all-cycles
..... 1.10.9 v0.3-cycles
..... 1.10.10 v0.2-auto-fetch


1 req-package
═════════════

1.1 Description
───────────────

  req-package is a macro wrapper on top of [use-package].  It's goal is
  to simplify package dependencies management, when using use-package
  for your .emacs.


  [use-package] https://github.com/jwiegley/use-package


1.2 Usage
─────────

  Load req-package:

  ┌────
  │ (require 'req-package)
  └────

  Define required packages with dependencies using `:require' like this:

  ┌────
  │ (req-package dired) ;; you can omit this empty requirement because of dired-single
  │
  │ (req-package dired-single
  │   :require dired
  │   :config (...))
  │
  │ (req-package lua-mode
  │   :config (...))
  │
  │ (req-package flymake)
  │
  │ (req-package flymake-lua
  │   :require (flymake lua-mode)
  │   :config (...))
  └────

  To start loading packages in right order:

  ┌────
  │ (req-package-finish)
  └────


1.3 El Get
──────────

  There is another benefit over use-package - `el-get' support.  No more
  thinking about sources for your packages.  Just install and configure
  your el-get.  Here is example:

  ┌────
  │ (require 'req-package'')
  │
  │ (req-package-force el-get
  │   :init (progn (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
  │ 			   (el-get 'sync)))
  │
  │ (req-package gotham-theme
  │   :config (print "gotham theme is here and installed from el-get"))
  │
  │ (req-package-finish)
  └────

  Also, of course, there could be dependencies between el-get and elpa
  packages


1.4 More?
─────────

  You can always extend list of package providers or change priorities
  if you want.  in which your packages are being installed.  It can be
  done by customizing `req-package-providers' list.  It's list of
  functions, which can install packages.

  Here are some rules for one such function:

  • check package presence at corresponding repo
  • check whether it installed or not
  • install that package if it is available and not installed
  • return nonnil only if package is installed already or successfully
    installed by this function"


1.5 Migrate from use-package
────────────────────────────

  Just replace all `(use-package ...)' with `(req-package [:require
  DEPS] ...)' and add `(req-package-finish)' at the end of your
  configuration file.


1.6 Note
────────

  All use-package parameters are supported, see use-package manual.  for
  additional info.

  However, there is no need for the `:ensure' keyword; req-package will
  add it automatically if needed.

  For each package you can manually specify loader fuction by `:loader'
  keyword.  It can be any acceptable item for `req-package-providers'
  list.

  Also there is a `req-package-force' function which simulates plain old
  use-package behavior.

  More complex req-package usage example can be found at
  [http://github.com/edvorg/emacs-configs].


1.7 Logging
───────────

  You cand use `req-package--log-open-log' to see, what is happening
  with your configuration.  You can choose log level in `req-package'
  group by `req-package-log-level' custom.  These log levels are
  supported: `fatal', `error', `warn', `info', `debug', `trace'.


1.8 Contribute
──────────────

  Please, commit and pull-request your changes to `develop' branch.
  Master is used for automatic repo package builds by melpa's travis-ci.


1.9 Things to be done
─────────────────────

1.9.1 TODO take package dependencies from it's meta data
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌


1.9.2 TODO el-get/elpa packages must be in priority over builtin ones
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌


1.10 Changelog
──────────────

1.10.1 v0.9
╌╌╌╌╌╌╌╌╌╌╌

  • `:loader' keyword support


1.10.2 v0.8
╌╌╌╌╌╌╌╌╌╌╌

  • bugfixes


1.10.3 v0.7
╌╌╌╌╌╌╌╌╌╌╌

  • fixed some issues with packages installation. all packages will be
    installed at bootstrap time
  • custom package providers support by `req-package-providers'
  • priority feature for cross provider packages loading. you can
    choose, what to try first - elpa, el-get, or something else


1.10.4 v0.6
╌╌╌╌╌╌╌╌╌╌╌

  • `el-get' support


1.10.5 v0.5
╌╌╌╌╌╌╌╌╌╌╌

  • Major system refactoring.
  • Fixed bugs with defered loading.
  • Significant performance optimization.
  • `max-specpdl-size', `max-lisp-eval-depth' issues completely solved.
  • Flexible `:require' keyword parsing.


1.10.6 v0.4.2
╌╌╌╌╌╌╌╌╌╌╌╌╌

  • Bug fixes.


1.10.7 v0.4.1
╌╌╌╌╌╌╌╌╌╌╌╌╌

  • Various tweaks and bug fixes.


1.10.8 v0.4-all-cycles
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  • All cycles of your dependencies will be printed now.
  • Also there are more handy log messages and some bug fixes.


1.10.9 v0.3-cycles
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  • There are nice error messages about cycled dependencies now.
  • Cycles printed in a way: `pkg1 -> [pkg2 -> ...] pkg1'.
  • It means there is a cycle around `pkg1'.


1.10.10 v0.2-auto-fetch
╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌╌

  • There is no need of explicit `:ensure' in your code now.
  • When you req-package it adds `:ensure' if package is available in
    your repos.
  • Also package deps `:ensure''d automatically too.
  • Just write `(req-package pkg1 :require pkg2)' and all you need will
    be installed.
