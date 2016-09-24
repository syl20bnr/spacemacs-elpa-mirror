;;; xah-elisp-mode.el --- Major mode for editing emacs lisp.

;; Copyright © 2013-2015, by Xah Lee

;; Author: Xah Lee ( http://xahlee.org/ )
;; Version: 2.7.2
;; Package-Version: 20160923.2059
;; Created: 23 Mar 2013
;; Package-Requires: ((emacs "24.3"))
;; Keywords: lisp, languages
;; Homepage: http://ergoemacs.org/emacs/xah-elisp-mode.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; Major mode for editing emacs lisp.
;; This is alternative to GNU Emacs emacs-lisp-mode.

;; Major features different from emacs-lisp-mode:

;; • Syntax coloring of 99% statistically most frequently used elisp functions.
;; • Completion for function names with `ido-mode' interface. (press TAB after word)
;; • Function param template. (press space after function name.)
;; • 1 to 4 letters abbrevs for top 50 most used functions. e.g. “bsnp” → “buffer-substring-no-properties”
;; • Convenient formatting command that formats entire sexp expression unit. (press TAB before word.)

;; Call `xah-elisp-mode' to activate the mode.
;; Files ending in “.el” will also open in `xah-elisp-mode'.

;; Single letter abbrevs are:
;; d → defun
;; i → insert
;; l → let
;; m → message
;; p → point
;; s → setq

;; Call `list-abbrevs' to see the full list.

;; put this in your init to turn on abbrev
;; (abbrev-mode 1)

;; home page: http://ergoemacs.org/emacs/xah-elisp-mode.html

;; This mode is designed to be very different from the usual paredit/smartparens approach.
;; The focus of this mode is to eliminate any concept of {manual formatting, format “style”, “indentation”, “line of code”} from programer. Instead, pretty-print/rendering should be automated, as part of display system. ({Matematica, XML, HTML} are examples.)
;; The goal of this mode is for it to become 100% semantic lisp code editor, such that it is impossible to create mis-formed elisp expressions, yet being practical.

;; If you like the idea, please help fund the project. Buy Xah Emacs Tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html or make a donation. See home page. Thanks.

;; auto-complete-mode support
;; if you want also to use auto-complete-mode, add the following to your emacs init.

;; make auto-complete-mode support xah-elisp-mode
;; (when (boundp 'ac-modes)
;;   (add-to-list 'ac-modes 'xah-elisp-mode))

;; equires emacs 24.3 because of using setq-local

;;; INSTALL:

;; manual install.

;; Place the file at ~/.emacs.d/lisp/
;; Then put the following in ~/.emacs.d/init.el
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (autoload 'xah-elisp-mode "xah-elisp-mode" "xah emacs lisp major mode." t)

;;; todo:
;; 2015-05-17 type this “push-mark” and you get this instead (push NEWELT-▮ PLACE). temp solution, type “push” then tab. Always use tab to create functions...


;;; Code:

(require 'lisp-mode)

(defvar xah-elisp-mode-hook nil "Standard hook for `xah-elisp-mode'")

(defvar xah-elisp-elisp-lang-words nil "List of elisp keywords of “core” language. Core is not well defined here, but mostly in C.")
(setq xah-elisp-elisp-lang-words '(

"abs"
"add-to-list"
"alist-get"
"and"
"append"
"apply"
"aref"
"aset"
"assoc"
"assoc-default"
"assq"
"assq-delete-all"
"boundp"
"car"
"car-safe"
"catch"
"cdr"
"char-equal"
"char-to-string"
"commandp"
"concat"
"cond"
"condition-case"
"cons"
"consp"
"copy-alist"
"decode-coding-string"
"defalias"
"defconst"
"defmacro"
"defun"
"defvar"
"delete"
"delete-dups"
"delq"
"elt"
"eq"
"equal"
"eval"
"expt"
"fboundp"
"featurep"
"file-name-absolute-p"
"float"
"floatp"
"format"
"format-time-string"
"fset"
"funcall"
"function"
"functionp"
"get"
"gethash"
"hash-table-count"
"if"
"integerp"
"intern"
"lambda"
"last"
"length"
"let"
"let*"
"list"
"listp"
"load"
"load-file"
"make-hash-table"
"make-list"
"mapc"
"mapcar"
"mapconcat"
"maphash"
"max"
"member"
"member-ignore-case"
"memq"
"memql"
"message"
"min"
"mod"
"nil"
"not"
"nth"
"nthcdr"
"null"
"number-sequence"
"number-to-string"
"numberp"
"or"
"plist-get"
"plist-member"
"plist-put"
"pop"
"prin1"
"princ"
"print"
"print-length"
"print-level"
"progn"
"provide"
"push"
"put"
"puthash"
"quote"
"random"
"rassoc"
"rassq"
"rassq-delete-all"
"read"
"regexp-opt"
"regexp-quote"
"remhash"
"remove"
"remq"
"require"
"reverse"
"round"
"set"
"setplist"
"setq"
"sleep-for"
"sort"
"split-string"
"sqrt"
"string"
"string-equal"
"string-match"
"string-match-p"
"string-to-char"
"string-to-number"
"string="
"stringp"
"subrp"
"substring"
"symbol-function"
"symbol-name"
"symbol-plist"
"symbol-value"
"symbolp"
"t"
"terpri"
"throw"
"unless"
"url-unhex-string"
"vconcat"
"vector"
"vectorp"
"when"
"while"
"zerop"

))

(defvar xah-elisp-emacs-words nil "List of elisp keywords that's not core lisp language, such as buffer, marker, hook, editing, copy paste, ….")
(setq xah-elisp-emacs-words '(

"display-completion-list"
"all-completions"
"try-completion"

"image-flush"
"clear-image-cache"

"insert-image"
"insert-sliced-image"
"put-image"
"remove-images"
"image-size"

"font-lock-add-keywords"
"font-lock-fontify-buffer"

"insert-and-inherit"
"insert-before-markers-and-inherit"
"field-beginning"
"field-end"
"field-string"
"field-string-no-properties"
"delete-field"
"constrain-to-field"
"write-char"
"prin1-to-string"
"with-output-to-string"
"create-image"
"defimage"
"find-image"
"image-load-path-for-library"

"abbrev-insert"
"abbrev-symbol"
"add-hook"
"add-text-properties"
"add-to-invisibility-spec"
"append-to-file"
"ask-user-about-supersession-threat"
"atomic-change-group"
"autoload"
"backward-char"
"backward-sexp"
"backward-word"
"backward-up-list"
"barf-if-buffer-read-only"
"beginning-of-line"
"bobp"
"bolp"
"bounds-of-thing-at-point"
"buffer-base-buffer"
"buffer-chars-modified-tick"
"buffer-disable-undo"
"buffer-enable-undo"
"buffer-file-name"
"buffer-list"
"buffer-live-p"
"buffer-modified-p"
"buffer-modified-tick"
"buffer-name"
"buffer-string"
"buffer-substring"
"buffer-substring-no-properties"
"buffer-swap-text"
"bufferp"
"bury-buffer"
"call-interactively"
"called-interactively-p"
"capitalize"
"char-after"
"char-before"
"clear-visited-file-modtime"
"clone-indirect-buffer"
"completing-read"
"copy-directory"
"copy-file"
"copy-overlay"
"count-matches"
"current-buffer"
"current-word"
"custom-autoload"
"custom-set-faces"
"defcustom"
"defface"
"defgroup"
"define-abbrev-table"
"define-derived-mode"
"define-key"
"define-minor-mode"
"defsubst"
"delete-and-extract-region"
"delete-char"
"delete-directory"
"delete-file"
"delete-overlay"
"delete-region"
"directory-files"
"dolist"
"dotimes"
"downcase"
"downcase-region"
"emacs-version"
"end-of-line"
"eobp"
"eolp"
"erase-buffer"
"error"
"expand-file-name"
"file-directory-p"
"file-exists-p"
"file-name-directory"
"file-name-as-directory"
"directory-name-p"
"directory-file-name"
"abbreviate-file-name"
"file-name-extension"
"file-name-nondirectory"
"file-name-sans-extension"
"file-readable-p"
"file-regular-p"
"file-relative-name"
"find-buffer-visiting"
"find-file"
"following-char"
"font-family-list"
"forward-char"
"forward-comment"
"forward-line"
"forward-sexp"
"forward-symbol"
"forward-word"
"frame-parameter"
"frame-parameters"
"gap-position"
"gap-size"
"generate-new-buffer"
"generate-new-buffer-name"
"get-buffer"
"get-buffer-create"
"get-char-code-property"
"get-char-property"
"get-char-property-and-overlay"
"get-file-buffer"
"get-pos-property"
"get-text-property"
"getenv"
"global-unset-key"
"goto-char"
"ido-completing-read"
"ido-read-directory-name"
"insert"
"insert-buffer-substring-no-properties"
"insert-char"
"insert-file-contents"
"interactive"
"invisible-p"
"kill-all-local-variables"
"kill-buffer"
"kill-new"
"kill-append"
"kill-region"
"kill-ring-save"
"last-buffer"
"left-char"
"line-beginning-position"
"line-end-position"
"local-set-key"
"looking-at"
"looking-back"
"make-directory"
"make-indirect-buffer"
"make-local-variable"
"make-overlay"
"make-sparse-keymap"
"mark"
"match-beginning"
"match-data"
"match-end"
"match-string"
"match-string-no-properties"
"modify-all-frames-parameters"
"modify-frame-parameters"
"modify-syntax-entry"
"move-overlay"
"narrow-to-region"
"next-buffer"
"next-char-property-change"
"next-overlay-change"
"next-property-change"
"next-single-char-property-change"
"next-single-property-change"
"not-modified"
"number-or-marker-p"
"other-buffer"
"overlay-buffer"
"overlay-end"
"overlay-get"
"overlay-properties"
"overlay-put"
"overlay-recenter"
"overlay-start"
"overlayp"
"overlays-at"
"overlays-in"
"parse-partial-sexp"
"parse-time-string"
"point"
"point-marker"
"point-max"
"point-min"
"pop-mark"
"preceding-char"
"prefix-numeric-value"
"previous-buffer"
"previous-char-property-change"
"previous-overlay-change"
"previous-property-change"
"previous-single-char-property-change"
"previous-single-property-change"
"propertize"
"push-mark"
"put-text-property"
"re-search-backward"
"re-search-forward"
"read-directory-name"
"read-file-name"
"read-from-minibuffer"
"read-regexp"
"read-string"
"redraw-frame"
"region-active-p"
"region-beginning"
"region-end"
"remove-from-invisibility-spec"
"remove-hook"
"remove-list-of-text-properties"
"remove-overlays"
"remove-text-properties"
"rename-buffer"
"rename-file"
"repeat"
"replace-match"
"replace-regexp"
"replace-regexp-in-string"
"restore-buffer-modified-p"
"right-char"
"run-hooks"
"run-mode-hooks"
"run-with-timer"
"save-buffer"
"save-current-buffer"
"save-excursion"
"save-restriction"
"scan-lists"
"scan-sexps"
"search-backward"
"search-backward-regexp"
"search-forward"
"search-forward-regexp"
"selected-frame"
"set-buffer"
"set-buffer-modified-p"
"set-default"
"set-file-modes"
"set-fontset-font"
"set-frame-parameter"
"set-mark"
"set-syntax-table"
"set-text-properties"
"set-visited-file-modtime"
"set-visited-file-name"
"set-window-margins"
"setenv"
"setq-default"
"setq-local"
"shell-command"
"shell-command-to-string"
"skip-chars-backward"
"skip-chars-forward"
"skip-syntax-backward"
"skip-syntax-forward"
"standard-syntax-table"
"start-process"
"substring-no-properties"
"switch-to-buffer"
"syntax-ppss"
"text-properties-at"
"text-property-any"
"text-property-not-all"
"thing-at-point"
"toggle-read-only"
"unbury-buffer"
"upcase"
"upcase-initials"
"upcase-region"
"use-local-map"
"use-region-p"
"user-error"
"variable-pitch-mode"
"verify-visited-file-modtime"
"version"
"version<"
"version<="
"visited-file-modtime"
"widen"
"widget-get"
"window-body-width"
"window-margins"
"with-current-buffer"
"with-output-to-temp-buffer"
"with-syntax-table"
"with-temp-buffer"
"with-temp-buffer-window"
"with-temp-file"
"write-file"
"write-region"
"y-or-n-p"
"yes-or-no-p"

"eval-when-compile"
"make-network-process"
"make-network-process"

"process-send-string"
"process-send-region"
"set-process-filter"
"set-marker"
"process-mark"
"delete-process"
"current-time-string"
"process-status"
"process-buffer"
"processp"
"kill-process"

"process-datagram-address"
"process-list"
"get-process"
"process-command"
"process-type"
"process-id"
"process-contact"
"network-interface-list"

))

(defvar xah-elisp-emacs-user-commands nil "List of elisp keywords that are almost always called by user interactively.")
(setq xah-elisp-emacs-user-commands '(
"split-window-below"
"split-window-right"
"clear-rectangle"
"complete-symbol"
"define-prefix-command"
"delete-rectangle"
"delete-whitespace-rectangle"
"electric-indent-local-mode"
"electric-indent-mode"
"electric-layout-mode"
"electric-pair-mode"
"eval-buffer"
"eval-defun"
"eval-expression"
"eval-last-sexp"
"eval-region"
"global-set-key"
"kbd"
"key-translation-map"
"kill-rectangle"
"linum-mode"
"open-rectangle"
"prettify-symbols-mode"
"rectangle-mark-mode"
"rectangle-number-lines"
"repeat-complex-command"
"replace-rectangle"
"shell-command-on-region"
"shell-command-on-region"
"sort-lines"
"subword-mode"
"yank"
"yank-rectangle"))

(defvar xah-elisp-keyword-builtin nil "List of elisp names")
(setq xah-elisp-keyword-builtin '( "&optional" "&rest"))

(defvar xah-elisp-elisp-vars-1 nil "List elisp variables names")
;; todo. needs lots work. ideally, ALL elisp var names. But that's too many. Consider implementation to save memory or load on demand. Then, perhaps for simplicity just the top 300 most used ones (by stat of elisp source code).
;; consider find a way to programmatically list all elisp vars, loaded or even unloaded.
;; consider also leave this list empty, instead, dynamically determine when a symbol is a lisp var.
(setq xah-elisp-elisp-vars-1 '(

"load-in-progress"

"image-cache-eviction-delay"
"max-image-size"
"buffer-access-fontify-functions"
"buffer-access-fontified-property"
"text-property-default-nonsticky"
"image-load-path"

"Buffer-menu-buffer+size-width"
"Buffer-menu-mode-width"
"Buffer-menu-name-width"
"Buffer-menu-size-width"
"Buffer-menu-use-frame-buffer-list"
"Buffer-menu-use-header-line"
"Info-default-directory-list"
"Info-split-threshold"
"abbrev-all-caps"
"abbrev-file-name"
"ad-default-compilation-action"
"ad-redefinition-action"
"adaptive-fill-first-line-regexp"
"adaptive-fill-function"
"adaptive-fill-mode"
"adaptive-fill-regexp"
"add-log-current-defun-function"
"add-log-full-name"
"add-log-mailing-address"
"after-save-hook"
"allout-auto-activation"
"allout-widgets-auto-activation"
"apropos-compact-layout"
"apropos-do-all"
"apropos-documentation-sort-by-scores"
"apropos-match-face"
"apropos-sort-by-scores"
"async-shell-command-buffer"
"auth-source-cache-expiry"
"auto-coding-alist"
"auto-coding-functions"
"auto-coding-regexp-alist"
"auto-compression-mode"
"auto-encryption-mode"
"auto-fill-inhibit-regexp"
"auto-hscroll-mode"
"auto-image-file-mode"
"auto-insert-mode"
"auto-mode-alist"
"auto-mode-case-fold"
"auto-save-default"
"auto-save-file-name-transforms"
"auto-save-interval"
"auto-save-list-file-prefix"
"auto-save-timeout"
"auto-save-visited-file-name"
"autoarg-kp-mode"
"autoarg-mode"
"automatic-hscrolling"
"automount-dir-prefix"
"backup-by-copying"
"backup-by-copying-when-linked"
"backup-by-copying-when-mismatch"
"backup-by-copying-when-privileged-mismatch"
"backup-directory-alist"
"backward-delete-char-untabify-method"
"bahai-holidays"
"baud-rate"
"bdf-directory-list"
"before-save-hook"
"bidi-paragraph-direction"
"blink-cursor"
"blink-cursor-alist"
"blink-cursor-delay"
"blink-cursor-interval"
"blink-cursor-mode"
"blink-matching-delay"
"blink-matching-paren"
"blink-matching-paren-distance"
"blink-matching-paren-dont-ignore-comments"
"blink-matching-paren-on-screen"
"break-hardlink-on-save"
"browse-url-browser-function"
"buffer-file-name"
"buffer-file-number"
"buffer-file-truename"
"buffer-invisibility-spec"
"buffer-offer-save"
"buffer-read-only"
"buffer-save-without-query"
"buffers-menu-buffer-name-length"
"buffers-menu-max-size"
"buffers-menu-show-directories"
"buffers-menu-show-status"
"case-fold-search"
"case-replace"
"change-major-mode-with-file-name"
"char-property-alias-alist"
"charset-map-path"
"christian-holidays"
"colon-double-space"
"column-number-mode"
"comment-auto-fill-only-comments"
"comment-column"
"comment-empty-lines"
"comment-fill-column"
"comment-inline-offset"
"comment-multi-line"
"comment-padding"
"comment-style"
"compilation-ask-about-save"
"compilation-disable-input"
"compilation-mode-hook"
"compilation-search-path"
"compilation-start-hook"
"compilation-window-height"
"compile-command"
"completion-auto-help"
"completion-category-overrides"
"completion-cycle-threshold"
"completion-ignored-extensions"
"completion-in-region-mode"
"completion-pcm-complete-word-inserts-delimiters"
"completion-pcm-word-delimiters"
"completion-show-help"
"completion-styles"
"completions-format"
"compose-mail-user-agent-warnings"
"confirm-kill-emacs"
"confirm-nonexistent-file-or-buffer"
"create-lockfiles"
"crisp-mode"
"ctl-arrow"
"cua-mode"
"current-language-environment"
"current-prefix-arg"
"cursor-in-non-selected-windows"
"custom-browse-sort-alphabetically"
"custom-buffer-sort-alphabetically"
"custom-enabled-themes"
"custom-file"
"custom-menu-sort-alphabetically"
"custom-safe-themes"
"custom-theme-directory"
"custom-theme-load-path"
"cvs-dired-action"
"cvs-dired-use-hook"
"deactivate-mark"
"debug-ignored-errors"
"debug-on-error"
"debug-on-event"
"debug-on-quit"
"debug-on-signal"
"default-directory"
"default-frame-alist"
"default-input-method"
"default-justification"
"default-text-properties"
"defun-prompt-regexp"
"delete-active-region"
"delete-auto-save-files"
"delete-by-moving-to-trash"
"delete-exited-processes"
"delete-old-versions"
"delete-selection-mode"
"delete-trailing-lines"
"desktop-locals-to-save"
"desktop-save-mode"
"diff-command"
"diff-switches"
"directory-abbrev-alist"
"directory-free-space-args"
"directory-free-space-program"
"dired-kept-versions"
"dired-listing-switches"
"display-battery-mode"
"display-buffer-alist"
"display-buffer-base-action"
"display-buffer-function"
"display-buffer-reuse-frames"
"display-hourglass"
"display-mm-dimensions-alist"
"display-time-day-and-date"
"display-time-mode"
"dnd-open-file-other-window"
"dnd-open-remote-file-function"
"dnd-protocol-alist"
"double-click-fuzz"
"double-click-time"
"dynamic-completion-mode"
"echo-keystrokes"
"edebug-all-defs"
"edebug-all-forms"
"eldoc-minor-mode-string"
"emacs-lisp-docstring-fill-column"
"emacs-lisp-mode-hook"
"emacs-major-version"
"emacs-minor-version"
"enable-kinsoku"
"enable-local-eval"
"enable-local-variables"
"enable-recursive-minibuffers"
"enable-remote-dir-locals"
"eol-mnemonic-dos"
"eol-mnemonic-mac"
"eol-mnemonic-undecided"
"eol-mnemonic-unix"
"epa-file-inhibit-auto-save"
"epa-file-name-regexp"
"epa-global-mail-mode"
"erc-track-minor-mode"
"eval-expression-debug-on-error"
"eval-expression-print-length"
"eval-expression-print-level"
"even-window-heights"
"exec-path"
"exec-suffixes"
"exit-language-environment-hook"
"face-font-family-alternatives"
"face-font-registry-alternatives"
"face-font-selection-order"
"face-x-resources"
"facemenu-add-face-function"
"facemenu-end-add-face"
"facemenu-keybindings"
"facemenu-listed-faces"
"facemenu-new-faces-at-end"
"facemenu-remove-face-function"
"fancy-splash-image"
"ff-special-constructs"
"file-coding-system-alist"
"file-name-at-point-functions"
"file-name-shadow-mode"
"file-name-shadow-properties"
"file-name-shadow-tty-properties"
"file-precious-flag"
"fill-column"
"fill-individual-varying-indent"
"fill-nobreak-invisible"
"fill-nobreak-predicate"
"fill-prefix"
"find-directory-functions"
"find-file-existing-other-name"
"find-file-hook"
"find-file-hooks"
"find-file-run-dired"
"find-file-suppress-same-file-warnings"
"find-file-visit-truename"
"find-file-wildcards"
"find-tag-default-function"
"find-tag-hook"
"fit-frame-to-buffer"
"fit-frame-to-buffer-bottom-margin"
"focus-follows-mouse"
"font-list-limit"
"font-lock-builtin-face"
"font-lock-comment-delimiter-face"
"font-lock-comment-face"
"font-lock-constant-face"
"font-lock-defaults"
"font-lock-doc-face"
"font-lock-function-name-face"
"font-lock-global-modes"
"font-lock-keyword-face"
"font-lock-maximum-decoration"
"font-lock-maximum-size"
"font-lock-negation-char-face"
"font-lock-preprocessor-face"
"font-lock-reference-face"
"font-lock-string-face"
"font-lock-support-mode"
"font-lock-type-face"
"font-lock-variable-name-face"
"font-lock-verbose"
"font-lock-warning-face"
"font-use-system-font"
"frame-auto-hide-function"
"frame-background-mode"
"fringe-mode"
"garbage-collection-messages"
"gc-cons-percentage"
"gc-cons-threshold"
"gdb-enable-debug"
"general-holidays"
"global-auto-revert-mode"
"global-cwarn-mode"
"global-ede-mode"
"global-font-lock-mode"
"global-hi-lock-mode"
"global-highlight-changes-mode"
"global-hl-line-mode"
"global-linum-mode"
"global-mark-ring-max"
"global-reveal-mode"
"global-subword-mode"
"global-visual-line-mode"
"global-whitespace-mode"
"global-whitespace-newline-mode"
"glyphless-char-display-control"
"gnus-select-method"
"gnutls-min-prime-bits"
"goal-column"
"gpm-mouse-mode"
"grep-command"
"grep-find-command"
"grep-setup-hook"
"grep-window-height"
"gud-tooltip-mode"
"hebrew-holidays"
"help-at-pt-display-when-idle"
"help-char"
"help-enable-auto-load"
"help-event-list"
"help-mode-hook"
"help-window-select"
"highlight-nonselected-windows"
"hippie-expand-try-functions-list"
"history-delete-duplicates"
"history-length"
"holiday-bahai-holidays"
"holiday-christian-holidays"
"holiday-general-holidays"
"holiday-hebrew-holidays"
"holiday-islamic-holidays"
"holiday-local-holidays"
"holiday-oriental-holidays"
"holiday-other-holidays"
"holiday-solar-holidays"
"hourglass-delay"
"hscroll-margin"
"hscroll-step"
"icomplete-mode"
"icon-map-list"
"idle-update-delay"
"ido-mode"
"image-file-name-extensions"
"image-file-name-regexps"
"image-load-path"
"imagemagick-enabled-types"
"imagemagick-types-inhibit"
"imenu-sort-function"
"indent-tabs-mode"
"indicate-buffer-boundaries"
"indicate-empty-lines"
"indicate-unused-lines"
"inhibit-default-init"
"inhibit-eol-conversion"
"inhibit-local-menu-bar-menus"
"inhibit-read-only"
"inhibit-splash-screen"
"inhibit-startup-buffer-menu"
"inhibit-startup-echo-area-message"
"inhibit-startup-message"
"inhibit-startup-screen"
"initial-buffer-choice"
"initial-frame-alist"
"initial-major-mode"
"initial-scratch-message"
"input-method-activate-hook"
"input-method-after-insert-chunk-hook"
"input-method-deactivate-hook"
"input-method-highlight-flag"
"input-method-inactivate-hook"
"input-method-use-echo-area"
"input-method-verbose-flag"
"insert-default-directory"
"inverse-video"
"isearch-allow-scroll"
"isearch-hide-immediately"
"isearch-lazy-highlight"
"isearch-lazy-highlight-cleanup"
"isearch-lazy-highlight-initial-delay"
"isearch-lazy-highlight-interval"
"isearch-lazy-highlight-max-at-a-time"
"isearch-resume-in-command-history"
"islamic-holidays"
"ispell-personal-dictionary"
"iswitchb-mode"
"jit-lock-chunk-size"
"jit-lock-context-time"
"jit-lock-contextually"
"jit-lock-defer-contextually"
"jit-lock-defer-time"
"jit-lock-stealth-load"
"jit-lock-stealth-nice"
"jit-lock-stealth-time"
"jit-lock-stealth-verbose"
"jka-compr-compression-info-list"
"jka-compr-load-suffixes"
"jka-compr-mode-alist-additions"
"jka-compr-verbose"
"kept-new-versions"
"kept-old-versions"
"keyboard-coding-system"
"keypad-numlock-setup"
"keypad-numlock-shifted-setup"
"keypad-setup"
"keypad-shifted-setup"
"kill-buffer-hook"
"kill-buffer-query-functions"
"kill-do-not-save-duplicates"
"kill-read-only-ok"
"kill-ring-max"
"kill-whole-line"
"language-info-custom-alist"
"large-file-warning-threshold"
"last-command"
"latex-block-names"
"latex-inputenc-coding-alist"
"latex-run-command"
"latin1-display"
"latin1-display-ucs-per-lynx"
"lazy-highlight-cleanup"
"lazy-highlight-initial-delay"
"lazy-highlight-interval"
"lazy-highlight-max-at-a-time"
"left-margin"
"line-move-ignore-invisible"
"line-move-visual"
"line-number-display-limit"
"line-number-display-limit-width"
"line-number-mode"
"line-spacing"
"lisp-body-indent"
"lisp-indent-function"
"lisp-indent-offset"
"lisp-interaction-mode-hook"
"lisp-mode-hook"
"list-buffers-directory"
"list-colors-sort"
"list-directory-brief-switches"
"list-directory-verbose-switches"
"list-matching-lines-buffer-name-face"
"list-matching-lines-default-context-lines"
"list-matching-lines-face"
"load-file-name"
"load-path"
"local-holidays"
"locate-ls-subdir-switches"
"lpr-command"
"lpr-switches"
"ls-lisp-support-shell-wildcards"
"major-mode"
"make-backup-file-name-function"
"make-backup-files"
"make-cursor-line-fully-visible"
"make-pointer-invisible"
"mark-active"
"mark-even-if-inactive"
"mark-ring-max"
"max-lisp-eval-depth"
"max-mini-window-height"
"max-specpdl-size"
"menu-bar-mode"
"menu-prompting"
"message-log-max"
"messages-buffer-max-lines"
"meta-prefix-char"
"minibuffer-auto-raise"
"minibuffer-depth-indicate-mode"
"minibuffer-electric-default-mode"
"minibuffer-frame-alist"
"minibuffer-history-case-insensitive-variables"
"minibuffer-prompt-properties"
"mode-line-default-help-echo"
"mode-line-format"
"mode-line-in-non-selected-windows"
"mode-name"
"mode-require-final-newline"
"mouse-1-click-follows-link"
"mouse-1-click-in-non-selected-windows"
"mouse-autoselect-window"
"mouse-avoidance-mode"
"mouse-buffer-menu-maxlen"
"mouse-buffer-menu-mode-mult"
"mouse-drag-copy-region"
"mouse-highlight"
"mouse-scroll-delay"
"mouse-scroll-min-lines"
"mouse-wheel-click-event"
"mouse-wheel-down-event"
"mouse-wheel-follow-mouse"
"mouse-wheel-inhibit-click-time"
"mouse-wheel-mode"
"mouse-wheel-progressive-speed"
"mouse-wheel-scroll-amount"
"mouse-wheel-up-event"
"mouse-yank-at-point"
"msb-mode"
"multibyte-syntax-as-symbol"
"next-error-highlight"
"next-error-highlight-no-select"
"next-error-hook"
"next-error-recenter"
"next-line-add-newlines"
"next-screen-context-lines"
"no-redraw-on-reenter"
"normal-erase-is-backspace"
"occur-excluded-properties"
"occur-hook"
"occur-mode-find-occurrence-hook"
"occur-mode-hook"
"only-global-abbrevs"
"open-paren-in-column-0-is-defun-start"
"oriental-holidays"
"other-holidays"
"overflow-newline-into-fringe"
"overline-margin"
"package-enable-at-startup"
"page-delimiter"
"paragraph-ignore-fill-prefix"
"paragraph-separate"
"paragraph-start"
"parens-require-spaces"
"parse-sexp-ignore-comments"
"parse-sexp-lookup-properties"
"password-cache"
"password-cache-expiry"
"polling-period"
"pop-up-frame-alist"
"pop-up-frame-function"
"pop-up-frames"
"pop-up-windows"
"pre-abbrev-expand-hook"
"printer-name"
"process-connection-type"
"ps-page-dimensions-database"
"ps-paper-type"
"ps-print-color-p"
"query-replace-from-history-variable"
"query-replace-highlight"
"query-replace-lazy-highlight"
"query-replace-show-replacement"
"query-replace-skip-read-only"
"query-replace-to-history-variable"
"rcirc-track-minor-mode"
"read-buffer-completion-ignore-case"
"read-buffer-function"
"read-file-name-completion-ignore-case"
"read-mail-command"
"read-quoted-char-radix"
"recenter-positions"
"recenter-redisplay"
"recentf-mode"
"regexp-search-ring-max"
"register-separator"
"remote-file-name-inhibit-cache"
"remote-shell-program"
"replace-lax-whitespace"
"replace-regexp-lax-whitespace"
"require-final-newline"
"revert-without-query"
"safe-local-eval-forms"
"safe-local-variable-values"
"same-window-buffer-names"
"same-window-regexps"
"save-abbrevs"
"save-interprogram-paste-before-kill"
"savehist-mode"
"scalable-fonts-allowed"
"scroll-all-mode"
"scroll-bar-mode"
"scroll-conservatively"
"scroll-down-aggressively"
"scroll-error-top-bottom"
"scroll-margin"
"scroll-preserve-screen-position"
"scroll-step"
"scroll-up-aggressively"
"search-exit-option"
"search-highlight"
"search-invisible"
"search-nonincremental-instead"
"search-ring-max"
"search-ring-update"
"search-slow-speed"
"search-slow-window-lines"
"search-upper-case"
"search-whitespace-regexp"
"select-active-regions"
"selection-coding-system"
"selective-display-ellipses"
"semantic-default-submodes"
"semantic-mode"
"send-mail-function"
"sentence-end"
"sentence-end-base"
"sentence-end-double-space"
"sentence-end-without-period"
"sentence-end-without-space"
"server-mode"
"set-language-environment-hook"
"set-mark-command-repeat-pop"
"set-mark-default-inactive"
"shell-dumb-shell-regexp"
"shell-file-name"
"shift-select-mode"
"show-paren-mode"
"show-trailing-whitespace"
"site-run-file"
"size-indication-mode"
"slitex-run-command"
"small-temporary-file-directory"
"solar-holidays"
"special-display-buffer-names"
"special-display-frame-alist"
"special-display-function"
"special-display-regexps"
"split-height-threshold"
"split-width-threshold"
"split-window-keep-point"
"split-window-preferred-function"
"standard-indent"
"strokes-mode"
"suggest-key-bindings"
"switch-to-buffer-preserve-window-point"
"switch-to-visible-buffer"
"system-type"
"tab-always-indent"
"tab-stop-list"
"tab-width"
"table-cell-map-hook"
"table-load-hook"
"table-point-entered-cell-hook"
"table-point-left-cell-hook"
"tags-add-tables"
"tags-case-fold-search"
"tags-compression-info-list"
"tags-table-list"
"temp-buffer-max-height"
"temp-buffer-resize-mode"
"temp-buffer-show-function"
"temporary-file-directory"
"term-file-prefix"
"texinfo-close-quote"
"texinfo-open-quote"
"text-mode-hook"
"this-command"
"three-step-help"
"timer-max-repeats"
"tool-bar-max-label-size"
"tool-bar-mode"
"tool-bar-position"
"tool-bar-style"
"tooltip-delay"
"tooltip-frame-parameters"
"tooltip-hide-delay"
"tooltip-mode"
"tooltip-recent-seconds"
"tooltip-short-delay"
"tooltip-use-echo-area"
"tooltip-x-offset"
"tooltip-y-offset"
"tpu-edt-mode"
"trace-buffer"
"track-eol"
"tramp-mode"
"tramp-syntax"
"transient-mark-mode"
"trash-directory"
"truncate-lines"
"truncate-partial-width-windows"
"tutorial-directory"
"type-break-mode"
"underline-minimum-offset"
"undo-ask-before-discard"
"undo-limit"
"undo-outer-limit"
"undo-strong-limit"
"unibyte-display-via-language-environment"
"unify-8859-on-decoding-mode"
"unify-8859-on-encoding-mode"
"url-debug"
"url-handler-mode"
"use-dialog-box"
"use-empty-active-region"
"use-file-dialog"
"user-emacs-directory"
"user-full-name"
"user-mail-address"
"vc-before-checkin-hook"
"vc-checkin-hook"
"vc-checkout-hook"
"vc-consult-headers"
"vc-directory-exclusion-list"
"vc-display-status"
"vc-follow-symlinks"
"vc-handled-backends"
"vc-ignore-dir-regexp"
"vc-keep-workfiles"
"vc-make-backup-files"
"vc-mistrust-permissions"
"vc-rcs-master-templates"
"vc-sccs-master-templates"
"vc-stay-local"
"version-control"
"vertical-centering-font-regexp"
"view-read-only"
"view-remove-frame-by-deleting"
"visible-bell"
"visible-cursor"
"visual-line-fringe-indicators"
"void-text-area-pointer"
"which-function-mode"
"window-combination-limit"
"window-combination-resize"
"window-min-height"
"window-min-width"
"window-sides-slots"
"window-sides-vertical"
"winner-mode"
"woman-locale"
"word-wrap"
"words-include-escapes"
"x-select-enable-clipboard"
"x-select-enable-clipboard-manager"
"x-select-enable-primary"
"x-select-request-type"
"x-stretch-cursor"
"x-underline-at-descent-line"
"x-use-underline-position-properties"
"xterm-mouse-mode"
"yank-excluded-properties"
"yank-handled-properties"
"yank-menu-length"
"yank-pop-change-selection"

))

(defvar xah-elisp-elisp-all-keywords nil "List of all elisp keywords")
(setq xah-elisp-elisp-all-keywords (append xah-elisp-elisp-lang-words xah-elisp-emacs-words xah-elisp-emacs-user-commands xah-elisp-keyword-builtin xah-elisp-elisp-vars-1 ))



;; emacs 24.4 or 24.3 change fix

(defun xah-elisp-up-list (arg1 &optional arg2 arg3)
  "Backward compatibility fix for emacs 24.4's `up-list'.
emacs 25.x changed `up-list' to take up to 3 args. Before, only 1."
  (interactive)
  (if (>= emacs-major-version 25)
      (up-list arg1 arg2 arg3)
    (up-list arg1)))


;; completion

(defun xah-elisp-complete-symbol ()
  "Perform keyword completion on current symbol.
This uses `ido-mode' user interface for completion."
  (interactive)
  (let* (
         (-bds (bounds-of-thing-at-point 'symbol))
         (-p1 (car -bds))
         (-p2 (cdr -bds))
         (-current-sym
          (if  (or (null -p1) (null -p2) (equal -p1 -p2))
              ""
            (buffer-substring-no-properties -p1 -p2)))
         -result-sym)
    (when (not -current-sym) (setq -current-sym ""))
    (setq -result-sym
          (ido-completing-read "" xah-elisp-elisp-all-keywords nil nil -current-sym ))
    (delete-region -p1 -p2)
    (insert -result-sym)

    ;; use case of completion
    (when (not (xah-elisp-start-with-left-paren-p))
      (let ( (-abbrev-expanded-p (xah-elisp-expand-abbrev)))
        ;; (when (not (xah-elisp-start-with-left-paren-p)) (xah-elisp-add-paren-around-symbol))
))))

(defun xah-elisp-completion-function ()
  "This is the function for the hook `completion-at-point-functions'"
  (interactive)
  (let* (
         (-bds (bounds-of-thing-at-point 'symbol))
         (-p1 (car -bds))
         (-p2 (cdr -bds)))
    (list -p1 -p2 xah-elisp-elisp-all-keywords nil )))

(defun xah-elisp-start-with-left-paren-p ()
  "Returns t or nil"
  (interactive)
  (save-excursion
    (forward-symbol -1) (backward-char 1)
    (if (looking-at "(")
        t
      nil)))

(defun xah-elisp-add-paren-around-symbol ()
  "Add paren around symbol before cursor and add a space before closing paren, place cursor there.
 Example:
 my-xyz▮
becomes
 (my-xyz ▮)"
  (interactive)
  (forward-symbol -1) (insert "(") (forward-symbol 1) (insert " )")
  (backward-char 1))

(defun xah-elisp-remove-paren-pair ()
  "Remove closest left outer paren around cursor or remove string quote and activate the region.
Cursor is moved to the left deleted paren spot, mark is set to the right deleted paren spot.
Call `exchange-point-and-mark' \\[exchange-point-and-mark] to highlight them."
  (interactive)
  (let ((pos (point))
        p1 p2
        )
    (atomic-change-group
      (xah-elisp-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING")
      (while (not (char-equal (char-after) ?\( ))
        (xah-elisp-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING"))
      (setq p1 (point))
      (forward-sexp)
      (setq p2 (point))
      (delete-char -1)
      (push-mark (point) t t)
      (goto-char p1)
      (delete-char 1))))

(defun xah-elisp-expand-abbrev-maybe (&optional *expand-func)
  "Expand emacs lisp function name before cursor into template.
Returns true if there's a expansion, else nil."
  (interactive)
  (let (
        -p1 -p2
        -ab-str
        (-syntax-state (syntax-ppss)))
    (if (or (nth 3 -syntax-state) (nth 4 -syntax-state))
        nil
      (xah-elisp-expand-abbrev))))

(put 'xah-elisp-expand-abbrev-maybe 'no-self-insert t)

(defun xah-elisp-expand-abbrev ()
  "Expand the symbol before cursor.
Returns true if there's a expansion, else nil."
  (interactive)
  (let (
        -p1 -p2
        -ab-str
        )
    (save-excursion
      (forward-symbol -1)
      (setq -p1 (point))
      (forward-symbol 1)
      (setq -p2 (point)))
    (setq -ab-str (buffer-substring-no-properties -p1 -p2))
    (if (abbrev-symbol -ab-str)
        (progn
          (abbrev-insert (abbrev-symbol -ab-str) -ab-str -p1 -p2 )
          (xah-elisp--abbrev-position-cursor -p1)
          t)
      nil)))

(defun xah-elisp-abbrev-enable-function ()
  "Determine whether to expand abbrev.
This is called by emacs abbrev system."
  (let ((-syntax-state (syntax-ppss)))
    (if (or (nth 3 -syntax-state) (nth 4 -syntax-state))
        nil
      t)))

(defun xah-elisp--abbrev-position-cursor (&optional *pos)
  "Move cursor back to ▮ if exist, else put at end.
Limit backward search to at *pos or at beginning of line.
Return true if found, else false."
  (interactive)
  (let ((found-p (search-backward "▮" (if *pos *pos (line-beginning-position)) t )))
    (when found-p
      ;; (forward-char )
      (search-forward "▮"))
    found-p
    ))


;; indent/reformat related

(defun xah-elisp-complete-or-indent ()
  "Do keyword completion or indent/prettify-format.

If char before point is letters and char after point is whitespace or punctuation, then do completion, except when in string or comment. In these cases, do `xah-elisp-prettify-root-sexp'."
  (interactive)
  ;; consider the char to the left or right of cursor. Each side is either empty or char.
  ;; there are 4 cases:
  ;; space▮space → do indent
  ;; space▮char → do indent
  ;; char▮space → do completion
  ;; char ▮char → do indent
  (let ( (-syntax-state (syntax-ppss)))
    (if (or (nth 3 -syntax-state) (nth 4 -syntax-state))
        (progn
          (xah-elisp-prettify-root-sexp))
      (progn (if
                 (and (looking-back "[-_a-zA-Z]" 1)
                      (or (eobp) (looking-at "[\n[:blank:][:punct:]]")))
                 (xah-elisp-complete-symbol)
               (xah-elisp-prettify-root-sexp))))))

(defun xah-elisp-prettify-root-sexp ()
  "Prettify format current root sexp group.
Root sexp group is the outmost sexp unit."
  (interactive)
  (save-excursion
    (let (-p1 -p2)
      (xah-elisp-goto-outmost-bracket)
      (setq -p1 (point))
      (setq -p2 (scan-sexps (point) 1))
      (progn
        (goto-char -p1)
        (indent-sexp)
        (xah-elisp-compact-parens-region -p1 -p2)))))

(defun xah-elisp-goto-outmost-bracket (&optional *pos)
  "Move cursor to the beginning of outer-most bracket, with respect to *pos.
Returns true if point is moved, else false."
  (interactive)
  (let ((-i 0)
        (-p0 (if (number-or-marker-p *pos)
                 *pos
               (point))))
    (goto-char -p0)
    (while
        (and (< (setq -i (1+ -i)) 20)
             (not (eq (nth 0 (syntax-ppss (point))) 0)))
      (xah-elisp-up-list -1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING"))
    (if (equal -p0 (point))
        nil
      t
      )))

(defun xah-elisp-compact-parens (&optional *begin *end)
  "Remove whitespaces in ending repetition of parenthesises.
If there's a text selection, act on the region, else, on defun block."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (xah-elisp-goto-outmost-bracket)
       (list (point) (scan-sexps (point) 1)))))
  (let ((-p1 *begin) (-p2 *end))
    (when (null *begin)
      (save-excursion
        (xah-elisp-goto-outmost-bracket)
        (setq -p1 (point))
        (setq -p2 (scan-sexps (point) 1))))
    (xah-elisp-compact-parens-region -p1 -p2)))

(defun xah-elisp-compact-parens-region (*begin *end)
  "Remove whitespaces in ending repetition of parenthesises in region."
  (interactive "r")
  (let (-syntax-state)
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char (point-min))
      (while (search-forward-regexp ")[ \t\n]+)" nil t)
        (setq -syntax-state (syntax-ppss (match-beginning 0)))
        (if (or (nth 3 -syntax-state ) (nth 4 -syntax-state))
            (progn (search-forward ")"))
          (progn (replace-match "))")
                 (search-backward ")")))))))


;; abbrev

(setq xah-elisp-mode-abbrev-table nil)

(define-abbrev-table 'xah-elisp-mode-abbrev-table
  '(
    ("d" "(defun f▮ ()\n  \"DOCSTRING\"\n  (interactive)\n  (let (VAR)\n\n  ))" nil :system t)
    ("i" "(insert ▮)" nil :system t)
    ("l" "(let (x▮)\n x\n)" nil :system t)
    ("m" "(message \"%s▮\" ARGS)" nil :system t)
    ("p" "(point)" nil :system t)
    ("s" "(setq ▮ VAL)" nil :system t)
    ("w" "(when ▮)" nil :system t)

    ("ah" "add-hook" nil :system t)
    ("bc" "backward-char" nil :system t)
    ("bs" "buffer-substring" nil :system t)
    ("bw" "backward-word" nil :system t)
    ("ca" "custom-autoload" nil :system t)
    ("cb" "current-buffer" nil :system t)
    ("cc" "condition-case" nil :system t)
    ("cd" "copy-directory" nil :system t)
    ("cf" "copy-file" nil :system t)
    ("cw" "current-word" nil :system t)
    ("dc" "delete-char" nil :system t)
    ("dd" "delete-directory" nil :system t)
    ("df" "delete-file" nil :system t)
    ("dk" "define-key" nil :system t)
    ("dr" "delete-region" nil :system t)
    ("fc" "forward-char" nil :system t)
    ("fw" "forward-word" nil :system t)
    ("ff" "find-file" nil :system t)
    ("fl" "forward-line" nil :system t)
    ("gc" "goto-char" nil :system t)
    ("kb" "kill-buffer" nil :system t)
    ("kr" "kill-region" nil :system t)
    ("la" "looking-at" nil :system t)
    ("lc" "left-char" nil :system t)
    ("mb" "match-beginning" nil :system t)
    ("md" "make-directory" nil :system t)
    ("me" "match-end" nil :system t)
    ("ms" "match-string" nil :system t)
    ("pm" "point-min" nil :system t)
    ("px" "point-max" nil :system t)
    ("rb" "region-beginning" nil :system t)
    ("rc" "right-char" nil :system t)
    ("re" "region-end" nil :system t)
    ("rf" "rename-file" nil :system t)
    ("rm" "replace-match" nil :system t)
    ("rq" "regexp-quote" nil :system t)
    ("rr" "replace-regexp" nil :system t)
    ("sb" "search-backward" nil :system t)
    ("sc" "shell-command" nil :system t)
    ("se" "save-excursion" nil :system t)
    ("sf" "search-forward" nil :system t)
    ("sm" "string-match" nil :system t)
    ("sr" "save-restriction" nil :system t)
    ("ss" "split-string" nil :system t)
    ("wg" "widget-get" nil :system t)

    ("bfn" "buffer-file-name" nil :system t)
    ("bmp" "buffer-modified-p" nil :system t)
    ("bol" "beginning-of-line" nil :system t)
    ("cdr" "cdr" nil :system t)
    ("efn" "expand-file-name" nil :system t)
    ("eol" "end-of-line" nil :system t)
    ("fnd" "file-name-directory" nil :system t)
    ("fne" "file-name-extension" nil :system t)
    ("fnn" "file-name-nondirectory" nil :system t)
    ("frn" "file-relative-name" nil :system t)
    ("gnb" "generate-new-buffer" nil :system t)
    ("gsk" "global-set-key" nil :system t)
    ("ifc" "insert-file-contents" nil :system t)
    ("lbp" "line-beginning-position" nil :system t)
    ("lep" "line-end-position" nil :system t)
    ("mlv" "make-local-variable" nil :system t)
    ("ntr" "narrow-to-region" nil :system t)
    ("nts" "number-to-string" nil :system t)
    ("pmi" "point-min" nil :system t)
    ("rap" "region-active-p" nil :system t)
    ("rsb" "re-search-backward" nil :system t)
    ("rsf" "re-search-forward" nil :system t)
    ("sbr" "search-backward-regexp" nil :system t)
    ("scb" "skip-chars-backward" nil :system t)
    ("scf" "skip-chars-forward" nil :system t)
    ("sfm" "set-file-modes" nil :system t)
    ("sfr" "search-forward-regexp" nil :system t)
    ("stn" "string-to-number" nil :system t)
    ("tap" "thing-at-point" nil :system t)
    ("urp" "use-region-p" nil :system t)
    ("wcb" "with-current-buffer" nil :system t)

    ("bsnp" "buffer-substring-no-properties" nil :system t)
    ("fnse" "file-name-sans-extension" nil :system t)
    ("rris" "replace-regexp-in-string" nil :system t)
    ("yonp" "yes-or-no-p" nil :system t)

    ("botap" "bounds-of-thing-at-point" nil :system t)

    ("add-hook" "(add-hook 'HOOK▮ 'FUNCTION)" nil :system t)
    ("add-text-properties" "(add-text-properties START▮ END PROPS &optional OBJECT)" nil :system t)
    ("add-to-list" "(add-to-list LIST-VAR▮ ELEMENT &optional APPEND COMPARE-FN)" nil :system t)
    ("and" "(and ▮)" nil :system t )
    ("append" "(append ▮)" nil :system t)
    ("apply" "(apply ▮)" nil :system t)
    ("aref" "(aref ARRAY▮ INDEX)" nil :system t)
    ("aset" "(aset ARRAY▮ IDX NEWELT)" nil :system t)

    ("assoc" "(assoc KEY▮ LIST)" nil :system t)
    ("rassoc" "(rassoc value▮ alist)" nil :system t)
    ("assq" "(assq key▮ alist)" nil :system t)
    ("alist-get" "(alist-get key▮ value &optional default)" nil :system t)
    ("rassq" "(rassq value▮ alist)" nil :system t)
    ("assoc-default" "(assoc-default key▮ alist &optional test default)" nil :system t)
    ("copy-alist" "(copy-alist alist▮)" nil :system t)
    ("assq-delete-all" "(assq-delete-all key▮ alist)" nil :system t)
    ("rassq-delete-all" "(rassq-delete-all value▮ alist)" nil :system t)

    ("autoload" "(autoload 'FUNCNAME▮ \"FILENAME\" &optional \"DOCSTRING\" INTERACTIVE TYPE)" nil :system t)
    ("backward-char" "(backward-char ▮)" nil :system t)
    ("backward-word" "(backward-word ▮)" nil :system t)
    ("beginning-of-line" "(beginning-of-line)" nil :system t)
    ("boundp" "(boundp '▮)" nil :system t)
    ("bounds-of-thing-at-point" "(bounds-of-thing-at-point 'symbol▮ 'filename 'word 'whitespace 'line)" :system t)
    ("buffer-file-name" "(buffer-file-name)" nil :system t)
    ("buffer-modified-p" "(buffer-modified-p ▮)" nil :system t)
    ("buffer-name" "(buffer-name BUFFER▮)" nil :system t)
    ("buffer-substring" "(buffer-substring START▮ END)" nil :system t)
    ("buffer-substring-no-properties" "(buffer-substring-no-properties START▮ END)" nil :system t)
    ("bufferp" "(bufferp ▮)" nil :system t)
    ("call-interactively" "(call-interactively 'FUNCTION▮ &optional RECORD-FLAG KEYS)" nil :system t)
    ("called-interactively-p" "(called-interactively-p 'interactive▮)" nil :system t)
    ("car" "(car ▮)" nil :system t)
    ("catch" "(catch TAG▮ BODY)" nil :system t)
    ("cdr" "(cdr ▮)" nil :system t)
    ("char-to-string" "(char-to-string CHAR▮) " nil :system t)
    ("concat" "(concat \"▮\" \"▮\")" nil :system t)
    ("cond" "(cond\n(CONDITION▮ BODY)\n(CONDITION BODY)\n)" nil :system t)
    ("condition-case" "(condition-case ▮)" nil :system t)
    ("cons" "(cons CAR▮ CDR)" nil :system t)
    ("consp" "(consp ▮)" nil :system t)
    ("copy-directory" "(copy-directory ▮ NEWNAME &optional KEEP-TIME PARENTS)" nil :system t)
    ("copy-file" "(copy-file FILE▮ NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID)" nil :system t)
    ("current-buffer" "(current-buffer)" nil :system t)
    ("custom-autoload" "(custom-autoload ▮ SYMBOL LOAD &optional NOSET)" nil :system t)
    ("defalias" "(defalias 'SYMBOL▮ 'DEFINITION &optional DOCSTRING)" nil :system t)
    ("defconst" "(defconst ▮ INITVALUE \"DOCSTRING\")" nil :system t)
    ("defcustom" "(defcustom ▮ VALUE \"DOC\" &optional ARGS)" nil :system t)
    ("define-key" "(define-key KEYMAPNAME▮ (kbd \"M-b\") 'FUNCNAME)" nil :system t)
    ("defsubst" "(defsubst ▮)" nil :system t)
    ("defun" "(defun ▮ ()\n  \"DOCSTRING\"\n  (interactive)\n  (let (VAR)\n\n  ))" nil :system t)
    ("defvar" "(defvar ▮ &optional INITVALUE \"DOCSTRING\")" nil :system t)
    ("delete" "(delete OBJECT▮ SEQUENCE)" nil :system t)
    ("delete-char" "(delete-char ▮)" nil :system t)
    ("delete-directory" "(delete-directory ▮ &optional RECURSIVE)" nil :system t)
    ("delete-dups" "(delete-dups LIST▮)" nil :system t)
    ("delete-file" "(delete-file ▮)" nil :system t)
    ("delete-region" "(delete-region pos1▮ pos2)" nil :system t)
    ("delq" "(delq ELT▮ LIST)" nil :system t)
    ("directory-files" "(directory-files ▮ &optional FULL MATCH NOSORT)" nil :system t)
    ("dolist" "(dolist (VAR▮ LIST [RESULT]) BODY)" nil :system t)
    ("dotimes" "(dotimes (VAR▮ COUNT [RESULT]) BODY)" nil :system t)
    ("elt" "(elt SEQUENCE▮ N)" nil :system t)
    ("end-of-line" "(end-of-line ▮&optional N)" nil :system t)
    ("eq" "(eq ▮)" nil :system t)
    ("equal" "(equal ▮)" nil :system t)
    ("error" "(error \"%s\" ▮)" nil :system t)
    ("expand-file-name" "(expand-file-name ▮ &optional relativedir)" nil :system t)
    ("fboundp" "(fboundp '▮)" nil :system t)
    ("featurep" "(featurep 'FEATURE▮)" nil :system t)
    ("forward-word" "(forward-word ▮)" nil :system t)
    ("file-directory-p" "(file-directory-p ▮)" nil :system t)
    ("file-exists-p" "(file-exists-p ▮)" nil :system t)
    ("file-name-directory" "(file-name-directory ▮)" nil :system t)
    ("file-name-extension" "(file-name-extension ▮ &optional PERIOD)" nil :system t)
    ("file-name-nondirectory" "(file-name-nondirectory ▮)" nil :system t)
    ("file-name-as-directory" "(file-name-as-directory ▮)" nil :system t)
    ("directory-name-p" "(directory-name-p ▮)" nil :system t)
    ("directory-file-name" "(directory-file-name ▮)" nil :system t)
    ("abbreviate-file-name" "(abbreviate-file-name ▮)" nil :system t)
    ("file-name-sans-extension" "(file-name-sans-extension ▮)" nil :system t)
    ("file-regular-p" "(file-regular-p ▮)" nil :system t)
    ("file-relative-name" "(file-relative-name ▮)" nil :system t)
    ("find-file" "(find-file ▮)" nil :system t)
    ("format" "(format \"%s\" ▮)" nil :system t)
    ("format" "(format \"▮\" &optional OBJECTS)" nil :system t)
    ("forward-char" "(forward-char ▮)" nil :system t)
    ("forward-line" "(forward-line ▮)" nil :system t)
    ("funcall" "(funcall 'FUNCTION▮ &rest ARGUMENTS)" nil :system t)
    ("function" "(function ▮)" nil :system t)
    ("generate-new-buffer" "(generate-new-buffer ▮)" nil :system t)
    ("generate-new-buffer-name" "(generate-new-buffer-name STARTING-NAME▮ &optional IGNORE)" nil :system t)
    ("get" "(get SYMBOL▮ PROPNAME)" nil :system t)
    ("get-buffer" "(get-buffer BUFFER-OR-NAME▮)" nil :system t)
    ("get-char-code-property" "(get-char-code-property CHAR▮ PROPNAME)" nil :system t)
    ("get-char-property" "(get-char-property POSITION▮ PROP &optional OBJECT)" nil :system t)
    ("get-char-property-and-overlay" "(get-char-property-and-overlay POSITION▮ PROP &optional)" nil :system t)
    ("get-pos-property" "(get-pos-property POSITION▮ PROP &optional OBJECT)" nil :system t)
    ("get-text-property" "(get-text-property POS▮ PROP &optional OBJECT)" nil :system t)
    ("global-set-key" "(global-set-key (kbd \"C-▮\") 'COMMAND)" nil :system t)
    ("goto-char" "(goto-char ▮)" nil :system t)
    ("if" "(if ▮\n    (progn )\n  (progn )\n)" nil :system t)
    ("insert" "(insert ▮)" nil :system t)
    ("insert-char" "(insert-char CHARACTER▮ &optional COUNT INHERIT)" nil :system t)
    ("insert-file-contents" "(insert-file-contents ▮ &optional VISIT BEG END REPLACE)" nil :system t)
    ("interactive" "(interactive)" nil :system t)
    ("kbd" "(kbd \"▮\")" nil :system t)
    ("kill-buffer" "(kill-buffer ▮)" nil :system t)
    ("kill-region" "(kill-region BEG▮ END &optional REGION)" nil :system t)
    ("lambda" "(lambda (▮) BODY)" nil :system t)
    ("length" "(length ▮)" nil :system t)
    ("left-char" "(left-char ▮)" nil :system t)
    ("let" "(let (▮)\n x\n)" nil :system t)
    ("line-beginning-position" "(line-beginning-position)" nil :system t)
    ("line-end-position" "(line-end-position)" nil :system t)
    ("list" "(list ▮)" nil :system t)
    ("load" "(load FILE▮ &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)" nil :system t)
    ("load-file" "(load-file FILE▮)" nil :system t)
    ("looking-at" "(looking-at \"REGEXP▮\")" nil :system t)
    ("looking-back" "(looking-back \"REGEXP▮\" LIMIT &optional GREEDY)" nil :system t)
    ("make-directory" "(make-directory ▮ &optional PARENTS)" nil :system t)
    ("make-list" "(make-list LENGTH▮ INIT)" nil :system t)
    ("make-local-variable" "(make-local-variable ▮)" nil :system t)
    ("mapc" "(mapc '▮ SEQUENCE)" nil :system t)
    ("mapcar" "(mapcar '▮ SEQUENCE)" nil :system t)
    ("mapconcat" "(mapconcat FUNCTION▮ SEQUENCE SEPARATOR)" nil :system t)
    ("match-beginning" "(match-beginning N▮)" nil :system t)
    ("match-data" "(match-data &optional INTEGERS▮ REUSE RESEAT)" nil :system t)
    ("match-end" "(match-end N▮)" nil :system t)
    ("match-string" "(match-string NUM▮ &optional STRING)" nil :system t)
    ("member" "(member ELT▮ LIST)" nil :system t)
    ("member" "(member OBJECT▮ LIST)" nil :system t)
    ("member-ignore-case" "(member-ignore-case OBJECT▮ LIST)" nil :system t)
    ("memq" "(memq ELT▮ LIST)" nil :system t)
    ("memql" "(memql OBJECT▮ LIST)" nil :system t)
    ("message" "(message \"%s▮\" ARGS)" nil :system t)
    ("narrow-to-region" "(narrow-to-region START▮ END)" nil :system t)
    ("next-char-property-change" "(next-char-property-change POS &optional LIMIT)" nil :system t)
    ("next-property-change" "(next-property-change POS &optional OBJECT LIMIT)" nil :system t)
    ("next-single-char-property-change" "(next-single-char-property-change POS PROP &optional OBJECT LIMIT)" nil :system t)
    ("next-single-property-change" "(next-single-property-change POS PROP &optional OBJECT LIMIT)" nil :system t)
    ("not" "(not ▮)" nil :system t)
    ("nth" "(nth N▮ LIST)" nil :system t)
    ("null" "(null ▮)" nil :system t)
    ("number-sequence" "(number-sequence FROM▮ &optional TO INC)" nil :system t)
    ("number-to-string" "(number-to-string ▮)" nil :system t)
    ("or" "(or ▮)" nil :system t)
    ("point" "(point)" nil :system t)
    ("point-max" "(point-max)" nil :system t)
    ("point-min" "(point-min)" nil :system t)
    ("previous-char-property-change" "(previous-char-property-change POS &optional LIMIT)" nil :system t)
    ("previous-property-change" "(previous-property-change POS &optional OBJECT LIMIT)" nil :system t)
    ("previous-single-char-property-change" "(previous-single-char-property-change POS PROP &optional OBJECT LIMIT)" nil :system t)
    ("previous-single-property-change" "(previous-single-property-change POS PROP &optional OBJECT LIMIT)" nil :system t)
    ("prin1" "(prin1 ▮)" nil :system t)
    ("princ" "(princ ▮)" nil :system t)
    ("print" "(print ▮)" nil :system t)
    ("progn" "(progn\n▮)" nil :system t)
    ("propertize" "(propertize STRING▮ &rest PROPERTIES)" nil :system t)
    ("push" "(push NEWELT▮ PLACE)" nil :system t)
    ("push-mark" "(push-mark ▮&optional LOCATION NOMSG ACTIVATE)" nil :system t)
    ("put" "(put 'SYMBOL▮ PROPNAME VALUE)" nil :system t)
    ("put-text-property" "(put-text-property START▮ END PROP VALUE &optional OBJECT)" nil :system t)
    ("random" "(random ▮)" nil :system t)
    ("rassoc" "(rassoc KEY▮ LIST)" nil :system t)
    ("re-search-backward" "(re-search-backward \"REGEXP▮\" &optional BOUND 'NOERROR COUNT)" nil :system t)
    ("re-search-forward" "(re-search-forward \"REGEXP▮\" &optional BOUND 'NOERROR COUNT)" nil :system t)
    ("read-directory-name" "(read-directory-name \"▮\" &optional DIR DEFAULT-DIRNAME MUSTMATCH INITIAL)" nil :system t)
    ("read-file-name" "(read-file-name \"▮\" &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL PREDICATE)" nil :system t)
    ("read-regexp" "(read-regexp \"▮\" &optional DEFAULT-VALUE)" nil :system t)
    ("read-string" "(read-string \"▮\" &optional INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD)" nil :system t)
    ("regexp-opt" "(regexp-opt STRINGS▮ &optional PAREN)" nil :system t)
    ("regexp-quote" "(regexp-quote ▮)" nil :system t)
    ("region-active-p" "(region-active-p)" nil :system t)
    ("region-beginning" "(region-beginning)" nil :system t)
    ("region-end" "(region-end)" nil :system t)
    ("remove" "(remove OBJECT▮ SEQUENCE)" nil :system t)
    ("remove-list-of-text-properties" "(remove-list-of-text-properties START▮ END LIST OF PROPERTIES &optional OBJECT)" nil :system t)
    ("remove-text-properties" "(remove-text-properties START▮ END PROPS &optional OBJECT)" nil :system t)
    ("remq" "(remq OBJECT▮ LIST)" nil :system t)
    ("rename-buffer" "(rename-buffer NEWNAME▮ &optional UNIQUE)" nil :system t)
    ("rename-file" "(rename-file FILE▮ NEWNAME &optional OK-IF-ALREADY-EXISTS)" nil :system t)
    ("repeat" "(repeat ▮)" nil :system t)
    ("replace-match" "(replace-match NEWTEXT▮ &optional FIXEDCASE LITERAL \"STRING\" SUBEXP)" nil :system t)
    ("replace-regexp" "(replace-regexp \"REGEXP▮\" TO-STRING &optional DELIMITED START END)" nil :system t)
    ("replace-regexp-in-string" "(replace-regexp-in-string \"REGEXP▮\" REP \"STRING\" &optional FIXEDCASE LITERAL SUBEXP START)" nil :system t)
    ("require" "(require ▮)" nil :system t)
    ("reverse" "(reverse ▮)" nil :system t)
    ("right-char" "(right-char ▮)" nil :system t)
    ("save-buffer" "(save-buffer &optional ARG▮)" nil :system t)
    ("save-current-buffer" "(save-current-buffer ▮)" nil :system t)
    ("save-excursion" "(save-excursion ▮)" nil :system t)
    ("save-restriction" "(save-restriction ▮)" nil :system t)
    ("search-backward" "(search-backward \"▮\" &optional BOUND 'NOERROR COUNT)" nil :system t)
    ("search-backward-regexp" "(search-backward-regexp \"▮\" &optional BOUND 'NOERROR COUNT)" nil :system t)
    ("search-forward" "(search-forward \"▮\" &optional BOUND 'NOERROR COUNT)" nil :system t)
    ("search-forward-regexp" "(search-forward-regexp \"▮\" &optional BOUND 'NOERROR COUNT)" nil :system t)
    ("set-buffer" "(set-buffer ▮)" nil :system t)
    ("set-file-modes" "(set-file-modes ▮ MODE)" nil :system t)
    ("set-mark" "(set-mark ▮)" nil :system t)
    ("set-text-properties" "(set-text-properties START▮ END PROPS &optional OBJECT)" nil :system t)
    ("setq" "(setq ▮)" nil :system t)
    ("shell-command" "(shell-command ▮ &optional OUTPUT-BUFFER ERROR-BUFFER)" nil :system t)
    ("skip-chars-backward" "(skip-chars-backward \"▮\" &optional LIM)" nil :system t)
    ("skip-chars-forward" "(skip-chars-forward \"▮\" &optional LIM)" nil :system t)
    ("split-string" "(split-string ▮ &optional SEPARATORS OMIT-NULLS)" nil :system t)
    ("string" "(string ▮)" nil :system t)
    ("string-equal" "(string-equal str1▮ str2)" nil :system t)
    ("string-match" "(string-match \"REGEXP▮\" \"STRING\" &optional START)" nil :system t)
    ("string-match-p" "(string-match-p \"REGEXP▮\" \"STRING\" &optional START)" nil :system t)
    ("string-to-char" "(string-to-char \"▮\")" nil :system t)
    ("string-to-number" "(string-to-number \"▮\")" nil :system t)
    ("string=" "(string-equal str1▮ str2)" nil :system t)
    ("stringp" "(stringp ▮)" nil :system t)
    ("substring" "(substring STRING▮ FROM &optional TO)" nil :system t)
    ("substring-no-properties" "(substring-no-properties ▮ FROM TO)" nil :system t)
    ("text-properties-at" "(text-properties-at POSITION▮ &optional OBJECT)" nil :system t)
    ("text-property-any" "(text-property-any START END PROP VALUE &optional OBJECT)" nil :system t)
    ("text-property-not-all" "(text-property-not-all START END PROP VALUE &optional OBJECT)" nil :system t)
    ("thing-at-point" "(thing-at-point 'word▮ 'symbol 'list 'sexp 'defun 'filename 'url 'email 'sentence 'whitespace 'line 'number 'page)" :system t)
    ("throw" "(throw TAG▮ VALUE)" nil :system t)
    ("unless" "(unless ▮)" nil :system t)
    ("use-region-p" "(use-region-p)" nil :system t)
    ("user-error" "(user-error FORMAT▮ &rest ARGS)" nil :system t)
    ("vector" "(vector ▮)" nil :system t)
    ("version<" "(version< \"24.4\" emacs-version)" nil :system t )
    ("version<=" "(version<= \"24.4\" emacs-version)" nil :system t )
    ("when" "(when ▮)" nil :system t)
    ("while" "(while (< ii▮ 9)\n  (setq ii (1+ ii)))" nil :system t)
    ("widen" "(widen)" nil :system t)
    ("widget-get" "(widget-get ▮)" nil :system t)
    ("with-current-buffer" "(with-current-buffer BUFFER-OR-NAME▮ BODY)" nil :system t)
    ("with-temp-buffer" "(with-temp-buffer ▮)" nil :system t)
    ("with-temp-file" "(with-temp-file FILE▮)" nil :system t)
    ("write-file" "(write-file FILENAME▮ &optional CONFIRM)" nil :system t)
    ("write-region" "(write-region (point-min) (point-max) FILENAME &optional APPEND VISIT LOCKNAME MUSTBENEW)" nil :system t)
    ("y-or-n-p" "(y-or-n-p \"PROMPT▮ \")" nil :system t)
    ("yes-or-no-p" "(yes-or-no-p \"PROMPT▮ \")" nil :system t)

    ("get-file-buffer" "(get-file-buffer FILENAME▮)" nil :system t)
    ("find-buffer-visiting" "(find-buffer-visiting FILENAME▮ &optional PREDICATE)" nil :system t)
    ("set-visited-file-name" "(set-visited-file-name FILENAME▮ &optional NO-QUERY ALONG-WITH-FILE)" nil :system t)
    ("buffer-modified-p" "(buffer-modified-p BUFFER▮)" nil :system t)
    ("set-buffer-modified-p" "(set-buffer-modified-p FLAG▮)" nil :system t)
    ("restore-buffer-modified-p" "(restore-buffer-modified-p FLAG▮)" nil :system t)
    ("not-modified" "(not-modified &optional ARG▮)" nil :system t)
    ("buffer-modified-tick" "(buffer-modified-tick &optional BUFFER▮)" nil :system t)
    ("buffer-chars-modified-tick" "(buffer-chars-modified-tick &optional BUFFER▮)" nil :system t)
    ("verify-visited-file-modtime" "(verify-visited-file-modtime BUFFER▮)" nil :system t)
    ("clear-visited-file-modtime" "(clear-visited-file-modtime)" nil :system t)
    ("visited-file-modtime" "(visited-file-modtime)" nil :system t)
    ("set-visited-file-modtime" "(set-visited-file-modtime &optional TIME▮)" nil :system t)
    ("ask-user-about-supersession-threat" "(ask-user-about-supersession-threat FILENAME▮)" nil :system t)
    ("toggle-read-only" "(toggle-read-only &optional ARG▮)" nil :system t)
    ("barf-if-buffer-read-only" "(barf-if-buffer-read-only)" nil :system t)
    ("buffer-list" "(buffer-list &optional FRAME▮)" nil :system t)
    ("other-buffer" "(other-buffer &optional BUFFER▮ VISIBLE-OK FRAME)" nil :system t)
    ("last-buffer" "(last-buffer &optional BUFFER▮ VISIBLE-OK FRAME)" nil :system t)
    ("bury-buffer" "(bury-buffer &optional BUFFER-OR-NAME▮)" nil :system t)
    ("unbury-buffer" "(unbury-buffer)" nil :system t)
    ("get-buffer-create" "(get-buffer-create BUFFER-OR-NAME▮)" nil :system t)
    ("generate-new-buffer" "(generate-new-buffer NAME▮)" nil :system t)
    ("kill-buffer" "(kill-buffer &optional BUFFER-OR-NAME▮)" nil :system t)
    ("buffer-live-p" "(buffer-live-p OBJECT▮)" nil :system t)
    ("make-indirect-buffer" "(make-indirect-buffer BASE-BUFFER▮ NAME &optional CLONE)" nil :system t)
    ("clone-indirect-buffer" "(clone-indirect-buffer NEWNAME▮ DISPLAY-FLAG &optional NORECORD)" nil :system t)
    ("buffer-base-buffer" "(buffer-base-buffer &optional BUFFER▮)" nil :system t)
    ("buffer-swap-text" "(buffer-swap-text BUFFER▮)" nil :system t)
    ("gap-position" "(gap-position)" nil :system t)
    ("gap-size" "(gap-size)" nil :system t)
    ("with-output-to-temp-buffer" "(with-output-to-temp-buffer BUFNAME▮ &rest BODY)" nil :system t)

    ("defface" "(defface FACE▮ SPEC \"DOC\" &rest ARGS)" nil :system t)

    ("file-name-absolute-p" "(file-name-absolute-p ▮)" nil :system t)
    ("terpri" "(terpri ▮)" nil :system t)
    ("insert-and-inherit" "(insert-and-inherit ▮)" nil :system t)
    ("insert-before-markers-and-inherit" "(insert-before-markers-and-inherit ▮)" nil :system t)
    ("field-beginning" "(field-beginning &optional POS▮ ESCAPE-FROM-EDGE LIMIT)" nil :system t)
    ("field-end" "(field-end &optional POS▮ ESCAPE-FROM-EDGE LIMIT)" nil :system t)
    ("field-string" "(field-string &optional POS▮)" nil :system t)
    ("field-string-no-properties" "(field-string-no-properties &optional POS▮)" nil :system t)
    ("delete-field" "(delete-field &optional POS▮)" nil :system t)
    ("constrain-to-field" "(constrain-to-field NEW-POS▮ OLD-POS &optional ESCAPE-FROM-EDGE ONLY-IN-LINE INHIBIT-CAPTURE-PROPERTY)" nil :system t)
    ("write-char" "(write-char CHARACTER▮ &optional STREAM)" nil :system t)
    ("prin1-to-string" "(prin1-to-string▮ OBJECT &optional NOESCAPE)" nil :system t)
    ("with-output-to-string" "(with-output-to-string BODY▮)" nil :system t)
    ("create-image" "(create-image FILE-OR-DATA▮ &optional TYPE DATA-P &rest)" nil :system t)
    ("defimage" "(defimage SYMBOL▮ SPECS &optional DOC)" nil :system t)
    ("find-image" "(find-image SPECS▮)" nil :system t)
    ("image-load-path-for-library" "(image-load-path-for-library LIBRARY▮ IMAGE &optional PATH)" nil :system t)

    ("insert-image" "(insert-image IMAGE▮ &optional STRING AREA SLICE)" nil :system t)
    ("insert-sliced-image" "(insert-sliced-image IMAGE▮ &optional STRING AREA ROWS COLS)" nil :system t)
    ("put-image" "(put-image IMAGE▮ POS &optional STRING AREA)" nil :system t)
    ("remove-images" "(remove-images START▮ END &optional BUFFER)" nil :system t)
    ("image-size" "(image-size SPEC▮ &optional PIXELS FRAME)" nil :system t)
    ("image-flush" "(image-flush SPEC▮ &optional FRAME)" nil :system t)
    ("clear-image-cache" "(clear-image-cache &optional FILTER▮)" nil :system t)

    ("font-lock-add-keywords" "(font-lock-add-keywords MODE▮ KEYWORDS &optional HOW)" nil :system t)
    ("font-lock-fontify-buffer" "(font-lock-fontify-buffer ▮)" nil :system t)
    ("set-syntax-table" "(set-syntax-table ▮)" nil :system t)

    ("define-minor-mode" "(define-minor-mode MODE▮ \"DOC\" &optional INIT-VALUE LIGHTER KEYMAP &rest BODY)" nil :system t)

    ("kill-append" "(kill-append STRING▮ BEFORE-P)" nil :system t)
    ("run-with-timer" "(run-with-timer SECS▮ REPEAT FUNCTION &rest ARGS)" nil :system t)
    ;;
    )

  "abbrev table for `xah-elisp-mode'"
  ;; :regexp "\\_<\\([_-0-9A-Za-z]+\\)"
  :regexp "\\([_-0-9A-Za-z]+\\)"
  :case-fixed t
  :enable-function 'xah-elisp-abbrev-enable-function
  )


;; syntax coloring related

(defface xah-elisp-phi-word
  '(
    (t :foreground "black" :background "aquamarine")
    ;; (t :foreground "dark blue")
    ;; (t :foreground "black" :background "pink")
)
  "Face for function parameters."
  :group 'xah-elisp-mode )

(defface xah-elisp-star-word
  '(
    (t :foreground "red" :background "pink")
)
  "Face for function parameters."
  :group 'xah-elisp-mode )

(face-spec-set
 'xah-elisp-star-word
 '(
   (t :foreground "red"  :weight bold))
 'face-defface-spec
 )

(face-spec-set
 'xah-elisp-ttt
 '(
   (t :foreground "blue" :background "pink"))
 'face-defface-spec
 )

(defface xah-elisp-gamma-word
  '(
    (t :foreground "red"))
  "Face for global variable."
  :group 'xah-elisp-mode )

(defface xah-elisp-xi-word
  '(
    (t :foreground "dark green"))
   "Face for user variables."
  :group 'xah-elisp-mode )

(defface xah-elisp-dash-word
  '(
    (t :foreground "#ff00ff"))
  "Face for user variables."
  :group 'xah-elisp-mode )

(defface xah-elisp-cap-variable
  '(
    (t :foreground "firebrick"))
  "Face for capitalized word."
  :group 'xah-elisp-mode )

(setq xah-elisp-font-lock-keywords
      (let (
            (emacsWords (regexp-opt xah-elisp-emacs-words 'symbols))
            (emacsUserWords (regexp-opt xah-elisp-emacs-user-commands 'symbols))
            (emacsBuiltins (regexp-opt xah-elisp-keyword-builtin 'symbols))
            (elispLangWords (regexp-opt xah-elisp-elisp-lang-words 'symbols))
            (elispVars1 (regexp-opt xah-elisp-elisp-vars-1 'symbols))
            (phiWord "φ[-_?0-9A-Za-z]+" )
            (starWord "\\_<\\*[-_?0-9A-Za-z]+" )
            (funParamVar-tmp "\\_<_[-_?0-9A-Za-z]+" )
            (globalVar "\\_<γ[-_?0-9A-Za-z]+" )
            (userVars1 "\\_<ξ[-_?0-9A-Za-z]+" )
            (userVars2 "\\_<-[-_A-Za-z]+[-_?0-9A-Za-z]*" )
            (capVars "\\_<[A-Z][-_?0-9A-Za-z]+" ))
        `(
          (,emacsWords . font-lock-function-name-face)
          (,emacsUserWords . font-lock-type-face)
          (,emacsBuiltins . font-lock-builtin-face)
          (,elispLangWords . font-lock-keyword-face)
          (,elispVars1 . font-lock-variable-name-face)
          (,phiWord . 'xah-elisp-phi-word)
          (,starWord . 'xah-elisp-star-word)
          (,funParamVar-tmp . 'xah-elisp-ttt)
          (,globalVar . 'xah-elisp-gamma-word)
          (,userVars1 . 'xah-elisp-xi-word)
          (,userVars2 . 'xah-elisp-dash-word)
          (,capVars . 'xah-elisp-cap-variable)
          )))


;; ;; syntax table
;; (defvar xah-elisp-syntax-table nil "Syntax table for `xah-elisp-mode'.")
;; (setq xah-elisp-syntax-table
;;       (let ((synTable (make-syntax-table)))
;;         (modify-syntax-entry ?\; "<" synTable)
;;         (modify-syntax-entry ?\n ">" synTable)
;;         (modify-syntax-entry ?` "'   " synTable)
;;         (modify-syntax-entry ?' "'   " synTable)
;;         (modify-syntax-entry ?, "'   " synTable)
;;         (modify-syntax-entry ?@ "'   " synTable)

;;         synTable))


;; keybinding

(defvar xah-elisp-mode-map nil "Keybinding for `xah-elisp-mode'")
(progn
  (setq xah-elisp-mode-map (make-sparse-keymap))

  ;; painful to stick with emacs convention of not defining the key and get what i want
  (define-key xah-elisp-mode-map (kbd "TAB") 'xah-elisp-complete-or-indent)

  (progn
    (define-prefix-command 'xah-elisp-mode-no-chord-map)
    (define-key xah-elisp-mode-no-chord-map (kbd "u") 'xah-elisp-add-paren-around-symbol)
    (define-key xah-elisp-mode-no-chord-map (kbd "t") 'xah-elisp-prettify-root-sexp)
    (define-key xah-elisp-mode-no-chord-map (kbd "h") 'xah-elisp-remove-paren-pair)
    (define-key xah-elisp-mode-no-chord-map (kbd "p") 'xah-elisp-compact-parens)
    (define-key xah-elisp-mode-no-chord-map (kbd "c") 'xah-elisp-complete-symbol)
    (define-key xah-elisp-mode-no-chord-map (kbd "e") 'xah-elisp-expand-abbrev-maybe))

  ;; define separate, so that user can override the lead key
  (define-key xah-elisp-mode-map (kbd "C-c C-c") xah-elisp-mode-no-chord-map)
  )



;;;###autoload
(define-derived-mode xah-elisp-mode prog-mode "∑lisp"
  "A major mode for emacs lisp.

Most useful command is `xah-elisp-complete-or-indent'.

Press TAB before word to pretty format current lisp expression tree.
Press TAB after word to complete.
Press SPACE to expand function name to template.

I also recommend the following setup:
 URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
 URL `http://ergoemacs.org/emacs/modernization_mark-word.html'
 URL `http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html'

home page:
URL `http://ergoemacs.org/emacs/xah-elisp-mode.html'

\\{xah-elisp-mode-map}"
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq font-lock-defaults '((xah-elisp-font-lock-keywords)))

  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ;default to `;;' in comment-region
  (setq-local comment-column 2)

  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local tab-always-indent 'complete)

  (add-function :before-until (local 'eldoc-documentation-function)
                #'elisp-eldoc-documentation-function)

  (add-hook 'completion-at-point-functions 'xah-elisp-completion-function nil 'local)

  (make-local-variable abbrev-expand-function)
  (if (or
       (and (>= emacs-major-version 24)
            (>= emacs-minor-version 4))
       (>= emacs-major-version 25))
      (progn
        (setq abbrev-expand-function 'xah-elisp-expand-abbrev-maybe))
    (progn (add-hook 'abbrev-expand-functions 'xah-elisp-expand-abbrev-maybe nil t)))

  (setq prettify-symbols-alist '(("lambda" . 955)))

  :group 'xah-elisp-mode
  )

(add-to-list 'auto-mode-alist '("\\.el\\'" . xah-elisp-mode))

(provide 'xah-elisp-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-elisp-mode.el ends here
