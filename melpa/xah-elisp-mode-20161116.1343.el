;;; xah-elisp-mode.el --- Major mode for editing emacs lisp.

;; Copyright © 2013-2015, by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 2.11.6
;; Package-Version: 20161116.1343
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
"prog1"
"prog2"
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

"string-collate-equalp"
"string-prefix-p"
"string-suffix-p"
"string<"
"string-lessp"
"string-greaterp"
"string-collate-lessp"
"string-prefix-p"
"string-suffix-p"
"compare-strings"
"assoc-string"
))

(defvar xah-elisp-emacs-words nil "List of elisp keywords that's not core lisp language, such as buffer, marker, hook, editing, copy paste, ….")
(setq xah-elisp-emacs-words '(

"make-syntax-table"
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
"split-string-default-separators"
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

(defun xah-elisp-display-page-break-as-line ()
  "Display the formfeed ^L char as line.
Version 2016-10-11"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (null buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))



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
      (let ( (-abbrev-expanded-p (xah-elisp-expand-abbrev)))))))

(defun xah-elisp-completion-function ()
  "This is the function to be used for the hook `completion-at-point-functions'."
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

(defun xah-elisp-abbrev-enable-function ()
  "Return t if not in string or comment. Else nil.
This is for abbrev table property `:enable-function'.
Version 2016-10-24"
  (let ((-syntax-state (syntax-ppss)))
    (not (or (nth 3 -syntax-state) (nth 4 -syntax-state)))))

(defun xah-elisp-expand-abbrev ()
  "Expand the symbol before cursor,
if cursor is not in string or comment.
Returns the abbrev symbol if there's a expansion, else nil.
Version 2016-10-24"
  (interactive)
  (when (xah-elisp-abbrev-enable-function) ; abbrev property :enable-function doesn't seem to work, so check here instead
    (let (
          -p1 -p2
          -abrStr
          -abrSymbol
          )
      (save-excursion
        (forward-symbol -1)
        (setq -p1 (point))
        (forward-symbol 1)
        (setq -p2 (point)))
      (setq -abrStr (buffer-substring-no-properties -p1 -p2))
      (setq -abrSymbol (abbrev-symbol -abrStr))
      (if -abrSymbol
          (progn
            (abbrev-insert -abrSymbol -abrStr -p1 -p2 )
            (xah-elisp--abbrev-position-cursor -p1)
            -abrSymbol)
        nil))))

(defun xah-elisp--abbrev-position-cursor (&optional *pos)
  "Move cursor back to ▮ if exist, else put at end.
Return true if found, else false.
Version 2016-10-24"
  (interactive)
  (let ((-found-p (search-backward "▮" (if *pos *pos (max (point-min) (- (point) 100))) t )))
    (when -found-p (delete-char 1))
    -found-p
    ))

(defun xah-elisp--ahf ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
 the “ahf” stand for abbrev hook function.
Version 2016-10-24"
  t)

(put 'xah-elisp--ahf 'no-self-insert t)


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
Root sexp group is the outmost sexp unit.

Version 2016-10-13"
  (interactive)
  (save-excursion
    (let (-p1 -p2)
      (xah-elisp-goto-outmost-bracket)
      (setq -p1 (point))
      (setq -p2 (scan-sexps (point) 1))
      (save-excursion
        (save-restriction
          (narrow-to-region -p1 -p2)
          (progn
            (goto-char (point-min))
            (indent-sexp)
            (xah-elisp-compact-parens-region (point-min) (point-max))
            (xah-elisp-compact-blank-lines (point-min) (point-max))
            (delete-trailing-whitespace (point-min) (point-max))))))))

(defun xah-elisp-compact-blank-lines (&optional *begin *end *n)
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'.

*N is the number of newline chars to use in replacement.
If 0, it means lines will be joined.
By befault, *N is 2. It means, 1 visible blank line.

Version 2016-10-13"
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (when (null *begin)
    (setq *begin (point-min) *end (point-max)))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (progn
        (goto-char (point-min))
        (while (search-forward-regexp "\n\n\n+" nil "noerror")
          (replace-match (make-string (if (null *n) 2 *n ) 10)))))))

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

;; wanted to start clean, for development. eventually, prob not
(setq xah-elisp-mode-abbrev-table nil)

(define-abbrev-table 'xah-elisp-mode-abbrev-table
  '(

    ("d" "(defun f▮ ()\n  \"DOCSTRING\"\n  (interactive)\n  (let (VAR)\n\n  ))" xah-elisp--ahf)
    ("i" "(insert ▮)" xah-elisp--ahf)
    ("l" "(let (x▮)\n x\n)" xah-elisp--ahf)
    ("m" "(message \"%s▮\" ARGS)" xah-elisp--ahf)
    ("p" "(point)" xah-elisp--ahf)
    ("s" "(setq ▮ VAL)" xah-elisp--ahf)
    ("o" "&optional " xah-elisp--ahf)
    ("w" "(when ▮)" xah-elisp--ahf)
    ("ah" "add-hook" xah-elisp--ahf)
    ("bc" "backward-char" xah-elisp--ahf)
    ("bs" "buffer-substring" xah-elisp--ahf)
    ("bw" "backward-word" xah-elisp--ahf)
    ("ca" "char-after" xah-elisp--ahf)
    ("cb" "current-buffer" xah-elisp--ahf)
    ("cc" "condition-case" xah-elisp--ahf)
    ("cd" "copy-directory" xah-elisp--ahf)
    ("cf" "copy-file" xah-elisp--ahf)
    ("cw" "(current-word)" xah-elisp--ahf)
    ("dc" "(delete-char 1)" xah-elisp--ahf)
    ("dd" "delete-directory" xah-elisp--ahf)
    ("df" "delete-file" xah-elisp--ahf)
    ("dk" "define-key" xah-elisp--ahf)
    ("dr" "delete-region" xah-elisp--ahf)
    ("fc" "forward-char" xah-elisp--ahf)
    ("fw" "forward-word" xah-elisp--ahf)
    ("ff" "find-file" xah-elisp--ahf)
    ("fl" "forward-line" xah-elisp--ahf)
    ("gc" "goto-char" xah-elisp--ahf)
    ("kb" "kill-buffer" xah-elisp--ahf)
    ("kr" "kill-region" xah-elisp--ahf)
    ("la" "looking-at" xah-elisp--ahf)
    ("lc" "left-char" xah-elisp--ahf)
    ("mb" "match-beginning" xah-elisp--ahf)
    ("md" "make-directory" xah-elisp--ahf)
    ("me" "match-end" xah-elisp--ahf)
    ("mc" "mapcar" xah-elisp--ahf)
    ("ms" "match-string" xah-elisp--ahf)
    ("pm" "point-min" xah-elisp--ahf)
    ("px" "point-max" xah-elisp--ahf)
    ("rb" "region-beginning" xah-elisp--ahf)
    ("rc" "right-char" xah-elisp--ahf)
    ("re" "region-end" xah-elisp--ahf)
    ("rf" "rename-file" xah-elisp--ahf)
    ("rm" "replace-match" xah-elisp--ahf)
    ("rq" "regexp-quote" xah-elisp--ahf)
    ("rr" "replace-regexp" xah-elisp--ahf)
    ("sb" "search-backward" xah-elisp--ahf)
    ("sc" "shell-command" xah-elisp--ahf)
    ("se" "save-excursion" xah-elisp--ahf)
    ("sf" "search-forward" xah-elisp--ahf)
    ("sm" "string-match" xah-elisp--ahf)
    ("sr" "save-restriction" xah-elisp--ahf)
    ("ss" "split-string" xah-elisp--ahf)
    ("wg" "widget-get" xah-elisp--ahf)

    ("bfn" "buffer-file-name" xah-elisp--ahf)
    ("bmp" "buffer-modified-p" xah-elisp--ahf)
    ("bol" "beginning-of-line" xah-elisp--ahf)
    ("cdr" "cdr" xah-elisp--ahf)
    ("efn" "expand-file-name" xah-elisp--ahf)
    ("eol" "end-of-line" xah-elisp--ahf)
    ("fnd" "file-name-directory" xah-elisp--ahf)
    ("fne" "file-name-extension" xah-elisp--ahf)
    ("fnn" "file-name-nondirectory" xah-elisp--ahf)
    ("frn" "file-relative-name" xah-elisp--ahf)
    ("gnb" "generate-new-buffer" xah-elisp--ahf)
    ("gsk" "global-set-key" xah-elisp--ahf)
    ("ifc" "insert-file-contents" xah-elisp--ahf)
    ("lbp" "(line-beginning-position)" xah-elisp--ahf)
    ("lep" "(line-end-position)" xah-elisp--ahf)
    ("mlv" "make-local-variable" xah-elisp--ahf)
    ("ntr" "narrow-to-region" xah-elisp--ahf)
    ("nts" "number-to-string" xah-elisp--ahf)
    ("pmi" "point-min" xah-elisp--ahf)
    ("rap" "region-active-p" xah-elisp--ahf)
    ("rsb" "re-search-backward" xah-elisp--ahf)
    ("rsf" "re-search-forward" xah-elisp--ahf)
    ("sbr" "search-backward-regexp" xah-elisp--ahf)
    ("scb" "skip-chars-backward" xah-elisp--ahf)
    ("scf" "skip-chars-forward" xah-elisp--ahf)
    ("sfm" "set-file-modes" xah-elisp--ahf)
    ("sfr" "search-forward-regexp" xah-elisp--ahf)
    ("stn" "string-to-number" xah-elisp--ahf)
    ("tap" "thing-at-point" xah-elisp--ahf)
    ("urp" "use-region-p" xah-elisp--ahf)
    ("wcb" "with-current-buffer" xah-elisp--ahf)

    ("bsnp" "(buffer-substring-no-properties START▮ END)" xah-elisp--ahf)
    ("fnse" "file-name-sans-extension" xah-elisp--ahf)
    ("rris" "replace-regexp-in-string" xah-elisp--ahf)
    ("yonp" "yes-or-no-p" xah-elisp--ahf)
    ("botap" "bounds-of-thing-at-point" xah-elisp--ahf)

    ("abbreviate-file-name" "(abbreviate-file-name ▮)" xah-elisp--ahf)
    ("add-hook" "(add-hook 'HOOK▮ 'FUNCTION)" xah-elisp--ahf)
    ("add-text-properties" "(add-text-properties START▮ END PROPS &optional OBJECT)" xah-elisp--ahf)
    ("add-to-list" "(add-to-list LIST-VAR▮ ELEMENT &optional APPEND COMPARE-FN)" xah-elisp--ahf)
    ("alist-get" "(alist-get key▮ value &optional default)" xah-elisp--ahf)
    ("and" "(and ▮)" nil :system t )
    ("append" "(append ▮)" xah-elisp--ahf)
    ("apply" "(apply ▮)" xah-elisp--ahf)
    ("aref" "(aref ARRAY▮ INDEX)" xah-elisp--ahf)
    ("aset" "(aset ARRAY▮ IDX NEWELT)" xah-elisp--ahf)
    ("ask-user-about-supersession-threat" "(ask-user-about-supersession-threat FILENAME▮)" xah-elisp--ahf)
    ("assoc" "(assoc KEY▮ LIST)" xah-elisp--ahf)
    ("assoc-default" "(assoc-default key▮ alist &optional test default)" xah-elisp--ahf)
    ("assq" "(assq key▮ alist)" xah-elisp--ahf)
    ("assq-delete-all" "(assq-delete-all key▮ alist)" xah-elisp--ahf)
    ("autoload" "(autoload 'FUNCNAME▮ \"FILENAME\" &optional \"DOCSTRING\" INTERACTIVE TYPE)" xah-elisp--ahf)
    ("backward-char" "(backward-char ▮)" xah-elisp--ahf)
    ("backward-word" "(backward-word ▮)" xah-elisp--ahf)
    ("barf-if-buffer-read-only" "(barf-if-buffer-read-only)" xah-elisp--ahf)
    ("beginning-of-line" "(beginning-of-line)" xah-elisp--ahf)
    ("boundp" "(boundp '▮)" xah-elisp--ahf)
    ("bounds-of-thing-at-point" "(bounds-of-thing-at-point 'symbol▮ 'filename 'word 'whitespace 'line)" :system t)
    ("buffer-base-buffer" "(buffer-base-buffer &optional BUFFER▮)" xah-elisp--ahf)
    ("buffer-chars-modified-tick" "(buffer-chars-modified-tick &optional BUFFER▮)" xah-elisp--ahf)
    ("buffer-file-name" "(buffer-file-name)" xah-elisp--ahf)
    ("buffer-list" "(buffer-list &optional FRAME▮)" xah-elisp--ahf)
    ("buffer-live-p" "(buffer-live-p OBJECT▮)" xah-elisp--ahf)
    ("buffer-modified-p" "(buffer-modified-p BUFFER▮)" xah-elisp--ahf)
    ("buffer-modified-p" "(buffer-modified-p ▮)" xah-elisp--ahf)
    ("buffer-modified-tick" "(buffer-modified-tick &optional BUFFER▮)" xah-elisp--ahf)
    ("buffer-name" "(buffer-name BUFFER▮)" xah-elisp--ahf)
    ("buffer-substring" "(buffer-substring START▮ END)" xah-elisp--ahf)
    ("buffer-substring-no-properties" "(buffer-substring-no-properties START▮ END)" xah-elisp--ahf)
    ("buffer-swap-text" "(buffer-swap-text BUFFER▮)" xah-elisp--ahf)
    ("bufferp" "(bufferp ▮)" xah-elisp--ahf)
    ("bury-buffer" "(bury-buffer &optional BUFFER-OR-NAME▮)" xah-elisp--ahf)
    ("call-interactively" "(call-interactively 'FUNCTION▮ &optional RECORD-FLAG KEYS)" xah-elisp--ahf)
    ("called-interactively-p" "(called-interactively-p 'interactive▮)" xah-elisp--ahf)
    ("car" "(car ▮)" xah-elisp--ahf)
    ("catch" "(catch TAG▮ BODY)" xah-elisp--ahf)
    ("cdr" "(cdr ▮)" xah-elisp--ahf)
    ("char-to-string" "(char-to-string CHAR▮) " xah-elisp--ahf)
    ("clear-image-cache" "(clear-image-cache &optional FILTER▮)" xah-elisp--ahf)
    ("clear-visited-file-modtime" "(clear-visited-file-modtime)" xah-elisp--ahf)
    ("clone-indirect-buffer" "(clone-indirect-buffer NEWNAME▮ DISPLAY-FLAG &optional NORECORD)" xah-elisp--ahf)
    ("concat" "(concat \"▮\" \"▮\")" xah-elisp--ahf)
    ("cond" "(cond\n(CONDITION▮ BODY)\n(CONDITION BODY)\n)" xah-elisp--ahf)
    ("condition-case" "(condition-case ▮)" xah-elisp--ahf)
    ("cons" "(cons CAR▮ CDR)" xah-elisp--ahf)
    ("consp" "(consp ▮)" xah-elisp--ahf)
    ("constrain-to-field" "(constrain-to-field NEW-POS▮ OLD-POS &optional ESCAPE-FROM-EDGE ONLY-IN-LINE INHIBIT-CAPTURE-PROPERTY)" xah-elisp--ahf)
    ("copy-alist" "(copy-alist alist▮)" xah-elisp--ahf)
    ("copy-directory" "(copy-directory ▮ NEWNAME &optional KEEP-TIME PARENTS)" xah-elisp--ahf)
    ("copy-file" "(copy-file FILE▮ NEWNAME &optional OK-IF-ALREADY-EXISTS KEEP-TIME PRESERVE-UID-GID)" xah-elisp--ahf)
    ("create-image" "(create-image FILE-OR-DATA▮ &optional TYPE DATA-P &rest)" xah-elisp--ahf)
    ("current-word" "(current-word)" xah-elisp--ahf)
    ("current-buffer" "(current-buffer)" xah-elisp--ahf)
    ("custom-autoload" "(custom-autoload ▮ SYMBOL LOAD &optional NOSET)" xah-elisp--ahf)
    ("defalias" "(defalias 'SYMBOL▮ 'DEFINITION &optional DOCSTRING)" xah-elisp--ahf)
    ("defconst" "(defconst ▮ INITVALUE \"DOCSTRING\")" xah-elisp--ahf)
    ("defcustom" "(defcustom ▮ VALUE \"DOC\" &optional ARGS)" xah-elisp--ahf)
    ("defface" "(defface FACE▮ SPEC \"DOC\" &rest ARGS)" xah-elisp--ahf)
    ("defimage" "(defimage SYMBOL▮ SPECS &optional DOC)" xah-elisp--ahf)
    ("define-key" "(define-key KEYMAPNAME▮ (kbd \"M-b\") 'FUNCNAME)" xah-elisp--ahf)
    ("define-minor-mode" "(define-minor-mode MODE▮ \"DOC\" &optional INIT-VALUE LIGHTER KEYMAP &rest BODY)" xah-elisp--ahf)
    ("defsubst" "(defsubst ▮)" xah-elisp--ahf)
    ("defun" "(defun ▮ ()\n  \"DOCSTRING\"\n  (interactive)\n  (let (VAR)\n\n  ))" xah-elisp--ahf)
    ("defvar" "(defvar ▮ &optional INITVALUE \"DOCSTRING\")" xah-elisp--ahf)
    ("delete" "(delete OBJECT▮ SEQUENCE)" xah-elisp--ahf)
    ("delete-char" "(delete-char ▮)" xah-elisp--ahf)
    ("delete-directory" "(delete-directory ▮ &optional RECURSIVE)" xah-elisp--ahf)
    ("delete-dups" "(delete-dups LIST▮)" xah-elisp--ahf)
    ("delete-field" "(delete-field &optional POS▮)" xah-elisp--ahf)
    ("delete-file" "(delete-file ▮)" xah-elisp--ahf)
    ("delete-region" "(delete-region pos1▮ pos2)" xah-elisp--ahf)
    ("delq" "(delq ELT▮ LIST)" xah-elisp--ahf)
    ("directory-file-name" "(directory-file-name ▮)" xah-elisp--ahf)
    ("directory-files" "(directory-files ▮ &optional FULL MATCH NOSORT)" xah-elisp--ahf)
    ("directory-name-p" "(directory-name-p ▮)" xah-elisp--ahf)
    ("dolist" "(dolist (VAR▮ LIST [RESULT]) BODY)" xah-elisp--ahf)
    ("dotimes" "(dotimes (VAR▮ COUNT [RESULT]) BODY)" xah-elisp--ahf)
    ("elt" "(elt SEQUENCE▮ N)" xah-elisp--ahf)
    ("end-of-line" "(end-of-line ▮&optional N)" xah-elisp--ahf)
    ("eq" "(eq ▮)" xah-elisp--ahf)
    ("equal" "(equal ▮)" xah-elisp--ahf)
    ("error" "(error \"%s\" ▮)" xah-elisp--ahf)
    ("expand-file-name" "(expand-file-name ▮ &optional relativedir)" xah-elisp--ahf)
    ("fboundp" "(fboundp '▮)" xah-elisp--ahf)
    ("featurep" "(featurep 'FEATURE▮)" xah-elisp--ahf)
    ("field-beginning" "(field-beginning &optional POS▮ ESCAPE-FROM-EDGE LIMIT)" xah-elisp--ahf)
    ("field-end" "(field-end &optional POS▮ ESCAPE-FROM-EDGE LIMIT)" xah-elisp--ahf)
    ("field-string" "(field-string &optional POS▮)" xah-elisp--ahf)
    ("field-string-no-properties" "(field-string-no-properties &optional POS▮)" xah-elisp--ahf)
    ("file-directory-p" "(file-directory-p ▮)" xah-elisp--ahf)
    ("file-exists-p" "(file-exists-p ▮)" xah-elisp--ahf)
    ("file-name-absolute-p" "(file-name-absolute-p ▮)" xah-elisp--ahf)
    ("file-name-as-directory" "(file-name-as-directory ▮)" xah-elisp--ahf)
    ("file-name-directory" "(file-name-directory ▮)" xah-elisp--ahf)
    ("file-name-extension" "(file-name-extension ▮ &optional PERIOD)" xah-elisp--ahf)
    ("file-name-nondirectory" "(file-name-nondirectory ▮)" xah-elisp--ahf)
    ("file-name-sans-extension" "(file-name-sans-extension ▮)" xah-elisp--ahf)
    ("file-regular-p" "(file-regular-p ▮)" xah-elisp--ahf)
    ("file-relative-name" "(file-relative-name ▮)" xah-elisp--ahf)
    ("find-buffer-visiting" "(find-buffer-visiting FILENAME▮ &optional PREDICATE)" xah-elisp--ahf)
    ("find-file" "(find-file ▮)" xah-elisp--ahf)
    ("find-image" "(find-image SPECS▮)" xah-elisp--ahf)
    ("font-lock-add-keywords" "(font-lock-add-keywords MODE▮ KEYWORDS &optional HOW)" xah-elisp--ahf)
    ("font-lock-fontify-buffer" "(font-lock-fontify-buffer ▮)" xah-elisp--ahf)
    ("format" "(format \"%s\" ▮)" xah-elisp--ahf)
    ("format" "(format \"▮\" &optional OBJECTS)" xah-elisp--ahf)
    ("forward-char" "(forward-char ▮)" xah-elisp--ahf)
    ("forward-line" "(forward-line ▮)" xah-elisp--ahf)
    ("forward-word" "(forward-word ▮)" xah-elisp--ahf)
    ("funcall" "(funcall 'FUNCTION▮ &rest ARGUMENTS)" xah-elisp--ahf)
    ("function" "(function ▮)" xah-elisp--ahf)
    ("gap-position" "(gap-position)" xah-elisp--ahf)
    ("gap-size" "(gap-size)" xah-elisp--ahf)
    ("generate-new-buffer" "(generate-new-buffer NAME▮)" xah-elisp--ahf)
    ("generate-new-buffer" "(generate-new-buffer ▮)" xah-elisp--ahf)
    ("generate-new-buffer-name" "(generate-new-buffer-name STARTING-NAME▮ &optional IGNORE)" xah-elisp--ahf)
    ("get" "(get SYMBOL▮ PROPNAME)" xah-elisp--ahf)
    ("get-buffer" "(get-buffer BUFFER-OR-NAME▮)" xah-elisp--ahf)
    ("get-buffer-create" "(get-buffer-create BUFFER-OR-NAME▮)" xah-elisp--ahf)
    ("get-char-code-property" "(get-char-code-property CHAR▮ PROPNAME)" xah-elisp--ahf)
    ("get-char-property" "(get-char-property POSITION▮ PROP &optional OBJECT)" xah-elisp--ahf)
    ("get-char-property-and-overlay" "(get-char-property-and-overlay POSITION▮ PROP &optional)" xah-elisp--ahf)
    ("get-file-buffer" "(get-file-buffer FILENAME▮)" xah-elisp--ahf)
    ("get-pos-property" "(get-pos-property POSITION▮ PROP &optional OBJECT)" xah-elisp--ahf)
    ("get-text-property" "(get-text-property POS▮ PROP &optional OBJECT)" xah-elisp--ahf)
    ("global-set-key" "(global-set-key (kbd \"C-▮\") 'COMMAND)" xah-elisp--ahf)
    ("goto-char" "(goto-char ▮)" xah-elisp--ahf)
    ("if" "(if ▮\n    (progn )\n  (progn )\n)" xah-elisp--ahf)
    ("image-flush" "(image-flush SPEC▮ &optional FRAME)" xah-elisp--ahf)
    ("image-load-path-for-library" "(image-load-path-for-library LIBRARY▮ IMAGE &optional PATH)" xah-elisp--ahf)
    ("image-size" "(image-size SPEC▮ &optional PIXELS FRAME)" xah-elisp--ahf)
    ("insert" "(insert ▮)" xah-elisp--ahf)
    ("insert-and-inherit" "(insert-and-inherit ▮)" xah-elisp--ahf)
    ("insert-before-markers-and-inherit" "(insert-before-markers-and-inherit ▮)" xah-elisp--ahf)
    ("insert-char" "(insert-char CHARACTER▮ &optional COUNT INHERIT)" xah-elisp--ahf)
    ("insert-file-contents" "(insert-file-contents ▮ &optional VISIT BEG END REPLACE)" xah-elisp--ahf)
    ("insert-image" "(insert-image IMAGE▮ &optional STRING AREA SLICE)" xah-elisp--ahf)
    ("insert-sliced-image" "(insert-sliced-image IMAGE▮ &optional STRING AREA ROWS COLS)" xah-elisp--ahf)
    ("interactive" "(interactive)" xah-elisp--ahf)
    ("kbd" "(kbd \"▮\")" xah-elisp--ahf)
    ("kill-append" "(kill-append STRING▮ BEFORE-P)" xah-elisp--ahf)
    ("kill-buffer" "(kill-buffer &optional BUFFER-OR-NAME▮)" xah-elisp--ahf)
    ("kill-buffer" "(kill-buffer ▮)" xah-elisp--ahf)
    ("kill-region" "(kill-region BEG▮ END &optional REGION)" xah-elisp--ahf)
    ("lambda" "(lambda (x▮) (interactive) BODY)" xah-elisp--ahf)
    ("last-buffer" "(last-buffer &optional BUFFER▮ VISIBLE-OK FRAME)" xah-elisp--ahf)
    ("left-char" "(left-char ▮)" xah-elisp--ahf)
    ("length" "(length ▮)" xah-elisp--ahf)
    ("let" "(let* (▮)\n x\n)" 'xah-elisp--ahf)
    ("line-beginning-position" "(line-beginning-position)" xah-elisp--ahf)
    ("line-end-position" "(line-end-position)" xah-elisp--ahf)
    ("list" "(list ▮)" xah-elisp--ahf)
    ("load" "(load FILE▮ &optional NOERROR NOMESSAGE NOSUFFIX MUST-SUFFIX)" xah-elisp--ahf)
    ("load-file" "(load-file FILE▮)" xah-elisp--ahf)
    ("looking-at" "(looking-at \"REGEXP▮\")" xah-elisp--ahf)
    ("looking-back" "(looking-back \"REGEXP▮\" LIMIT &optional GREEDY)" xah-elisp--ahf)
    ("make-directory" "(make-directory ▮ &optional PARENTS)" xah-elisp--ahf)
    ("make-indirect-buffer" "(make-indirect-buffer BASE-BUFFER▮ NAME &optional CLONE)" xah-elisp--ahf)
    ("make-list" "(make-list LENGTH▮ INIT)" xah-elisp--ahf)
    ("make-local-variable" "(make-local-variable ▮)" xah-elisp--ahf)
    ("mapc" "(mapc '▮ SEQUENCE)" xah-elisp--ahf)
    ("mapcar" "(mapcar '▮ SEQUENCE)" xah-elisp--ahf)
    ("mapconcat" "(mapconcat FUNCTION▮ SEQUENCE SEPARATOR)" xah-elisp--ahf)
    ("match-beginning" "(match-beginning N▮)" xah-elisp--ahf)
    ("match-data" "(match-data &optional INTEGERS▮ REUSE RESEAT)" xah-elisp--ahf)
    ("match-end" "(match-end N▮)" xah-elisp--ahf)
    ("match-string" "(match-string NUM▮ &optional STRING)" xah-elisp--ahf)
    ("member" "(member ELT▮ LIST)" xah-elisp--ahf)
    ("member" "(member OBJECT▮ LIST)" xah-elisp--ahf)
    ("member-ignore-case" "(member-ignore-case OBJECT▮ LIST)" xah-elisp--ahf)
    ("memq" "(memq ELT▮ LIST)" xah-elisp--ahf)
    ("memql" "(memql OBJECT▮ LIST)" xah-elisp--ahf)
    ("message" "(message \"%s▮\" ARGS)" xah-elisp--ahf)
    ("narrow-to-region" "(narrow-to-region START▮ END)" xah-elisp--ahf)
    ("next-char-property-change" "(next-char-property-change POS &optional LIMIT)" xah-elisp--ahf)
    ("next-property-change" "(next-property-change POS &optional OBJECT LIMIT)" xah-elisp--ahf)
    ("next-single-char-property-change" "(next-single-char-property-change POS PROP &optional OBJECT LIMIT)" xah-elisp--ahf)
    ("next-single-property-change" "(next-single-property-change POS PROP &optional OBJECT LIMIT)" xah-elisp--ahf)
    ("not" "(not ▮)" xah-elisp--ahf)
    ("not-modified" "(not-modified &optional ARG▮)" xah-elisp--ahf)
    ("nth" "(nth N▮ LIST)" xah-elisp--ahf)
    ("null" "(null ▮)" xah-elisp--ahf)
    ("number-sequence" "(number-sequence FROM▮ &optional TO INC)" xah-elisp--ahf)
    ("number-to-string" "(number-to-string ▮)" xah-elisp--ahf)
    ("or" "(or ▮)" xah-elisp--ahf)
    ("other-buffer" "(other-buffer &optional BUFFER▮ VISIBLE-OK FRAME)" xah-elisp--ahf)
    ("point" "(point)" xah-elisp--ahf)
    ("point-max" "(point-max)" xah-elisp--ahf)
    ("point-min" "(point-min)" xah-elisp--ahf)
    ("previous-char-property-change" "(previous-char-property-change POS &optional LIMIT)" xah-elisp--ahf)
    ("previous-property-change" "(previous-property-change POS &optional OBJECT LIMIT)" xah-elisp--ahf)
    ("previous-single-char-property-change" "(previous-single-char-property-change POS PROP &optional OBJECT LIMIT)" xah-elisp--ahf)
    ("previous-single-property-change" "(previous-single-property-change POS PROP &optional OBJECT LIMIT)" xah-elisp--ahf)
    ("prin1" "(prin1 ▮)" xah-elisp--ahf)
    ("prin1-to-string" "(prin1-to-string▮ OBJECT &optional NOESCAPE)" xah-elisp--ahf)
    ("princ" "(princ ▮)" xah-elisp--ahf)
    ("print" "(print ▮)" xah-elisp--ahf)
    ("progn" "(progn\n▮)" xah-elisp--ahf)
    ("prog1" "(prog1\n▮)" xah-elisp--ahf)
    ("prog2" "(prog2\n▮)" xah-elisp--ahf)
    ("pop" "(pop ▮)" xah-elisp--ahf)
    ("propertize" "(propertize STRING▮ &rest PROPERTIES)" xah-elisp--ahf)
    ("push" "(push NEWELT▮ PLACE)" xah-elisp--ahf)
    ("push-mark" "(push-mark ▮&optional LOCATION NOMSG ACTIVATE)" xah-elisp--ahf)
    ("put" "(put 'SYMBOL▮ PROPNAME VALUE)" xah-elisp--ahf)
    ("put-image" "(put-image IMAGE▮ POS &optional STRING AREA)" xah-elisp--ahf)
    ("put-text-property" "(put-text-property START▮ END PROP VALUE &optional OBJECT)" xah-elisp--ahf)
    ("random" "(random ▮)" xah-elisp--ahf)
    ("rassoc" "(rassoc KEY▮ LIST)" xah-elisp--ahf)
    ("rassoc" "(rassoc value▮ alist)" xah-elisp--ahf)
    ("rassq" "(rassq value▮ alist)" xah-elisp--ahf)
    ("rassq-delete-all" "(rassq-delete-all value▮ alist)" xah-elisp--ahf)
    ("re-search-backward" "(re-search-backward \"REGEXP▮\" &optional BOUND 'NOERROR COUNT)" xah-elisp--ahf)
    ("re-search-forward" "(re-search-forward \"REGEXP▮\" &optional BOUND 'NOERROR COUNT)" xah-elisp--ahf)
    ("read-directory-name" "(read-directory-name \"▮\" &optional DIR DEFAULT-DIRNAME MUSTMATCH INITIAL)" xah-elisp--ahf)
    ("read-file-name" "(read-file-name \"▮\" &optional DIR DEFAULT-FILENAME MUSTMATCH INITIAL PREDICATE)" xah-elisp--ahf)
    ("read-regexp" "(read-regexp \"▮\" &optional DEFAULT-VALUE)" xah-elisp--ahf)
    ("read-string" "(read-string \"▮\" &optional INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD)" xah-elisp--ahf)
    ("regexp-opt" "(regexp-opt STRINGS▮ &optional PAREN)" xah-elisp--ahf)
    ("regexp-quote" "(regexp-quote ▮)" xah-elisp--ahf)
    ("region-active-p" "(region-active-p)" xah-elisp--ahf)
    ("region-beginning" "(region-beginning)" xah-elisp--ahf)
    ("region-end" "(region-end)" xah-elisp--ahf)
    ("remove" "(remove OBJECT▮ SEQUENCE)" xah-elisp--ahf)
    ("remove-images" "(remove-images START▮ END &optional BUFFER)" xah-elisp--ahf)
    ("remove-list-of-text-properties" "(remove-list-of-text-properties START▮ END LIST OF PROPERTIES &optional OBJECT)" xah-elisp--ahf)
    ("remove-text-properties" "(remove-text-properties START▮ END PROPS &optional OBJECT)" xah-elisp--ahf)
    ("remq" "(remq OBJECT▮ LIST)" xah-elisp--ahf)
    ("rename-buffer" "(rename-buffer NEWNAME▮ &optional UNIQUE)" xah-elisp--ahf)
    ("rename-file" "(rename-file FILE▮ NEWNAME &optional OK-IF-ALREADY-EXISTS)" xah-elisp--ahf)
    ("repeat" "(repeat ▮)" xah-elisp--ahf)
    ("replace-match" "(replace-match NEWTEXT▮ &optional FIXEDCASE LITERAL \"STRING\" SUBEXP)" xah-elisp--ahf)
    ("replace-regexp" "(replace-regexp \"REGEXP▮\" TO-STRING &optional DELIMITED START END)" xah-elisp--ahf)
    ("replace-regexp-in-string" "(replace-regexp-in-string \"REGEXP▮\" REP \"STRING\" &optional FIXEDCASE LITERAL SUBEXP START)" xah-elisp--ahf)
    ("require" "(require ▮)" xah-elisp--ahf)
    ("restore-buffer-modified-p" "(restore-buffer-modified-p FLAG▮)" xah-elisp--ahf)
    ("reverse" "(reverse ▮)" xah-elisp--ahf)
    ("right-char" "(right-char ▮)" xah-elisp--ahf)
    ("run-with-timer" "(run-with-timer SECS▮ REPEAT FUNCTION &rest ARGS)" xah-elisp--ahf)
    ("save-buffer" "(save-buffer &optional ARG▮)" xah-elisp--ahf)
    ("save-current-buffer" "(save-current-buffer ▮)" xah-elisp--ahf)
    ("save-excursion" "(save-excursion ▮)" xah-elisp--ahf)
    ("save-restriction" "(save-restriction ▮)" xah-elisp--ahf)
    ("search-backward" "(search-backward \"▮\" &optional BOUND 'NOERROR COUNT)" xah-elisp--ahf)
    ("search-backward-regexp" "(search-backward-regexp \"▮\" &optional BOUND  s/['NOERROR COUNT)" xah-elisp--ahf)
    ("search-forward" "(search-forward \"▮\" &optional BOUND 'NOERROR COUNT)" xah-elisp--ahf)
    ("search-forward-regexp" "(search-forward-regexp \"▮\" &optional BOUND 'NOERROR COUNT)" xah-elisp--ahf)
    ("set-buffer" "(set-buffer ▮)" xah-elisp--ahf)
    ("set-buffer-modified-p" "(set-buffer-modified-p FLAG▮)" xah-elisp--ahf)
    ("set-file-modes" "(set-file-modes ▮ MODE)" xah-elisp--ahf)
    ("set-mark" "(set-mark ▮)" xah-elisp--ahf)
    ("set-syntax-table" "(set-syntax-table ▮)" xah-elisp--ahf)
    ("set-text-properties" "(set-text-properties START▮ END PROPS &optional OBJECT)" xah-elisp--ahf)
    ("set-visited-file-modtime" "(set-visited-file-modtime &optional TIME▮)" xah-elisp--ahf)
    ("set-visited-file-name" "(set-visited-file-name FILENAME▮ &optional NO-QUERY ALONG-WITH-FILE)" xah-elisp--ahf)
    ("setq" "(setq ▮)" xah-elisp--ahf)
    ("shell-command" "(shell-command ▮ &optional OUTPUT-BUFFER ERROR-BUFFER)" xah-elisp--ahf)
    ("skip-chars-backward" "(skip-chars-backward \"▮\" &optional LIM)" xah-elisp--ahf)
    ("skip-chars-forward" "(skip-chars-forward \"▮\" &optional LIM)" xah-elisp--ahf)
    ("split-string" "(split-string ▮ &optional SEPARATORS OMIT-NULLS)" xah-elisp--ahf)
    ("string-equal" "(string-equal str1▮ str2)" xah-elisp--ahf)
    ("string-match" "(string-match \"REGEXP▮\" \"STRING\" &optional START)" xah-elisp--ahf)
    ("string-match-p" "(string-match-p \"REGEXP▮\" \"STRING\" &optional START)" xah-elisp--ahf)
    ("string-to-char" "(string-to-char \"▮\")" xah-elisp--ahf)
    ("string-to-number" "(string-to-number \"▮\")" xah-elisp--ahf)
    ("string=" "(string-equal str1▮ str2)" xah-elisp--ahf)
    ("stringp" "(stringp ▮)" xah-elisp--ahf)
    ("substring" "(substring STRING▮ FROM &optional TO)" xah-elisp--ahf)
    ("substring-no-properties" "(substring-no-properties ▮ FROM TO)" xah-elisp--ahf)
    ("terpri" "(terpri ▮)" xah-elisp--ahf)
    ("text-properties-at" "(text-properties-at POSITION▮ &optional OBJECT)" xah-elisp--ahf)
    ("text-property-any" "(text-property-any START END PROP VALUE &optional OBJECT)" xah-elisp--ahf)
    ("text-property-not-all" "(text-property-not-all START END PROP VALUE &optional OBJECT)" xah-elisp--ahf)
    ("thing-at-point" "(thing-at-point 'word▮ 'symbol 'list 'sexp 'defun 'filename 'url 'email 'sentence 'whitespace 'line 'number 'page)" :system t)
    ("throw" "(throw TAG▮ VALUE)" xah-elisp--ahf)
    ("toggle-read-only" "(toggle-read-only &optional ARG▮)" xah-elisp--ahf)
    ("unbury-buffer" "(unbury-buffer)" xah-elisp--ahf)
    ("unless" "(unless ▮)" xah-elisp--ahf)
    ("use-region-p" "(use-region-p)" xah-elisp--ahf)
    ("user-error" "(user-error \"%s▮\" &rest ARGS)" xah-elisp--ahf)
    ("vector" "(vector ▮)" xah-elisp--ahf)
    ("verify-visited-file-modtime" "(verify-visited-file-modtime BUFFER▮)" xah-elisp--ahf)
    ("version<" "(version< \"24.4\" emacs-version)" nil :system t )
    ("version<=" "(version<= \"24.4\" emacs-version)" nil :system t )
    ("visited-file-modtime" "(visited-file-modtime)" xah-elisp--ahf)
    ("when" "(when ▮)" xah-elisp--ahf)
    ("while" "(while (< i▮ 9)\n  (setq i (1+ i)))" xah-elisp--ahf)
    ("widen" "(widen)" xah-elisp--ahf)
    ("widget-get" "(widget-get ▮)" xah-elisp--ahf)
    ("with-current-buffer" "(with-current-buffer BUFFER-OR-NAME▮ BODY)" xah-elisp--ahf)
    ("with-output-to-string" "(with-output-to-string BODY▮)" xah-elisp--ahf)
    ("with-output-to-temp-buffer" "(with-output-to-temp-buffer BUFNAME▮ &rest BODY)" xah-elisp--ahf)
    ("with-temp-buffer" "(with-temp-buffer ▮)" xah-elisp--ahf)
    ("with-temp-file" "(with-temp-file FILE▮)" xah-elisp--ahf)
    ("write-char" "(write-char CHARACTER▮ &optional STREAM)" xah-elisp--ahf)
    ("write-file" "(write-file FILENAME▮ &optional CONFIRM)" xah-elisp--ahf)
    ("write-region" "(write-region (point-min) (point-max) FILENAME &optional APPEND VISIT LOCKNAME MUSTBENEW)" xah-elisp--ahf)
    ("y-or-n-p" "(y-or-n-p \"PROMPT▮ \")" xah-elisp--ahf)
    ("yes-or-no-p" "(yes-or-no-p \"PROMPT▮ \")" xah-elisp--ahf)

    ("make-string" "(make-string count character)" xah-elisp--ahf)
    ("string" "(string &rest characters)" xah-elisp--ahf)

    ("char-equal" "(char-equal char1▮ char1)" xah-elisp--ahf)

    ("string-collate-equalp" "(string-collate-equalp string1▮ string2 &optional locale)" xah-elisp--ahf)
    ("string-prefix-p" "(string-prefix-p prefixstr▮ string2 &optional ignore-case)" xah-elisp--ahf)
    ("string-suffix-p" "(string-suffix-p suffix▮ string &optional ignore-case)" xah-elisp--ahf)
    ("string-lessp" "(string-lessp string1▮ string2)" xah-elisp--ahf)
    ("string-greaterp" "(string-greaterp string1▮ string2)" xah-elisp--ahf)
    ("string-collate-lessp" "(string-collate-lessp string1▮ string2 &optional locale)" xah-elisp--ahf)
    ("string-prefix-p" "(string-prefix-p string1▮ string2 &optional ignore-case)" xah-elisp--ahf)
    ("string-suffix-p" "(string-suffix-p suffix▮ string &optional ignore-case)" xah-elisp--ahf)
    ("compare-strings" "(compare-strings string1▮ start1 end1 string2 start2 end2)" xah-elisp--ahf)
    ("assoc-string" "(assoc-string key▮ alist &optional case-fold)" xah-elisp--ahf)

    ;;
    )

  "Abbrev table for `xah-elisp-mode'"
  )

(abbrev-table-put xah-elisp-mode-abbrev-table :regexp "\\([_-*0-9A-Za-z]+\\)")
(abbrev-table-put xah-elisp-mode-abbrev-table :case-fixed t)
(abbrev-table-put xah-elisp-mode-abbrev-table :system t)
(abbrev-table-put xah-elisp-mode-abbrev-table :enable-function 'xah-elisp-abbrev-enable-function)


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
    (t :foreground "red" :background "pink"))
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


;; syntax table
(defvar xah-elisp-mode-syntax-table nil "Syntax table for `xah-elisp-mode'.")

(setq xah-elisp-mode-syntax-table
      (let ((synTable (make-syntax-table emacs-lisp-mode-syntax-table)))

        (modify-syntax-entry ?\* "w" synTable)
        (modify-syntax-entry ?\- "_" synTable)

        ;; (modify-syntax-entry ?\; "<" synTable)
        ;; (modify-syntax-entry ?\n ">" synTable)
        ;; (modify-syntax-entry ?` "'   " synTable)
        ;; (modify-syntax-entry ?' "'   " synTable)
        ;; (modify-syntax-entry ?, "'   " synTable)
        ;; (modify-syntax-entry ?@ "'   " synTable)

        synTable))


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
    )

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
  ;; (set-syntax-table emacs-lisp-mode-syntax-table)
  (setq font-lock-defaults '((xah-elisp-font-lock-keywords)))

  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-add 1) ;default to `;;' in comment-region
  (setq-local comment-column 2)

  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local tab-always-indent 'complete)

  ;; (add-function :before-until (local 'eldoc-documentation-function)
  ;;               #'elisp-eldoc-documentation-function)

  ;; when calling emacs's complete-symbol, follow convention. When pressing TAB, do xah way.
  (add-hook 'completion-at-point-functions 'elisp-completion-at-point nil 'local)

  (make-local-variable 'abbrev-expand-function)
  (if (or
       (and (>= emacs-major-version 24)
            (>= emacs-minor-version 4))
       (>= emacs-major-version 25))
      (progn
        (setq abbrev-expand-function 'xah-elisp-expand-abbrev))
    (progn (add-hook 'abbrev-expand-functions 'xah-elisp-expand-abbrev nil t)))

  (abbrev-mode 1)

  (xah-elisp-display-page-break-as-line)
  (setq prettify-symbols-alist '(("lambda" . 955)))

  (make-local-variable 'ido-separator)
  (setq ido-separator "\n")

  :group 'xah-elisp-mode
  )

(add-to-list 'auto-mode-alist '("\\.el\\'" . xah-elisp-mode))

(provide 'xah-elisp-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; xah-elisp-mode.el ends here
