;;; aumix-mode.el --- run the aumix program in a buffer

;; Copyright (C) 2006,2009-2011,2013-2014  Free Software Foundation, Inc.

;; Author: Kevin Ryde <user42_kevin@yahoo.com.au>
;; Version: 7
;; Keywords: multimedia, mixer, aumix
;; URL: http://user42.tuxfamily.org/aumix-mode/index.html
;; EmacsWiki: Aumix
;;
;; aumix-mode.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; aumix-mode.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses/>.


;;; Commentary:

;; "M-x aumix" runs the aumix sound volume adjuster program in a buffer.
;; See the docstrings of `aumix' and `aumix-mode' below.

;;; Emacsen:

;; Designed for Emacs 20 up.  A bit doubtful with XEmacs 21 term.el (see
;; `aumix-mode' docstring below).

;;; Install:

;; To make M-x aumix available put aumix-mode.el in one of your `load-path'
;; directories and the following in your .emacs
;;
;;     (autoload 'aumix "aumix-mode" nil t)
;;
;; If you use desktop.el then make an autoload for the aumix-mode function
;; too so `desktop-read' can load it when restoring an aumix-mode buffer.
;;
;;     (autoload 'aumix-mode "aumix-mode" nil t)
;;
;; There's autoload cookies for these functions below if you install via
;; `M-x package-install' or know how to use `update-file-autoloads'.

;;; History:

;; Version 1 - the first version
;; Version 2 - new program and switches variables
;;           - correction to `provide'
;; Version 3 - flag aumix-mode-switches as risky-local-variable
;; Version 4 - correction to `process-kill-without-query' for emacs21
;;           - cl for `dotimes' in emacs20
;; Version 5 - ensure curses interface if aumix built with gtk interface
;;           - add desktop.el save/restore
;; Version 6 - autoload aumix-mode for desktop.el
;; Version 7 - don't autoload the variables

;;; Code:

(require 'term)
(eval-when-compile
  (unless (fboundp 'dotimes)
    (require 'cl))) ;; for emacs20 dotimes


;;;###autoload
(defgroup aumix-mode nil "Aumix Mode"
 :prefix "aumix-mode-"
 :group  'multimedia
 :link   '(url-link :tag "aumix-mode.el home page"
                    "http://user42.tuxfamily.org/aumix-mode/index.html")
 :link   '(url-link :tag "Aumix home page"
                    "http://jpj.net/~trevor/aumix.html"))

(defcustom aumix-mode-program "aumix"
  "Executable program name for aumix."
  :type  'string
  :group 'aumix-mode)
;; automatically `risky-local-variable-p' due to name `-program'

(defcustom aumix-mode-switches nil
  "List of command line switches to pass to aumix.
Only a few aumix options do anything for interactive use.
\"-d\" can set a mixer device file (default /dev/mixer), for
example

    (setq aumix-mode-switches '(\"-d\" \"/dev/mixer2\"))"

  :type  '(repeat string)
  :group 'aumix-mode)
;;
;; Not sure if switches can do very much damage.  Make risky just in case
;; "-d filename" causes ioctls to some device other than a mixer.
;;;###autoload
(put 'aumix-mode-switches 'risky-local-variable t)

(defcustom aumix-mode-hook nil
  "Hook called by `aumix-mode'.
This hook, and the parent `term-mode-hook', are run when the
buffer setups are made but before the \"aumix\" program is
started.  This is convenient for applying `font-lock-mode' or
similar before the program output starts.

See also `term-exec-hook' which runs after \"aumix\" is started."

  :type    'hook
  :group   'aumix-mode
  :options '(turn-on-filladapt-mode))

;;-----------------------------------------------------------------------------

(defvar aumix-mode-map (make-sparse-keymap)
  "Keymap for `aumix-mode'.
This inherits from parent keymap `term-raw-map'.  That parent
keymap is only set when `aumix-mode' is first called, since in
Emacs 21 `term-raw-map' is only created then.

Explicit nil settings in `aumix-mode-map' let control characters
have their usual `global-map' meanings instead of `term-send-raw'
in `term-raw-map'.  This is good for keys aumix doesn't use, in
particular C-x and C-h have their usual prefix meanings.

C-c is left for the various prefixed bindings of `term-raw-map'.
C-l and Tab are `term-send-raw' since they're used by the aumix
program.")
;;
(dotimes (i 31)
  (let ((key (1+ i))) ;; 1 to 31
    (unless (member key '(?\C-c ?\C-l ?\t))
      (define-key aumix-mode-map (vector key) nil))))
(define-key aumix-mode-map "Q"    'aumix-mode-quit)
(define-key aumix-mode-map "q"    'aumix-mode-quit)
(define-key aumix-mode-map "\C-l" 'term-send-raw)

;;-----------------------------------------------------------------------------

(defun aumix-mode-quit ()
  "Quit from aumix, and kill the buffer.
This is a direct kill of the subprocess (rather than sending
\"q\" to aumix and waiting for it to exit itself).

For reference, if you use $LANG or $LANGUAGE set to something
other than English then the quit key in aumix might be
translated.  For example \"b\" (Beenden) in German.  In that case
the aumix program exits but the buffer remains.  The buffer is
left on aumix exit so that if it dies then an error message is
seen.  Re-bind the keys in `aumix-mode-map' if something other
than \"q\" for quit is desired."

  (interactive)
  (kill-buffer nil))

(defun aumix-mode-kill-process ()
  "An internal part of aumix-mode.el.
This function is designed to be called from `kill-buffer-hook'.
Kill the aumix sub-process when killing the buffer."
  (if (get-buffer-process (current-buffer))
      (delete-process nil)))

;; this is autoloaded for the benefit of desktop.el
;;;###autoload
(defun aumix-mode ()
  "Major mode for running the aumix program.
Key bindings are per the aumix program.  Press `k' for a summary,
or see KEYS in \"man aumix\".  `\\[aumix-mode-quit]' quits and kills the buffer.

The various terminal setups of `term-mode' are unchanged.  Note
in Emacs 24 the ANSI colours use `font-lock-mode' so be sure
that's turned on (which it is by default).  In Emacs 21 the
opposite was true; if you turn on font lock then you lose the
colours.

In XEmacs 21.4, term.el doesn't seem to work very well with
ncurses programs.  A message \"Error opening terminal: eterm\" is
from aumix (the ncurses library) complaining the terminfo is not
found.  Adding TERMINFO=/usr/share/xemacs21/xemacs-packages/etc/
or wherever the /e/eterm file lives might help.  If something
doesn't work then C-c C-x C-k will kill the buffer.

`clone-buffer' doesn't work on an aumix-mode buffer, as of Emacs
24.3.  Two or more aumix programs can run simultaneously (their
displays update with the underlying mixer settings), so perhaps
`clone-buffer' could be made to work.  A `rename-buffer' and
second `M-x aumix' works.  But normally only a single aumix-mode
buffer is desired.

----
The aumix home page is
  URL `http://jpj.net/~trevor/aumix.html'
The aumix-mode.el home page is
  URL `http://user42.tuxfamily.org/aumix-mode/index.html'

----
\\{aumix-mode-map}
"

  (term-mode)

  ;; kill sub-process when killing buffer
  (if (eval-when-compile (fboundp 'make-local-hook))
      (make-local-hook 'kill-buffer-hook)) ;; for xemacs21
  (add-hook 'kill-buffer-hook 'aumix-mode-kill-process
            nil ;; not append
            t)  ;; buffer-local

  (setq major-mode 'aumix-mode
        mode-name  "Aumix")

  ;; desktop.el no misc-data needed
  (set (make-local-variable 'desktop-save-buffer) 'ignore)

  (if (eval-when-compile (fboundp 'run-mode-hooks))
      (run-mode-hooks 'aumix-mode-hook)
    (run-hooks 'aumix-mode-hook))

  ;; Unset $DISPLAY to ensure aumix uses the curses interface, not the gtk
  ;; interface if it was built with gtk interface.  The "-C" command line
  ;; option can do that too, but as of aumix 2.9.1 in the curses build -C
  ;; demands a colour file whereas prefer to leave that as the default
  ;; colours or let the user put a "-C" colours in aumix-mode-switches.
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "DISPLAY" nil)

    (term-exec (current-buffer)
               "aumix" ;; process name
               aumix-mode-program nil aumix-mode-switches))

  (term-char-mode)

  (use-local-map aumix-mode-map)

  ;; In term.el of emacs21 and xemacs21 `term-raw-map' is only created
  ;; when `term-char-mode' is called, so must wait until this point to set
  ;; it as the parent keymap.
  (set-keymap-parent aumix-mode-map term-raw-map)

  ;; `term-in-char-mode' looks for (current-local-map) equal to
  ;; `term-raw-map' to identify char mode, so buffer-local setting of
  ;; `term-raw-map' to be `aumix-mode-map' instead.
  (set (make-local-variable 'term-raw-map) aumix-mode-map)

  (let ((proc (get-buffer-process (current-buffer))))
    ;; adapt to emacs22 pointless incompatible rename of
    ;; `process-kill-without-query'
    (if (eval-when-compile (fboundp 'set-process-query-on-exit-flag))
        (set-process-query-on-exit-flag proc nil) ;; emacs22
      (process-kill-without-query proc))))        ;; emacs21

(put 'aumix-mode 'derived-mode-parent 'term-mode)

;;;###autoload
(defun aumix ()
  "Run the aumix program in a buffer.
An *aumix* buffer is created if it doesn't already exist.
See `aumix-mode' for details of operating the mode."

  (interactive)
  (switch-to-buffer "*aumix*")
  (unless (eq major-mode 'aumix-mode)
    (aumix-mode)))

;;-----------------------------------------------------------------------------
;; desktop.el save/restore (for emacs22 up)
;;
;; `desktop-create-buffer' loads the .el of an autoload mode func such as
;; `aumix-mode' when it appears in a saved desktop.  The alternative would
;; be to autoload `aumix-mode-desktop-restore' and the `eval-after-load',
;; but that seems like more.

(defun aumix-mode-desktop-restore (file-name buffer-name misc-data)
  "Restore `aumix-mode' buffer for desktop.el.
This is designed for use from `desktop-buffer-mode-handlers'."
  ;; checkdoc-params: (file-name buffer-name misc-data)

  (set-buffer (get-buffer-create buffer-name))
  (aumix-mode)
  (current-buffer)) ;; success, return buffer

(eval-after-load "desktop"
  '(when (boundp 'desktop-buffer-mode-handlers) ;; new in emacs22
     (add-to-list 'desktop-buffer-mode-handlers
                  '(aumix-mode . aumix-mode-desktop-restore))))

;;-----------------------------------------------------------------------------

;; LocalWords: docstrings dev subprocess el ncurses terminfo eterm usr

;;;; ChangeLog:

;; 2014-06-19  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* aumix-mode: New package.
;; 


(provide 'aumix-mode)

;;; aumix-mode.el ends here
