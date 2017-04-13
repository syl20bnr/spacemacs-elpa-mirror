;;; picpocket.el --- Image viewer -*- lexical-binding: t; coding: utf-8-unix -*-

;; Copyright (C) 2017 Johan Claesson
;; Author: Johan Claesson <johanclaesson@bredband.net>
;; Maintainer: Johan Claesson <johanclaesson@bredband.net>
;; Version: 30
;; Package-Version: 20170305.259
;; Keywords: multimedia
;; Package-Requires: ((emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Picpocket is an image viewer which requires GNU Emacs 24.4+
;; compiled with ImageMagick.  It have commands for:
;;
;; * File operations on the picture files (delete, move, copy, hardlink).
;; * Scale and rotate the picture.
;; * Associate pictures with tags which are saved to disk.
;; * Filter pictures according to tags.
;; * Customizing keystrokes for quick tagging and file operations.
;; * Undo and browse history of undoable commands.
;;
;;
;; Main entry point
;; ----------------
;;
;; Command: picpocket
;;
;; View the pictures in the current directory.
;;
;;
;; Main keybindings
;; ----------------
;;
;; Space     - Next picture
;; BackSpace - Previous picture
;; r         - Rename picture file
;; c         - Copy picture file
;; k         - Delete picture file
;; t         - Edit tags
;; s         - Slide-show mode
;; [         - Rotate counter-clockwise
;; ]         - Rotate clockwise
;; +         - Scale in
;; -         - Scale out
;; u         - Undo
;; M-u       - View history of undoable actions
;; e         - Customize keystrokes (see below)
;; TAB f     - Toggle full-screen
;; TAB r     - Toggle recursive inclusion of pictures in sub-directories
;;
;; With prefix argument many of the commands will operatate on all the
;; pictures in the current list instead of just the current picture.
;;
;;
;; Disclaimer
;; ----------
;;
;; Picpocket will secretly do stuff in the background with idle
;; timers.  This includes to load upcoming pictures into the image
;; cache.  The intention is that they should block Emacs for so short
;; periods that it is not noticable.  But if you want to get rid of
;; them set `picpocket-inhibit-timers' or kill the picpocket buffer.
;;
;; Picpocket is to be considered beta software.  Keybindings,
;; variables and such may change in future versions.  Tag database
;; file format will remain backwards compatible though.
;;
;;
;; Keystroke customization
;; -----------------------
;;
;; Keystokes can be bound to commands that move/copy pictures into
;; directories and/or tag them.  The following creates commands on
;; keys 1 though 5 to tag according to liking.  Also creates some
;; commands to move picture files to directories according to genre.
;; Finally creates also one command to copy pictures to a backup
;; directory in the user's home directory.
;;
;; (defvar my-picpocket-alist
;;   '((?1 tag "bad")
;;     (?2 tag "sigh")
;;     (?3 tag "good")
;;     (?4 tag "great")
;;     (?5 tag "awesome")
;;     (?F move "fantasy")
;;     (?S move "scifi")
;;     (?P move "steampunk")
;;     (?H move "horror")
;;     (?U move "urban-fantasy")
;;     (?B copy "~/backup")))
;;
;; (setq picpocket-keystroke-alist 'my-picpocket-alist)
;;
;; Digits and capital letters with no modifiers is reserved for these
;; kind of user keybindings.
;;
;; It is recommended to set `picpocket-keystroke-alist' to a symbol as
;; above.  That makes the command `picpocket-edit-keystrokes' (bound
;; to `e' in picpocket buffer) jump to your definition for quick
;; changes.  Edit the list and type M-C-x to save it.
;;
;; See the doc of `picpocket-keystroke-alist' for about the same thing
;; but with a few more details.

;;
;; Tag database
;; ------------
;;
;; Tags associated with pictures are saved to disk.  By default to
;; ~/.emacs.d/picpocket/.  This database maps the sha1 checksum of
;; picture files to list of tags.  This implies that you can rename or
;; move around the file anywhere and the tags will still be remembered
;; when you view it with picpocket again.
;;
;; If you change the picture file content the sha1 checksum will
;; change.  For example this would happen if you rotate or crop the
;; picture with an external program.  That will break the association
;; between sha1 checksum and tags.  However picpocket also stores the
;; file name for each entry of tags.  The command
;; `picpocket-db-update' will go through the database and offer to
;; recover such lost associations.
;;
;; If you change the file-name and the file content at the same time
;; there is no way to recover automatically.

;;; Code:

(eval-when-compile
  (require 'time-date)
  (require 'ido))

(require 'cl-lib)
(require 'dired)
(require 'subr-x)
(require 'ring)
(require 'ewoc)

;;; Options

(defgroup picpocket nil "Picture viewer."
  :group 'multimedia)

(defcustom picpocket-keystroke-alist nil
  "Symbol of variable with an alist of picpocket keystrokes.
\\<picpocket-mode-map>
Elements in the alist have the form (KEY ACTION TARGET).

KEY is a single character, a string accepted by `kbd' or a vector
accepted by `define-key'.

ACTION is tag, move, copy or hardlink.

TARGET is a string.  For ACTION tag it is the tag to add.  For
the other actions it is the destination directory.

Example:

 (defvar my-picpocket-alist
   '((?1 tag \"bad\")
     (?2 tag \"sigh\")
     (?3 tag \"good\")
     (?4 tag \"great\")
     (?5 tag \"awesome\")))

 (setq picpocket-keystroke-alist 'my-picpocket-alist)

There exist a convenience command `picpocket-edit-keystrokes' that
will jump to the definition that the variable
`picpocket-keystroke-alist' points to.  It is bound to
\\[picpocket-keystroke-alist] in the picpocket buffer."
  :risky t
  :type 'symbol)

(defcustom picpocket-filter-consider-dir-as-tag t
  "Whether filter will consider the directory as a tag.

If non-nil add the containing directory name to the list of tags
stored in database.  This matters only when considering a filter.
For example the potential extra tag is not shown in the list of
tags in the header line.

The special value `all' means add all directories in the whole
path (after all soft links have been resolved)."
  :type 'boolean)

(defvar picpocket-filter-ignore-hyphens t
  "If non-nil the filter ignores underscores and dashes.")


(defcustom picpocket-fit :x-and-y
  "Fit picture size to window when non-nil."
  :type '(choice (const :tag "Fit to both width and height" :x-and-y)
                 (const :tag "Fit to width" :x)
                 (const :tag "Fit to height" :y)
                 (const :tag "Show picture in it's natural size" nil)))

(defcustom picpocket-scale 100
  "Picture scaling in percent."
  :type 'integer)

(defcustom picpocket-header t
  "If non-nil display a header line in picpocket buffer."
  :type 'boolean)

(defcustom picpocket-header-line-format '(:eval (picpocket-header-line))
  "The value for `header-line-format' in picpocket buffers.
Enabled if `picpocket-header' is non-nil."
  :type 'sexp)

(defcustom picpocket-header-full-path nil
  "If non-nil display the whole file path in header line."
  :type 'boolean)

(defcustom picpocket-tags-style :list
  "How a list of tags are displayed."
  :type '(choice (const :tag "Org style :tag1:tag2:" :org)
                 (const :tag "Lisp list style (tag1 tag2)" :list)))

(defcustom picpocket-confirm-delete (not noninteractive)
  "If non-nil let user confirm file delete."
  :type 'boolean)

(defcustom picpocket-backdrop-command nil
  "Command to set desktop backdrop with.
\\<picpocket-mode-map>
Used by `picpocket-set-backdrop' bound to \\[picpocket-backdrop-command] in
picpocket buffer.  If this variable is nil then
`picpocket-set-backdrop' will attempt to find an usable command and
assign it to this variable."
  :type 'string)

(defcustom picpocket-recursive nil
  "If non-nil include sub-directories recursively.
\\<picpocket-mode-map>
This variable can be toggled with the `picpocket-toggle-recursive'
command.  It is bound to \\[picpocket-toggle-recursive] in the
picpocket buffer."
  :type 'boolean)

(defcustom picpocket-follow-symlinks t
  "If non-nil follow symlinks while traversing directories recursively.

Symlinks that points to picture files are always followed.
This option concerns only symlinks that points to directories."
  :type 'boolean)

(defcustom picpocket-destination-dir "~/"
  "Destination dir if `picpocket-destination-relative-current' is nil.

Destination dir is the default target dir for move, copy and
hardlink commands."
  :type 'directory)

(defcustom picpocket-destination-relative-current t
  "If non-nil the destination dir is the current directory.
\\<picpocket-mode-map>
If nil it is the value of variable `picpocket-destination-dir'.  This
variable can be toggled with the command
`picpocket-toggle-destination-dir' bound to
\\[picpocket-toggle-destination-dir] in the picpocket buffer."
  :type 'boolean)

(defcustom picpocket-undo-thumbnails-size 7
  "The heigth of picpocket undo thumbnails.
Specified in number of default line heigths."
  :type 'integer)

;;; Internal variables

(defconst picpocket-version 30)
(defconst picpocket-buffer "*picpocket*")
(defconst picpocket-undo-buffer "*picpocket-undo*")

(defvar picpocket-max-undo-thumbnails 4)
(defvar picpocket-gimp-executable (executable-find "gimp"))
(defvar picpocket-look-ahead-max 5)
(defvar picpocket-frame nil)
(defvar picpocket-old-frame nil)
(defvar picpocket-last-action nil)
(defvar picpocket-last-arg nil)
(defvar picpocket-debug nil)
(defvar picpocket-sum 0)
(defvar picpocket-picture-regexp nil)
(defvar picpocket-last-look-ahead nil)
(defvar picpocket-clock-alist nil)
(defvar picpocket-adapt-to-window-size-change t)
(defvar picpocket-entry-function nil)
(defvar picpocket-entry-args nil)
(defvar picpocket-filter nil)
(defvar picpocket-filter-index nil)
(defvar picpocket-compute-filter-index-from-scratch nil)
(defvar picpocket-filter-match-count-done nil)
(defvar picpocket-filter-match-count nil)
(defvar picpocket-window-size nil
  "The current window size in pixels.
This is kept for the benefit of the idle timer functions that do
not necessarily run with the picpocket window selected.")
(defvar picpocket-header-text "")
(defvar picpocket-demote-warnings nil)
(defvar picpocket-sha1sum-executable nil)
(defvar picpocket-debug-idle-timers nil)
(defvar picpocket-old-keystroke-alist nil)
(defvar picpocket-default-backdrop-commands
  '(("display" . "-window root")
    ("feh" . "--bg-file")
    ("hsetroot" . "-tile")
    ("xsetbg")))
(defvar picpocket-tag-completion-table (make-vector 127 0))
(defvar picpocket-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map [tab] 'completion-at-point)
    map)
  "Keymap used for completing tags in minibuffer.")

(defvar picpocket-undo-list-size 25)
(defvar picpocket-undo-ring nil)


;; Variables displayed in the header-line must be marked as risky.
(dolist (symbol '(picpocket-index
                  picpocket-list-length
                  picpocket-current
                  picpocket-filter
                  picpocket-filter-index
                  picpocket-filter-match-count
                  picpocket-header-text))
  (put symbol 'risky-local-variable t))


;;; The list of pictures

(cl-defstruct picpocket-pos
  current
  index
  filter-index)

(defvar picpocket-list nil
  "The `picpocket-list' is a double-linked list of all pictures in directory.
The car contains a `picpocket-pic' struct whose prev slot points to
the previous cons cell.  The next cell is in the cdr.

Note that this is a circular data structure and `print-circle'
should be non-nil when printing it.")
(defvar picpocket-list-length nil)
(defvar picpocket-current nil)
(defvar picpocket-index 0
  "The `picpocket-index' is starting from 1 (incompatible with elt).
When there are no pictures in the list `picpocket-index' is zero.")

(cl-defstruct picpocket-pic
  prev
  dir
  file
  sha
  size
  bytes
  (rotation 0.0))


(defsubst picpocket-pic (pic)
  (car (or pic picpocket-current)))


(defsubst picpocket-prev (&optional pic)
  (picpocket-pic-prev (picpocket-pic pic)))

(defsubst picpocket-dir (&optional pic)
  (picpocket-pic-dir (picpocket-pic pic)))

(defsubst picpocket-file (&optional pic)
  (picpocket-pic-file (picpocket-pic pic)))

(defsubst picpocket-sha (&optional pic)
  (picpocket-pic-sha (picpocket-pic pic)))

(defsubst picpocket-size (&optional pic)
  (picpocket-pic-size (picpocket-pic pic)))

(defsubst picpocket-bytes (&optional pic)
  (picpocket-pic-bytes (picpocket-pic pic)))

(defsubst picpocket-rotation (&optional pic)
  (picpocket-pic-rotation (picpocket-pic pic)))


(defsubst picpocket-set-prev (pic value)
  (setf (picpocket-pic-prev (picpocket-pic pic)) value))

(defsubst picpocket-set-dir (pic value)
  (setf (picpocket-pic-dir (picpocket-pic pic)) value))

(defsubst picpocket-set-file (pic value)
  (setf (picpocket-pic-file (picpocket-pic pic)) value))

(defsubst picpocket-set-sha (pic value)
  (setf (picpocket-pic-sha (picpocket-pic pic)) value))

(defsubst picpocket-set-size (pic value)
  (setf (picpocket-pic-size (picpocket-pic pic)) value))

(defsubst picpocket-set-bytes (pic value)
  (setf (picpocket-pic-bytes (picpocket-pic pic)) value))

(defsubst picpocket-set-rotation (pic value)
  (setf (picpocket-pic-rotation (picpocket-pic pic)) value))


;;; Macros


(defmacro picpocket-command (&rest body)
  "Macro used in normal picpocket commands.
It is used in all commands except for those that switch to
another buffer (those use `picpocket-bye-command').  It takes care of
common stuff that shall be done before and after all commands in
picpocket mode.  The convention is to invoke this macro in the
body of all command functions.  It is not used in subroutines
because that would make it harder to verify that it is used in
all commands (and preferably only used one time).
\(fn &rest BODY)"
  (declare (indent defun)
           (debug (body)))
  `(progn
     (picpocket-ensure-picpocket-buffer)
     (let ((rc (progn ,@body)))
       (picpocket-ensure-picpocket-buffer)
       (picpocket-update-buffer)
       (when (buffer-live-p (get-buffer picpocket-undo-buffer))
         (picpocket-update-undo-buffer))
       rc)))

(defmacro picpocket-bye-command (&rest body)
  "Macro used in picpocket commands that switch to another buffer.
See `picpocket-command'.
\(fn &rest BODY)"
  (declare (indent defun)
           (debug (body)))
  `(progn
     (picpocket-ensure-picpocket-buffer)
     ,@body))



(cl-defmacro picpocket-when-let ((var value) &rest body)
  "Evaluate VALUE, if the result is non-nil bind it to VAR and eval BODY.
\(fn (VAR VALUE) &rest BODY)"
  (declare (debug ((symbolp form) body))
           (indent 1))
  `(let ((,var ,value))
     (when ,var ,@body)))

(defmacro picpocket-time-string (&rest forms)
  (declare (indent defun)
           (debug (body)))
  `(picpocket-sec-string (cadr (picpocket-time ,@forms))))

(defun picpocket-sec-string (time)
  (let ((sec (format "%.06f" (float-time time))))
    (concat (substring sec 0 -3)
            "_"
            (substring sec -3)
            "s")))

(defmacro picpocket-time (&rest forms)
  "Evaluate FORMS and return (rc time).
The reason it does not return (rc . time) is to be able to bind
with `cl-multiple-value-bind' etc."
  (declare (indent defun)
           (debug (body)))
  (let ((before (make-symbol "before"))
        (rc (make-symbol "rc")))
    `(let ((,before (current-time))
           (,rc (progn ,@forms)))
       (list ,rc (time-since ,before)))))

(defmacro picpocket-clock (&rest body)
  (declare (indent defun))
  (let ((thing (caar body)))
    `(picpocket-clock-thing ',thing ,@body)))

(defmacro picpocket-clock-thing (thing &rest body)
  (declare (indent 1))
  (let ((rc (make-symbol "rc"))
        (s (make-symbol "s"))
        (sum (make-symbol "sum")))
    `(cl-destructuring-bind (,rc ,s) (picpocket-time (progn ,@body))
       (let ((,sum (assq ,thing picpocket-clock-alist)))
         (if ,sum
             (setcdr ,sum (time-add ,s (cdr ,sum)))
           (push (cons ,thing ,s) picpocket-clock-alist)))
       ,rc)))

(defmacro picpocket-with-clock (title &rest body)
  (declare (debug (stringp body))
           (indent 1))
  `(let (picpocket-clock-alist)
     (prog1
         (progn ,@body)
       (picpocket-clock-report ,title))))


;;; Picp mode

(define-derived-mode picpocket-base-mode special-mode "picpocket-base"
  "Base major mode for buffers with images.
This mode is not used directly.  Other modes inherit from this mode."
  (buffer-disable-undo)
  ;; Ensure imagemagick is preferred.
  (unless (eq 'imagemagick (cdar image-type-file-name-regexps))
    (let ((entry (rassq 'imagemagick image-type-file-name-regexps)))
      (setq-local image-type-file-name-regexps
                  (cons entry
                        (delete entry
                                (cl-copy-list image-type-file-name-regexps))))))
  (setq-local image-type-header-regexps nil)
  (when (boundp 'image-scaling-factor)
    (setq-local image-scaling-factor 1.0))
  ;; image-map is embedded as text property on all images,
  ;; we do not want that in this buffer.
  (when (boundp 'image-map)
    (setq-local image-map nil))
  (setq truncate-lines t
        auto-hscroll-mode nil))

(define-derived-mode picpocket-mode picpocket-base-mode "picpocket"
  "Major mode for the main *picpocket* buffer."
  (picpocket-db-init)
  (picpocket-db-compile-tags-for-completion)
  (setq header-line-format (when picpocket-header
                             picpocket-header-line-format)
        vertical-scroll-bar nil
        cursor-type nil
        left-fringe-width 0
        right-fringe-width 0)
  ;; Call set-window-buffer to update the fringes.
  (set-window-buffer (selected-window) (current-buffer))
  (when (eq (selected-frame) picpocket-frame)
    (setq mode-line-format nil))
  (add-hook 'kill-emacs-hook #'picpocket-delete-trashcan)
  (add-hook 'kill-buffer-hook #'picpocket-save-journal nil t)
  (add-hook 'kill-buffer-hook #'picpocket-cleanup-most-hooks nil t)
  (add-hook 'window-size-change-functions
            #'picpocket-window-size-change-function)
  (add-hook 'buffer-list-update-hook #'picpocket-maybe-update-keymap)
  (add-hook 'buffer-list-update-hook #'picpocket-maybe-rescale)
  (picpocket-init-timers)
  (picpocket-update-keymap))

(defun picpocket-unload-function ()
  (message "Unloading picpocket")
  (picpocket-cancel-timers)
  (picpocket-delete-trashcan)
  (picpocket-cleanup-most-hooks)
  (remove-hook 'kill-emacs-hook #'picpocket-delete-trashcan)
  nil)

(let ((map (make-sparse-keymap))
      (toggle-map (make-sparse-keymap
                   (concat "Toggle: [f - fullscreen]"
                           " [h - header]"
                           " [d - destination]"
                           " [r - recursive]"
                           " [l - follow symlinks]"
                           " [D - debug]"))))
  (define-key toggle-map [?f] #'picpocket-toggle-fullscreen-frame)
  (define-key toggle-map [?d] #'picpocket-toggle-destination-dir)
  (define-key toggle-map [?h] #'picpocket-toggle-header)
  (define-key toggle-map [?r] #'picpocket-toggle-recursive)
  (define-key toggle-map [?l] #'picpocket-toggle-follow-symlinks)
  (define-key toggle-map [?D] #'picpocket-toggle-debug)

  (suppress-keymap map)
  (define-key map [tab] toggle-map)
  (define-key map [backspace] #'picpocket-previous)
  (define-key map [prior] #'picpocket-previous)
  (define-key map [?p] #'picpocket-previous)
  (define-key map [?\s] #'picpocket-next)
  (define-key map [next] #'picpocket-next)
  (define-key map [?n] #'picpocket-next)
  (define-key map [?d] #'picpocket-dired)
  (define-key map [?v] #'picpocket-visit-file)
  (define-key map [?e] #'picpocket-edit-keystrokes)
  (define-key map [home] #'picpocket-home)
  (define-key map [?k] #'picpocket-delete-file)
  (define-key map [(control ?d)] #'picpocket-delete-file)
  (define-key map [deletechar] #'picpocket-delete-file)
  (define-key map [end] #'picpocket-end)
  (define-key map [?q] #'picpocket-quit)
  (define-key map [?s] #'picpocket-slideshow)
  (define-key map [?r] #'picpocket-rename)
  (define-key map [?m] #'picpocket-move)
  (define-key map [?c] #'picpocket-copy)
  (define-key map [?l] #'picpocket-hardlink)
  (define-key map [(meta ?m)] #'picpocket-move-by-keystroke)
  (define-key map [(meta ?c)] #'picpocket-copy-by-keystroke)
  (define-key map [(meta ?l)] #'picpocket-hardlink-by-keystroke)
  (define-key map [(meta ?t)] #'picpocket-tag-by-keystroke)
  (define-key map [?t] #'picpocket-edit-tags)
  (define-key map [?z] #'picpocket-repeat)
  (define-key map [?g] #'picpocket-revert)
  (define-key map [(meta ?b)] #'picpocket-set-backdrop)
  (define-key map [?.] #'picpocket-dired-up-directory)
  (define-key map [?f] #'picpocket-set-filter)
  (define-key map [(meta ?f)] #'picpocket-set-filter-by-keystroke)
  (define-key map [?\[] #'picpocket-rotate-counter-clockwise)
  (define-key map [?\]] #'picpocket-rotate-clockwise)
  (define-key map [?{] #'picpocket-rotate-counter-clockwise-10-degrees)
  (define-key map [?}] #'picpocket-rotate-clockwise-10-degrees)
  (define-key map [(meta ?\[)] #'picpocket-reset-rotation)
  (define-key map [(meta ?\])] #'picpocket-reset-rotation)
  (define-key map [?+] #'picpocket-scale-in)
  (define-key map [?=] #'picpocket-scale-in)
  (define-key map [?-] #'picpocket-scale-out)
  (define-key map [?0] #'picpocket-reset-scale)
  (define-key map [(meta ?a)] #'picpocket-fit-to-width-and-height)
  (define-key map [(meta ?w)] #'picpocket-fit-to-width)
  (define-key map [(meta ?h)] #'picpocket-fit-to-height)
  (define-key map [(meta ?n)] #'picpocket-no-fit)
  (define-key map [?i ?a] #'picpocket-fit-to-width-and-height)
  (define-key map [?i ?w] #'picpocket-fit-to-width)
  (define-key map [?i ?h] #'picpocket-fit-to-height)
  (define-key map [?i ?n] #'picpocket-no-fit)
  (define-key map [??] #'picpocket-help)
  (define-key map [left] #'scroll-right)
  (define-key map [right] #'scroll-left)
  (define-key map [(control ?b)] #'scroll-right)
  (define-key map [(control ?f)] #'scroll-left)
  (define-key map [?u] #'picpocket-undo)
  (define-key map [(meta ?u)] #'picpocket-visit-undo-list)
  (define-key map [?j] #'picpocket-jump)
  (define-key map [?o] #'picpocket-gimp-open)
  (define-key map [??] #'picpocket-help)
  (setq picpocket-mode-map map))


;;; Entry points

;;;###autoload
(defun picpocket ()
  "View the pictures in the current directory."
  (interactive)
  (let ((selected-file (cond ((buffer-file-name)
                              (file-truename (buffer-file-name)))
                             ((eq major-mode 'dired-mode)
                              (dired-get-filename nil t))
                             ((and (eq major-mode 'picpocket-mode)
                                   picpocket-current)
                              (picpocket-absfile))
                             (t nil))))
    (picpocket-directory default-directory selected-file)))

(defun picpocket-directory (dir &optional selected-file)
  "View the pictures in DIR starting with SELECTED-FILE."
  (setq picpocket-entry-function 'picpocket-directory
        picpocket-entry-args (list dir))
  (let ((files (picpocket-file-list dir)))
    (picpocket-create-buffer files selected-file dir)))

(defun picpocket-files (files &optional selected-file)
  "View the list of image files in FILES starting with SELECTED-FILE."
  (setq picpocket-entry-function 'picpocket-files
        picpocket-entry-args (list files))
  (picpocket-create-buffer files selected-file))

;;;###autoload
(defun picpocket-db-update ()
  "Manage the tag database.

Enter a special buffer where any suspicious database entries are
listed.  Suspicious entries are for example when files that have
disappeared.  Maybe they have been deleted outside of picpocket.
And the entries in picpocket now points to nowhere.  If there are
any such entries they will be listed in this buffer.  And there
will be an offer to clean up those entries from the database.

Note that this command can take some time to finish since it goes
through the entire database."
  (interactive)
  (picpocket-do-db-update))


;;; Picpocket mode commands

(defun picpocket-rotate-counter-clockwise (&optional arg)
  "Display the current picture rotated 90 degrees to the left.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picpocket-command
    (picpocket-rotate -90.0 arg)))

(defun picpocket-rotate-clockwise (&optional arg)
  "Display the current picture rotated 90 degrees to the right.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picpocket-command
    (picpocket-rotate 90.0 arg)))

(defun picpocket-rotate-counter-clockwise-10-degrees (&optional arg)
  "Display the current picture rotated 10 degrees to the left.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picpocket-command
    (picpocket-rotate -10.0 arg)))

(defun picpocket-rotate-clockwise-10-degrees (&optional arg)
  "Display the current picture rotated 10 degrees to the right.
This command do not change the picture file on disk.  With prefix
arg (ARG) read a number in the minibuffer and set rotation to
that."
  (interactive "P")
  (picpocket-command
    (picpocket-rotate 10.0 arg)))

(defun picpocket-reset-rotation ()
  "Display the current picture as is without any rotation."
  (interactive)
  (picpocket-command
    (picpocket-set-rotation picpocket-current 0.0)))

(defun picpocket-rotate (delta arg)
  (let ((degrees (if arg
                     (float (read-number "Set rotation in degrees"
                                         (picpocket-rotation)))
                   (+ (picpocket-rotation) delta))))
    (picpocket-error-if-rotation-is-unsupported)
    (picpocket-set-rotation picpocket-current degrees)
    (picpocket-old-update-buffer)))


(defun picpocket-scale-in (&optional arg)
  "Zoom in 10%.
With prefix arg (ARG) read scale percent in minibuffer."
  (interactive "P")
  (picpocket-command
    (if arg
        (setq picpocket-scale (read-number "Scale factor: " picpocket-scale))
      (picpocket-alter-scale 10))
    (picpocket-warn-if-scaling-is-unsupported)
    (picpocket-old-update-buffer)))

(defun picpocket-scale-out (&optional arg)
  "Zoom out 10%.
With prefix arg (ARG) read scale percent in minibuffer."
  (interactive "P")
  (picpocket-command
    (if arg
        (setq picpocket-scale (read-number "Scale factor: " picpocket-scale))
      (picpocket-alter-scale -10))
    (picpocket-warn-if-scaling-is-unsupported)
    (picpocket-old-update-buffer)))

(defun picpocket-reset-scale ()
  "Reset the scale to 100%."
  (interactive)
  (picpocket-command
    (setq picpocket-scale 100)
    (message "Restore the scale to 100%%.")
    (picpocket-old-update-buffer)))

(defun picpocket-fit-to-width-and-height ()
  "Fit the picture to both width and height of window.
Fitting is done before applying the scaling factor.  That is, it
will only really fit when the scaling is the default 100%.  The
scaling can be restored to 100% by typing \\[picpocket-reset-scale]
\(bound to the command `picpocket-reset-scale')."
  (interactive)
  (picpocket-command
    (setq picpocket-fit :x-and-y)
    (message "Fit picture to both width and height")
    (picpocket-warn-if-scaling-is-unsupported)
    (picpocket-old-update-buffer)))

(defun picpocket-fit-to-width ()
  "Fit the picture to the width of window.
Fitting is done before applying the scaling factor.  That is, it
will only really fit when the scaling is the default 100%.  The
scaling can be restored to 100% by typing \\[picpocket-reset-scale]
\(bound to the command `picpocket-reset-scale')."
  (interactive)
  (picpocket-command
    (setq picpocket-fit :x)
    (message "Fit picture to width")
    (picpocket-warn-if-scaling-is-unsupported)
    (picpocket-old-update-buffer)))

(defun picpocket-fit-to-height ()
  "Fit the picture to the height of window.
Fitting is done before applying the scaling factor.  That is, it
will only really fit when the scaling is the default 100%.  The
scaling can be restored to 100% by typing \\[picpocket-reset-scale]
\(bound to the command `picpocket-reset-scale')."
  (interactive)
  (picpocket-command
    (setq picpocket-fit :y)
    (message "Fit picture to height")
    (picpocket-warn-if-scaling-is-unsupported)
    (picpocket-old-update-buffer)))

(defun picpocket-no-fit ()
  "Do not fit the picture to the window."
  (interactive)
  (picpocket-command
    (setq picpocket-fit nil)
    (message "Do not fit picture to window size")
    (picpocket-old-update-buffer)))


(defun picpocket-set-backdrop ()
  "Attempt to install the current picture as desktop backdrop."
  (interactive)
  (picpocket-command
    (picpocket-ensure-current-pic)
    (setq picpocket-backdrop-command (or picpocket-backdrop-command
                                         (picpocket-default-backdrop-command)))
    (unless picpocket-backdrop-command
      (error (concat "Command to set backdrop not found."
                     " Set picpocket-backdrop-command or install %s")
             (picpocket-default-backdrop-commands-string)))
    (let* ((words (split-string picpocket-backdrop-command))
           (cmd (car words))
           (file (picpocket-absfile))
           (args (append (cdr words) (list file))))
      (with-temp-buffer
        (unless (zerop (apply #'call-process cmd nil t nil args))
          (error "Command \"%s %s\" failed with output \"%s\""
                 picpocket-backdrop-command file (buffer-string))))
      (message "%s installed %s as backdrop" cmd file))))

(defun picpocket-default-backdrop-command ()
  (cl-loop for (cmd . args) in picpocket-default-backdrop-commands
           when (executable-find cmd)
           return (concat cmd " " args " ")))


(defun picpocket-default-backdrop-commands-string ()
  (concat
   (mapconcat #'identity
              (butlast (mapcar #'car picpocket-default-backdrop-commands))
              ", ")
   " or "
   (caar (last picpocket-default-backdrop-commands))))

(defun picpocket-toggle-debug ()
  "Toggle debug mode."
  (interactive)
  (picpocket-command
    (setq picpocket-debug (not picpocket-debug))
    (message "Picpocket debug is %s." (if picpocket-debug "on" "off"))))

(defun picpocket-toggle-recursive ()
  "Toggle recursive inclusion of sub-directories.
Directories whose name starts with a dot will not be traversed.
However picture files whose name starts with dot will be
included."
  (interactive)
  (picpocket-command
    (setq picpocket-recursive (not picpocket-recursive))
    (picpocket-do-revert)
    (message (if picpocket-recursive
                 "Recursively include pictures in subdirectories."
               "Only show pictures in current directory."))))

(defun picpocket-toggle-follow-symlinks ()
  "Toggle whether to follow symlinks while recursively traversing directories.
Symlinks that points to picture files are always followed.
This command concerns only symlinks that points to directories."
  (interactive)
  (picpocket-command
    (setq picpocket-follow-symlinks (not picpocket-follow-symlinks))
    (picpocket-do-revert)
    (message (if picpocket-follow-symlinks
                 "Follow symlinks."
               "Do not follow symlinks."))))




(defun picpocket-quit ()
  "Quit picpocket."
  (interactive)
  (picpocket-bye-command
    (picpocket-disable-fullscreen)
    (picpocket-save-journal)
    (quit-window)))

(defun picpocket-disable-fullscreen ()
  (when (picpocket-fullscreen-p)
    (picpocket-toggle-fullscreen-frame)))

(defun picpocket-toggle-destination-dir (&optional ask-for-dir)
  "Toggle the destination for file operations.

File operations are move, copy and hardlink.  Either the
destination is relative to the current directory.  Or it is
relative the value of variable `picpocket-destination-dir'.

With prefix arg ask for a directory and set variable
`picpocket-destination-dir' to that.  Calling from Lisp with the argument
ASK-FOR-DIR non-nil will also do that."
  (interactive "P")
  (picpocket-command
    (if ask-for-dir
        (setq picpocket-destination-relative-current nil
              picpocket-destination-dir
              (read-directory-name "Destination dir: "))
      (setq picpocket-destination-relative-current
            (not picpocket-destination-relative-current)))
    (message "Destination directory is relative to %s."
             (if picpocket-destination-relative-current
                 "the current directory"
               picpocket-destination-dir))))



(defun picpocket-edit-keystrokes ()
  "Move to definition of variable `picpocket-keystroke-alist'.
To use this command you must set variable `picpocket-keystroke-alist'
to a variable symbol.  The purpose of this command is to be
able to quickly move to the definition and edit keystrokes."
  (interactive)
  (picpocket-bye-command
    (unless (and picpocket-keystroke-alist
                 (symbolp picpocket-keystroke-alist))
      (user-error (concat "You need to set picpocket-keystroke-alist "
                          "to a symbol for this command to work")))
    (find-variable-other-window picpocket-keystroke-alist)
    (goto-char (point-at-eol))))

(defun picpocket-slideshow ()
  "Start slide-show."
  (interactive)
  (picpocket-command
    (while (and (not (input-pending-p))
                (picpocket-next-pos))
      (picpocket-next)
      (when (picpocket-next-pos)
        (sit-for 8)))
    (message "End of slideshow.")))

(defun picpocket-visit-file ()
  "Open the current picture in default mode (normally `image-mode')."
  (interactive)
  (picpocket-bye-command
    (find-file (picpocket-absfile))))

(defun picpocket-toggle-fullscreen-frame ()
  "Toggle use of fullscreen frame.

The first call will show the picpocket buffer in a newly created
frame in fullscreen mode.  It is meant to only show the picpocket
buffer (but this is not enforced).  The second call will delete
this frame and go back to the old frame."
  (interactive)
  (picpocket-command
    (if (picpocket-fullscreen-p)
        (progn
          (delete-frame picpocket-frame)
          (setq picpocket-frame nil)
          (picpocket-select-frame picpocket-old-frame)
          (setq mode-line-format (default-value 'mode-line-format))
          (picpocket-old-update-buffer))
      (setq picpocket-old-frame (selected-frame)
            ;; Do not disable scroll bars and fringes, they are disabled
            ;; on buffer level instead.
            picpocket-frame (make-frame `((name . "picpocket")
                                          (menu-bar-lines . 0)
                                          (tool-bar-lines . 0)
                                          (minibuffer . nil)
                                          (fullscreen . fullboth)
                                          ;; PENDING - background-color seem
                                          ;; to mess up the cache.
                                          ;; See image.c:search_image_cache.
                                          ;; (foreground-color . "white")
                                          ;; (background-color . "black")
                                          )))
      (picpocket-select-frame picpocket-frame)
      (setq mode-line-format nil)
      (add-hook 'focus-in-hook #'picpocket-focus)
      (add-hook 'minibuffer-setup-hook #'picpocket-minibuffer-setup)
      (add-hook 'minibuffer-exit-hook #'picpocket-minibuffer-exit)

      ;; Resdisplay seem to be needed to get accurate return value from
      ;; window-inside-pixel-edges.
      (redisplay)
      (picpocket-old-update-buffer))))

(defvar picpocket-last-frame-that-used-minibuffer nil)

(defun picpocket-focus ()
  "Update picture when `picpocket-minibuffer-exit' select `picpocket-frame'.
Without this the picture size may be fitted to the wrong frame.
This hook make sure it is fitted to `picpocket-frame'."
  (and (eq (selected-frame) picpocket-frame)
       (eq (current-buffer) (get-buffer picpocket-buffer))
       (picpocket-update-buffer)))

(defun picpocket-minibuffer-setup ()
  (setq picpocket-last-frame-that-used-minibuffer last-event-frame)
  (when (eq picpocket-frame last-event-frame)
    (select-frame-set-input-focus default-minibuffer-frame)))

(defun picpocket-minibuffer-exit ()
  (when (and (picpocket-fullscreen-p)
             (eq picpocket-frame picpocket-last-frame-that-used-minibuffer))
    (select-frame-set-input-focus picpocket-frame)))

(defun picpocket-fullscreen-p ()
  (and picpocket-frame
       (frame-live-p picpocket-frame)))


(defun picpocket-select-frame (frame)
  (select-frame-set-input-focus frame)
  (switch-to-buffer picpocket-buffer)
  ;; set-window-buffer will update the fringes.
  (set-window-buffer (selected-window) (current-buffer)))


(defun picpocket-next ()
  "Move to the next picture in the current list."
  (interactive)
  (picpocket-command
    (let ((next (picpocket-next-pos)))
      (if next
          (picpocket-list-set-pos next)
        (picpocket-no-file "next")))
    (picpocket-old-update-buffer)))

(defun picpocket-next-pos (&optional pos)
  (unless pos
    (setq pos (picpocket-current-pos)))
  (cl-loop for pic = (cdr (picpocket-pos-current pos)) then (cdr pic)
           for index = (1+ (picpocket-pos-index pos)) then (1+ index)
           while pic
           when (picpocket-filter-match-p pic)
           return
           (make-picpocket-pos :current pic
                               :index index
                               :filter-index
                               (when (picpocket-pos-filter-index pos)
                                 (1+ (picpocket-pos-filter-index pos))))))

(defun picpocket-current-pos ()
  (make-picpocket-pos :current picpocket-current
                      :index picpocket-index
                      :filter-index picpocket-filter-index))

(defun picpocket-next-pic ()
  (picpocket-when-let (pos (picpocket-next-pos))
    (picpocket-pos-current pos)))

(defun picpocket-previous ()
  "Move to the previous picture in the current list."
  (interactive)
  (picpocket-command
    (let ((prev (picpocket-previous-pos)))
      (if prev
          (picpocket-list-set-pos prev)
        (picpocket-no-file "previous")))
    (picpocket-old-update-buffer)))

(defun picpocket-previous-pic ()
  (picpocket-when-let (pos (picpocket-previous-pos))
    (picpocket-pos-current pos)))

(defun picpocket-previous-pos ()
  (cl-loop for pic = (picpocket-safe-prev picpocket-current)
           then (picpocket-safe-prev pic)
           for index = (1- picpocket-index) then (1- index)
           while pic
           when (picpocket-filter-match-p pic)
           return (make-picpocket-pos :current pic
                                      :index index
                                      :filter-index
                                      (when picpocket-filter-index
                                        (1- picpocket-filter-index)))))

(defun picpocket-safe-prev (pic)
  (when pic
    (picpocket-prev pic)))

(defun picpocket-home ()
  "Move to the first picture in the current list."
  (interactive)
  (picpocket-command
    (picpocket-list-set-pos (picpocket-first-pos))
    (unless (picpocket-filter-match-p picpocket-current)
      (let ((next (picpocket-next-pos)))
        (if next
            (picpocket-list-set-pos next)
          (picpocket-no-file))))
    (picpocket-old-update-buffer)))

(defun picpocket-first-pos ()
  (make-picpocket-pos :current picpocket-list
                      :index 1
                      :filter-index
                      (if (picpocket-filter-match-p picpocket-list)
                          1
                        0)))

(defun picpocket-end ()
  "Move to the last picture in the current list."
  (interactive)
  (picpocket-command
    (picpocket-list-set-pos (picpocket-last-pos))
    (unless (picpocket-filter-match-p picpocket-current)
      (let ((prev (picpocket-previous-pos)))
        (if prev
            (picpocket-list-set-pos prev)
          (picpocket-no-file))))
    (picpocket-old-update-buffer)))

(defun picpocket-last-pos ()
  (cl-loop for pic on picpocket-current
           for index = picpocket-index then (1+ index)
           when (null (cdr pic))
           return (make-picpocket-pos :current pic
                                      :index index)))

(defun picpocket-delete-file ()
  "Permanently delete the current picture file."
  (interactive)
  (picpocket-command
    (picpocket-ensure-current-pic)
    (when (or (not picpocket-confirm-delete)
              (picpocket-y-or-n-p "Delete file %s from disk? "
                                  (picpocket-file)))
      (picpocket-action 'delete nil)
      (picpocket-old-update-buffer))))

(defun picpocket-y-or-n-p (format &rest objects)
  (let* ((prompt (apply #'format format objects))
         (header-line-format (concat prompt " (y or n)")))
    (y-or-n-p prompt)))

(defun picpocket-repeat ()
  "Repeat the last repeatable action.
The repeatable actions are:
1. Move/copy/hardlink the current picture to a directory.
2. Add a tag to or remove a tag from the current picture."
  (interactive)
  (picpocket-command
    (unless picpocket-last-action
      (user-error "No repeatable action have been done"))
    (picpocket-action picpocket-last-action picpocket-last-arg)
    (picpocket-old-update-buffer)))

(defun picpocket-dired ()
  "Visit the current directory in `dired-mode'."
  (interactive)
  (picpocket-bye-command
    (if picpocket-current
        (let ((dir (picpocket-dir))
              (file (picpocket-absfile)))
          (dired default-directory)
          (when (and (equal dir (file-truename default-directory))
                     (file-exists-p file))
            (dired-goto-file file)))
      (dired default-directory))))

(defun picpocket-dired-up-directory ()
  "Enter Dired mode in the parent directory."
  (interactive)
  (picpocket-bye-command
    (let ((dir default-directory))
      (quit-window)
      (dired (file-name-directory (directory-file-name dir)))
      (dired-goto-file dir))))

(defun picpocket-toggle-header ()
  "Toggle the display of the header line."
  (interactive)
  (picpocket-command
    (setq picpocket-header (not picpocket-header)
          header-line-format (when picpocket-header
                               picpocket-header-line-format))
    (force-mode-line-update)))


(defun picpocket-revert ()
  "Revert the current picpocket buffer.
Update the current list of pictures.
When called from Lisp return the new picpocket buffer."
  (interactive)
  (picpocket-command
    (picpocket-do-revert)))

(defun picpocket-do-revert ()
  ;; Selected-file is the second arg to all possible
  ;; picpocket-entry-functions.
  (apply picpocket-entry-function (append picpocket-entry-args
                                          (when picpocket-current
                                            (list (picpocket-absfile))))))

(defun picpocket-rename (dst)
  "Edit the filename of current picture.
If only the filename is changed the picture will stay as the
current picture.  But if it is moved to another directory it will
be removed from the picpocket list.
When called from Lisp DST is the new absolute filename."
  (interactive (list (progn
                       (picpocket-ensure-current-pic)
                       (when (boundp 'ido-read-file-name-non-ido)
                         (add-to-list 'ido-read-file-name-non-ido
                                      #'picpocket-rename))
                       (read-file-name "To: " nil (picpocket-file) nil
                                       (picpocket-file)))))
  (picpocket-command
    (picpocket-action (if (file-directory-p dst)
                          'move
                        'rename)
                      dst)
    (picpocket-old-update-buffer)))


(defun picpocket-move (all dst)
  "Move current picture to another directory.
With prefix arg (ALL) move all pictures in the picpocket list.
The picture will also be removed from the picpocket list.
When called from Lisp DST is the destination directory."
  (interactive (picpocket-read-destination 'move))
  (picpocket-command
    (picpocket-action 'move dst all)
    (picpocket-old-update-buffer)))

(defun picpocket-copy (all dst)
  "Copy the current picture to another directory.
With prefix arg (ALL) copy all pictures in the current list.
When called from Lisp DST is the destination directory."
  (interactive (picpocket-read-destination 'copy))
  (picpocket-command
    (picpocket-action 'copy dst all)
    (picpocket-old-update-buffer)))

(defun picpocket-hardlink (all dst)
  "Make a hard link to the current picture in another directory.
With prefix arg (ALL) hard link all pictures in the current list.
When called from Lisp DST is the destination directory."
  (interactive (picpocket-read-destination 'hardlink))
  (picpocket-command
    (picpocket-action 'hardlink dst all)
    (picpocket-old-update-buffer)))

(defun picpocket-read-destination (action)
  (picpocket-ensure-current-pic)
  (list (when current-prefix-arg
          'all)
        (file-name-as-directory
         (read-directory-name (format "%s%s to: "
                                      (capitalize (symbol-name action))
                                      (if current-prefix-arg
                                          " all pictures"
                                        ""))
                              (picpocket-destination-dir)))))

(defun picpocket-ensure-current-pic ()
  (unless (picpocket-filter-match-p picpocket-current)
    (user-error "No current picture"))
  (unless (file-exists-p (picpocket-absfile))
    (error "File %s no longer exists" (picpocket-file))))

(defun picpocket-destination-dir ()
  (if picpocket-destination-relative-current
      default-directory
    picpocket-destination-dir))

(defun picpocket-move-by-keystroke (all)
  "Move the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picpocket-keystroke-alist'.
With prefix arg (ALL) move all pictures in the current list."
  (interactive "P")
  (picpocket-command
    (picpocket-file-by-keystroke-command 'move all)))

(defun picpocket-copy-by-keystroke (all)
  "Copy the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picpocket-keystroke-alist'.
With prefix arg (ALL) copy all pictures in the current list."
  (interactive "P")
  (picpocket-command
    (picpocket-file-by-keystroke-command 'copy all)))

(defun picpocket-hardlink-by-keystroke (all)
  "Make a hard link to the current picture.
The destination directory is determined by a keystroke that is
lookup up in the variable `picpocket-keystroke-alist'.
With prefix arg (ALL) hard link all pictures in the current list."
  (interactive "P")
  (picpocket-command
    (picpocket-file-by-keystroke-command 'hardlink all)))

(defun picpocket-file-by-keystroke-command (action all)
  (picpocket-ensure-current-pic)
  (let ((prompt (format "directory to %s%s to"
                        (symbol-name action)
                        (if all " all pictures" ""))))
    (picpocket-action action
                      (picpocket-read-key prompt)
                      (when all 'all))
    (picpocket-old-update-buffer)))


(defun picpocket-tag-by-keystroke (&optional all)
  "Add a tag to the current picture.
The tag is determined by a keystroke that is looked up in the
variable `picpocket-keystroke-alist'.

With prefix arg (ALL) the tag is added to all pictures in the
current list.  Type a minus sign (-) before the keystroke to
remove the tag from all pictures instead."
  (interactive "P")
  (picpocket-command
    (picpocket-ensure-current-pic)
    (pcase (picpocket-read-key-to-add-or-remove-tag nil all)
      (`(,remove ,tag)
       (picpocket-action (if remove 'remove-tag 'add-tag)
                         tag
                         (if all 'all))))
    (picpocket-old-update-buffer)))

(defun picpocket-edit-tags (&optional all tags-string)
  "Edit the tags associated with current picture.
To enter multiple tags separate them with spaces.

With prefix arg (ALL) enter a single tag and add it to all
pictures in the current list.  If TAGS-STRING begins with a minus
sign (-) then the tag is removed from all pictures instead."
  (interactive "P")
  (picpocket-command
    (if all
        (picpocket-tag-to-all (or tags-string
                                  (picpocket-read-tag-for-all)))
      (picpocket-ensure-current-pic)
      (let* ((old-tags-string (picpocket-tags-string-to-edit (picpocket-tags)))
             (new-tags-string (or tags-string
                                  (picpocket-read-tags "Tags: "
                                                       old-tags-string))))
        (picpocket-action 'set-tags new-tags-string)))
    (picpocket-old-update-buffer)))

(defun picpocket-read-tag-for-all ()
  (picpocket-read-tags "Type tag to add to all files (-tag to remove): "))

(defun picpocket-tag-to-all (tag-string)
  "Add a tag (TAG-STRING) to all pictures in current picpocket buffer.
If tag starts with minus remove tag instead of add."
  (cond ((string-match "\\`[:space:]*\\'" tag-string)
         (message "Empty string, no action."))
        ((not (eq 1 (length (split-string tag-string))))
         (message "Cannot handle more than one tag at a time"))
        ((eq (elt tag-string 0) ?-)
         (let ((tag (substring tag-string 1)))
           (picpocket-action 'remove-tag tag 'all)
           (message "Tag %s was removed from all." tag)))
        (t
         (picpocket-action 'add-tag tag-string 'all)
         (message "All tagged with %s." tag-string))))

(defun picpocket-read-tags (prompt &optional old-tags-string)
  (let ((string (minibuffer-with-setup-hook
                    (lambda ()
                      (setq-local completion-at-point-functions
                                  (list #'picpocket-tag-completion-at-point)))
                  (read-from-minibuffer prompt
                                        old-tags-string
                                        picpocket-minibuffer-map))))
    (dolist (tag (split-string string))
      (intern (string-remove-prefix "-" tag)
              picpocket-tag-completion-table))
    string))

(defun picpocket-tag-completion-at-point ()
  (list (save-excursion
          (skip-chars-backward "^ ")
          (skip-chars-forward "-")
          (point))
        (point)
        picpocket-tag-completion-table))

(defun picpocket-tags-string-to-edit (tags)
  (when tags
    (concat (mapconcat #'symbol-name tags " ") " ")))

(defun picpocket-set-filter (filter-string)
  "Enter the current picpocket filter.
The filter is a list of tags.  Only pictures with all the tags in
the filter is shown.  To enter multiple tags separate them with
spaces.

If `picpocket-filter-consider-dir-as-tag' is non-nil also the
containing directory counts as a tag as far as the filter is
concerned.

When called from Lisp the argument FILTER-STRING is a
space-separated string."
  (interactive (list (picpocket-read-tags
                      "Show only pictures with these tags: ")))
  (picpocket-command
    (picpocket-do-set-filter (mapcar #'intern (split-string filter-string)))
    (if picpocket-filter
        (message "Filter is %s" (picpocket-format-tags picpocket-filter))
      (message "No filter"))
    (picpocket-old-update-buffer)))

(defun picpocket-do-set-filter (filter)
  (setq picpocket-filter filter)
  (picpocket-reset-filter-counters))

(defun picpocket-reset-filter-counters ()
  (setq picpocket-filter-index nil
        picpocket-compute-filter-index-from-scratch t
        picpocket-filter-match-count nil
        picpocket-filter-match-count-done nil))

(defun picpocket-filter-match-p (pic)
  (cl-subsetp (picpocket-remove-hyphens
               picpocket-filter)
              (picpocket-remove-hyphens
               (append (picpocket-tags pic)
                       (picpocket-extra-tags-for-filter pic)))))

(defun picpocket-remove-hyphens (list-or-symbol)
  (cond ((not picpocket-filter-ignore-hyphens)
         list-or-symbol)
        ((consp list-or-symbol)
         (mapcar #'picpocket-remove-hyphens list-or-symbol))
        (t (let ((str (symbol-name list-or-symbol)))
             (if (string-match "[-_]" str)
                 (let ((chars (string-to-list str)))
                   (intern (apply #'string (delete ?- (delete ?_ chars)))))
               list-or-symbol)))))

(defun picpocket-extra-tags-for-filter (pic)
  (mapcar #'intern
          (pcase picpocket-filter-consider-dir-as-tag
            (`nil nil)
            (`all (split-string (picpocket-dir pic) "/" t))
            (_ (list (file-name-nondirectory
                      (directory-file-name (picpocket-dir pic))))))))

(defun picpocket-no-file (&optional direction)
  (user-error (picpocket-join "No"
                              direction
                              "file"
                              (when picpocket-filter
                                (format "match filter %s" picpocket-filter)))))

(defun picpocket-set-filter-by-keystroke ()
  "Show only pictures having the tag in the current filter."
  (interactive)
  (picpocket-command
    (picpocket-set-filter (picpocket-read-key "filtering tag"))))


(defun picpocket-jump ()
  "Jump to picture specified by file-name or index number."
  (interactive)
  (picpocket-command
    (let ((nr-or-file-name (completing-read "Jump to index or file-name: "
                                            (picpocket-mapcar
                                             'picpocket-file))))
      (or (picpocket-jump-to-index nr-or-file-name)
          (picpocket-jump-to-file nr-or-file-name))
      (picpocket-old-update-buffer))))

(defun picpocket-jump-to-index (string)
  (when (string-match "^[0-9]+$" string)
    (let ((index (string-to-number string)))
      (picpocket-when-let (pic (picpocket-pic-by-index index))
        (picpocket-list-set-pos (make-picpocket-pos :current pic
                                                    :index index))
        t))))


(defun picpocket-pic-by-index (n)
  (and (< 0 n)
       (<= n picpocket-list-length)
       (cl-loop for pic on picpocket-list
                repeat (1- n)
                finally return pic)))

(defun picpocket-jump-to-file (file)
  (let ((pos-list (picpocket-pos-list-by-file file)))
    (cond ((null pos-list)
           (user-error "Picture not found (%s)" file))
          ((eq 1 (length pos-list))
           (picpocket-list-set-pos (car pos-list)))
          (t
           (let ((prompt (format "%s is available in %s directories.  Select: "
                                 file (length pos-list))))
             (picpocket-list-set-pos (picpocket-select-pos-by-dir pos-list
                                                                  prompt))
             t)))))

(defun picpocket-pos-list-by-file (file)
  (cl-loop for pic on picpocket-list
           for index = 1 then (1+ index)
           when (equal (picpocket-file pic) file)
           collect (make-picpocket-pos :current pic
                                       :index index)))

(defun picpocket-select-pos-by-dir (pos-list prompt)
  (let* ((dirs (cl-loop for pos in pos-list
                        collect (picpocket-dir (picpocket-pos-current pos))))
         (dir (completing-read prompt dirs nil t)))
    (cl-loop for pos in pos-list
             when (equal dir (picpocket-dir (picpocket-pos-current pos)))
             return pos)))


(defun picpocket-gimp-open ()
  "Run gimp on the current picture."
  (interactive)
  (and picpocket-gimp-executable
       (not (file-name-absolute-p picpocket-gimp-executable))
       (setq picpocket-gimp-executable
             (executable-find picpocket-gimp-executable)))
  (unless (picpocket-absfile)
    (user-error "No current picture"))
  (start-process picpocket-gimp-executable
                 nil
                 picpocket-gimp-executable
                 (picpocket-absfile)))


;;; Pic double-linked list functions

;; These will call tag handling functions.

(defun picpocket-absfile (&optional pic)
  (unless pic
    (setq pic picpocket-current))
  (concat (picpocket-dir pic) (picpocket-file pic)))

(defun picpocket-set-absfile (pic absfile)
  (picpocket-set-file pic (file-name-nondirectory absfile))
  (picpocket-set-dir pic (file-name-directory absfile)))

(defun picpocket-make-pic (path)
  (make-picpocket-pic :dir (file-truename (file-name-directory path))
                      :file (file-name-nondirectory path)))

(defun picpocket-list-reset ()
  (setq picpocket-list nil
        picpocket-list-length 0)
  (picpocket-list-set-pos (make-picpocket-pos))
  (picpocket-do-set-filter nil))

(defun picpocket-list-set-pos (pos)
  (let ((inhibit-quit t))
    (setq picpocket-current (picpocket-pos-current pos))
    (setq picpocket-index (or (picpocket-pos-index pos)
                              (picpocket-calculate-index picpocket-current)))
    (setq picpocket-filter-index (picpocket-pos-filter-index pos))
    (setq picpocket-compute-filter-index-from-scratch
          (null picpocket-filter-index))))

(defun picpocket-mapc (f)
  (cl-loop for pic on picpocket-list
           when (picpocket-filter-match-p pic)
           do (funcall f pic)))

(defun picpocket-mapcar (f)
  (cl-loop for pic on picpocket-list
           when (picpocket-filter-match-p pic)
           collect (funcall f pic)))

(defun picpocket-list-search (absfile)
  (cl-loop for pic on picpocket-list
           when (equal absfile (picpocket-absfile pic))
           return pic))

(defun picpocket-calculate-index (&optional current)
  (cl-loop for pic on picpocket-list
           count 1
           until (eq pic (or current picpocket-current))))

(defun picpocket-list-delete (&optional pic filter-match-cell)
  (setq pic (or pic
                picpocket-current))
  (let ((filter-match (if filter-match-cell
                          (car filter-match-cell)
                        (picpocket-filter-match-p pic))))
    (clear-image-cache (picpocket-absfile pic))
    (setq picpocket-list-length (1- picpocket-list-length))
    (if (picpocket-prev pic)
        (setcdr (picpocket-prev pic) (cdr pic))
      (setq picpocket-list (cdr pic)))
    (if (cdr pic)
        (progn
          (picpocket-set-prev (cdr pic) (picpocket-prev pic))
          (when (eq picpocket-current pic)
            (picpocket-list-set-pos (make-picpocket-pos
                                     :current (cdr pic)
                                     :index picpocket-index))))
      (if (picpocket-prev pic)
          (when (eq picpocket-current pic)
            (picpocket-list-set-pos (make-picpocket-pos
                                     :current (picpocket-prev pic)
                                     :index (1- picpocket-index))))
        (picpocket-list-reset)))
    (and picpocket-filter
         filter-match
         (if picpocket-filter-match-count-done
             (cl-decf picpocket-filter-match-count)
           (setq picpocket-filter-match-count nil)))))


;; (defun picpocket-list-insert-after-current (p)
;; "Insert P after current pic."
;; (let ((inhibit-quit t)
;; (prev picpocket-current)
;; (next (cdr picpocket-current)))
;; (setq picpocket-list-length (1+ picpocket-list-length))
;; (setf (picpocket-pic-prev p) prev)
;; (picpocket-list-set-pos (make-picpocket-pos :current (cons p next)
;; :index (1+ picpocket-index)))
;; (if prev
;; (setcdr prev picpocket-current)
;; (setq picpocket-list picpocket-current))
;; (when next
;; (picpocket-set-prev next picpocket-current))
;; (and picpocket-filter
;; (picpocket-filter-match-p picpocket-current)
;; (cl-incf picpocket-filter-match-count))))

(defun picpocket-list-insert-before-current (p)
  "Insert P before current pic."
  (let ((inhibit-quit t)
        (prev (and picpocket-current
                   (picpocket-prev picpocket-current)))
        (next picpocket-current))
    (setq picpocket-list-length (1+ picpocket-list-length))
    (setf (picpocket-pic-prev p) prev)
    (picpocket-list-set-pos (make-picpocket-pos :current (cons p next)
                                                :index (max 1 picpocket-index)))
    (if prev
        (setcdr prev picpocket-current)
      (setq picpocket-list picpocket-current))
    (when next
      (picpocket-set-prev next picpocket-current))
    (and picpocket-filter
         (picpocket-filter-match-p picpocket-current)
         (cl-incf picpocket-filter-match-count))))


(defun picpocket-create-picpocket-list (files &optional selected-file)
  (picpocket-list-reset)
  (setq picpocket-list
        (cl-loop for path in files
                 if (file-exists-p path)
                 collect (picpocket-make-pic path)
                 else do (message "%s do not exist" path)))
  (cl-loop for pic on picpocket-list
           with prev = nil
           do (picpocket-set-prev pic prev)
           do (setq prev pic))
  (setq picpocket-list-length (length picpocket-list))
  (picpocket-list-set-pos (or (and selected-file
                                   (string-match (picpocket-picture-regexp)
                                                 selected-file)
                                   (cl-loop for pic on picpocket-list
                                            for index = 1 then (1+ index)
                                            when (equal selected-file
                                                        (picpocket-absfile pic))
                                            return (make-picpocket-pos
                                                    :current pic
                                                    :index index)))
                              (make-picpocket-pos :current picpocket-list
                                                  :index 1))))



(defvar picpocket-done-dirs nil)
(defvar picpocket-file-count 0)

(defun picpocket-file-list (dir)
  (let ((picpocket-file-count 0)
        (picpocket-done-dirs nil))
    (prog1
        (picpocket-file-list2 (directory-file-name (file-truename dir)))
      (message "Found %s pictures" picpocket-file-count))))

(defun picpocket-file-list2 (dir)
  (push dir picpocket-done-dirs)
  (condition-case err
      (let ((files (picpocket-sort-files (directory-files dir nil "[^.]" t)))
            path pic-files sub-files subdirs)
        (dolist (file files)
          (setq path (expand-file-name file dir))
          (if (file-directory-p path)
              (push path subdirs)
            (when (string-match (picpocket-picture-regexp) file)
              (push path pic-files)
              (when (zerop (% (cl-incf picpocket-file-count) 100))
                (message "Found %s pictures so far %s"
                         picpocket-file-count
                         (if picpocket-recursive
                             (format "(%s)" dir)
                           ""))))))
        (setq pic-files (nreverse pic-files))
        (when picpocket-recursive
          (dolist (subdir subdirs)
            (when (or picpocket-follow-symlinks
                      (not (file-symlink-p subdir)))
              (let ((true-subdir (directory-file-name
                                  (file-truename subdir))))
                (unless (or (picpocket-dot-file-p subdir)
                            (member true-subdir picpocket-done-dirs))
                  (setq sub-files (append (picpocket-file-list2 true-subdir)
                                          sub-files)))))))
        (append pic-files sub-files))
    (file-error (progn
                  (warn "Failed to access %s (%s)" dir err)
                  nil))))

(defun picpocket-sort-files (files)
  (sort files #'picpocket-file-name-lessp))

(defun picpocket-file-name-lessp (a b)
  (cond ((and (equal "" a)
              (not (equal "" b)))
         t)
        ((equal "" b)
         nil)
        ((and (picpocket-string-start-with-number-p a)
              (picpocket-string-start-with-number-p b))
         (pcase-let ((`(,a-number . ,a-rest) (picpocket-parse-number a))
                     (`(,b-number . ,b-rest) (picpocket-parse-number b)))
           (if (= a-number b-number)
               (picpocket-file-name-lessp a-rest b-rest)
             (< a-number b-number))))
        (t (let ((a-char (aref a 0))
                 (b-char (aref b 0)))
             (if (= a-char b-char)
                 (picpocket-file-name-lessp (substring a 1) (substring b 1))
               (< a-char b-char))))))

(defun picpocket-parse-number (string)
  (picpocket-do-parse-number string ""))

(defun picpocket-do-parse-number (string acc)
  (if (picpocket-string-start-with-number-p string)
      (picpocket-do-parse-number (substring string 1)
                                 (concat acc (substring string 0 1)))
    (cons (string-to-number acc) string)))

(defun picpocket-string-start-with-number-p (s)
  (unless (zerop (length s))
    (picpocket-char-is-number-p (aref s 0))))

(defun picpocket-char-is-number-p (c)
  (and (>= c ?0)
       (<= c ?9)))

;; (defun picpocket-parse-number (string)
;; (cl-loop for i from 0 to (1- (length string))
;; while (picpocket-char-is-number-p (aref string i))
;; finally return (cons (string-to-number (substring string 0 i))
;; (substring string i))))

(defun picpocket-dot-file-p (file)
  "Return t if the FILE's name start with a dot."
  (eq (elt (file-name-nondirectory file) 0) ?.))



;;; Tag database interface functions

;; This layer translates from struct picpocket-pic to a sha1 checksum.
;; This checksum is refered to as sha and is used as index in the
;; database below.

(defun picpocket-tags (&optional pic)
  (picpocket-db-tags (picpocket-sha-force pic)))

(defun picpocket-tags-set (pic new-tags)
  (let ((match-before (picpocket-filter-match-p pic)))
    (picpocket-db-tags-set (picpocket-sha-force pic)
                           (picpocket-absfile pic)
                           new-tags)
    (let ((match-after (picpocket-filter-match-p pic)))
      (when (picpocket-xor match-before match-after)
        (setq picpocket-compute-filter-index-from-scratch t
              picpocket-filter-index nil)
        (if picpocket-filter-match-count-done
            (cl-incf picpocket-filter-match-count (if match-after 1 -1))
          (setq picpocket-filter-match-count nil
                picpocket-filter-match-count-done nil))))))

(defun picpocket-xor (a b)
  (not (eq (not a) (not b))))


(defun picpocket-tags-move-file (pic old-file new-file)
  (picpocket-db-tags-move-file (picpocket-sha-force pic) old-file new-file))

(defun picpocket-tags-copy-file (pic new-file)
  (picpocket-db-tags-copy-file (picpocket-sha-force pic) new-file))

(defun picpocket-tags-delete-file (pic deleted-file)
  (picpocket-db-tags-delete-file (picpocket-sha-force pic) deleted-file))


(defun picpocket-sha-force (pic)
  "Return the sha1 checksum of PIC.
The checksum will be computed if not already available.
Also, if there is a matching entry in the database with tags
then the file of PIC will be added to that entry."
  (or (picpocket-sha pic)
      (picpocket-save-sha-in-pic-and-db pic)))


(defun picpocket-save-sha-in-pic-and-db (pic)
  (let* ((file (picpocket-absfile pic))
         (sha (picpocket-sha1sum file))
         (inhibit-quit t))
    (picpocket-set-sha pic sha)
    (picpocket-db-tags-add-file sha file)
    sha))


(defun picpocket-sha1sum (file)
  (if (or picpocket-sha1sum-executable
          (setq picpocket-sha1sum-executable (executable-find "sha1sum")))
      (with-temp-buffer
        (unless (zerop (call-process picpocket-sha1sum-executable
                                     nil t nil file))
          (error "Failed to compute sha for %s" file))
        (goto-char (point-min))
        (skip-chars-forward "0-9a-f")
        (unless (eq (point) 41)
          (error "Unrecognized output from sha1sum for %s" file))
        (buffer-substring (point-min) (point)))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (sha1 (current-buffer)))))

;;; Tag database internal functions

;; This layer translates from sha to data.  This layer knows about the
;; representation of the database entries.  The database maps from sha
;; to a plist with the following keys:
;;
;; :files - list of truename file-names with this sha.
;; :tags - list of tag symbols.
;;

(defmacro picpocket-with-db (sha var-list &rest body)
  "Convenience macro for tag database access.
SHA is the sha1sum of the picture to lookup.  VAR-LIST contains
one or more of the symbols plist, tags and files.  BODY will be
evaluated with the symbols in VAR-LIST bound to their values in
the database for the given SHA."
  (declare (indent 2)
           (debug (form form body)))
  (let ((invalid (cl-set-difference var-list '(plist files tags))))
    (when invalid
      (error "Invalid symbols in picpocket-with-db var-list: %s"
             invalid)))
  `(let* ((plist (picpocket-db-get ,sha))
          ,(if (memq 'files var-list)
               '(files (plist-get plist :files))
             'ignored)
          ,(if (memq 'tags var-list)
               '(tags (plist-get plist :tags))
             'ignored))
     ,@body))

(defun picpocket-db-tags (sha)
  (picpocket-with-db sha (tags)
    tags))

(defun picpocket-db-files (sha)
  (picpocket-with-db sha (files)
    files))

(defun picpocket-db-tags-add-file (sha file)
  (picpocket-with-db sha (plist files tags)
    (when tags
      (unless (member file files)
        (setq plist (plist-put plist :files (cons file files)))
        (picpocket-db-put sha plist)))))

(defun picpocket-db-tags-set (sha file new-tags)
  (picpocket-with-db sha (plist files)
    (if new-tags
        (picpocket-db-put sha (list :files (picpocket-add-to-list file files)
                                    :tags new-tags))
      (when plist
        (picpocket-db-remove sha)))))

(defun picpocket-add-to-list (element list)
  (cl-adjoin element list :test 'equal))

(defun picpocket-db-tags-move-file (sha old-file new-file)
  (picpocket-with-db sha (plist files)
    (when plist
      (let ((new-files (picpocket-add-to-list new-file
                                              (delete old-file files))))
        (picpocket-db-put sha (plist-put plist :files new-files))))))

(defun picpocket-db-tags-copy-file (sha new-file)
  (picpocket-with-db sha (plist files)
    (when plist
      (unless (member new-file files)
        (picpocket-db-put sha
                          (plist-put plist :files (cons new-file files)))))))

(defun picpocket-db-tags-delete-file (sha deleted-file)
  (picpocket-with-db sha (plist files)
    (when plist
      (let ((new-files (delete deleted-file files)))
        (if new-files
            (picpocket-db-put sha (plist-put plist :files new-files))
          (picpocket-db-remove sha))))))


;;; Tag database management

(defvar picpocket-db-mode-map nil)
(defvar picpocket-db nil)
(defvar picpocket-db-update-buffer "*picpocket-db-update*")

(defun picpocket-do-db-update ()
  (when (get-buffer picpocket-db-update-buffer)
    (kill-buffer picpocket-db-update-buffer))
  (with-current-buffer (get-buffer-create picpocket-db-update-buffer)
    (switch-to-buffer (current-buffer))
    (picpocket-really-do-db-update)))

(defun picpocket-really-do-db-update ()
  (let* ((alist (picpocket-db-traverse))
         (sha-changed (cdr (assq :sha-changed alist)))
         (unique-file-missing (cdr (assq :unique-file-missing alist)))
         (redundant-file-missing (cdr (assq :redundant-file-missing alist)))
         buffer-read-only)
    (buffer-disable-undo)
    (erase-buffer)
    (setq truncate-lines t)
    (insert "\n")

    (setq picpocket-db-mode-map (make-sparse-keymap))

    ;; exif -c -o x.jpg --ifd=EXIF -t0x9286 --set-value=foo x.jpg
    (if (null sha-changed)
        (picpocket-emph "No files with changed sha1 checksum found.\n\n")
      (let ((n (length sha-changed)))
        (picpocket-db-update-command [?s]
          (lambda ()
            (insert (picpocket-emph "The following %s file%s have "
                                    n (picpocket-plural-s n))
                    (picpocket-emph "changed %s sha1 checksum.\n"
                                    (picpocket-plural-its-their n))
                    "Type "
                    (picpocket-emph "s")
                    " to update picpocket database with the new"
                    " sha1 checksum values.\n\n")
            (picpocket-insert-file-list sha-changed))
          (lambda ()
            (picpocket-update-sha sha-changed)
            (insert
             (picpocket-emph "Sha1 checksums for %s file%s were updated.\n"
                             n (picpocket-plural-s n)))))))

    (if (null redundant-file-missing)
        (insert (picpocket-emph "No missing redundant files.\n\n"))
      (let ((n (length redundant-file-missing)))
        (picpocket-db-update-command [?r]
          (lambda ()
            (insert (picpocket-emph "The following %s redundant file name%s"
                                    n (picpocket-plural-s n))
                    (picpocket-emph " were not found on disk.\n")
                    "Their database entries contains at least one other"
                    " file that do exist.\n"
                    "Type "
                    (picpocket-emph "r")
                    " to remove these file names from the picpocket database.\n"
                    "(Their database entries will not be removed.)\n\n")
            (picpocket-insert-file-list redundant-file-missing))
          (lambda ()
            (picpocket-remove-file-names-in-db redundant-file-missing)
            (insert (picpocket-emph "Removed %s missing "
                                    n)
                    (picpocket-emph "redundant file name%s from database."
                                    (picpocket-plural-s n)))))))

    (if (null unique-file-missing)
        (insert (picpocket-emph "No missing unique files.\n\n"))
      (let ((n (length unique-file-missing)))
        (picpocket-db-update-command [?u]
          (lambda ()
            (insert (picpocket-emph "The following %s unique file name%s"
                                    n (picpocket-plural-s n))
                    (picpocket-emph " were not found on disk.\n")
                    "Their database entries do not contain any"
                    " existing files.\n"
                    "Type "
                    (picpocket-emph "u")
                    " to remove these entries from the picpocket database.\n"
                    "(Their database entries will be removed.)\n\n")
            (picpocket-insert-file-list unique-file-missing))
          (lambda ()
            (picpocket-remove-file-names-in-db unique-file-missing)
            (insert (picpocket-emph "Removed %s missing unique file name%s"
                                    n (picpocket-plural-s n))
                    (picpocket-emph " and their entries from database."))))))

    (goto-char (point-min))
    (picpocket-db-mode)))

(defun picpocket-db-update-command (key text-function command-function)
  "Create a command and bind it to KEY.

The TEXT-FUNCTION will be called immediately and is supposed to
insert some text describing what COMMAND-FUNCTION does.  When KEY
is typed that text will be deleted and the COMMAND-FUNCTION will
be called.  COMMAND-FUNCTION may also insert some text and that
will end up replacing the deleted text."
  (declare (indent defun))
  (let ((start (point)))
    (funcall text-function)
    (insert "\n")
    (let ((overlay (make-overlay start (1- (point)))))
      (define-key picpocket-db-mode-map key
        (lambda ()
          (interactive)
          (if (null (overlay-start overlay))
              (message "Nothing more to do.")
            (let (buffer-read-only)
              (goto-char (overlay-start overlay))
              (delete-region (overlay-start overlay)
                             (overlay-end overlay))
              (delete-overlay overlay)
              (funcall command-function)
              (insert "\n"))))))))


(defun picpocket-update-sha (sha-changed)
  (cl-loop for (file new-tags sha new-sha) in sha-changed
           do (picpocket-with-db new-sha (plist files tags)
                (picpocket-db-put new-sha (list :files
                                                (picpocket-add-to-list file
                                                                       files)
                                                :tags
                                                (cl-union tags
                                                          new-tags))))
           do (picpocket-with-db sha (plist files)
                (let ((remaining-files (delete file files)))
                  (if remaining-files
                      (picpocket-db-put sha (plist-put plist
                                                       :files
                                                       remaining-files))
                    (picpocket-db-remove sha))))))

(defun picpocket-remove-file-names-in-db (missing-files)
  (cl-loop for (file ignored sha) in missing-files
           do (picpocket-with-db sha (plist files)
                (let ((new-files (delete file files)))
                  (if new-files
                      (picpocket-db-put sha (plist-put plist
                                                       :files
                                                       new-files))
                    (picpocket-db-remove sha))))))

(defun picpocket-emph (format &rest args)
  (propertize (apply #'format format args)
              'face 'bold
              'font-lock-face 'bold))

(defun picpocket-insert-file-list (list)
  (dolist (entry list)
    (insert "  "
            (picpocket-join (car entry)
                            (picpocket-format-tags (cadr entry)))
            "\n")))

(define-derived-mode picpocket-db-mode special-mode "picpocket-db"
  (define-key picpocket-db-mode-map [?g] #'picpocket-db-update)
  (setq truncate-lines t))

(defun picpocket-db-traverse ()
  (picpocket-db-init)
  (let ((progress (make-progress-reporter "Traversing database "
                                          0
                                          (hash-table-count picpocket-db)))
        (i 0)
        sha-changed
        unique-file-missing
        redundant-file-missing)
    (maphash (lambda (sha plist)
               (let ((tags (plist-get plist :tags))
                     (files (plist-get plist :files))
                     missing-files existing-files)
                 (dolist (file files)
                   (if (file-exists-p file)
                       (let ((new-sha (picpocket-sha1sum file)))
                         (unless (equal sha new-sha)
                           (push (list file tags sha new-sha) sha-changed))
                         (push file existing-files))
                     (push (list file tags sha) missing-files)))
                 (when missing-files
                   (if existing-files
                       (setq redundant-file-missing
                             (append missing-files
                                     redundant-file-missing))
                     (setq unique-file-missing
                           (append missing-files
                                   unique-file-missing))))
                 (cl-incf i)
                 (progress-reporter-update progress i)))
             picpocket-db)
    (progress-reporter-done progress)
    (list (cons :sha-changed sha-changed)
          (cons :unique-file-missing unique-file-missing)
          (cons :redundant-file-missing redundant-file-missing))))

(defun picpocket-db-compile-tags-for-completion ()
  (maphash (lambda (_ plist)
             (dolist (tag (plist-get plist :tags))
               (intern (symbol-name tag)
                       picpocket-tag-completion-table)))
           picpocket-db))



;;; Database

;; This "database" stores a hash table in a text file.
;; The file format is:
;;
;; (version VERSION)
;; (format FORMAT)
;; (data-version DATA-VERSION)
;;
;; where VERSION is the integer version of the picp database and
;; DATA-VERSION is the integer version of the stored data.  FORMAT is
;; either list or hash-table.  In case of hash-table format the next
;; value is the hash table itself.  The hash table maps sha1 checksums
;; to data entries.  In case of list format lists like (SHA DATA)
;; follows for every hash table entry.
;;
;; TODO
;; SQL support (no, not really)
;; NoSQL support (don't we have that already?)


(defvar picpocket-db-dir (concat user-emacs-directory "picpocket/"))

(defconst picpocket-db-version 1)

(defvar picpocket-db nil)

(defvar picpocket-db-remove-corrupted-files nil)

(defvar picpocket-db-format 'list
  "Either `list' or `hash-table'.
`hash-table' is faster.
`list' makes the database files more readable.")

(defvar picpocket-db-valid-formats '(list hash-table))

(defvar picpocket-db-journal-size 0)


(defun picpocket-db-journal-size ()
  picpocket-db-journal-size)

(defun picpocket-db-get (sha)
  (gethash sha picpocket-db))

(defun picpocket-db-put (sha data)
  (let ((inhibit-quit t)
        (coding-system-for-write 'utf-8-unix)
        print-level print-length)
    (if data
        (puthash sha data picpocket-db)
      (remhash sha picpocket-db))
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (unless (file-exists-p (picpocket-db-file :journal))
          (picpocket-db-insert-header)
          (prin1 (list 'version picpocket-db-version))
          (insert "\n"))
        (picpocket-db-insert-list-item (list sha data))
        (write-region (point-min)
                      (point-max)
                      (picpocket-db-file :journal)
                      t
                      'silent)))
    (cl-incf picpocket-db-journal-size)))

(defun picpocket-db-insert-header ()
  (insert ";; -*- coding: utf-8-unix; no-byte-compile: t -*-\n")
  (insert ";; This file is auto-generated by picpocket.el in Emacs.\n")
  (insert ";; If you plan to manually edit this file you should first\n")
  (insert ";; kill the *picpocket* buffer in Emacs.  Otherwise your\n")
  (insert ";; edits may become overwritten.\n\n"))

(defun picpocket-db-insert-list-item (item)
  (prin1 item)
  (insert "\n"))

(defun picpocket-db-remove (sha)
  (picpocket-db-put sha nil))

(defun picpocket-db-clear ()
  (when (hash-table-p picpocket-db)
    (clrhash picpocket-db))
  (setq picpocket-db (picpocket-db-new-hash-table)))

(defun picpocket-db-count ()
  (hash-table-count picpocket-db))

(defun picpocket-db-init ()
  (make-directory picpocket-db-dir t)
  (let ((db (picpocket-db-read nil))
        (old (picpocket-db-read :old)))
    (setq picpocket-db
          (cond ((and (hash-table-p db) (null old))
                 db)
                ((and (null db) (null old))
                 (picpocket-db-new-hash-table))
                ((and (not (hash-table-p db)) (hash-table-p old))
                 (picpocket-warn "Recovering with picpocket old file")
                 old)
                ((and (hash-table-p db) (hash-table-p old))
                 (picpocket-warn "Ignoring spurious picpocket old file (%s)"
                                 (picpocket-db-file :old))
                 (when picpocket-db-remove-corrupted-files
                   (delete-file (picpocket-db-file :old)))
                 db)
                ((and (hash-table-p db) (eq old 'error))
                 (picpocket-warn "Ignoring corrupt picpocket old file (%s)"
                                 (picpocket-db-file :old))
                 (when picpocket-db-remove-corrupted-files
                   (delete-file (picpocket-db-file :old)))
                 db)
                (t
                 (message "(hash-table-p db) %s" (hash-table-p db))
                 (message "(hash-table-p old) %s" (hash-table-p old))
                 (message "db %s" db)
                 (message "old %s" old)
                 ;; PENDING - kill picpocket buffer?
                 (error "Cannot recover picpocket database"))))
    (when (file-exists-p (picpocket-db-file :journal))
      (picpocket-db-read-journal)
      (picpocket-db-save))))

(defun picpocket-db-new-hash-table ()
  (make-hash-table :test 'equal))

(defun picpocket-db-read (file-symbol)
  (let ((db-file (picpocket-db-file file-symbol)))
    (when (file-exists-p db-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents db-file)
            (let* ((standard-input (current-buffer))
                   (version (cadr (read)))
                   (format (cadr (read)))
                   (ignored (cadr (read))))
              (unless (equal version picpocket-db-version)
                (error "Unknown picpocket database version %s" version))
              (cl-case format
                (hash-table (picpocket-db-read-hash-table))
                (list (picpocket-db-read-list))
                (t (error "Unknown format %s in %s (%s)"
                          format
                          db-file
                          (picpocket-db-valid-formats-string))))))
        (error (picpocket-warn "Failed to read %s - %s" db-file err)
               'error)))))

(defun picpocket-db-read-hash-table ()
  (let ((db (read)))
    (unless (hash-table-p db)
      (error "Not a proper hash table"))
    db))

(defun picpocket-db-read-list ()
  (picpocket-db-read-and-hash-list (picpocket-db-new-hash-table)))

(defun picpocket-db-read-and-hash-list (hash-table &optional counter)
  (condition-case ignored
      (while t
        (cl-destructuring-bind (key value) (read)
          (if value
              (puthash key value hash-table)
            (remhash key hash-table))
          (when counter
            (set counter (1+ (symbol-value counter))))))
    (end-of-file))
  hash-table)


(defun picpocket-db-save ()
  (let ((db-file (picpocket-db-file))
        (tmp-file (picpocket-db-file :tmp))
        (old-file (picpocket-db-file :old))
        (journal-file (picpocket-db-file :journal)))
    (with-temp-file tmp-file
      (set-buffer-file-coding-system 'utf-8-unix)
      (let ((standard-output (current-buffer))
            (print-level print-length))
        (picpocket-db-insert-header)
        (prin1 (list 'version picpocket-db-version))
        (insert "\n")
        (prin1 (list 'format picpocket-db-format))
        (insert "\n")
        (prin1 (list 'data-version 1))
        (insert "\n")
        (cl-case picpocket-db-format
          (hash-table (picpocket-db-save-hash-table))
          (list (picpocket-db-save-list))
          (t (error "Unknown value of picpocket-db-format %s (%s)"
                    picpocket-db-format (picpocket-db-valid-formats-string))))
        (insert "\n")))
    (let ((inhibit-quit t))
      (when (file-exists-p db-file)
        (copy-file db-file old-file))
      (copy-file tmp-file db-file t)
      (delete-file tmp-file)
      (when (file-exists-p old-file)
        (delete-file old-file))
      (when (file-exists-p journal-file)
        (delete-file journal-file))
      (setq picpocket-db-journal-size 0))))




(defun picpocket-db-valid-formats-string ()
  (format "should be %s"
          (mapconcat #'symbol-name picpocket-db-valid-formats " or ")))


(defun picpocket-db-save-hash-table ()
  (prin1 picpocket-db)
  (insert "\n"))

(defun picpocket-db-dump ()
  (with-temp-buffer
    (picpocket-db-save-list)
    (buffer-string)))

(defun picpocket-db-save-list ()
  (let (list)
    (maphash (lambda (key value)
               (push (list key value) list))
             picpocket-db)
    ;; PENDING - optionally sort the list.
    (dolist (element list)
      (picpocket-db-insert-list-item element))))

(defun picpocket-db-file (&optional symbol)
  (concat picpocket-db-dir
          "picpocket-db"
          (when symbol
            (concat "-" (substring (symbol-name symbol) 1)))
          ".el"))

(defun picpocket-db-read-journal ()
  (setq picpocket-db-journal-size 0)
  (let ((journal-file (picpocket-db-file :journal)))
    (when (file-exists-p journal-file)
      (with-temp-buffer
        (insert-file-contents journal-file)
        (let* ((standard-input (current-buffer))
               (version (cadr (read))))
          (if (not (equal picpocket-db-version version))
              (picpocket-warn
               "Ignoring picpocket journal %s of unknown version %s"
               journal-file version)
            (picpocket-db-read-and-hash-list picpocket-db
                                             'picpocket-db-journal-size)))))))

(defun picpocket-warn (format &rest args)
  (let ((format (concat "picpocket-warn: " format)))
    (apply #'message format args)
    (unless picpocket-demote-warnings
      (apply #'warn format args))))



;;; Idle timer functions

(defvar picpocket-timers nil)
(defvar picpocket-idle-timer-work-functions
  '((picpocket-update-current-bytes 0.1)
    (picpocket-maybe-save-journal 0.2)
    (picpocket-look-ahead-next 0.2)
    ;; PENDING
    ;; (picpocket-look-ahead-more 2)
    (picpocket-compute-filter-index 0.5)
    (picpocket-compute-filter-match-count 1)
    (picpocket-traverse-pic-list 3)
    (picpocket-save-journal 60)
    (picpocket-update-header picpocket-update-header-seconds)))
(defvar picpocket-inhibit-timers nil)
(defvar picpocket-idle-timer-deadline 0.1)




(defun picpocket-init-timers ()
  (picpocket-cancel-timers)
  (unless picpocket-inhibit-timers
    (add-hook 'kill-buffer-hook #'picpocket-cancel-timers nil t)
    (setq picpocket-timers
          (cl-loop for (f s) in picpocket-idle-timer-work-functions
                   for sec = (if (functionp s)
                                 (funcall s)
                               s)
                   collect (run-with-idle-timer sec
                                                t
                                                #'picpocket-run-idle-timer
                                                f)))))

(defun picpocket-cancel-timers ()
  (dolist (timer picpocket-timers)
    (cancel-timer timer))
  (setq picpocket-timers nil)
  (dolist (ft picpocket-idle-timer-work-functions)
    (let* ((f (car ft))
           (resume-timer (get f 'picpocket-resume-timer)))
      (when resume-timer
        (cancel-timer resume-timer)))))


(defun picpocket-run-idle-timer (f &optional state)
  (let ((debug-on-error picpocket-debug-idle-timers)
        (resume-timer (get f 'picpocket-resume-timer))
        (buffer (get-buffer picpocket-buffer)))
    (when (timerp resume-timer)
      (cancel-timer resume-timer))
    (cond (picpocket-inhibit-timers
           (picpocket-cancel-timers))
          (buffer
           (with-current-buffer buffer
             (picpocket-run-idle-timer-in-buffer f state)))
          (t (picpocket-cancel-timers)))))

(defun picpocket-run-idle-timer-in-buffer (f state)
  (cond ((null picpocket-list)
         (picpocket-cancel-timers))
        ((null picpocket-db)
         (message "Cancel idle timers since picpocket-db is nil.")
         (picpocket-cancel-timers))
        ((not (file-directory-p default-directory))
         (message "Closing picpocket buffer since %s do not exist any more."
                  default-directory)
         (kill-buffer picpocket-buffer)
         (picpocket-cancel-timers))
        (t
         (condition-case err
             (funcall f (picpocket-make-deadline-function f) state)
           (quit (message "picpocket-run-idle-timer %s interrupted by quit" f)
                 (signal (car err) (cdr err)))))))

(defun picpocket-make-deadline-function (f)
  (let ((start (current-time)))
    (lambda (state)
      (when (time-less-p (seconds-to-time picpocket-idle-timer-deadline)
                         (time-subtract (current-time) start))
        (put f
             'picpocket-resume-timer
             (run-with-idle-timer (time-add (or (current-idle-time)
                                                (seconds-to-time 0))
                                            (seconds-to-time
                                             picpocket-idle-timer-deadline))
                                  nil
                                  #'picpocket-run-idle-timer
                                  f
                                  state))))))


(defun picpocket-traverse-pic-list (deadline-function state)
  (with-current-buffer picpocket-buffer
    (cl-loop for pic on (picpocket-continue-or-start-over state)
             do (picpocket-ensure-cache pic)
             until (funcall deadline-function pic))
    ;; picpocket-size-force may push out the very next pic from the
    ;; emacs image cache.  Call picpocket-look-ahead-next to put it back if
    ;; needed.
    (picpocket-look-ahead-next)))

(defun picpocket-ensure-cache (pic)
  (when (file-exists-p (picpocket-absfile pic))
    (picpocket-sha-force pic)
    (picpocket-size-force pic)
    (picpocket-bytes-force pic)))

(defun picpocket-continue-or-start-over (state)
  "Continue from saved STATE or start from scratch if STATE is invalid.
STATE is the last pic the idle timer worked on.  If that for
example have been deleted from the picture list then the state is
considered invalid and we start from the beginning again."
  (or (and state
           (memq state picpocket-list))
      picpocket-list))

(defun picpocket-compute-filter-match-count (deadline-function state)
  (with-current-buffer picpocket-buffer
    (unless picpocket-filter-match-count
      (setq picpocket-filter-match-count-done nil))
    (when (and picpocket-filter
               (not picpocket-filter-match-count-done))
      (when (or (null picpocket-filter-match-count)
                (and state
                     (not (memq state picpocket-list))))
        (setq state picpocket-list
              picpocket-filter-match-count 0))
      (unless (cl-loop for pic on state
                       do (when (picpocket-filter-match-p pic)
                            (cl-incf picpocket-filter-match-count))
                       when (funcall deadline-function pic)
                       return t)
        (setq picpocket-filter-match-count-done t)))))

(defun picpocket-compute-filter-index (deadline-function state)
  (with-current-buffer picpocket-buffer
    (when (and picpocket-filter
               (null picpocket-filter-index))
      (when (or (null state)
                picpocket-compute-filter-index-from-scratch
                (not (memq state picpocket-list)))
        (setq state (picpocket-first-pos)))
      (cl-loop for pos = state then (picpocket-next-pos pos)
               while pos
               when (eq (picpocket-pos-current pos) picpocket-current)
               return (setq picpocket-filter-index
                            (picpocket-pos-filter-index pos))
               until (funcall deadline-function pos)))))


(defun picpocket-update-current-bytes (&rest ignored)
  (let ((bytes (picpocket-bytes)))
    (picpocket-bytes-force picpocket-current)
    (unless bytes
      (force-mode-line-update))))

(defun picpocket-maybe-save-journal (&rest ignored)
  (when (> (picpocket-db-journal-size) 100)
    (picpocket-db-save)))

(defun picpocket-save-journal (&rest ignored)
  (unless (zerop (picpocket-db-journal-size))
    (picpocket-db-save)))

(defun picpocket-look-ahead-next (&rest ignored)
  (let ((pic (or (picpocket-next-pic) (picpocket-previous-pic))))
    (when (and pic
               (not (eq pic picpocket-last-look-ahead)))
      (picpocket-look-ahead-and-save-time pic)
      (setq picpocket-last-look-ahead pic))))


(defun picpocket-look-ahead-more (deadline-function ignored)
  (let ((s (cadr (picpocket-time (picpocket-look-ahead-more2
                                  deadline-function)))))
    (picpocket-debug s "more")))

(defun picpocket-look-ahead-more2 (deadline-function)
  (cl-loop for pic on picpocket-current
           for count = 0 then (1+ count)
           until (funcall deadline-function nil)
           finally return count
           repeat picpocket-look-ahead-max
           do (picpocket-look-ahead-and-save-time pic)))


;;; Buffer content functions

;; PENDING - Replaced by picpocket-command.....to be removed.....
(defun picpocket-old-update-buffer ())

(defun picpocket-ensure-picpocket-buffer ()
  (unless (and (equal (buffer-name) picpocket-buffer)
               (eq major-mode 'picpocket-mode))
    (message "buffer %s, mode %s" (buffer-name) major-mode)
    (error "%s requires picpocket mode" (or this-command
                                            "This"))))


(defun picpocket-update-buffer ()
  (let ((s (cadr (picpocket-time (picpocket-do-update-buffer)))))
    (picpocket-debug s "%s %s" this-command picpocket-index)))

(defun picpocket-do-update-buffer ()
  (picpocket-ensure-picpocket-buffer)
  (let (buffer-read-only)
    (erase-buffer)
    (if (picpocket-try-set-matching-picture)
        (progn
          (cd (picpocket-dir))
          (if (file-exists-p (picpocket-absfile))
              (picpocket-insert picpocket-current)
            (insert "\n\nFile " (picpocket-file) " no longer exist.\n")))
      (picpocket-no-pictures))
    (force-mode-line-update)))

(defun picpocket-try-set-matching-picture ()
  "Return nil if no matching picture was found."
  (when picpocket-current
    (or (picpocket-filter-match-p picpocket-current)
        (picpocket-when-let (pos (or (picpocket-next-pos)
                                     (picpocket-previous-pos)))
          (picpocket-list-set-pos pos)
          t))))

(defun picpocket-no-pictures ()
  (insert (propertize (format "\n\nNo pictures in list%s.\n\n"
                              (if picpocket-filter
                                  (format " matching filter %s"
                                          picpocket-filter)
                                ""))
                      'face 'bold))
  (when picpocket-filter
    (insert (format "Type %s to edit filter.\n"
                    (picpocket-where-is 'picpocket-set-filter))))
  (and (eq picpocket-entry-function 'picpocket-directory)
       (not picpocket-recursive)
       (insert
        (format "Type %s to recursively include pictures in subdirectories.\n"
                (picpocket-where-is 'picpocket-toggle-recursive))))
  (insert (format "Type %s for dired in %s.\n"
                  (picpocket-where-is 'picpocket-dired)
                  (abbreviate-file-name default-directory))))

(defun picpocket-where-is (command)
  (let ((binding (where-is-internal command overriding-local-map t)))
    (propertize (if binding
                    (key-description binding)
                  (concat "M-x " (symbol-name command)))
                'face 'bold)))



(defun picpocket-create-buffer (files &optional selected-file dir)
  (when selected-file
    (setq selected-file (file-truename
                         (expand-file-name selected-file dir))))
  (let ((old-buffer (get-buffer picpocket-buffer)))
    (when old-buffer
      (kill-buffer old-buffer)))
  (with-current-buffer (get-buffer-create picpocket-buffer)
    (when dir
      (cd dir))
    (picpocket-mode)
    (condition-case err
        (picpocket-create-picpocket-list files selected-file)
      (quit (picpocket-list-reset)
            (signal (car err) (cdr err))))
    (picpocket-update-buffer)
    (if (called-interactively-p 'any)
        (switch-to-buffer (current-buffer))
      (set-buffer (current-buffer)))
    (current-buffer))
  (set-buffer picpocket-buffer))


;;; Image handling

(defun picpocket-insert (pic)
  (if (display-images-p)
      (insert-image (picpocket-create-image pic (picpocket-save-window-size)))
    (insert "\n\nThis display does not support images."))
  (goto-char (point-min)))

(defun picpocket-save-window-size ()
  "Save the current window size.
This is for the benefit of timer functions that do not
necessarily run with the picpocket window selected."
  (setq picpocket-window-size (picpocket-current-window-size)))

(defun picpocket-current-window-size ()
  (cl-destructuring-bind (x0 y0 x1 y1) (window-inside-pixel-edges)
    ;; For some reason Emacs 25.0 refuses to draw image in a right
    ;; margin that seem to be (frame-char-width) pixels wide.
    ;; Therefore subtract that.
    (cons (- x1 x0 (frame-char-width))
          (- y1 y0))))

(defun picpocket-create-image (pic canvas-size)
  (pcase-let ((`(,keyword . ,value) (picpocket-clock
                                      (picpocket-size-param pic canvas-size))))
    (create-image (picpocket-absfile pic)
                  (picpocket-image-type pic)
                  nil
                  :rotation (picpocket-rotation pic)
                  keyword (picpocket-scale value))))

(defun picpocket-image-type (pic-or-filename)
  (let ((filename (if (stringp pic-or-filename)
                      pic-or-filename
                    (picpocket-file pic-or-filename))))
    (unless (string-suffix-p ".svg" filename t)
      'imagemagick)))

(defun picpocket-error-if-rotation-is-unsupported ()
  (unless (eq (picpocket-image-type picpocket-current) 'imagemagick)
    (error "Svg images cannot be rotated")))

(defun picpocket-warn-if-scaling-is-unsupported ()
  (unless (eq (picpocket-image-type picpocket-current) 'imagemagick)
    (message "Svg images cannot be scaled")))


(defun picpocket-size-param (pic canvas-size)
  (pcase-let ((canvas-ratio (picpocket-cons-ratio canvas-size))
              (rot-ratio (picpocket-cons-ratio (picpocket-rotated-size pic)))
              (`(,pic-x . ,_) (picpocket-size-force pic)))
    (pcase picpocket-fit
      (:x-and-y (if (> canvas-ratio rot-ratio)
                    (picpocket-height-size-param pic canvas-size)
                  (picpocket-width-size-param pic canvas-size)))
      (:x (picpocket-width-size-param pic canvas-size))
      (:y (picpocket-height-size-param pic canvas-size))
      (_ (cons :width pic-x)))))

(defun picpocket-height-size-param (pic canvas-size)
  (pcase-let* ((`(,_ . ,canvas-y) canvas-size)
               (`(,_ . ,pic-y) (picpocket-size-force pic))
               (`(,_ . ,rot-y) (picpocket-rotated-size pic))
               (y-ratio (picpocket-ratio pic-y rot-y)))
    (cons :height (round (* y-ratio canvas-y)))))

(defun picpocket-width-size-param (pic canvas-size)
  (pcase-let* ((`(,canvas-x . ,_) canvas-size)
               (`(,pic-x . ,_) (picpocket-size-force pic))
               (`(,rot-x . ,_) (picpocket-rotated-size pic))
               (x-ratio (picpocket-ratio pic-x rot-x)))
    (cons :width (round (* x-ratio canvas-x)))))

(defun picpocket-size-force (pic)
  (or (picpocket-size pic)
      (picpocket-save-size-in-pic pic)))

(defun picpocket-save-size-in-pic (pic)
  (picpocket-set-size pic (image-size (create-image (picpocket-absfile pic)
                                                    (picpocket-image-type pic)
                                                    nil
                                                    :rotation 0.0)
                                      t)))

(defun picpocket-rotated-size (pic)
  (if (or (zerop (picpocket-rotation pic))
          (not (eq 'imagemagick (picpocket-image-type pic))))
      (picpocket-size-force pic)
    (image-size (create-image (picpocket-absfile pic)
                              (picpocket-image-type pic)
                              nil
                              :rotation (picpocket-rotation pic))
                t)))

(cl-defun picpocket-cons-ratio ((a . b))
  (/ (float a) b))

(defun picpocket-ratio (a b)
  (/ (float a) b))

(defun picpocket-look-ahead-and-save-time (pic)
  (let ((s (cadr (picpocket-time (picpocket-look-ahead pic))))
        (picpocket-sum 0))
    (picpocket-debug s "look")))

(defun picpocket-look-ahead (pic)
  (picpocket-ensure-cache pic)
  (image-size (picpocket-create-image pic picpocket-window-size)
              t
              (if (picpocket-fullscreen-p)
                  picpocket-frame
                (selected-frame))))

(defun picpocket-scale (n)
  (/ (* picpocket-scale n) 100))

(defun picpocket-alter-scale (delta)
  (setq picpocket-scale
        (max 10 (+ picpocket-scale delta)))
  (message "Scaling factor is %s%%" picpocket-scale))

(defun picpocket-imagemagick-p ()
  (picpocket-picture-regexp))

(defun picpocket-picture-regexp ()
  (or picpocket-picture-regexp
      (setq picpocket-picture-regexp
            (car (rassq 'imagemagick image-type-file-name-regexps)))))


(defun picpocket-clear-image-cache ()
  "Clear image cache.  Only useful for benchmarks."
  (interactive)
  (picpocket-command
    (setq picpocket-sum 0)
    (message "Clear image cache %s"
             (picpocket-time-string (clear-image-cache t)))))


;;; English functions

(defun picpocket-plural-s (n)
  (if (eq n 1)
      ""
    "s"))

(defun picpocket-plural-its-their (n)
  (if (eq n 1)
      "its"
    "their"))



;;; Keystroke and keymap functions

(defun picpocket-read-key (what)
  (let* ((prompt (format "Type a keystroke to select %s (type ? for help): "
                         what))
         (key (read-key-sequence-vector prompt)))
    (cond ((equal key [7])
           (keyboard-quit))
          ((equal key [??])
           (picpocket-key-help what)
           (with-current-buffer picpocket-buffer
             (picpocket-read-key what)))
          (t (picpocket-lookup-key-strict key)))))

(defun picpocket-read-key-to-add-or-remove-tag (&optional remove all)
  (let* ((prompt
          (format "Type a keystroke to select tag to %s %s(type ? for help): "
                  (if remove "remove" "add")
                  (if all "to all pictures " "")))
         (key (read-key-sequence-vector prompt)))
    (cond ((equal key [7])
           (keyboard-quit))
          ((equal key [??])
           (picpocket-key-help (if remove
                                   "tag to remove"
                                 "tag to add"))
           (with-current-buffer picpocket-buffer
             (picpocket-read-key-to-add-or-remove-tag remove all)))
          ((equal key [?-])
           (picpocket-read-key-to-add-or-remove-tag (not remove) all))
          (t (list remove (picpocket-lookup-key-strict key))))))

(defun picpocket-lookup-key-strict (key)
  (or (picpocket-lookup-key key)
      (error "Keystroke %s is not defined in picpocket-keystroke-alist" key)))

(defun picpocket-lookup-key (x)
  (cl-loop for (key ignored arg) in (picpocket-keystroke-alist-nodups)
           when (equal (picpocket-key-vector x)
                       (picpocket-key-vector key))
           return arg))

(defun picpocket-keystroke-alist-nodups ()
  (let ((input (picpocket-keystroke-alist))
        (output nil))
    (while input
      (let* ((first-entry (car input))
             (first-key (car first-entry))
             (remaining-entries (cdr input)))
        (unless (assoc first-key remaining-entries)
          (setq output (cons first-entry output)))
        (setq input remaining-entries)))
    (reverse output)))


(defun picpocket-keystroke-alist ()
  (if (symbolp picpocket-keystroke-alist)
      (symbol-value picpocket-keystroke-alist)
    picpocket-keystroke-alist))


(defun picpocket-key-vector (key)
  (if (vectorp key)
      key
    (if (stringp key)
        (apply #'vector (listify-key-sequence (kbd key)))
      (vector key))))


(defun picpocket-describe-keymap (prefix map)
  (map-keymap (lambda (key binding)
                (if (keymapp binding)
                    (picpocket-describe-keymap (vconcat prefix (vector key))
                                               binding)
                  (unless (eq binding 'undefined)
                    (princ (format "%16s - %s\n"
                                   (key-description (vconcat prefix
                                                             (vector key)))
                                   binding)))))
              map))

(defun picpocket-keymap-to-list (prefix map predicate)
  (let (list)
    (map-keymap (lambda (key binding)
                  (if (keymapp binding)
                      (setq list
                            (append list
                                    (picpocket-keymap-to-list (vconcat prefix
                                                                       (vector
                                                                        key))
                                                              binding
                                                              predicate)))
                    (when (funcall predicate binding)
                      (push (cons (vconcat prefix (vector key))
                                  binding)
                            list))))
                map)
    list))

(defun picpocket-key-sort-string (keystroke)
  (cl-loop for key across keystroke
           concat (format "%10s%2s"
                          (event-basic-type key)
                          (picpocket-modifier-weigth key))))

(defun picpocket-modifier-weigth (key)
  (cl-loop for modifier in (event-modifiers key)
           sum (cl-case modifier
                 (shift 1)
                 (control 2)
                 (meta 4)
                 (super 8)
                 (t 0))))



(defun picpocket-update-keymap ()
  (picpocket-cleanup-keymap nil picpocket-mode-map)
  ;; Do not call picpocket-define-keymap here because that may
  ;; override user regular define-key settings.
  (cl-loop for (key action arg) in (picpocket-keystroke-alist)
           do (define-key picpocket-mode-map
                (picpocket-key-vector key)
                (cond ((memq action '(tag add-tag))
                       (intern arg picpocket-tag-completion-table)
                       (picpocket-user-tag-command arg))
                      ((memq action '(move copy hardlink))
                       (picpocket-user-file-command action arg))
                      ((symbolp action)
                       action)
                      (t
                       (error (concat "Invalid entry in"
                                      " picpocket-keystroke-alist"
                                      " (%s %s %s)")
                              key action arg)))))
  (when (buffer-live-p (get-buffer picpocket-buffer))
    (with-current-buffer picpocket-buffer
      (use-local-map picpocket-mode-map)))
  (setq picpocket-old-keystroke-alist (picpocket-keystroke-alist)))


(defvar picpocket-tmp-map nil)

(defun picpocket-cleanup-keymap (key value)
  (cond ((keymapp value)
         (let ((picpocket-tmp-map value))
           (map-keymap #'picpocket-cleanup-keymap value)))
        ((and (symbolp value)
              (get value 'picpocket-user-command)
              (characterp key))
         (define-key picpocket-tmp-map (vector key) #'undefined))))


(defun picpocket-user-tag-command (tag)
  "Create a command that add TAG to current picture."
  (let ((symbol (intern (picpocket-command-name 'add-tag tag))))
    (fset symbol `(lambda ()
                    ,(format "Add tag %s." tag)
                    (interactive)
                    (picpocket-command
                      (picpocket-action 'add-tag ,tag)
                      (picpocket-old-update-buffer))))
    (put symbol 'picpocket-user-command 'add-tag)
    symbol))

(defun picpocket-user-file-command (action dst)
  "Create a command that move/copy/hardlink the current picture.
ACTION is one of the symbols move, copy or hardlink.
DST is the destination directory."
  (let ((symbol (intern (picpocket-command-name action dst))))
    (fset symbol `(lambda ()
                    ,(picpocket-command-doc action dst)
                    (interactive)
                    (picpocket-command
                      (picpocket-action ',action ,dst)
                      ,(when (eq action 'move)
                         '(picpocket-old-update-buffer)))))
    (put symbol 'picpocket-user-command 'file)
    symbol))

(defun picpocket-command-name (action arg)
  (pcase action
    ((or `add-tag `tag) (concat "picpocket-add-tag-" arg))
    (`move (concat "picpocket-move-to-" arg))
    (`copy (concat "picpocket-copy-to-" arg))
    (`hardlink (concat "picpocket-hardlink-to-" arg))
    (f (symbol-name f))))

(defun picpocket-command-doc (action dst)
  (format "%s image file to directory %s."
          (capitalize (symbol-name action))
          dst))

;;; Help functions

(defvar picpocket-help-count 0)
(defvar picpocket-is-sole-window nil)

(defvar picpocket-help-map
  (let ((map (make-sparse-keymap)))
    (define-key map [??] 'picpocket-help)
    map))

(defun picpocket-help ()
  "Toggle display of help for commands.

First invocation will display help for user defined commands if
there are any.  User defined commands are defined by setting the
variable `picpocket-keystroke-alist').

Second invocation will display help for built-in commands for
picpocket mode.

Third invocation will hide the help buffer."
  (interactive)
  (picpocket-command
    (if (eq last-command this-command)
        (setq picpocket-help-count (1+ picpocket-help-count))
      (setq picpocket-help-count 0
            picpocket-is-sole-window (eq 1 (count-windows))))
    (if (picpocket-keystroke-alist)
        (pcase picpocket-help-count
          (0 (picpocket-help-user-commands)
             (picpocket-help-finish))
          (1 (picpocket-help-mode-commands)
             (picpocket-help-finish))
          (_ (setq picpocket-help-count -1)
             (picpocket-hide-help)))
      (pcase picpocket-help-count
        (0 (picpocket-help-mode-commands)
           (picpocket-help-finish))
        (_ (setq picpocket-help-count -1)
           (picpocket-hide-help))))))

(defun picpocket-help-finish ()
  (when picpocket-is-sole-window
    (with-selected-window (picpocket-visible-window (help-buffer))
      (picpocket-shrink-to-fit)))
  ;; This is only needed if help buffer was selected,
  ;; see `help-window-select'.
  (set-transient-map picpocket-help-map))

(defun picpocket-hide-help ()
  (let ((help (picpocket-visible-window (help-buffer))))
    (when help
      (if picpocket-is-sole-window
          (delete-window help)
        (with-selected-window help
          (quit-window))))))

(defun picpocket-visible-window (buffer-name)
  (cl-loop for window being the windows
           when (string-equal buffer-name
                              (buffer-name (window-buffer window)))
           return window))

(defun picpocket-visible-buffers ()
  (mapcar (lambda (window)
            (buffer-name (window-buffer window)))
          (window-list)))

(defun picpocket-shrink-to-fit ()
  (when (window-combined-p nil t)
    (shrink-window-horizontally (- (window-width) (picpocket-buffer-width) 1))))

(defun picpocket-buffer-width ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop until (eobp)
             maximize (- (point-at-eol) (point-at-bol))
             do (forward-line 1))))


(defun picpocket-help-mode-commands ()
  (help-setup-xref (list #'picpocket-help-mode-commands)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (princ "Picpocket mode commands:\n\n")
      (princ "key             binding\n")
      (princ "---             -------\n\n")
      (let* ((predicate (lambda (binding)
                          (not (or (eq binding 'undefined)
                                   (get binding 'picpocket-user-command)))))
             (commands (sort (picpocket-keymap-to-list nil
                                                       picpocket-mode-map
                                                       predicate)
                             (lambda (a b)
                               (string-lessp
                                (picpocket-key-sort-string (car a))
                                (picpocket-key-sort-string (car b)))))))
        (cl-loop for (key . binding) in commands
                 do (progn
                      (princ (format "%-16s" (key-description key)))
                      (if (eq binding 'picpocket-repeat)
                          (picpocket-repeat-help)
                        (princ (symbol-name binding)))
                      (princ "\n")))))))

(defun picpocket-repeat-help ()
  ;; Help mode will make hyperlinks for all commands found at end of
  ;; line.  For picpocket-repeat we add stuff after command name so that
  ;; will not trigger.  Therefore make our own hyperlink for
  ;; picpocket-repeat.
  (insert-text-button "picpocket-repeat"
                      'type 'help-function
                      'help-args (list 'picpocket-repeat))
  (princ (format " (%s %s)" picpocket-last-action picpocket-last-arg)))


(defun picpocket-help-user-commands ()
  (help-setup-xref (list #'picpocket-help-user-commands)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (princ "User defined picpocket commands:\n\n")
    (princ "key             binding\n")
    (princ "---             -------\n\n")
    (cl-loop for (key action arg) in (picpocket-keystroke-alist-nodups)
             do (princ (format "%-16s%s\n"
                               (key-description (picpocket-key-vector key))
                               (picpocket-command-name action arg))))))

(defun picpocket-key-help (&optional what)
  (help-setup-xref (list #'picpocket-key-help what)
                   (called-interactively-p 'interactive))
  (with-help-window (help-buffer)
    (setq what (or what "directory/tag"))
    (princ (format "key             %s\n" what))
    (princ (format "---             %s\n\n" (make-string (length what) ?-)))
    (when (string-equal what "tag to remove")
      (princ (format "-               add tag instead of remove\n")))
    (when (string-equal what "tag to add")
      (princ (format "-               remove tag instead of add\n")))
    (cl-loop for (key ignored arg) in (picpocket-keystroke-alist-nodups)
             do (princ (format "%-16s%s\n"
                               (key-description (picpocket-key-vector key))
                               arg)))))


;;; Undoable actions functions

(defvar picpocket-undo-ewoc nil)
(defvar picpocket-undo-legend-ewoc nil)
(defvar picpocket-undo-window nil)
(defvar picpocket-current-undo-node nil)
(defvar picpocket-trashcan nil)

(defface picpocket-dim-face '((t (:foreground "gray")))
  "Face for unavailable commands.")



(cl-defstruct picpocket-undoable
  ;; State is the symbol incomplete, done or undone.
  state
  ;; Action is a symbol in the list picpocket-undoable-actions.
  action
  ;; Arg is a string dependant on action:
  ;;   add-tag    - tag
  ;;   remove-tag - tag
  ;;   set-tags   - space-separated tags
  ;;   delete     - nil
  ;;   rename     - new absolute filename (directory may be changed)
  ;;   move       - new absolute directory
  ;;   copy       - absolute destination directory
  ;;   hardlink   - absolute destination directory
  arg
  ;; If all is non-nil then the action is applied to all pictures in
  ;; the current picpocket list.
  all
  ;; ops is a list of picpocket-op structs.  Usually there is only a single
  ;; operation in this list.  But in case the all slot is non-nil
  ;; there will be one entry in this list per picture.
  ops)

(cl-defstruct picpocket-op
  action
  file
  sha
  to-file
  trash-file
  tags
  tag)

(cl-defstruct picpocket-legend
  key
  text
  predicate)

(defconst picpocket-undoable-actions '(add-tag remove-tag set-tags delete
                                               rename move copy hardlink))
(defconst picpocket-repeatable-actions '(move copy hardlink add-tag remove-tag))

(unless (cl-subsetp picpocket-repeatable-actions picpocket-undoable-actions)
  (error "Some repeatable action is not undoable"))

(defun picpocket-action (action arg &optional pic)
  "All undoable actions go through this function.

A subset of the picpocket commands trigger undoable actions.
These are listed in the constant `picpocket-undoable-actions'.  ACTION
is a symbol from this list.

A subset of the undoable actions are repeatable.  These are
listed in the constant `picpocket-repeatable-actions'.  Repeatable
actions can be repeated with the command `picpocket-repeat'.

ARG is a string.  For file operations it is the destination
directory or filename.  For tag operations it is the tag or tags
separated with space.

PIC is the picture to work on.  It defaults to `picpocket-current'.
If PIC is the symbol `all' then the action is applied to all
pictures in the current picpocket list (this is not supported for
the delete action, though)."
  (unless (memq action picpocket-undoable-actions)
    (error "Action %s is not undoable" action))
  (picpocket-stash-undo-begin :action action
                              :arg arg
                              :all (eq pic 'all))
  (if (eq pic 'all)
      (let ((pic picpocket-list)
            next)
        (while pic
          (setq next (cdr pic))
          (when (picpocket-filter-match-p pic)
            (picpocket-do-action action arg pic))
          (setq pic next)))
    (picpocket-ensure-current-pic)
    (picpocket-do-action action arg pic)
    (when (memq action picpocket-repeatable-actions)
      (picpocket-save-repeatable-action action arg)))
  (picpocket-stash-undo-end))

(defun picpocket-save-repeatable-action (action arg)
  (setq picpocket-last-action action
        picpocket-last-arg arg))

;; Currently single pic actions print message here.
;; PENDING - Move the message calls to the sub-routines....
;; Callers of picpocket-action with 'all also print a summary
;; message when all pictures are handled.
(defun picpocket-do-action (action arg pic)
  (pcase action
    (`set-tags
     (picpocket-set-tags-action arg pic)
     (if (picpocket-tags pic)
         (message "Tags set to %s." (picpocket-format-tags
                                     (picpocket-tags pic)))
       (message "Tags cleared")))
    (`add-tag
     (picpocket-add-tag-action arg pic)
     (message "%s is tagged with %s." (picpocket-file pic) arg))
    (`remove-tag
     (picpocket-remove-tag-action arg pic)
     (message "Tag %s is removed from %s." arg (picpocket-file pic)))
    (`delete
     (when (eq 'all pic)
       (error "Refusing to delete all pictures"))
     (let ((file (picpocket-file pic)))
       (picpocket-delete-action pic)
       (message "%s is no more." file)))
    ((or `move `rename `copy `hardlink)
     (picpocket-file-action action arg pic))
    (_
     (error "Unknown action %s %s" action arg))))

(defvar picpocket-undo-fail nil)
(defvar picpocket-undo-ok nil)

(defun picpocket-undo-action (undoable)
  (unless (picpocket-undoable-is-undoable-p undoable)
    (error "Action is not undoable"))
  (let (picpocket-undo-fail picpocket-undo-ok)
    (dolist (op (picpocket-undoable-ops undoable))
      (picpocket-undo-op op))
    (cond ((and (null picpocket-undo-fail) (null picpocket-undo-ok))
           (message "Nothing to undo")
           (setf (picpocket-undoable-state undoable) 'undone))
          ((null picpocket-undo-fail)
           (message "Undo ok %s"
                    (picpocket-undo-summary picpocket-undo-ok))
           (setf (picpocket-undoable-state undoable) 'undone))
          ((null picpocket-undo-ok)
           (message "Undo failed %s"
                    (picpocket-undo-summary picpocket-undo-fail)))
          (t
           (message "Undo partly failed (%s actions failed, %s actions ok)"
                    (length picpocket-undo-fail)
                    (length picpocket-undo-ok))
           (setf (picpocket-undoable-state undoable) 'incomplete)))))

(defun picpocket-undo-op (op)
  (pcase (picpocket-op-action op)
    (`delete (picpocket-undo-delete-action op))
    (`add-tag (picpocket-undo-add-tag-action op))
    (`remove-tag (picpocket-undo-remove-tag-action op))
    ((or `rename `move) (picpocket-undo-file-relocate-action op))
    ((or `copy `hardlink) (picpocket-undo-file-duplicate-action op))))

(defun picpocket-undo-summary (list)
  (if (cdr list)
      (format "(%s actions)" (length list))
    (format "(%s)" (car list))))

(defun picpocket-undo-fail (format &rest args)
  (let ((text (apply #'format format args)))
    (message text)
    (warn text)
    (push text picpocket-undo-fail)))

(defun picpocket-undo-ok (format &rest args)
  (let ((text (apply #'format format args)))
    (message text)
    (push text picpocket-undo-ok)))



(defun picpocket-delete-action (pic)
  (let ((inhibit-quit t)
        (file (picpocket-absfile pic))
        (filter-match (picpocket-filter-match-p pic))
        (trash-file (picpocket-trash-file (picpocket-file pic))))
    (picpocket-stash-undo-op :action 'delete
                             :file (picpocket-absfile pic)
                             :tags (picpocket-tags pic)
                             :trash-file trash-file)
    (rename-file file trash-file)
    (picpocket-tags-delete-file pic file)
    (picpocket-list-delete pic (list filter-match))))


(defun picpocket-undo-delete-action (op)
  (let ((trash-file (picpocket-op-trash-file op))
        (file (picpocket-op-file op))
        (tags (picpocket-op-tags op))
        (inhibit-quit t))
    (if (not (file-exists-p trash-file))
        (picpocket-undo-fail "Cannot undelete %s, %s does not exist"
                             (file-name-nondirectory file)
                             trash-file)
      (make-directory (file-name-directory file) t)
      (rename-file trash-file file)
      (picpocket-list-insert-before-current (picpocket-make-pic file))
      (picpocket-tags-set picpocket-current tags)
      (picpocket-undo-ok "Undeleted %s" (file-name-nondirectory file)))))

(defun picpocket-trash-file (filename)
  (setq filename (file-name-nondirectory filename))
  (unless picpocket-trashcan
    (setq picpocket-trashcan (file-name-as-directory
                              (make-temp-file "picpocket-trash" t))))
  (make-directory picpocket-trashcan t)
  (cl-loop for i = 1 then (1+ i)
           for f = (picpocket-trash-file-candidate filename i)
           unless (file-exists-p f)
           return f))

(defun picpocket-trash-file-candidate (filename i)
  (if (eq i 1)
      (expand-file-name filename picpocket-trashcan)
    (concat picpocket-trashcan
            (file-name-sans-extension filename)
            "_"
            (number-to-string i)
            (if (file-name-extension filename) "." "")
            (file-name-extension filename))))


(defun picpocket-add-tag-action (tag-string &optional pic)
  (let* ((pic (or pic picpocket-current))
         (tag (intern tag-string))
         (tags (picpocket-tags pic))
         (inhibit-quit t))
    (unless (memq tag tags)
      (picpocket-tags-set pic (append tags (list tag)))
      (picpocket-stash-undo-op :action 'add-tag
                               :file (picpocket-absfile pic)
                               :sha (picpocket-sha-force pic)
                               :tag tag))))


(defun picpocket-undo-add-tag-action (op)
  (let* ((file (picpocket-op-file op))
         (tag (picpocket-op-tag op))
         (sha (picpocket-op-sha op))
         (current-tags (picpocket-db-tags sha)))
    (when (memq tag current-tags)
      (picpocket-db-tags-set sha file (delq tag current-tags))
      (picpocket-reset-filter-counters))
    (picpocket-undo-ok "Undo add tag %s to %s"
                       tag
                       (file-name-nondirectory file))))

(defun picpocket-remove-tag-action (tag-string &optional pic)
  (let* ((pic (or pic picpocket-current))
         (tag (intern tag-string))
         (tags (picpocket-tags pic))
         (inhibit-quit t))
    (when (memq tag tags)
      (picpocket-tags-set pic (delq tag tags))
      (picpocket-stash-undo-op :action 'remove-tag
                               :file (picpocket-absfile pic)
                               :sha (picpocket-sha-force pic)
                               :tag tag))))

(defun picpocket-undo-remove-tag-action (op)
  (let* ((file (picpocket-op-file op))
         (tag (picpocket-op-tag op))
         (sha (picpocket-op-sha op))
         (current-tags (picpocket-db-tags sha)))
    (unless (memq tag current-tags)
      (picpocket-db-tags-set sha file (append current-tags (list tag)))
      (picpocket-reset-filter-counters))
    (picpocket-undo-ok "Undo remove tag %s from %s"
                       tag
                       (file-name-nondirectory file))))

(defun picpocket-set-tags-action (tags-string pic)
  (let* ((pic (or pic picpocket-current))
         (old-tags (picpocket-tags pic))
         (new-tags (picpocket-tags-string-to-list tags-string))
         (inhibit-quit t))
    (picpocket-tags-set pic new-tags)
    (dolist (tag (cl-set-difference old-tags new-tags))
      (picpocket-stash-undo-op :action 'remove-tag
                               :file (picpocket-absfile pic)
                               :sha (picpocket-sha-force pic)
                               :tag tag))
    (dolist (tag (cl-set-difference new-tags old-tags))
      (picpocket-stash-undo-op :action 'add-tag
                               :file (picpocket-absfile pic)
                               :sha (picpocket-sha-force pic)
                               :tag tag))))


(defun picpocket-tags-string-to-list (tags-string)
  (cl-delete-duplicates
   (mapcar #'intern
           (split-string tags-string))))

(defun picpocket-new-path-for-file-action (action dst pic)
  (file-truename
   (if (eq action 'rename)
       dst
     (expand-file-name (picpocket-file pic)
                       (if (or (file-name-absolute-p dst)
                               picpocket-destination-relative-current)
                           dst
                         (expand-file-name dst picpocket-destination-dir))))))

(defun picpocket-file-action (action dst pic)
  (let* ((pic (or pic picpocket-current))
         (new-path (picpocket-new-path-for-file-action action dst pic))
         (old-dir (picpocket-dir pic))
         (old-file (picpocket-file pic))
         (old-path (concat old-dir old-file))
         (new-dir (file-name-directory new-path))
         (new-file (file-name-nondirectory new-path))
         (ok-if-already-exists noninteractive))
    (make-directory new-dir t)
    (while (and (file-exists-p new-path)
                (not ok-if-already-exists))
      (cond ((equal old-path new-path)
             (user-error "Attempt to %s file to itself"
                         (symbol-name action)))
            ((file-directory-p new-path)
             (error "%s already exists as a directory" new-path))
            ((picpocket-files-identical-p old-path new-path)
             (if (y-or-n-p (concat "Identical file already exists in "
                                   new-dir ".  Overwrite? "))
                 (setq ok-if-already-exists t)
               (user-error "Not overwriting %s" new-path)))
            (t
             (setq new-file (picpocket-compare pic new-path)
                   new-path (concat new-dir new-file)))))
    (pcase action
      ((or `move `rename)
       (picpocket-file-relocate-action action old-path new-path pic))
      ((or `copy `hardlink)
       (picpocket-file-duplicate-action action old-path new-path pic))
      (_ (error "Invalid picpocket action %s" action)))))

(defun picpocket-files-identical-p (a b)
  (and (file-exists-p a)
       (file-exists-p b)
       (let ((a-bytes (picpocket-file-bytes a))
             (b-bytes (picpocket-file-bytes b)))
         (eq a-bytes b-bytes))
       (if (executable-find "diff")
           (zerop (call-process "diff" nil nil nil "-q"
                                (expand-file-name a)
                                (expand-file-name b)))
         (picpocket-elisp-files-identical-p a b))))

(defun picpocket-elisp-files-identical-p (a b)
  (string-equal (picpocket-file-content a)
                (picpocket-file-content b)))

(defun picpocket-file-content (file)
  (with-temp-buffer
    (buffer-disable-undo)
    (insert-file-contents-literally file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun picpocket-file-relocate-action (action old-path new-path pic)
  (let ((new-dir (file-name-directory new-path))
        (new-file (file-name-nondirectory new-path))
        (old-dir (file-name-directory old-path))
        (old-file (file-name-nondirectory old-path))
        (inhibit-quit t)
        trash-file)
    (when (file-exists-p new-path)
      (setq trash-file (picpocket-trash-file new-file))
      (rename-file new-path trash-file))
    (picpocket-stash-undo-op :action action
                             :file old-path
                             :to-file new-path
                             :trash-file trash-file
                             :sha (picpocket-sha-force pic))
    (rename-file old-path new-path t)
    (picpocket-tags-move-file pic old-path new-path)
    (cond ((or (eq action 'move)
               (equal old-file new-file))
           (picpocket-list-delete pic)
           (message "Moved %s to %s."
                    (file-name-nondirectory old-path)
                    (file-name-directory new-path)))
          ((equal old-dir new-dir)
           (picpocket-set-file pic new-file)
           (message "Renamed %s to %s." old-file new-file))
          (t
           (picpocket-list-delete pic)
           (message "Renamed and moved %s to %s." old-file new-path)))))

(defun picpocket-undo-file-relocate-action (op)
  (let ((file (picpocket-op-file op))
        (to-file (picpocket-op-to-file op))
        (trash-file (picpocket-op-trash-file op))
        (sha (picpocket-op-sha op))
        (inhibit-quit t))
    (cond ((not (file-exists-p to-file))
           (picpocket-undo-fail "Cannot undo %s, %s does not exist"
                                (picpocket-op-action op)
                                to-file))
          ((file-exists-p file)
           (picpocket-undo-fail "Cannot undo %s, %s already exist"
                                (picpocket-op-action op)
                                file))
          (t
           (make-directory (file-name-directory file) t)
           (rename-file to-file file)
           (picpocket-db-tags-move-file sha to-file file)
           (when trash-file
             (rename-file trash-file to-file))
           (let ((pic (picpocket-list-search to-file)))
             (if pic
                 (picpocket-set-absfile pic file)
               (picpocket-list-insert-before-current (picpocket-make-pic
                                                      file))))
           (picpocket-undo-ok "%s %s back"
                              (picpocket-action-past-tense
                               (picpocket-op-action op))
                              (file-name-nondirectory file))))))

(defun picpocket-file-duplicate-action (action old-path new-path pic)
  (let ((old-file (file-name-nondirectory old-path))
        (inhibit-quit t)
        trash-file)
    (picpocket-tags-copy-file picpocket-current new-path)
    (when (file-exists-p new-path)
      (setq trash-file (picpocket-trash-file new-path))
      (rename-file new-path trash-file))
    (if (eq action 'copy)
        (copy-file old-path new-path t)
      (add-name-to-file old-path new-path t))
    (picpocket-stash-undo-op :action action
                             :file old-path
                             :to-file new-path
                             :trash-file trash-file
                             :sha (picpocket-sha-force pic))
    (picpocket-duplicate-message action old-file new-path)))

(defun picpocket-undo-file-duplicate-action (op)
  (let ((to-file (picpocket-op-to-file op))
        (trash-file (picpocket-op-trash-file op))
        (sha (picpocket-op-sha op))
        (inhibit-quit t))
    (cond ((not (file-exists-p to-file))
           (picpocket-undo-fail "Cannot undo %s, %s does not exist"
                                (picpocket-op-action op)
                                to-file))
          (t
           (delete-file to-file)
           (when trash-file
             (rename-file trash-file to-file))
           (picpocket-db-tags-delete-file sha to-file)
           (if (eq 'copy (picpocket-op-action op))
               (picpocket-undo-ok "Uncopied %s"
                                  (file-name-nondirectory to-file))
             (picpocket-undo-ok "Un-hard-linked %s"
                                (file-name-nondirectory to-file)))))))

(defun picpocket-duplicate-message (action old dst)
  (message "%s %s to %s."
           (if (eq action 'copy)
               "Copied"
             "Hard linked")
           old
           dst))

(defun picpocket-compare (pic new-path)
  (unwind-protect
      (let (picpocket-adapt-to-window-size-change)
        (picpocket-show-two-pictures pic new-path)
        (read-string (format (concat "File already exists (size %s)."
                                     "  Rename this (size %s) to: ")
                             (picpocket-kb (picpocket-file-bytes new-path))
                             (picpocket-kb (picpocket-bytes-force pic)))
                     (picpocket-file pic)))
    (picpocket-update-buffer)))

(defun picpocket-show-two-pictures (pic new)
  (picpocket-ensure-picpocket-buffer)
  (cl-destructuring-bind (window-width . window-height)
      (picpocket-save-window-size)
    (let* ((line-height (+ (frame-char-height)
                           (or line-spacing
                               (frame-parameter nil 'line-spacing)
                               0)))
           (pic-height (/ (- window-height (* 2 line-height)) 2))
           (picpocket-fit (picpocket-standard-value 'picpocket-fit))
           (picpocket-scale (picpocket-standard-value 'picpocket-scale))
           buffer-read-only)
      (erase-buffer)
      (insert (format "About to overwrite this picture (%s):\n"
                      (picpocket-kb (picpocket-file-bytes new))))
      (insert-image (picpocket-create-image (list (picpocket-make-pic new))
                                            (cons window-width pic-height)))
      (insert (format "\nWith this picture (%s):\n"
                      (picpocket-kb (picpocket-bytes-force pic))))
      (insert-image (picpocket-create-image pic (cons window-width pic-height)))
      (goto-char (point-min)))))

(defun picpocket-standard-value (symbol)
  (eval (car (get symbol 'standard-value))))


;;; Undo commands

(defun picpocket-undo ()
  "Undo last command.
\\<picpocket-mode-map>
Most commands are undoable.  All commands that delete, move, copy
or hardlink files are undoable.  Also all commands that adds or
removes tags.  This includes commands that have been defined by
the user customizing variable `picpocket-keystroke-alist'.

Navigational commands and commands that affect the
display (rotation and scaling) are not undoable.

The last `picpocket-undo-list-size' undoable commands are saved.  Type
\\[picpocket-visit-undo-list] to view a list of them in a special
buffer.  In that buffer it is possible to select and undo any
command in the list.

This command picks the first undoable command in that list."
  (interactive)
  (picpocket-command
    (let ((undoable (when picpocket-undo-ring
                      (cl-loop for i from 0 to (ring-length picpocket-undo-ring)
                               for undoable = (ring-ref picpocket-undo-ring i)
                               while undoable
                               when (picpocket-undoable-is-undoable-p undoable)
                               return undoable))))
      (if undoable
          (picpocket-undo-action undoable)
        (user-error "No undoable actions have been done")))))


(defun picpocket-undoable-is-undoable-p (undoable)
  (memq (picpocket-undoable-state undoable)
        '(done incomplete)))

(defun picpocket-visit-undo-list ()
  "List the current undoable commands in a separate buffer."
  (interactive)
  (picpocket-bye-command
    (when (> (window-width) 100)
      (setq picpocket-undo-window (split-window-right)))
    (switch-to-buffer (get-buffer-create picpocket-undo-buffer))
    (with-current-buffer picpocket-undo-buffer
      (picpocket-undo-mode)
      (picpocket-update-undo-buffer))))


;;; Undo stash functions

(defun picpocket-stash-undo-begin (&rest args)
  (unless picpocket-undo-ring
    (setq picpocket-undo-ring (make-ring picpocket-undo-list-size)))
  (picpocket-maybe-grow-undo-ring)
  (picpocket-remove-empty-incomplete-entries)
  (picpocket-remove-one-if-stash-is-full)
  (ring-insert picpocket-undo-ring
               (apply #'make-picpocket-undoable
                      :state 'incomplete
                      args)))

(defun picpocket-maybe-grow-undo-ring ()
  ;; Grow if needed, but in contrast the ring is never shrinked
  ;; dynamically.
  (let ((size (ring-size picpocket-undo-ring)))
    (when (> picpocket-undo-list-size size)
      (ring-extend picpocket-undo-ring (- picpocket-undo-list-size size)))))

(defun picpocket-remove-empty-incomplete-entries ()
  (while (let ((newest (and (not (ring-empty-p picpocket-undo-ring))
                            (ring-ref picpocket-undo-ring 0))))
           (and newest
                (eq (picpocket-undoable-state newest) 'incomplete)
                (null (picpocket-undoable-ops newest))))
    (picpocket-cleanup-undo-entry (ring-remove picpocket-undo-ring 0))))

(defun picpocket-remove-one-if-stash-is-full ()
  (when (= (ring-length picpocket-undo-ring)
           (ring-size picpocket-undo-ring))
    (picpocket-cleanup-undo-entry (ring-remove picpocket-undo-ring))))

(defun picpocket-cleanup-undo-entry (undo)
  (dolist (op (picpocket-undoable-ops undo))
    (and (picpocket-op-trash-file op)
         (file-exists-p (picpocket-op-trash-file op))
         (delete-file (picpocket-op-trash-file op)))))

(defun picpocket-stash-undo-op (&rest args)
  (when (or (null picpocket-undo-ring)
            (ring-empty-p picpocket-undo-ring))
    (error "Call to picpocket-stash-undo-op before picpocket-stash-undo-begin"))
  ;; PENDING - maybe should append instead of push?
  ;; Currently picpocket-stash-undo-end is reversing the list.
  ;; (let ((undoable (ring-ref picpocket-undo-ring 0)))
  ;; (setf (picpocket-undoable-ops undoable)
  ;; (append (picpocket-undoable-ops undoable)
  ;; (list (apply #'make-picpocket-op args))))))
  (push (apply #'make-picpocket-op args)
        (picpocket-undoable-ops (ring-ref picpocket-undo-ring 0))))

(defun picpocket-stash-undo-end ()
  (let ((current-undo (ring-ref picpocket-undo-ring 0)))
    (setf (picpocket-undoable-ops current-undo)
          (reverse (picpocket-undoable-ops current-undo)))
    (setf (picpocket-undoable-state current-undo) 'done)))




;;; The undo buffer's ewoc population

;; There are two ewocs in the undo buffer.
;;
;; One is called picpocket-legend-ewoc and shows a legend for the available
;; commands in the undo-buffer.  The node data is instances of the
;; struct picpocket-undo-legend.  The text for a command is dimmed out if
;; it is not appropriate for the thing at point.
;;
;; The other is called picpocket-undo-ewoc and shows the list of undoable
;; things.  The node data is instances of the struct picpocket-undoable.  It is
;; a mirror of the picpocket-undo-ring - it contain the same data in the
;; same order.  Whenever a command alter the picpocket-undo-ring the
;; picpocket-undo-ewoc will be rebuilt from scratch (the picpocket-command macro
;; takes care of that).



(defun picpocket-update-undo-buffer ()
  (with-current-buffer picpocket-undo-buffer
    (let ((progress nil)
          (i 0)
          (start-time (current-time))
          (buffer-read-only nil))
      (erase-buffer)
      (insert "\n"
              (picpocket-emph "Picpocket undo buffer")
              "\n")
      (setq picpocket-undo-legend-ewoc (ewoc-create #'picpocket-undo-legend-pp))
      (picpocket-undo-legend-add "u"
                                 "undo an entry"
                                 #'picpocket-current-undoable-p)
      ;; (picpocket-undo-legend-add "r"
      ;; "redo an entry"
      ;; #'picpocket-current-redoable-p)
      (picpocket-undo-legend-add "n"
                                 "move to next entry"
                                 #'picpocket-current-have-next-p)
      (picpocket-undo-legend-add "p"
                                 "move to previous entry"
                                 #'picpocket-current-have-previous-p)
      (picpocket-undo-legend-add "q"
                                 "return to picpocket buffer"
                                 #'picpocket-true)
      (goto-char (point-max))
      (insert "List of undoable actions with the most recent first:\n")
      (setq picpocket-current-undo-node nil
            picpocket-undo-ewoc (ewoc-create #'picpocket-undo-pp
                                             nil nil t))
      (if (or (null picpocket-undo-ring)
              (ring-empty-p picpocket-undo-ring))
          (insert "\n(There is nothing to undo)")
        (dolist (undoable (ring-elements picpocket-undo-ring))
          (ewoc-enter-last picpocket-undo-ewoc undoable)
          (cl-incf i)
          (when (picpocket-more-than-half-a-second-since-p start-time)
            (setq progress (or progress (make-progress-reporter
                                         "Making undo buffer "
                                         0
                                         (ring-length picpocket-undo-ring))))
            (progress-reporter-update progress i))))
      (picpocket-init-current-undo)
      (picpocket-update-current-undo)
      (when progress
        (progress-reporter-done progress)))))

(defun picpocket-more-than-half-a-second-since-p (time)
  (time-less-p (seconds-to-time 0.5)
               (time-subtract (current-time) time)))


(defun picpocket-undo-pp (undoable)
  (insert (if (and picpocket-current-undo-node
                   (eq undoable (ewoc-data picpocket-current-undo-node)))
              (picpocket-emph " -> ")
            "    ")
          (capitalize (symbol-name (picpocket-undoable-state undoable)))
          " action: "
          (picpocket-undoable-text undoable))
  (insert "\n      ")
  (let ((ops (picpocket-undoable-ops undoable)))
    (cl-loop for i from 0 to (1- picpocket-max-undo-thumbnails)
             for op = (elt ops i)
             while op
             do (picpocket-mini-image undoable op)
             do (insert " "))
    (when (elt ops picpocket-max-undo-thumbnails)
      (insert "....")))
  (insert "\n")
  (insert (propertize "\n" 'line-height 1.5)))

(defun picpocket-mini-image (undoable &optional op)
  (let ((op (or op (car (picpocket-undoable-ops undoable)))))
    (picpocket-insert-mini-image (picpocket-op-image-file undoable op))))

(defun picpocket-insert-mini-image (file)
  (and (display-images-p)
       file
       (file-exists-p file)
       (insert-image (create-image file
                                   (picpocket-image-type file)
                                   nil
                                   :height (* picpocket-undo-thumbnails-size
                                              (frame-char-height))))))

(defun picpocket-op-image-file (undoable op)
  (if (eq (picpocket-undoable-state undoable) 'undone)
      (picpocket-op-file op)
    (pcase (picpocket-undoable-action undoable)
      ((or `set-tags `add-tag `remove-tag) (picpocket-op-file op))
      (`delete (picpocket-op-trash-file op))
      (_ (picpocket-op-to-file op)))))


(defconst picpocket-empty-op (make-picpocket-op :file "nothing"))

(defun picpocket-undoable-text (undoable)
  (let* ((action (picpocket-undoable-action undoable))
         (arg (picpocket-undoable-arg undoable))
         (ops (picpocket-undoable-ops undoable))
         (first-op (or (car ops) picpocket-empty-op))
         (file (if (picpocket-undoable-all undoable)
                   (format "all %s pictures" (length ops))
                 (file-name-nondirectory (picpocket-op-file first-op)))))
    (pcase action
      (`set-tags (concat "set tags to "
                         (picpocket-format-tags arg)
                         " on "
                         file))
      (`add-tag (concat "add tag "
                        arg
                        " to "
                        file))
      (`remove-tag (concat "remove tag "
                           arg
                           " from "
                           file))
      (`delete (concat "delete " file))
      (_ (concat (symbol-name action)
                 " "
                 file
                 " to "
                 arg)))))


(defun picpocket-undo-legend-add (key text predicate)
  (ewoc-enter-last picpocket-undo-legend-ewoc
                   (make-picpocket-legend :key key
                                          :text text
                                          :predicate predicate)))

(defun picpocket-current-have-previous-p (current)
  (and current
       (not (eq current
                (picpocket-first-undo-node)))))

(defun picpocket-current-have-next-p (current)
  (and current
       (not (eq current
                (picpocket-last-undo-node)))))

(defun picpocket-current-undoable-p (current)
  (and current
       (memq (picpocket-undoable-state (ewoc-data current))
             '(done incomplete))))

(defun picpocket-current-redoable-p (current)
  (and current
       (eq (picpocket-undoable-state (ewoc-data current))
           'undone)))

(defun picpocket-true (&rest ignored)
  t)

(defun picpocket-undo-legend-pp (legend)
  (let ((valid (funcall (picpocket-legend-predicate legend)
                        picpocket-current-undo-node)))
    (insert (propertize (concat "  Type "
                                (if valid
                                    (picpocket-emph (picpocket-legend-key
                                                     legend))
                                  (picpocket-legend-key legend))
                                " to "
                                (picpocket-legend-text legend)
                                ".")
                        'font-lock-face
                        (if valid
                            'default
                          'picpocket-dim-face)))))


(defun picpocket-init-current-undo ()
  (setq picpocket-current-undo-node (picpocket-first-undo-node)))

(defun picpocket-update-current-undo ()
  (when picpocket-current-undo-node
    (ewoc-invalidate picpocket-undo-ewoc picpocket-current-undo-node)
    (ewoc-goto-node picpocket-undo-ewoc picpocket-current-undo-node))
  (save-excursion
    (ewoc-refresh picpocket-undo-legend-ewoc)))

(defun picpocket-first-undo-node ()
  (ewoc-nth picpocket-undo-ewoc 0))

(defun picpocket-last-undo-node ()
  (ewoc-nth picpocket-undo-ewoc -1))

(defun picpocket-ewoc-find-node (ewoc data)
  (cl-loop for i = 0 then (1+ i)
           for node = (ewoc-nth ewoc i)
           while node
           when (eq data (ewoc-data node))
           return node))


;;; The undo buffer's commands

(define-derived-mode picpocket-undo-mode picpocket-base-mode "picpocket-undo"
  "Major mode for picpocket undo buffer.")

(let ((map (make-sparse-keymap)))
  (suppress-keymap map)
  (define-key map [?u] #'picpocket-undo-undo)
  ;; (define-key map [?r] #'picpocket-undo-redo)
  (define-key map [?n] #'picpocket-undo-next)
  (define-key map [?p] #'picpocket-undo-previous)
  (define-key map [return] #'picpocket-select-undo-entry-at-point)
  (define-key map [?q] #'picpocket-undo-quit)
  (setq picpocket-undo-mode-map map))

(defun picpocket-undo-undo ()
  "Undo the current action."
  (interactive)
  (unless picpocket-current-undo-node
    (picpocket-select-undo-entry-at-point))
  (unless picpocket-current-undo-node
    (error "No action available"))
  (picpocket-undo-action (ewoc-data picpocket-current-undo-node))
  (picpocket-update-picpocket-buffer)
  (ewoc-invalidate picpocket-undo-ewoc picpocket-current-undo-node))

;; (defun picpocket-undo-redo ()
;; (interactive)
;; (unless picpocket-current-undo-node
;; (picpocket-select-undo-entry-at-point))
;; (unless picpocket-current-undo-node
;; (error "No action available"))
;; ...
;; (picpocket-update-picpocket-buffer)
;; (ewoc-invalidate picpocket-undo-ewoc picpocket-current-undo-node))

(defun picpocket-select-undo-entry-at-point ()
  "Select the action at point."
  (interactive)
  (picpocket-undo-move 0))

(defun picpocket-undo-next ()
  "Move forward to next action."
  (interactive)
  (picpocket-undo-move 1))

(defun picpocket-undo-previous ()
  "Move backward to previous action."
  (interactive)
  (picpocket-undo-move -1))

(defun picpocket-undo-move (direction)
  (unless (picpocket-first-undo-node)
    (error "No undoable or redoable actions available"))
  (let ((old-node picpocket-current-undo-node))
    (setq picpocket-current-undo-node nil)
    (when old-node
      (ewoc-invalidate picpocket-undo-ewoc old-node)))
  (pcase direction
    (-1 (ewoc-goto-prev picpocket-undo-ewoc 1))
    (1 (ewoc-goto-next picpocket-undo-ewoc 1)))
  (setq picpocket-current-undo-node (ewoc-locate picpocket-undo-ewoc))
  (picpocket-update-current-undo)
  (picpocket-show-legend-at-top))

(defun picpocket-show-legend-at-top ()
  (when (eq picpocket-current-undo-node (picpocket-first-undo-node))
    (recenter)))

(defun picpocket-undo-quit ()
  "Delete the undo buffer and go back to the picpocket buffer."
  (interactive)
  (kill-buffer picpocket-undo-buffer)
  (when (window-live-p picpocket-undo-window)
    (delete-window picpocket-undo-window))
  (if (null (buffer-live-p (get-buffer picpocket-buffer)))
      (message "No picpocket buffer found")
    (picpocket-update-picpocket-buffer)
    (or (picpocket-select-visible-picpocket-window)
        (switch-to-buffer picpocket-buffer))))

(defun picpocket-update-picpocket-buffer ()
  (when (buffer-live-p (get-buffer picpocket-buffer))
    (with-current-buffer picpocket-buffer
      (picpocket-update-buffer))))

(defun picpocket-select-visible-picpocket-window ()
  "Return non-nil if visible picpocket window was found."
  (cl-loop for window in (window-list nil 'no-mini)
           when (equal (buffer-name (window-buffer window))
                       picpocket-buffer)
           return (progn
                    (select-window window)
                    t)))




;;; Header line functions

(defvar picpocket-last-msg nil)
(defvar picpocket-last-msg-timestamp nil)
(defvar picpocket-show-msg-seconds 1.5)

(defun picpocket-update-header-seconds ()
  (+ picpocket-show-msg-seconds 0.1))

(defun picpocket-header-line ()
  (if (eq (selected-frame) picpocket-frame)
      (picpocket-fullscreen-header-line)
    (picpocket-header-pic-info)))

(defun picpocket-fullscreen-header-line ()
  (let ((msg (picpocket-escape-percent (current-message))))
    (picpocket-msg "  msg " msg)
    (cond ((null msg)
           (if (or (null picpocket-last-msg)
                   (picpocket-last-msg-too-old-p))
               (progn
                 (picpocket-msg "  null msg - nothing")
                 (setq picpocket-last-msg nil)
                 (picpocket-header-pic-info))
             (picpocket-msg "  null msg - show picpocket-last-msg")
             picpocket-last-msg))
          ((null picpocket-current)
           (setq picpocket-last-msg nil)
           msg)
          ((not (string-equal msg picpocket-last-msg))
           (picpocket-msg "  fresh msg")
           (setq picpocket-last-msg msg
                 picpocket-last-msg-timestamp (current-time))
           msg)
          ((not (picpocket-last-msg-too-old-p))
           (picpocket-msg "  keep msg")
           msg)
          (t
           (picpocket-msg "  pic-info")
           (picpocket-header-pic-info)))))

(defun picpocket-last-msg-too-old-p ()
  (time-less-p (seconds-to-time picpocket-show-msg-seconds)
               (time-since picpocket-last-msg-timestamp)))

(defun picpocket-update-header (&rest ignored)
  (picpocket-msg "picpocket-update-header: called")
  (when (eq (selected-frame) picpocket-frame)
    (picpocket-msg "picpocket-update-header: fullscreen")
    (force-mode-line-update)))

;; PENDING - keeping this debug stuff for now.
(defvar picpocket-start (current-time))
(defun picpocket-msg (&rest args)
  (when nil
    (with-current-buffer (get-buffer-create "*piclog*")
      (save-excursion
        (goto-char (point-max))
        (insert (format "%f %s\n"
                        (time-to-seconds (time-since picpocket-start))
                        (mapconcat (lambda (x)
                                     (if (stringp x)
                                         x
                                       (prin1-to-string x)))
                                   args
                                   " ")))))))


(defun picpocket-header-pic-info ()
  (and picpocket-current
       picpocket-list
       picpocket-db
       (picpocket-join (concat (format "%s/%s "
                                       picpocket-index
                                       picpocket-list-length)
                               (picpocket-escape-percent (picpocket-header-dir))
                               "/"
                               (propertize
                                (picpocket-escape-percent (picpocket-file))
                                'face 'highlight))
                       (when picpocket-debug
                         picpocket-header-text)
                       (picpocket-maybe-kb)
                       (picpocket-scale-info)
                       (picpocket-rotation-info)
                       (picpocket-format-tags (picpocket-tags
                                               picpocket-current))
                       (picpocket-filter-info))))

(defun picpocket-join (&rest strings)
  (mapconcat 'identity
             (delete nil (delete "" strings))
             " "))

(defun picpocket-escape-percent (string)
  (when string
    (replace-regexp-in-string "%" "%%" string)))

(defun picpocket-header-dir ()
  (if picpocket-header-full-path
      ;; abbreviate-file-name substitutes the users home directory
      ;; with "~".  This do not work if the home directory is a
      ;; symbolic link.  That case is fixed by appending to
      ;; directory-abbrev-alist here.
      (let ((directory-abbrev-alist
             (append directory-abbrev-alist
                     (list (cons (file-truename "~")
                                 (getenv "HOME"))))))
        (abbreviate-file-name (directory-file-name (picpocket-dir))))
    (file-name-nondirectory (directory-file-name (picpocket-dir)))))

(defun picpocket-maybe-kb ()
  (let ((bytes (picpocket-bytes picpocket-current)))
    (if bytes
        (picpocket-kb bytes)
      "")))


(defun picpocket-bytes-force (pic)
  (or (picpocket-bytes pic)
      (picpocket-save-bytes-in-pic pic)))

(defun picpocket-save-bytes-in-pic (pic)
  (picpocket-set-bytes pic (picpocket-file-bytes (picpocket-absfile pic))))

(defun picpocket-file-bytes (file)
  (let ((attributes (file-attributes file)))
    (if attributes
        (elt attributes 7)
      (error "File %s do not exist" file))))

(defun picpocket-kb (bytes)
  (if (<= 1024 bytes)
      (format "%sk" (/ bytes 1024))
    (format "%s" bytes)))

(defun picpocket-scale-info ()
  (unless (eq picpocket-scale 100)
    (format "%s%%%%" picpocket-scale)))

(defun picpocket-rotation-info ()
  (let ((degrees (picpocket-rotation picpocket-current)))
    (unless (zerop degrees)
      (format "%s°" (truncate degrees)))))

(defun picpocket-format-tags (tags)
  (if (stringp tags)
      (picpocket-format-tags (picpocket-tags-string-to-list tags))
    (when tags
      (cl-case picpocket-tags-style
        (:org (format ":%s:" (mapconcat #'symbol-name tags ":")))
        (t (format "(%s)" (mapconcat #'symbol-name tags " ")))))))

(defun picpocket-filter-info ()
  (when picpocket-filter
    (format "[filter: %s %s/%s]"
            (picpocket-format-tags picpocket-filter)
            (or picpocket-filter-index "?")
            (cond (picpocket-filter-match-count-done
                   picpocket-filter-match-count)
                  ((null picpocket-filter-match-count)
                   "?")
                  ((zerop picpocket-filter-match-count)
                   "?")
                  (t
                   (format "%s+" picpocket-filter-match-count))))))


;;; Hook functions

(defun picpocket-cleanup-most-hooks ()
  (remove-hook 'window-size-change-functions
               #'picpocket-window-size-change-function)
  (remove-hook 'buffer-list-update-hook
               #'picpocket-maybe-update-keymap)
  (remove-hook 'buffer-list-update-hook
               #'picpocket-maybe-rescale)
  (remove-hook 'focus-in-hook
               #'picpocket-focus)
  (remove-hook 'minibuffer-setup-hook
               #'picpocket-minibuffer-setup)
  (remove-hook 'minibuffer-exit-hook
               #'picpocket-minibuffer-exit))


(defun picpocket-window-size-change-function (frame)
  (when picpocket-adapt-to-window-size-change
    (dolist (window (window-list frame 'no-minibuffer))
      (when (eq (get-buffer picpocket-buffer) (window-buffer window))
        (with-selected-window window
          (with-current-buffer picpocket-buffer
            (unless (equal picpocket-window-size (picpocket-save-window-size))
              (picpocket-update-buffer))))))))

(defun picpocket-maybe-update-keymap ()
  (and picpocket-keystroke-alist
       (get-buffer picpocket-buffer)
       (eq (current-buffer) (get-buffer picpocket-buffer))
       (not (eq (picpocket-keystroke-alist)
                picpocket-old-keystroke-alist))
       (picpocket-update-keymap)))

(defun picpocket-maybe-rescale ()
  (let ((buffer (get-buffer picpocket-buffer)))
    (and buffer
         picpocket-adapt-to-window-size-change
         (eq (current-buffer) buffer)
         (eq (window-buffer) buffer)
         ;; It is important to check and save window size here.
         ;; Otherwise the call to picpocket-old-update-buffer may trigger an
         ;; infinite loop via buffer-list-update-hook.
         (not (equal picpocket-window-size (picpocket-save-window-size)))
         (picpocket-update-buffer))))

(defun picpocket-delete-trashcan ()
  (when picpocket-trashcan
    (delete-directory picpocket-trashcan t)
    (setq picpocket-trashcan nil)))



;;; Debug functions

;; PENDING call at start, finish and interrupt of idle functions...
(defun picpocket-debug2 (format &optional args)
  (apply #'message format args))
;; (update-header-debug-thing...)
;; (scroll-message-buffer-if-visible...))


(defun picpocket-debug (s format &rest args)
  (when picpocket-debug
    (setq picpocket-sum (time-add picpocket-sum s))
    (setq picpocket-header-text (format "(%s %s (%s))"
                                        (apply #'format format args)
                                        (picpocket-sec-string s)
                                        (picpocket-sec-string picpocket-sum)))
    (message "picpocket-debug: %s" picpocket-header-text)))

(defun picpocket-dump ()
  "Print some picpocket variables."
  (interactive)
  (picpocket-command
    (picpocket-print 'picpocket-list)
    (picpocket-print 'picpocket-current)
    (picpocket-print 'picpocket-index)
    (picpocket-print 'picpocket-list-length)
    (message "")
    (view-echo-area-messages)
    t))

(defun picpocket-print (var)
  (let ((value (symbol-value var)))
    (and (listp value)
         (picpocket-pic-p (car value))
         (setq value (picpocket-simplify-list value)))
    (message "%-20s%s"
             var
             (with-temp-buffer
               (pp value (current-buffer))
               (goto-char (point-min))
               (forward-line)
               (indent-rigidly (point) (point-max) 20)
               (buffer-string)))))

(defun picpocket-simplify-list (&optional list)
  "Print the LIST or current picture list without the prev links.
With the prev links it is harder to follow the list."
  (cl-loop for pic in (or list picpocket-list)
           for copy = (copy-picpocket-pic pic)
           do (setf (picpocket-pic-prev copy) :prev)
           collect copy))

(defun picpocket-dump-list (&optional list)
  (pp (picpocket-simplify-list list)))

(defun picpocket-action-past-tense (action)
  (cl-case action
    (add-tag "Tagged")
    (copy "Copied")
    (move "Moved")
    (rename "Renamed")
    (hardlink "Hard linked")))

;; PENDING - just for comparing between single-linked and
;; double-linked list.
(defun picpocket-backwards ()
  "Move backwards without using double-linked list."
  (interactive)
  (picpocket-command
    (when (eq picpocket-list picpocket-current)
      (user-error "No previous pic"))
    (let* ((list picpocket-list)
           (time (picpocket-time-string
                   (while (not (eq picpocket-current (cdr list)))
                     (setq list (cdr list))))))
      (picpocket-list-set-pos (make-picpocket-pos :current list
                                                  :index (1- picpocket-index)))
      (message "Back %s" time))))


(provide 'picpocket)

;;; picpocket.el ends here
