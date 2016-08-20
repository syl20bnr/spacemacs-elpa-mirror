;;; scroll-restore.el --- restore original position after scrolling  -*- lexical-binding:t -*-

;; Copyright (C) 2007,2014  Free Software Foundation, Inc.

;; Time-stamp: "2007-12-05 10:44:11 martin"
;; Author: Martin Rudalics <rudalics@gmx.at>
;; Keywords: scrolling
;; Version: 1.0

;; scroll-restore.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; scroll-restore.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Scroll Restore mode is a minor mode to restore the position of
;; `point' in a sequence of scrolling commands whenever that position
;; has gone off-screen and becomes visible again.  The user option
;; `scroll-restore-commands' specifies the set of commands that may
;; constitute such a sequence.

;; The following additional options are provided:

;; - Recenter the window when restoring the original position, see
;;   `scroll-restore-recenter'.

;; - Jump back to the original position before executing a command not
;;   in `scroll-restore-commands', see `scroll-restore-jump-back'.  The
;;   resulting behavior is similar to that provided by a number of word
;;   processors.

;; - Change the appearance of the cursor in the selected window to
;;   indicate that the original position is off-screen, see
;;   `scroll-restore-handle-cursor'.

;; - With `transient-mark-mode' non-nil Emacs highlights the region
;;   between `point' and `mark' when the mark is active.  If you scroll
;;   `point' off-screen, Emacs relocates `point' _and_ the region.
;;   Customizing `scroll-restore-handle-region' permits to highlight the
;;   original region as long as the original position of `point' is
;;   off-screen, and restore the original region whenever the original
;;   position of `point' becomes visible again.


;; Caveats:

;; - Scroll Restore mode does not handle `switch-frame' and
;;   `vertical-scroll-bar' events executed within the loops in
;;   `mouse-show-mark' and `scroll-bar-drag' (these don't call
;;   `post-command-hook' as needed by Scroll Restore mode).

;; - Scroll Restore mode may disregard your customizations of
;;   `scroll-margin'.  Handling `scroll-margin' on the Elisp level is
;;   tedious and might not work correctly.

;; - Scroll Restore mode should handle `make-cursor-line-fully-visible'
;;   but there might be problems.

;; - Scroll Restore mode can handle region and cursor only in the
;;   selected window.  This makes a difference when you have set
;;   `highlight-nonselected-windows' to a non-nil value.

;; - Scroll Restore mode has not been tested with emulation modes like
;;   `cua-mode' or `pc-selection-mode'.  In particular, the former's
;;   handling of `cursor-type' and `cursor-color' might be affected by
;;   Scroll Restore mode."

;; - Scroll Restore mode might interact badly with `follow-mode'.  For
;;   example, the latter may deliberately select a window A when the
;;   original position of a window B appears in it.  This won't restore
;;   the appearance of the cursor when Scroll Restore mode handles it.


;;; Code:

(defgroup scroll-restore nil
  "Restore original position after scrolling."
  :version "23.1"
  :group 'windows)

(defun scroll-restore--set (symbol value)
  (set-default symbol value)
  (when (and (boundp 'scroll-restore-mode) scroll-restore-mode)
    (scroll-restore-mode -1)
    (scroll-restore-mode 1)))

(defcustom scroll-restore-commands
  ;; FIXME: How 'bout using the `scroll-command' property?
  '(handle-select-window handle-switch-frame
    scroll-up scroll-down
    scroll-up-command scroll-down-command
    scroll-bar-toolkit-scroll mwheel-scroll
    scroll-other-window scroll-other-window-down
    scroll-bar-scroll-up scroll-bar-scroll-down scroll-bar-drag)
  "Commands handled by Scroll Restore mode.
Scroll Restore mode will try to restore the original position of
`point' after executing a sequence of any of these commands."
  :type '(repeat symbol)
  :set #'(lambda (symbol value)
           (when (boundp 'scroll-restore-commands)
             (dolist (cmd scroll-restore-commands)
               (put cmd 'scroll-restore nil)))
           (set-default symbol value)
           (dolist (cmd scroll-restore-commands)
             (put cmd 'scroll-restore t))))

;; Recenter.
(defcustom scroll-restore-recenter nil
  "Non-nil means scrolling back recenters the original position.
Setting this to a non-nil value can be useful to detect the original
position more easily and coherently when scrolling back."
  :type 'boolean)

;; Jump back.
(defcustom scroll-restore-jump-back nil
  "Non-nil means jump back to original position after scrolling.
When this option is non-nil, Scroll Restore mode resets `point'
to the original position when scrolling has moved that position
off-screen and a command not in `scroll-restore-commands' shall
be executed.  The resulting behavior is similar to that of some
word processors.  You probably want to remove commands like
`scroll-up' and `scroll-down' from `scroll-restore-commands' when
activating this option.

Alternatively you may consider binding the command
`scroll-restore-jump-back' to a key of your choice."
  :type 'boolean
  :set #'scroll-restore--set)

;;; Cursor handling.
(defvar scroll-restore-buffer nil
  "Buffer for `scroll-restore-cursor-type'.")

;; Note: nil is a valid cursor-type.
(defvar scroll-restore-buffer-cursor-type 'invalid
  "Original cursor-type of `scroll-restore-buffer'.")

(defvar scroll-restore-frame nil
  "Frame for `scroll-restore-cursor-color'.")

(defvar scroll-restore-frame-cursor-color nil
  "Original cursor-color of `scroll-restore-frame'.")

(defcustom scroll-restore-handle-cursor nil
  "Non-nil means Scroll Restore mode may change appearance of cursor.
Scroll Restore mode can change the appearance of the cursor in
the selected window while the original position is off-screen.
Customize `scroll-restore-cursor-type' to change the type of the
cursor and `scroll-restore-cursor-color' to change its color."
  :type '(choice
          (const :tag "Off" nil)
          (const :tag "Cursor type" type)
          (const :tag "Cursor color" color)
          (const :tag "Type and color" t))
  :set #'scroll-restore--set)

(defcustom scroll-restore-cursor-type 'box
  "Type of cursor when original position is off-screen.
Applied if and only if `scroll-restore-handle-cursor' is either
'type or t.

Be careful when another application uses that type.  Otherwise,
you might get unexpected results when Scroll Restore mode resets
the cursor type to its \"original\" value after a sequence of
scrolling commands and the application has changed the cursor
type in between.

To guard against unexpected results, Scroll Restore mode does not
reset the type of the cursor whenever its value does not equal
the value of scroll-restore-cursor-type."
  :type '(choice
          (const :tag "No cursor" nil)
          (const :tag "Filled box" box)
          (const :tag "Hollow box" hollow)
          (const :tag "Vertical bar" bar)
          (const :tag "Horizontal bar" hbar))
  :set #'scroll-restore--set)

(defcustom scroll-restore-cursor-color "DarkCyan"
  "Background color of cursor when original position is off-screen.
Applied if and only if `scroll-restore-handle-cursor' is either
'color or t.

Observe that when Emacs changes the color of the cursor, the
change applies to all windows on the associated frame.

Be careful when another application is allowed to change the
cursor-color.  Otherwise, you might get unexpected results when
Scroll Restore mode resets the cursor color to its \"original\"
value and the application has changed the cursor color in
between.

To guard against unexpected results Scroll Restore mode does not
reset the color of the cursor whenever its value does not equal
the value of scroll-restore-cursor-color."
  :type 'color
  :set #'scroll-restore--set)

;;; Region handling.

;; FIXME: We should try to use pre-redisplay-function instead.

(defvar scroll-restore-region-overlay
  (let ((overlay (make-overlay (point-min) (point-min))))
    (overlay-put overlay 'face 'scroll-restore-region)
    (delete-overlay overlay)
    overlay)
  "Overlay used for highlighting the region.")

(defcustom scroll-restore-handle-region nil
  "Non-nil means Scroll Restore mode handles the region.
This affects the behavior of Emacs in `transient-mark-mode' only.
In particular, Emacs will suppress highlighting the region as
long as the original position of `point' is off-screen.  Rather,
Emacs will highlight the original region \(the region before
scrolling started\) in `scroll-restore-region' face.  Scrolling
back to the original position will restore the region to its
original state.

Note that Scroll Restore mode does not deactivate the mark during
scrolling.  Hence any operation on the region will not use the
original but the _actual_ value of `point'.

If you mark the region via `mouse-drag-region', setting this
option has no effect since Scroll Restore mode cannot track mouse
drags."
  :type 'boolean
  :set #'scroll-restore--set)

(defface scroll-restore-region
  '((t :inherit region))
  "Face for Scroll Restore region when `scroll-restore-handle-region' is 
non-nil.")

;; Note: We can't use `point-before-scroll' for our purposes because
;; that variable is buffer-local.  We need a variable that recorded
;; `window-point' before a sequence of scroll operations.  Also
;; `point-before-scroll' is not handled by mwheel.el and some other
;; commands that do implicit scrolling.  hence, the original position is
;; handled, among others, by the following alist.
(defvar scroll-restore-alist nil
  "List of <window, buffer, point> quadruples.
`window' is the window affected, `buffer' its buffer.  `pos' is
the original position of `point' in that window.  `off' non-nil
means `pos' was off-screen \(didn't appear in `window'\).")

(defun scroll-restore-pre-command ()
  "Scroll Restore's pre-command function."
  (let ((overlay-buffer (overlay-buffer scroll-restore-region-overlay)))
    ;; Handle region overlay.
    (when overlay-buffer
      ;; Remove `transient-mark-mode' binding in any case.
      (with-current-buffer overlay-buffer
        (kill-local-variable 'transient-mark-mode))
      (delete-overlay scroll-restore-region-overlay)))
  ;; Handle cursor-type.
  (when (and scroll-restore-buffer
             (not (eq scroll-restore-buffer-cursor-type 'invalid))
             (with-current-buffer scroll-restore-buffer
               (eq cursor-type scroll-restore-cursor-type)))
    (with-current-buffer scroll-restore-buffer
      (setq cursor-type scroll-restore-buffer-cursor-type)
      (setq scroll-restore-buffer-cursor-type 'invalid)))
  ;; Handle cursor-color.
  (when (and scroll-restore-frame scroll-restore-frame-cursor-color
             (eq (frame-parameter scroll-restore-frame 'cursor-color)
                 scroll-restore-cursor-color))
    (let ((frame (selected-frame)))
      (select-frame scroll-restore-frame)
      (set-cursor-color scroll-restore-frame-cursor-color)
      (setq scroll-restore-frame-cursor-color nil)
      (select-frame frame)))
  ;; Handle jumping.
  (when (and scroll-restore-jump-back
             (not (get this-command 'scroll-restore)))
    (let ((entry (assq (selected-window) scroll-restore-alist)))
      (when entry
        (let ((window (car entry))
              ;; (buffer (nth 1 entry))
              (pos (nth 2 entry)))
          (set-window-point window pos)
          ;; We are on-screen now.
          (setcdr (nthcdr 2 entry) (list nil))))))
  ;; Paranoia.
  (unless (or scroll-restore-jump-back scroll-restore-handle-region
              scroll-restore-handle-cursor)
    ;; Should be never reached.
    (remove-hook 'pre-command-hook 'scroll-restore-pre-command)))

(defun scroll-restore-remove (&optional all)
  "Remove stale entries from `scroll-restore-alist'.
Optional argument ALL non-nil means remove them all."
  (dolist (entry scroll-restore-alist)
    (let ((window (car entry))
          (buffer (nth 1 entry))
          (pos (nth 2 entry)))
      (when (or all (not (window-live-p window))
                (not (eq (window-buffer window) buffer))
                (not (markerp pos)) (not (marker-position pos)))
        (when (markerp pos)
          (set-marker pos nil))
        (setq scroll-restore-alist
              (assq-delete-all window scroll-restore-alist))))))

(defun scroll-restore-add ()
  "Add new entries to `scroll-restore-alist'."
  (walk-windows
   (lambda (window)
     (unless (assq window scroll-restore-alist)
       (let ((buffer (window-buffer window)))
             (setq scroll-restore-alist
                   (cons
                    (list
                     window buffer
                     (with-current-buffer buffer
                       (copy-marker (window-point window)))
                     nil)
                    scroll-restore-alist)))))
   'no-mini t))

(defun scroll-restore-update (how window buffer pos)
  "Update various things in `scroll-restore-post-command'.
HOW must be either on-off, on-on, off-off, off-on, or t.  WINDOW
and BUFFER are affected window and buffer.  POS is the original
position."
  (when (eq window (selected-window))
    (with-current-buffer buffer
      ;; Handle region.
      (when scroll-restore-handle-region
        (if (and transient-mark-mode mark-active
                 (not deactivate-mark)
                 (memq how '(on-off off-off)))
            (progn
              (move-overlay scroll-restore-region-overlay
                            (min pos (mark)) (max pos (mark)) buffer)
              (overlay-put scroll-restore-region-overlay 'window window)
              ;; Temporarily disable `transient-mark-mode' in this buffer.
              (set (make-local-variable 'transient-mark-mode) nil))
          (delete-overlay scroll-restore-region-overlay)))
      ;; Handle cursor.
      (when (and scroll-restore-handle-cursor
                 (memq how '(on-off off-off))
                 ;; Change cursor iff there was a visible cursor.
                 cursor-type)
        (when (memq scroll-restore-handle-cursor '(type t))
          (setq scroll-restore-buffer buffer)
          (setq scroll-restore-buffer-cursor-type cursor-type)
          (setq cursor-type scroll-restore-cursor-type))
        (when (memq scroll-restore-handle-cursor '(color t))
          (setq scroll-restore-frame (window-frame window))
          (setq scroll-restore-frame-cursor-color
                (frame-parameter scroll-restore-frame 'cursor-color))
          (let ((frame (selected-frame)))
            (select-frame scroll-restore-frame)
            (set-cursor-color scroll-restore-cursor-color)
            (select-frame frame)))))))

(defun scroll-restore-post-command ()
  "Scroll Restore mode post-command function."
  (scroll-restore-remove)
  (let (recenter)
    (dolist (entry scroll-restore-alist)
      (let ((window (car entry))
            (buffer (nth 1 entry))
            (pos (nth 2 entry))
            (off (nth 3 entry)))
        (if (get this-command 'scroll-restore)
            ;; A scroll restore command.
            (if off
                ;; `pos' was off-screen.
                (if (pos-visible-in-window-p (marker-position pos) window)
                    ;; `pos' is on-screen now.
                    (progn
                      ;; Move cursor to original position.
                      (set-window-point window pos)
                      ;; Recenter if desired.
                      (when (and scroll-restore-recenter
                                 (eq window (selected-window)))
                        (setq recenter (/ (window-height window) 2)))
                      ;; Record on-screen status.
                      (setcdr (nthcdr 2 entry) (list nil))
                      (scroll-restore-update 'off-on window buffer pos))
                  ;; `pos' is still off-screen
                  (scroll-restore-update 'off-off window buffer pos))
              ;; `pos' was on-screen.
              (if (pos-visible-in-window-p pos window)
                  ;; `pos' is still on-screen.
                  (progn
                    ;; Occasionally Emacs deliberately changes
                    ;; `window-point' during scrolling even when
                    ;; it's visible.  Maybe this is due to
                    ;; `make-cursor-line-fully-visible' maybe due to
                    ;; `scroll-margin' maybe due to something else.
                    ;; We override that behavior here.
                    (unless (= (window-point) pos)
                      (set-window-point window pos))
                    (scroll-restore-update 'on-on window buffer pos))
                ;; `pos' moved off-screen.
                ;; Record off-screen state.
                (setcdr (nthcdr 2 entry) (list t))
                (scroll-restore-update 'on-off window buffer pos)))
          ;; Not a scroll-restore command.
          (let ((window-point (window-point window)))
                  (when (and (eq window (selected-window))
                             (or (/= window-point pos) off))
                    ;; Record position and on-screen status.
                    (setcdr
                     (nthcdr 1 entry)
                     (list (move-marker pos (window-point window)) nil)))
                  (scroll-restore-update t window buffer pos)))))
    (scroll-restore-add)
    (when recenter (recenter recenter))))

(defun scroll-restore-jump-back ()
  "Jump back to original position.
The orginal position is the value of `window-point' in the
selected window before you started scrolling.

This command does not push the mark."
  (interactive)
  (let ((entry (assq (selected-window) scroll-restore-alist)))
    (if entry
        (goto-char (nth 2 entry))
      (error "No jump-back position available"))))

;;;###autoload
(define-minor-mode scroll-restore-mode
  "Toggle Scroll Restore mode.
With arg, turn Scroll Restore mode on if arg is positive, off
otherwise.

In Scroll Restore mode Emacs attempts to restore the original
position that existed before executing a sequence of scrolling
commands whenever that position becomes visible again.  The
option `scroll-restore-commands' permits to specify the set of
commands that may constitute such a sequence.  In addition you
can

- recenter the window when you scroll back to the original
  position, see the option `scroll-restore-recenter',

- aggressively jump back to the original position before
  executing a command not in `scroll-restore-commands', see
  `scroll-restore-jump-back',

- change the appearance of the cursor in the selected window
  while the original position is off-screen, see the option
  `scroll-restore-handle-cursor',

- change the appearance of the region in the selected window
  while the original position is off-screen, see the option
  `scroll-restore-handle-region'."
  :global t
  :group 'scroll-restore
  :init-value nil
  :link '(emacs-commentary-link "scroll-restore.el")
  (if scroll-restore-mode
      (progn
        (scroll-restore-add)
        (when (or scroll-restore-jump-back scroll-restore-handle-region
                  scroll-restore-handle-cursor)
          (add-hook 'pre-command-hook 'scroll-restore-pre-command))
        (add-hook 'post-command-hook 'scroll-restore-post-command t))
    (scroll-restore-remove 'all)
    (remove-hook 'pre-command-hook 'scroll-restore-pre-command)
    (remove-hook 'post-command-hook 'scroll-restore-post-command)))

;;;; ChangeLog:

;; 2014-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* scroll-restore.el (scroll-restore--set): New function.  Use it as
;; 	setter instead of calling scroll-restore-restart.
;; 	(scroll-restore-restart): Remove.
;; 	(scroll-restore-mode): Add autoload cookie.
;; 	(scroll-restore-commands): Add scroll-up-command and
;; 	scroll-down-command.
;; 
;; 2014-10-15  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* scroll-restore: New package.
;; 


(provide 'scroll-restore)
;;; scroll-restore.el ends here
