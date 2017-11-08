;;; rcirc-menu --- A menu of all your rcirc connections

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Alex Schroeder <alex@gnu.org>
;; Maintainer: Alex Schroeder <alex@gnu.org>
;; Created: 2017-08-10
;; Version: 1.1
;; Keywords: comm

;; This file is part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; If you are connected to too many channels, `rcirc-track-minor-mode'
;; is useless because the modeline is too short. Bind `rcirc-menu' to
;; a key instead:
;;
;; (global-set-key (kbd "C-c r") 'rcirc-menu)

;;; Code:
(require 'rcirc)

;;;###autoload
(defun rcirc-menu ()
  "Show a list of all your `rcirc' buffers."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Rcirc Menu*"))
  (rcirc-menu-mode)
  (rcirc-menu-refresh)
  (tabulated-list-print))

(defvar rcirc-menu-mode-map
  (let ((map (make-sparse-keymap))
	(menu-map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "v" 'Buffer-menu-select)
    (define-key map "2" 'Buffer-menu-2-window)
    (define-key map "1" 'Buffer-menu-1-window)
    (define-key map "f" 'Buffer-menu-this-window)
    (define-key map "e" 'Buffer-menu-this-window)
    (define-key map "\C-m" 'Buffer-menu-this-window)
    (define-key map "o" 'Buffer-menu-other-window)
    (define-key map "\C-o" 'Buffer-menu-switch-other-window)
    (define-key map "d" 'Buffer-menu-delete)
    (define-key map "k" 'Buffer-menu-delete)
    (define-key map "\C-k" 'Buffer-menu-delete)
    (define-key map "\C-d" 'Buffer-menu-delete-backwards)
    (define-key map "x" 'Buffer-menu-execute)
    (define-key map " " 'next-line)
    (define-key map "\177" 'Buffer-menu-backup-unmark)
    (define-key map "u" 'Buffer-menu-unmark)
    (define-key map "m" 'Buffer-menu-mark)
    (define-key map "b" 'Buffer-menu-bury)
    (define-key map (kbd "M-s a C-s")   'Buffer-menu-isearch-buffers)
    (define-key map (kbd "M-s a M-C-s") 'Buffer-menu-isearch-buffers-regexp)
    (define-key map (kbd "M-s a C-o") 'Buffer-menu-multi-occur)
    (define-key map "c" 'rcirc-menu-catchup)
    (define-key map "C" 'rcirc-menu-catchup-all)

    (define-key map [mouse-2] 'Buffer-menu-mouse-select)
    (define-key map [follow-link] 'mouse-face)

    (define-key map [menu-bar rcirc-menu-mode] (cons (purecopy "Rcirc-Menu") menu-map))
    (bindings--define-key menu-map [quit]
      '(menu-item "Quit" quit-window
		 :help "Remove the rcirc menu from the display"))
    (bindings--define-key menu-map [rev]
      '(menu-item "Refresh" revert-buffer
		 :help "Refresh the *Rcirc Menu* buffer contents"))
    (bindings--define-key menu-map [s0] menu-bar-separator)
    (bindings--define-key menu-map [cata]
      '(menu-item "Catch up All" rcirc-menu-catchup-all
		 :help "Mark all buffers as read"))
    (bindings--define-key menu-map [cat]
      '(menu-item "Catch up" rcirc-menu-catchup
		 :help "Mark this buffer as read"))
    (bindings--define-key menu-map [s1] menu-bar-separator)
    (bindings--define-key menu-map [sel]
      '(menu-item "Select Marked" Buffer-menu-select
		 :help "Select this line's buffer; also display buffers marked with `>'"))
    (bindings--define-key menu-map [bm2]
      '(menu-item "Select Two" Buffer-menu-2-window
		 :help "Select this line's buffer, with previous buffer in second window"))
    (bindings--define-key menu-map [bm1]
      '(menu-item "Select Current" Buffer-menu-1-window
		 :help "Select this line's buffer, alone, in full frame"))
    (bindings--define-key menu-map [ow]
      '(menu-item "Select in Other Window" Buffer-menu-other-window
		 :help "Select this line's buffer in other window, leaving buffer menu visible"))
    (bindings--define-key menu-map [tw]
      '(menu-item "Select in Current Window" Buffer-menu-this-window
		 :help "Select this line's buffer in this window"))
    (bindings--define-key menu-map [s2] menu-bar-separator)
    (bindings--define-key menu-map [is]
      '(menu-item "Regexp Isearch Marked Buffers..." Buffer-menu-isearch-buffers-regexp
		 :help "Search for a regexp through all marked buffers using Isearch"))
    (bindings--define-key menu-map [ir]
      '(menu-item "Isearch Marked Buffers..." Buffer-menu-isearch-buffers
		 :help "Search for a string through all marked buffers using Isearch"))
    (bindings--define-key menu-map [mo]
      '(menu-item "Multi Occur Marked Buffers..." Buffer-menu-multi-occur
		 :help "Show lines matching a regexp in marked buffers using Occur"))
    (bindings--define-key menu-map [s3] menu-bar-separator)
    (bindings--define-key menu-map [by]
      '(menu-item "Bury" Buffer-menu-bury
		 :help "Bury the buffer listed on this line"))
    (bindings--define-key menu-map [ex]
      '(menu-item "Execute" Buffer-menu-execute
		 :help "Delete buffers marked with k commands"))
    (bindings--define-key menu-map [s4] menu-bar-separator)
    (bindings--define-key menu-map [delb]
      '(menu-item "Mark for Delete and Move Backwards" Buffer-menu-delete-backwards
		 :help "Mark buffer on this line to be deleted by x command and move up one line"))
    (bindings--define-key menu-map [del]
      '(menu-item "Mark for Delete" Buffer-menu-delete
		 :help "Mark buffer on this line to be deleted by x command"))
    (bindings--define-key menu-map [umk]
      '(menu-item "Unmark" Buffer-menu-unmark
		 :help "Cancel all requested operations on buffer on this line and move down"))
    (bindings--define-key menu-map [mk]
      '(menu-item "Mark" Buffer-menu-mark
		 :help "Mark buffer on this line for being displayed by v command"))
    map)
  "Local keymap for `rcirc-menu-mode' buffers.")

(define-derived-mode rcirc-menu-mode tabulated-list-mode "Rcirc Menu"
  "Major mode for Rcirc Menu buffers.
The Rcirc Menu is invoked by the command \\[rcirc-menu].

Columns:
  T - this buffer has a target, a live connection
  P - buffer priority

In Rcirc Menu mode, the following commands are defined:
\\<rcirc-menu-mode-map>
\\[quit-window]    Remove the Buffer Menu from the display.
\\[tabulated-list-sort]    sorts buffers according to the current
     column. With a numerical argument, sort by that column.
\\[Buffer-menu-this-window]  Select current line's buffer in place of the buffer menu.
\\[Buffer-menu-other-window]    Select that buffer in another window,
     so the Buffer Menu remains visible in its window.
\\[Buffer-menu-switch-other-window]  Make another window display that buffer.
\\[Buffer-menu-mark]    Mark current line's buffer to be displayed.
\\[Buffer-menu-select]    Select current line's buffer.
     Also show buffers marked with m, in other windows.
\\[Buffer-menu-1-window]    Select that buffer in full-frame window.
\\[Buffer-menu-2-window]    Select that buffer in one window, together with the
     buffer selected before this one in another window.
\\[Buffer-menu-isearch-buffers]    Incremental search in the marked buffers.
\\[Buffer-menu-isearch-buffers-regexp]  Isearch for regexp in the marked buffers.
\\[Buffer-menu-multi-occur] Show lines matching regexp in the marked buffers.
\\[Buffer-menu-delete]  Mark that buffer to be deleted, and move down.
\\[Buffer-menu-delete-backwards]  Mark that buffer to be deleted, and move up.
\\[Buffer-menu-execute]    Delete or save marked buffers.
\\[Buffer-menu-unmark]    Remove all marks from current line.
     With prefix argument, also move up one line.
\\[Buffer-menu-backup-unmark]  Back up a line and remove marks.
\\[revert-buffer]    Update the list of buffers.
\\[Buffer-menu-bury]    Bury the buffer listed on this line."
  (add-hook 'tabulated-list-revert-hook 'rcirc-menu-refresh nil t))

(defun rcirc-menu-refresh ()
  "Refresh the list of buffers."
    ;; Set up `tabulated-list-format'.
    (setq tabulated-list-format
	  (vector '("T" 1 t)
		  '("P" 1 rcirc-menu-sort-priority)
		  '("Target" 30 t)
		  '("Server" 20 t)
		  '("Activity" 10 rcirc-menu-sort-activity))
	  tabulated-list-sort-key '("Activity"))
    ;; Collect info for each buffer we're interested in.
    (let* ((pair (rcirc-split-activity rcirc-activity))
	   (lopri (car pair))
	   (hipri (cdr pair))
	   entries)
      (dolist (buf (buffer-list))
	(with-current-buffer buf
	  (when (eq major-mode 'rcirc-mode)
	    (push (list buf
			(vector
			 (if rcirc-target "•" " ") ;; "T"
			 (cond ((memq buf hipri) "↑")
			       ((memq buf lopri) "↓")
			       (t " ")) ;; "P"
			 (or rcirc-target
			     (car (split-string
				   (buffer-name) "@"))) ;; "Target"
			 (with-current-buffer rcirc-server-buffer
			   rcirc-server-name) ;; "Server"
			 (rcirc-menu-activity))) ;; "Activity"
			entries))))
      (setq tabulated-list-entries (nreverse entries)))
    (tabulated-list-init-header))

(defun rcirc-menu-sort-priority (&rest args)
  "Sort by priority.
ARGS is a list of two elements having the same form as the
elements of ‘tabulated-list-entries’."
  (setq args (mapcar (lambda (v)
		       (let ((s (aref (cadr v) 1)))
			 (cond ((string= s "↑") 1)
			       ((string= s "↓") 3)
			       (t 2))))
		     args))
  (apply '< args))

(defun rcirc-menu-sort-activity (&rest args)
  "Sort by activity.
ARGS is a list of two elements having the same form as the
elements of ‘tabulated-list-entries’. At this point, we only have
the comma-separated string produced by `rcirc-menu-activity' in
the vector. The alternative is to simply visit the buffers and
examine `rcirc-activity-types'."
  (setq args (mapcar (lambda (v)
		       (let ((buf (car v)))
			 (with-current-buffer buf
			   (cond ((memq 'nick rcirc-activity-types) 1)
				 ((memq 'keyword  rcirc-activity-types) 2)
				 (rcirc-activity-types 3)
				 (t 4)))))
		     args))
  (apply '< args))

(defun rcirc-menu-sort-activity-symbols (&rest args)
  "Sort by activity symbols.
ARGS are symbols from `rcirc-activity-types'."
  (setq args (mapcar (lambda (v)
		       (cond ((eq v 'nick) 1)
			     ((eq v 'keyword) 2)
			     ((not v) 3)
			     (t 4)))
		     args))
  (apply '< args))

(defun rcirc-menu-activity ()
  "Return string describing activity in the current buffer."
  (mapconcat (lambda (s)
	       (cond ((eq s 'nick)
		      (rcirc-facify "nick" 'rcirc-track-nick))
		     ((eq s 'keyword)
		      (rcirc-facify "keyword" 'rcirc-track-keyword))
		     (t "yes")))
	     (sort (copy-sequence rcirc-activity-types)
		   'rcirc-menu-sort-activity-symbols)
	     ", "))

(defun rcirc-menu-catchup ()
  "Mark the current buffer or the marked buffers as read.
This resets their activity."
  (interactive)
  (let* ((this-buffer (list (Buffer-menu-buffer t)))
	 (marked-buffers (Buffer-menu-marked-buffers))
	 (buffers (or marked-buffers this-buffer)))
    (dolist (buf buffers)
      (rcirc-clear-activity buf)))
  (rcirc-menu-catchup-finish))

(defun rcirc-menu-catchup-all ()
  "Mark all the buffers as read, i.e. no activity."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when rcirc-activity-types
	(rcirc-clear-activity buf))))
  (rcirc-menu-catchup-finish))

(defun rcirc-menu-catchup-finish ()
  "Update buffer and activity string after catching up."
  ;; Don't call rcirc-menu-update which reverts the tabulated list.
  (let ((rcirc-update-activity-string-hook
	 (delete 'rcirc-menu-update rcirc-update-activity-string-hook)))
    (rcirc-update-activity-string))
  ;; These two are from `tabulated-list-revert' but we don't want to
  ;; move point.
  (run-hooks 'tabulated-list-revert-hook)
  (tabulated-list-print))

(add-hook 'rcirc-update-activity-string-hook
	  'rcirc-menu-update)

(defun rcirc-menu-update ()
  "Update the Rcirc Menu buffer, if any."
  (let ((buf (get-buffer "*Rcirc Menu*")))
    (when buf
      (with-current-buffer buf
	(when (derived-mode-p 'tabulated-list-mode)
	  ;; this will move point
	  (tabulated-list-revert))))))

;;;; ChangeLog:

;; 2017-08-21  Alex Schroeder  <alex@gnu.org>
;; 
;; 	rcirc-menu: reset activity string when catching up
;; 
;; 	The rcirc activity string only gets updated when there is any new 
;; 	activity to record, or the window configuration changes. This is 
;; 	particularly noticeable after doing a catchup all for the rcirc-menu: 
;; 	the activity string in the modeline might remain unchanged for quite a 
;; 	while. This commit calls rcirc-update-activity-string after catching up
;; 	and makes sure that rcirc-update-activity-string-hook is bound to a 
;; 	value without rcirc-menu-update while doing so, in order to prevent the
;; 	rcirc-menu-update from calling tabulated-list-revert because we don't
;; 	like to revert. Reverting moves point and the existing catchup code
;; 	carefully avoids it by calling tabulated-list-print without the 
;; 	remember-pos argument.
;; 
;; 2017-08-16  Alex Schroeder  <alex@gnu.org>
;; 
;; 	rcirc-menu: fix tabulated lists everywhere
;; 
;; 	rcirc-menu-refresh is now added to tabulated-list-revert-hook locally in
;; 	order to fix all other modes based on tabulated-lists, e.g. the package
;; 	list.
;; 
;; 2017-08-14  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* rcirc-menu, rcirc-color: Fix copyright
;; 
;; 2017-08-12  Alex Schroeder  <alex@gnu.org>
;; 
;; 	rcirc-menu.el: explain what T and P are
;; 
;; 2017-08-12  Alex Schroeder  <alex@gnu.org>
;; 
;; 	rcirc-menu.el: guess target when rcirc-target is nil
;; 
;; 	Use the first part of buffer name instead of just showing no name.
;; 
;; 2017-08-11  Alex Schroeder  <alex@gnu.org>
;; 
;; 	rcirc-menu.el: fontify activity column
;; 
;; 2017-08-11  Alex Schroeder  <alex@gnu.org>
;; 
;; 	rcirc-menu.el: catchup marked buffers
;; 
;; 	When catching up, check for marked buffers. If there are any, catch up 
;; 	for each of them. If there are no marked buffers, use the current 
;; 	buffer.
;; 
;; 2017-08-11  Alex Schroeder  <alex@gnu.org>
;; 
;; 	rcirc-menu.el: make rcirc-menu-update more resilient
;; 
;; 	If the buffer doesn't exist, don't create it. If it exists but it 
;; 	doesn't have use a mode derived from tabulated-list-mode, ignore it. If
;; 	we don't do that, updating the rcirc activity string will call 
;; 	tabulated-list-revert which might throw an error.
;; 
;; 2017-08-10  Alex Schroeder  <alex@gnu.org>
;; 
;; 	rcirc-menu.el: add catching up and updating
;; 
;; 	c and C will now mark a buffer as read. Changes to the rcirc activity 
;; 	string will also update the menu buffer.
;; 
;; 2017-08-10  Alex Schroeder  <alex@gnu.org>
;; 
;; 	Add rcirc-menu
;; 


(provide 'rcirc-menu)

;;; rcirc-menu.el ends here
