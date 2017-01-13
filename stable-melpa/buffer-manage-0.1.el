;;; buffer-manage.el --- manage buffers

;; Copyright (C) 2015 - 2017 Paul Landes

;; Version: 0.1
;; Package-Version: 0.1
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: interactive buffer management
;; URL: https://github.com/plandes/buffer-manage
;; Package-Requires: ((emacs "25") (choice-program "0.1"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides support to manage buffers of any kind.  This is helpful for using
;; multiple inferior shells, multiple SQL session buffers or any other piped
;; process that requires multiple buffers.

;; The library includes support for:
;; * A major mode and buffer for listing, switching, and organizing multiple Emacs
;;   buffers.
;; * Fast switching with customized key bindings through the customize framework.
;; * Switch between managers providing the same key bindings for buffer entries
;;   with the same key bindings for creation, switching and managing.
;; * Create your own trivial implementations in minimal set of Emacs Lisp code.
;; * Interact with buffer entries and manager as objects with a straight forward
;;   API.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'derived)
(require 'choice-program-complete)

(defgroup buffer-manage nil
  "Buffer management and tracking"
  :group 'buffer-manage
  :prefix "buffer-manage-")

;;; key bindings

(defcustom buffer-manage-key-bindings
  '((nil (("switch" nil "C-x C-h")
	  ("list" nil "C-<tab>")
	  ("new" nil "M-C-<tab>")
	  ("toggle-cycle-method" nil "C-x C-'"))))
  "Specifies key bindings for buffer manager instance\(s) functions.
See `buffer-manager-bind-functions'."
  :group 'buffer-manage
  :type '(repeat
	  (list :tag "Key Bindings"
		(choice :tag "Applies To"
			(const :tag "All Managers" nil)
			(string :tag "Manager Name"))
		(repeat :tag "Manager Key Binding"
			(list :tag "Binding Entry"
			      (choice :tag "Action"
				      (const :tag "New Entry" "new")
				      (const :tag "List Entries" "list")
				      (const :tag "Switch To Entry" "switch")
				      (const :tag "Toggle Cycle Method" "toggle-cycle-method")
				      (string :tag "Other"))
			      (choice :tag "Keymap"
				      (const :tag "Global Keymap" nil)
				      (symbol :tag "Specific"))
			      (string :tag "Binding"))))))

;; buffer local vars to silence compiler
(defvar buffer-manager-instance)
(defvar buffer-entry-instance)
(defvar buffer-entry-status)
(defvar org-window-config)

(defvar buffer-manage-remap-instances nil
  "List of all instances that are eligible for remapping of keybindings.
This enables keybinding switches for various instances of managers to their
respect generated functions \(see `buffer-manager-interactive-functions').")

(defvar buffer-manage-on-mouse-down nil
  "Name of buffer entry for `buffer-manage-mode-mouse-*' functions.")



;;; base class
(defclass buffer-object-base ()
  ()
  :abstract true
  :documentation "Baes class for all buffer objects \(entries, managers etc).")

(cl-defmethod object-print-fields ((this buffer-object-base)) nil)

(cl-defmethod object-print ((this buffer-object-base) &optional strings)
  "Return a string as a representation of the in memory instance of THIS."
  (cl-flet* ((format-obj
	      (slot)
	      (let ((obj (eieio-oref this slot)))
		(format "%S %s"
			slot
			(cond ((eieio-object-p obj) (object-print obj))
			      ((stringp obj) (format "'%s'" obj))
			      (obj))))))
    (apply #'cl-call-next-method this
	   (cons (concat " " (mapconcat #'format-obj
					(object-print-fields this)
					" "))
		 strings))))



;;; buffer-entry class
(defclass buffer-entry (buffer-object-base)
  ((buffer :initarg :buffer
	   :reader buffer-entry-buffer
	   :protection :private
	   :documentation
	   "Contains the emacs buffer object."
	   )
   (kill-frame-p :initarg :kill-frame-p
		 :initform nil
		 :writer buffer-entry-set-kill-frame-p
		 :type boolean
		 :protection :protected
		 :documentation
		 "Whether or not to kill the containing frame when the
process dies."
		 )
   (name :initarg :name
	 :initform nil
	 :type (or null string)
	 :protection :protected
	 )
   (sentinel :initarg :sentinel
	     :initform nil
	     :type (or null function)
	     :protection :protected
	     )
   (manager :initarg :manager
	    :initform nil
	    :protection :protected
	    )
   )
  :abstract true
  :documentation "Abstract class for all buffer entry like objects.")

(cl-defmethod initialize-instance ((this buffer-entry) &rest rest)
  (apply #'cl-call-next-method this rest)
  (let ((win-cfg (current-window-configuration))
	new-buf)
    (with-slots (name sentinel manager) this
      (setq name (or name (buffer-manager-conical-name manager)))
      (unwind-protect
	  (let ((new-buf (buffer-entry-create-buffer this)))
	    (setq name (buffer-entry-format-buffer-name this name))
	    (oset this :buffer new-buf)
	    (with-current-buffer new-buf
	      (rename-buffer name t)
	      (set (make-local-variable 'buffer-entry-instance) this)
	      (add-hook 'post-command-hook
			'buffer-manage-post-command-hook nil t)
	      (when sentinel
		(set-process-sentinel
		 (get-buffer-process new-buf)
		 `(lambda (process msg)
		    (funcall (quote ,sentinel) process
			     msg ,this ,manager))))))
	(set-window-configuration win-cfg)))))

(cl-defmethod destructor ((this buffer-entry))
  "Dispose a `buffer-entry' instance by killing it's process.
Don't use this instance after this method is called.  Instead, don't reference
it and let the garbage collector get it."
  (when (buffer-entry-live-p this)
    (with-slots (buffer) this
     (when (get-buffer-process buffer)
       (buffer-entry-insert this "exit" t)
       (sit-for 0.5)))
    (if (oref this kill-frame-p)
	(delete-frame (window-frame (selected-window))))
    (kill-buffer (buffer-entry-buffer this))))

(cl-defmethod object-print-fields ((this buffer-entry))
  (append (cl-call-next-method) '(:buffer :kill-frame-p)))

(cl-defmethod buffer-entry-format-buffer-name ((this buffer-entry) name)
  "Create a buffer name from the entry instance's NAME."
  (format "*%s*" name))
;; we don't look for ending -1, -2...-N anymore--I think this was a
;; carry over from when duplicate buffer names weren't used and this
;; had to create them, `generate-new-buffer' etc adds the -1 for you
;; and no parsing needed--PL 11/20/2009

(cl-defmethod buffer-entry-name ((this buffer-entry))
  "Return the name of the entry."
  (buffer-entry-live-p this t)
  (with-slots (buffer) this
    (with-temp-buffer
      (insert (buffer-name buffer))
      (goto-char (point-min))
      (if (re-search-forward "^\\*\\(.*\\)\\*\\(?:<\\([0-9]+\\)>\\)?$" nil t)
	  (let ((first (match-string 1))
		(card (match-string 2)))
	    (if card
		(format "%s-%s" first card)
	      first))))))

(cl-defmethod buffer-entry-rename ((this buffer-entry) name)
  "Rename the buffer entry to NAME and return the new entry.
Note that NAME is not buffer name syntax, it is the name of the
entry."
  (buffer-entry-live-p this t)
  (with-current-buffer (buffer-entry-buffer this)
    (let ((buf-name (buffer-entry-format-buffer-name this name)))
      (rename-buffer buf-name t)))
  this)

(cl-defmethod buffer-entry-create-buffer ((this buffer-entry))
  "Create the buffer for this entry.
This is a factory method that is called once by the constructor of the object."
  (error "Must override `buffer-entry-create-buffer' for class `%S'"
	 (eieio-object-class this)))

(cl-defmethod buffer-entry-live-p ((this buffer-entry) &optional error-p)
  "Return non-nil if this entry is still active, otherwise it is useless and
the garbage collector should clean this instance up.

If ERROR-P is non-nil, raise an error stating that the buffer is no longer
valid.  This is useful for doing a check and getting the buffer at the same
time."
  (let ((live-p (buffer-live-p (buffer-entry-buffer this))))
    (if (and error-p (not (buffer-entry-live-p this)))
	(error "Buffer isn't valid"))
    live-p))

(cl-defmethod buffer-entry-end-of-window ((this buffer-entry))
  (let ((buf (buffer-entry-buffer this)))
    (with-current-buffer buf
      (goto-char (point-max))
      (set-window-point (get-buffer-window (current-buffer)) (point)))))



;;; class buffer-manager
(defclass buffer-manager (buffer-object-base)
  ((start-dir :initarg :start-dir
	      :initform "~/"
	      ;; customized vars need public access
					;:protection :protected
	      :reader buffer-manager-start-dir
	      :type (or null string)
	      :label "Start directory of the entry process"
	      :custom (choice
		       (const :tag "Default directory" nil)
		       (directory :tag "Specific Directory"))
	      :documentation
	      "Used for `default-directory' for new entries.
If `nil', the current value of `default directory' is used."
	      )
   (cycle-method :initarg :cycle-method
		 :initform last-visit
		 :reader buffer-manager-cycle-method
		 :writer buffer-manager-set-cycle-method
		 ;;:protection :private
		 :type symbol
		 :custom (choice :tag "Cycle Method"
				 (const :tag "Last Visited" last-visit)
				 (const :tag "Next In Line" next))
		 :documentation "\
How entries are cycled by default when invoking `buffer-manager-switch'.
This parameter is used as the default for `criteria' \(see
`buffer-manager-switch'), which is `cycle'."
		 )
   (include-frame-wins :initarg :include-frame-wins
		       :initform t
		       :reader buffer-manager-include-frame-wins
		       :writer buffer-manager-set-include-frame-wins
		       ;;:protection :private
		       :type boolean
		       :custom boolean
		       :documentation "\
When true, don't necessarily got to another entry in the frame just because
it's visible.  One way this happens is to chose the next desired entry
based on `cycle-method' regardeless of the last visited entry or what entries
are in the current frame."
		       )
   (entries :initarg :entries
	    :initform nil		; initialize with 0 entries
	    :type (or null list)
	    :protection :private
	    :documentation
	    "Contains the data structure for the buffer entries."
	    )
   (read-history :initform nil
		 :protection :private
		 :documentation "\
Used for history when reading user input when switching to other buffers."
		 )
   ;; careful: this slot keeps stale entries after they've been removed/killed
   (last-switched-to :initform nil
		     :protection :private
		     :documentation "\
Keeps track of the last entry for last-visit cycle method."
		     )
   )
  :abstract true
  :documentation "Manages buffer entries.")

(cl-defmethod destructor ((this buffer-manager))
  "Dispose by disping all buffer entries.
Don't use this instance after this method is called.  Instead, don't reference
it and let the garbage collector get it."
  (with-slots (entries) this
    (dolist (entry entries)
      (destructor entry))
    (let (new-entries)
      (dolist (entry entries)
	(if (buffer-entry-live-p entry)
	    (setq new-entries (append new-entries (cons entry nil)))))
      (setq entries new-entries))))

(cl-defmethod buffer-manager-create-entry ((this buffer-manager) &rest args)
  "Factory method to create a buffer entry \(factory method)."
  (error "No implementation of `buffer-manager-create-entry' for class `%S'"
	 (eieio-object-class this)))

(cl-defmethod buffer-manager-conical-name ((this buffer-manager))
  "The conical name of a `buffer-entry' instance \(i.e. `shell')."
  (error "Class `%S' has unsupported `buffer-manager-conical-name' method"
	 (eieio-object-class this)))

(cl-defmethod buffer-manager-name ((this buffer-manager))
  "The name of the manager \(i.e. `shell').
This is a descriptor used for interactive prompts etc.  It should be all lower
case."
  (error "Class `%S' has unsupported `buffer-manager-name' method"
	 (eieio-object-class this)))

(cl-defmethod buffer-manager-new-entry ((this buffer-manager) &optional
					name start-dir new-frame-p switchp)
  "Create a new entry instance and return its name.
If NAME is non-nil, use it as the name of the buffer entry,
otherwise, create a use a auto generated name."
  (with-slots (entries) this
    (let* ((default-directory (file-name-as-directory
			       (or start-dir
				   (buffer-manager-start-dir this)
				   default-directory)))
	   (new-entry (buffer-manager-create-entry
		       this
		       :sentinel 'buffer-manager-process-sentinel
		       :manager this
		       :kill-frame-p new-frame-p
		       :name name)))
      (setq entries (cons new-entry entries))
      (setq name (buffer-entry-name new-entry))
      (with-current-buffer (buffer-entry-buffer new-entry)
	(set (make-local-variable 'buffer-manager-instance) this))
      (message "Created %s `%s'" (buffer-manager-name this) name)
      (if switchp (buffer-manager-switch this new-entry new-frame-p))
      new-entry)))

(cl-defmethod buffer-manager-remove-entry ((this buffer-manager) entry)
  "Remove/kill ENTRY from this manager."
  (with-slots (entries) this
    (when (memq entry entries)
      (let ((name (buffer-entry-name entry)))
	(setq entries (remove entry entries))
	(destructor entry)
	entry))))

(cl-defmethod buffer-manager-remove-entries ((this buffer-manager)
					     &optional include-fn exclude-fn)
  "Remove/all buffer entries that satisfy criteria INCLUDE-FN EXCLUDE-FN."
  (let ((entries (buffer-manager-entries this include-fn exclude-fn)))
    (dolist (entry entries)
      (buffer-manager-remove-entry this entry))))

(cl-defmethod buffer-manager-display-entries ((this buffer-manager)
					      &optional include-fn exclude-fn
					      other-buffers sort-form)
  (let* ((entries (buffer-manager-entries this include-fn exclude-fn sort-form))
	 (bufs (append other-buffers
		       (mapcar 'buffer-entry-buffer entries))))
    (when bufs
      (let* ((wins (length bufs))
	     (win-height (/ (window-body-height) wins)))
	(delete-other-windows)
	(switch-to-buffer (car bufs))
	(dolist (buf (cdr bufs))
	  (split-window-vertically win-height)
	  (other-window 1)
	  (switch-to-buffer buf))
	(other-window 1)))))

(cl-defmethod buffer-manager-display-given-entries ((this buffer-manager)
						    entries &optional
						    other-buffers sort-form)
  (cl-flet ((include-fn
	     (entry)
	     (memq entry entries)))
    (buffer-manager-display-entries this #'include-fn
				    nil other-buffers sort-form)))

(cl-defmethod buffer-manager-cleanup ((this buffer-manager))
  "Remove all dead buffer entries."
  (condition-case err
      (with-slots (entries) this
	(setq entries
	      (remove nil
		      (mapcar #'(lambda (entry)
				  (if (buffer-entry-live-p entry) entry))
			      entries))))
    (error (with-current-buffer
	       (get-buffer-create "*Manage Buffer Errors*")
	     (goto-char (point-max))
	     (insert (format "In `buffer-manager-cleanup': %S\n" err))
	     (display-buffer (current-buffer))))))

(defun buffer-manager-process-sentinel (process msg entry manager)
  "Sentinal call back.
PROCESS the that finished.
MSG the message that resulted from the process.
ENTRY is the `buffer-entry' instance.
MANAGER the `buffer-manager' singleton instance."
  (if (or (string= msg "finished\n")
	  (save-match-data (string-match "^exited" msg)))
      (buffer-manager-remove-entry manager entry)))

(defun buffer-manager-kill-buffer-callback ()
  "Called by `kill-buffer-hook'."
  (when (and (boundp 'buffer-manager-instance)
	     ;; some buffers use managers and might not be entries
	     (boundp 'buffer-entry-instance))
    (let ((this buffer-manager-instance))
      (buffer-manager-remove-entry this buffer-entry-instance)
      (buffer-manager-cleanup this))))
(add-hook 'kill-buffer-hook 'buffer-manager-kill-buffer-callback)

(cl-defmethod buffer-manager-entries ((this buffer-manager)
				      &optional include-fn exclude-fn sort-form)
  "Return entries that match INCLUDE-FN and don't match EXCLUDE-FN.
Entries returned are only entries contained in this instance of the
`buffer-manager'.

Sorting on the returned entries are done when SORT-FORM is non-`nil'.  Any
sorting is only done on the returned set of entries and doesn't change any
of the object's internal state.  Sorting is done based on SORT-FORM's value:
 - symbol 'lexical: sort lexically based on the buffer entry's name
 - function: sort using SORT-FORM as a predicate \(see `sort')."
  (with-slots (entries) this
    (setq include-fn (or include-fn #'(lambda (entry) t))
	  exclude-fn (or exclude-fn #'(lambda (entry) nil)))
    (let ((entries
	   (remove nil (mapcar #'(lambda (entry)
				   (if (and (funcall include-fn entry)
					    (not (funcall exclude-fn entry)))
				       entry))
			       entries))))
      (when sort-form
	(cl-flet ((lexical-fn
		   (a b)
		   (string< (buffer-entry-name a) (buffer-entry-name b))))
	  (let ((sort-fn (cond ((eq sort-form 'lexical) 'lexical-fn)
			       ((functionp sort-form) sort-form)
			       (t (error "Illegal sort form: %S" sort-form)))))
	    (setq entries (sort entries sort-fn)))))
      entries)))

(cl-defmethod buffer-manager-first-entry ((this buffer-manager)
					  &optional include-fn exclude-fn assertp)
  "Return the first entry matching INCLUDE-FN that doesn't match EXCUDE-FN.
Entries returned are only entries contained in this instance of the
`buffer-manager'.
If ASSERTP is non-nil, raise an error if there is no such entry."
  (with-slots (entries) this
    (setq include-fn (or include-fn #'(lambda (entry) t))
	  exclude-fn (or exclude-fn #'(lambda (entry) nil)))
    (let ((entry (dolist (entry entries)
		   (if (and (funcall include-fn entry)
			    (not (funcall exclude-fn entry)))
		       (cl-return entry)))))
      (if (and assertp (null entry))
	  (error "No first entry"))
      entry)))

(cl-defmethod buffer-manager-window-entries ((this buffer-manager)
					     &optional exclude-fn)
  "Return `buffer-entry' instances contained in windows for this frame."
  (buffer-manager-entries this
			  #'(lambda (entry)
			      (dolist (win (window-list))
				(with-current-buffer (window-buffer win)
				  (if (and (boundp 'buffer-entry-instance)
					   (eq buffer-entry-instance entry))
				      (cl-return t)))))
			  exclude-fn))

(cl-defmethod buffer-manager-current-instance ((this buffer-manager)
					       &optional assertp)
  "Get the current buffer entry instance in the current buffer.
Return `nil' if this isn't an entry buffer that belongs to this
`buffer-manager' instance."
  (with-slots (entries) this
    (if (and (boundp 'buffer-entry-instance)
	     (member buffer-entry-instance entries))
	buffer-entry-instance
      (if assertp (error "Missing buffer entry or wrong buffer")))))

(cl-defmethod buffer-manager-cycle-methods ((this buffer-manager))
  "All valid cycle methods (see `buffer-manager-entry-cycle')."
  '(last-visit next))

(cl-defmethod buffer-manager-toggle-cycle-method ((this buffer-manager))
  (let* ((methods (buffer-manager-cycle-methods this))
	 (method (buffer-manager-cycle-method this)))
    (setq method (or (cadr (member method methods)) (car methods)))
    (buffer-manager-set-cycle-method this method)
    method))

(defvar buffer-manage-current-buffer (current-buffer))

(defun buffer-manage-post-command-hook ()
  "Switch/enter next buffer entry."
  (let ((curr-buff (current-buffer))
	(old-buff buffer-manage-current-buffer))
    (setq buffer-manage-current-buffer curr-buff)
    (unless (equal curr-buff old-buff)
      (if (and (boundp 'buffer-manager-instance)
	       (boundp 'buffer-entry-instance))
	  (buffer-manager-enter-buffer buffer-manager-instance
				       buffer-entry-instance)))))

(cl-defmethod buffer-manager-enter-buffer ((this buffer-manager) entry)
  (with-slots (last-switched-to) this
    (setq last-switched-to entry)
    (buffer-manager-cycle-entries this entry)))

(cl-defmethod buffer-manager-cycle-entries ((this buffer-manager) entry)
  "Rearrange the entry order to place ENTRY in place after cycling."
  (with-slots (entries cycle-method) this
    (let ((first (car entries)))
      (setq entries (cons entry (remove entry entries)))
      (if (and (eq 'next cycle-method)
	       (> (length entries) 1)
	       (not (eq first entry)))
	  (setq entries (append (remove first entries) (list first)))))))

(cl-defmethod buffer-manager-entry-cycle ((this buffer-manager))
  "Cycle based on slot `cycle-method'.
The default uses:
  last-visit: go to the last visited buffer entry
	next: go to the next highest priority buffer entry"
  (cl-flet* ((current-fn
	      (entry)
	      (eq (buffer-manager-current-instance this) entry)))
    (with-slots (last-switched-to include-frame-wins cycle-method) this
      (let ((entries (buffer-manager-entries this))
	    ;; get all entries displayed in windows on this frame except the
	    ;; current entry we're in
	    (win-entries (buffer-manager-window-entries this #'current-fn))
	    ;; the current entry we're in (if there is one)
	    (cur-entry (buffer-manager-current-instance this))
	    (method cycle-method))
	(if (not (member method (buffer-manager-cycle-methods this)))
	    (error "Invalid cycle method: %S" method))
	(or
	 ;; only can switch to one if there is one
	 (if (= (length entries) 1) (car entries))
	 ;; try to use the last entry that was switched into if we can
	 (if (and last-switched-to
		  ;; don't pick the one we are in or we won't switch at all
		  (not cur-entry)
		  ;; make sure this isn't a stale entry that's been killed
		  (buffer-live-p (buffer-entry-buffer last-switched-to))
		  ;; when other windows in the frame aren't candidates, the
		  ;; last visited entry isn't a candidate either
		  (or include-frame-wins
		      (not (memq last-switched-to win-entries))))
	     last-switched-to)
	 (let ((bak-entries (copy-tree entries)))
	   (when (not include-frame-wins)
	     ;; get rid of current frame window entries when the user doesn't
	     ;; want them; this shrinks the candidate pool to all but what's on
	     ;; the frame
	     (setq entries
		   (remove nil
			   (mapcar #'(lambda (arg)
				       (unless (memq arg win-entries) arg))
				   entries)))
	     ;; get rid of `win-entries' now that we've used it to set diff
	     (setq win-entries nil)
	     ;; revert to the set of entries if narrowed to tight
	     (if (or (null entries)
		     (and (= 1 (length entries))
			  (eq (car entries) cur-entry)))
		 (setq entries bak-entries)))
	   (or
	    (cl-case method
	      (last-visit (or
			   ;; win-entries is nil if no entries
			   (car win-entries)
			   ;; last visited will be second in the queu
			   (cl-second entries)))
	      (next (or
		     ;; next is also the second, when we cycle, it's put at the
		     ;; back after the switch
		     (cl-second entries)))
	      (otherwise (error "Unimplemented (but value) cycle method: %S"
				method)))
	    (and (not include-frame-wins) (car entries)))))))))

(cl-defmethod buffer-entry-insert ((this buffer-entry) command
				   &optional send-command-p)
  "Add COMMAND to the buffer prompt.
If the buffer doesn't have the point at the prompt, then create an error.

SEND-COMMAND-P, if non-nil, actually execute the command inserted as if the
user hit ENTER."
  (let ((buf (buffer-entry-buffer this)))
    (save-match-data
      (with-current-buffer buf
	(goto-char (point-max))
	(insert command)
	(if (and (fboundp 'comint-send-input) send-command-p)
	    (comint-send-input))
	(goto-char (point-max))
	(set-window-point (get-buffer-window (current-buffer)) (point-max))))))

(cl-defmethod buffer-manager-entry-exists-p ((this buffer-manager) entry)
  "If ENTRY is an instance of a class or subclass of `buffer-entry' return it."
  (with-slots (entries) this
    (and (eieio-object-p entry)
	 (object-of-class-p entry 'buffer-entry)
	 (member entry entries)
	 entry)))

(cl-defmethod buffer-manager-entry ((this buffer-manager)
				    criteria &optional assertp)
  "This returns an entry based on CRITERIA.
CRITERIA is:
  a string: the buffer name to switch to the buffer entry with that name
  a symbol: if `first', the highest priority buffer entry is selected,
	    if `last' the last most priority buffer entry is selected,
	    if `next' the next entry in the list or start back with the first
	    if `cycle' the next (after the current) most desirable
	    buffer entry is selected based on the value of slot `cycle-method'"
  (let* ((entries (buffer-manager-entries this))
	 (len (length entries))
	 entry)
    (setq entry
	  (cond ((stringp criteria)
		 (buffer-manager-first-entry
		  this #'(lambda (entry)
			   (string-equal criteria
					 (buffer-entry-name entry)))))
		((buffer-manager-entry-exists-p this criteria))
		((= len 0) nil)
		((= len 1) (car entries))
		((eq criteria 'first) (car entries))
		((eq criteria 'last) (last entries))
		((eq criteria 'cycle) (buffer-manager-entry-cycle this))
		(t (error "Illegal argument for criteria: %S"
			  criteria))))
    (if (and assertp (null entry))
	(error "No entry exists that satisfies criteria `%S'" criteria))
    entry))

(cl-defmethod buffer-manager-switch ((this buffer-manager) criteria
				     &optional new-frame-p window-cfg)
  "Switch to a buffer entry.
If the buffer CRITERIA is the name of the buffer to switch to, go to that
buffer, otherwise, create a new one with that name and switch to it.
Returns the buffer entry we switched to based on CRITERIA \(see
`buffer-manager-entry').
NEW-FRAME-P, if non-`nil', create a new frame and switch to the new buffer in
it.
WINDOW-CFG, if non-`nil', split the window based on the value, which is
currently just the symbol `split'."
  (let ((entry (or (buffer-manager-entry this criteria)
		   (buffer-manager-new-entry
		    this (and (stringp criteria) criteria)))))
    (if new-frame-p
	(save-window-excursion
	  (select-window (frame-first-window (make-frame-command)))
	  (buffer-manager-switch this entry))
      (let ((win-entries (buffer-manager-window-entries this))
	    (buf (buffer-entry-buffer entry)))
	;; if entry we're going to is already in the frame, switch over to that
	;; window in the current frame instead of duplicating windows
	(if (member entry win-entries)
	    (select-window (get-buffer-window buf))
	  (if (eq 'split window-cfg)
	      (save-excursion (display-buffer buf))
	    (switch-to-buffer buf)))))
    (buffer-manager-cycle-entries this entry)
    entry))

(cl-defmethod buffer-manager-read-new-name ((this buffer-manager)
					    &optional prompt auto-generate-p)
  "Read a buffer name from user input."
  (let ((def (buffer-manager-conical-name this))
	name)
    (if auto-generate-p
	def
      (setq prompt (or prompt (capitalize (buffer-manager-name this))))
      (setq prompt (choice-program-default-prompt prompt def))
      (setq name (read-string prompt nil nil def))
      (if (= 0 (length name)) (setq name nil))
      name)))

(cl-defmethod buffer-manager-read-name ((this buffer-manager)
					&optional prompt require-match
					default name-fn)
  "Read a buffer entry name from the user.

PRMOPT is given to the user, or defaults to the
`buffer-manager-name'.  Don't add any trainling `: ' type
characters as `default' syntax is added.

REQUIRE-MATCH, if non-`nil' don't allow the user to produce a name that
doesn't already exist as a entry name for this manager.

DEFAULT, if non-`nil' use as the default instead of next buffer entry name.
Otherwise the default is selected an entry that isn't currently in any window
in the current frame.

NAME-FN, if non-`nil' use to create a name when prompting the user for each
buffer.  The default is `buffer-entry-name'."
  (setq name-fn (or name-fn 'buffer-entry-name))
  (let ((entries (buffer-manager-entries this))
	def-entry def name-map)
    (with-slots (include-frame-wins) this
      ;; it isn't useful to use a default based on the entry chosen by the
      ;; normal cycle method; instead pick on the other `include-frame-windows'
      ;; method
      (let ((include-frame-wins (not include-frame-wins)))
	(setq def-entry (cond ((= 1 (length entries)) (car entries))
			      (t (buffer-manager-entry this 'cycle)))
	      def (or default (if def-entry (funcall name-fn def-entry))))))
    (setq prompt (or prompt (capitalize (buffer-manager-name this))))
    (setq prompt (choice-program-default-prompt prompt def))
    (setq name-map (mapcar #'(lambda (entry)
			       (cons (funcall name-fn entry)
				     (buffer-entry-name entry)))
			   entries))
    (with-slots (read-history) this
      (let ((hist read-history)
	    input)
	(setq input
	      (completing-read prompt (mapcar name-fn entries)
			       nil require-match nil 'hist def))
	(if (= 0 (length input))
	    (setq input nil)
	  ;; keep non-matching name for new shell names, rename shell etc.
	  (setq input (or (cdr (assoc input name-map))
			  (unless require-match input)))
	  (if input
	      (setq read-history (append (list input) read-history))))
	input))))

(defconst buffer-manager-list-header-fields
  '("C" "Name"  "Working Directory")
  "*List of fields used in output of `buffer-list'.")

(defconst buffer-manager-list-col-space 4
  "Space between columns.")

(defconst buffer-manager-status-defs
  '((alive . " ")
    (to-delete . "D")
    (to-show . "S"))
  "Enumeration of status for buffers in `buffer-manager-list-entries'.")

(cl-defmethod buffer-manager-list-entries ((this buffer-manager))
  "Return a multi-listing of the buffer entries contained in this manager."
  (cl-flet* ((get-entries
	      ()
	      (sort (buffer-manager-entries this)
		    #'(lambda (a b)
			(string< (buffer-entry-name a)
				 (buffer-entry-name b)))))
	     (get-max
	      (getter-fn)
	      (let ((entries (get-entries)))
		(when entries
		  (apply #'max
			 (mapcar #'(lambda (entry)
				     (length (funcall getter-fn entry)))
				 (get-entries))))))
	     (entry-wd
	      (entry)
	      (with-current-buffer (buffer-entry-buffer entry)
		default-directory))
	     (get-wd
	      (entry col-space name-len)
	      (let* ((name (entry-wd entry))
		     (len (length name))
		     (width 79)
		     (max-len (- (- width col-space) name-len 0)))
		(if (> len max-len)
		    (concat (substring name 0 (- max-len 3)) "...")
		  name))))
    (when (not (boundp 'buffer-entry-status))
      (set (make-local-variable 'buffer-entry-status)
	   (make-hash-table :test 'equal)))
    (dolist (entry (buffer-manager-entries this))
      (let ((name (buffer-entry-name entry)))
	(unless (gethash name buffer-entry-status)
	  (puthash name 'alive buffer-entry-status))))
    (let ((name-len (get-max #'buffer-entry-name))
	  (col-space buffer-manager-list-col-space))
      (setq name-len (or name-len col-space))
      (let ((entries (get-entries))
	    (headers buffer-manager-list-header-fields)
	    format-meta)
	(setq format-meta (format "%%-%ds %%-%ds%%s"
				  col-space (+ col-space name-len)))
	(insert (apply 'format format-meta headers)
		"\n"
		(apply 'format format-meta
		       (mapcar #'(lambda (arg)
				   (make-string (length arg) ?-))
			       headers))
		"\n")
	(cl-do ((lst entries (setq lst (cdr lst)))
		entry)
	    ((null lst))
	  (setq entry (car lst))
	  (let ((name (copy-sequence (buffer-entry-name entry)))
		(status (cdr (assq (gethash (buffer-entry-name entry)
					    buffer-entry-status)
				   buffer-manager-status-defs))))
	    (put-text-property 0 (length name) 'mouse-face 'highlight name)
	    (insert (apply #'format format-meta
			   (append (list status name
					 (get-wd entry col-space name-len))))))
	  (if (cdr lst) (insert "\n")))))))

(cl-defmethod buffer-manager-interactive-functions ((this buffer-manager)
						    singleton-variable-sym)
  (let ((cname (buffer-manager-conical-name this)))
    `(("new" (defun ,(intern (format "%s-new" cname))
		 (&optional name start-dir new-frame-p)
	       ,(format "Create a new %s entry.
When invoked with \\[universal-argument] a new NAME for the %s is prompted
to the user.  If the name of the %s doesn't yet exist, it will be created.

START-DIR, if non-`nil', use this directory for the buffer
process to start in instead of the `buffer-manager' instance's
`start-dir' slot value.

NEW-FRAME-P, if non-`nil', create a new frame and switch to the
new buffer in it."
			(buffer-manager-name this)
			(buffer-manager-name this)
			(buffer-manager-name this))
	       (interactive
		(list (buffer-manager-read-new-name ,singleton-variable-sym nil
						    (not current-prefix-arg))))
	       (let* ((this ,singleton-variable-sym))
		 (buffer-manager-new-entry this name start-dir new-frame-p t))))

      ("switch" (defun ,(intern (format "%s-switch" cname)) (&optional name)
		  ,(format "\
Switch to %s NAME, which is prompted from the user.

When invoked with \\[universal-argument] a new NAME for the %s is
prompted to the user.  If the name of the %s doesn't yet exist,
it will be created.  Otherwise, the default is used which is a
selected entry that isn't currently in any window in the current
frame."
			   (buffer-manager-name this)
			   (buffer-manager-name this)
			   (buffer-manager-name this))
		  (interactive
		   (list
		    (let ((this ,singleton-variable-sym))
		      (if current-prefix-arg
			  (buffer-manager-read-name
			   this (format "Switch to %s"
					(buffer-manager-name this)) t)
			(if (= 0 (length (buffer-manager-entries this)))
			    (buffer-manager-read-new-name ,singleton-variable-sym
							  nil t))))))
		  (let* ((this ,singleton-variable-sym)
			 (entry (buffer-manager-switch this (or name 'cycle))))
		    ;;(message "Switched to `%s'" (buffer-entry-name entry))
		    )))
      ("toggle-cycle-method"
       (defun ,(intern (format "%s-toggle-cycle-method" cname)) ()
	 "Toggle cycle methods \(i.e. last visited vs. next buffer)."
	 (interactive)
	 (let ((this ,singleton-variable-sym)
	       method)
	   (setq method (buffer-manager-toggle-cycle-method this))
	   (message "Set cycle method to `%S'" method))))

      ("toggle-include-frame-windows"
       (defun ,(intern (format "%s-toggle-include-frame-windows" cname)) ()
	 "Toggle cycle methods \(i.e. last visited vs. next buffer)."
	 (interactive)
	 (let* ((this ,singleton-variable-sym)
		(incp (buffer-manager-include-frame-wins this)))
	   (setq incp (not incp))
	   (buffer-manager-set-include-frame-wins this incp)
	   (message "Now %sincluding frame windows" (if incp "" "not ")))))

      ("list" (defun ,(intern (format "%s-list" cname)) ()
		,(format "\
List the information for all %ss.
In this buffer, you can rename and go to %ss"
			 (buffer-manager-name this)
			 (buffer-manager-name this))
		(interactive)
		(let ((this ,singleton-variable-sym))
		  (buffer-manage-list
		   ,singleton-variable-sym
		   (format "*%s Entries*"
			   (capitalize (buffer-manager-name this)))))))

      ("rename" (defun ,(intern (format "%s-rename" cname)) ()
		  ,(format "Rename the buffer entry %ss."
			   (buffer-manager-name this))
		  (interactive)
		  (let* ((this ,singleton-variable-sym)
			 (entry (buffer-manager-current-instance this t))
			 (default (file-name-nondirectory
				   (directory-file-name default-directory)))
			 (name (buffer-manager-read-name this "New name")))
		    (buffer-entry-rename entry name))))

      ("display-all" (defun ,(intern (format "%s-display-all" cname)) ()
		       "Show all entries in the current frame."
		       (interactive)
		       (let* ((this ,singleton-variable-sym))
			 (buffer-manager-display-entries this))))
      ("remove-all" (defun ,(intern (format "%s-remove-all" cname)) ()
		      "Kill \(remove) all entries in the current frame."
		      (interactive)
		      (let* ((this ,singleton-variable-sym))
			(buffer-manager-remove-entries this)))))))

(cl-defmethod buffer-manager-bind-interactive-functions ((this buffer-manager))
  (let ((funcs (buffer-manager-interactive-functions this 'none))
	(bindings (car (delq nil (mapcar #'(lambda (entry)
					     (if (null (car entry))
						 (cadr entry)))
					 buffer-manage-key-bindings)))))
    (setq bindings
	  (append bindings
		  (cadr (assoc (buffer-manager-name this)
			       buffer-manage-key-bindings))))
    (dolist (binding bindings)
      (let ((def (cadr (assoc (cl-first binding) funcs)))
	    (keymap (cl-second binding))
	    (key (read-kbd-macro (cl-third binding))))
	(setq keymap (if keymap (symbol-value keymap) (current-global-map)))
	(define-key keymap key (cl-second def))
	(message "Bound function `%S' key `%s' on %S"
		 (cl-second def)
		 (cl-third binding)
		 (or (cl-second binding) 'global))))))

(cl-defmethod buffer-manager-key-bindings ((this buffer-manager)) nil)

(cl-defmethod buffer-manager-create-interactive-functions ((this buffer-manager)
							   singleton-variable-sym)
  (add-to-list 'buffer-manage-remap-instances singleton-variable-sym)
  (let ((defs (mapcar 'cl-second
		      (buffer-manager-interactive-functions
		       this singleton-variable-sym)))
	(more-bindings (buffer-manager-key-bindings this))
	(name (buffer-manager-name this))
	fns)
    (dolist (def defs)
      (let ((fn (eval def)))
	(setq fns (append fns (list fn)))
	(message "Created function `%S'" fn)))
    (when (and more-bindings
	       (not (assq name buffer-manage-key-bindings)))
      (setq buffer-manage-key-bindings
	    (append buffer-manage-key-bindings `((,name ,more-bindings)))))
    fns))



;;; key bindings interactive
(defvar buffer-manager-read-bind-choices-history nil)

(defun buffer-manager-read-bind-choices (&optional use-last-default-p)
  "Read user input that indicates how to switch between buffer entries.
USE-LAST-DEFAULT-P, switch to the previous setting if non-nil."
  (let ((choices (mapcar #'(lambda (inst)
			     (cons (buffer-manager-name (symbol-value inst))
				   inst))
			 buffer-manage-remap-instances))
	(second (cl-second buffer-manager-read-bind-choices-history)))
    (cdr (assoc
	  (if (and use-last-default-p second)
	      (progn
		(setq buffer-manager-read-bind-choices-history
		      (cons second buffer-manager-read-bind-choices-history))
		second)
	    (let* ((def (or second
			    (car buffer-manager-read-bind-choices-history)))
		   (prompt (choice-program-default-prompt
			    "Buffer manager bind" def)))
	      (completing-read
	       prompt (mapcar 'car choices) nil t
	       nil 'buffer-manager-read-bind-choices-history def)))
	  choices))))

(defun buffer-manager-bind-functions (buffer-manager-instance-var)
  "Rebind the mapping of the keys defined in `buffer-manage-key-bindings'.
These keys are then mapped to the instance specified from user
input.  Switch to the previous setting \(if there is one) when
invoked with \\[universal-argument].

For example, for a shell buffer managment instance, there could be one key to
create a new shell, another key to switch and a third to go to the listing of
current shells.

BUFFER-MANAGER-INSTANCE-VAR is symbol variable that has the
binding (see `buffer-manager-read-bind-choices')."
  (interactive (list (buffer-manager-read-bind-choices current-prefix-arg)))
  (let ((this (symbol-value buffer-manager-instance-var)))
    (buffer-manager-bind-interactive-functions this)
    (message "Bound keys to manager `%s'" (buffer-manager-name this))))



;;; modal

(defcustom buffer-manage-highlight t
  "Whether or not to hightlight buffer using `buffer-manage-list'."
  :group 'buffer-manage
  :type 'boolean)

(defgroup buffer-manage-font-lock-faces nil
  "Buffer Manage Faces"
  :group 'buffer-manage
  :prefix "buffer-manage-font-lock-")

;; face definitions
(defface buffer-manage-font-lock-headers-face
  '((t (:foreground "red")))
  "Font Lock mode face used to highlight buffer headerss."
  :group 'buffer-manage-font-lock-faces)
(defface buffer-manage-font-lock-name-face
  '((t (:foreground "darkcyan")))
  "Font Lock mode face used to highlight buffer names."
  :group 'buffer-manage-font-lock-faces)
(defface buffer-manage-font-lock-wd-face
  '((t (:foreground "blue")))
  "Font Lock mode face used to highlight working directories."
  :group 'buffer-manage-font-lock-faces)

;; font variables
(defvar buffer-manage-font-lock-headers-face
  'buffer-manage-font-lock-headers-face
  "Face headers to use for headerss.")
(defvar buffer-manage-font-lock-name-face
  'buffer-manage-font-lock-name-face
  "Face name to use for names.")
(defvar buffer-manage-font-lock-wd-face
  'buffer-manage-font-lock-wd-face
  "Face name to use for working directories.")

(defvar buffer-manage-font-lock-keywords
  `((,(format "^.\\{%d\\}\\(.*?\\)[ \t]+.*$" (1+ buffer-manager-list-col-space))
     1 buffer-manage-font-lock-name-face t)
    (,(format "^.\\{%d\\}.*?[ \t]+\\(.*\\)$" (1+ buffer-manager-list-col-space))
     1 buffer-manage-font-lock-wd-face t)
    ,(list (format "^\\(%s.*\\)$" (cl-second buffer-manager-list-header-fields))
	   1 buffer-manage-font-lock-headers-face t)
    ("^\\([- \t]+\\)$" 1 buffer-manage-font-lock-headers-face t))
  "Additional expressions to highlight in buffer manage mode.")
(if (featurep 'xemacs)
    (put 'buffer-manage-mode 'font-lock-defaults
	 '(buffer-manage-font-lock-keywords t)))

(defun buffer-manage-mode-assert ()
  "Throw an error if not in `buffer-manage-mode'."
  (if (not (eq major-mode 'buffer-manage-mode))
      (error "Must be in `buffer-manage-mode' for this command")))

(defun buffer-manage-mode-quit ()
  "Quit from within the `buffer-manage-mode'."
  (interactive)
  (buffer-manage-mode-assert)
  (if t
      (bury-buffer)
    (let ((cfg org-window-config))
      (kill-buffer (current-buffer))
      (set-window-configuration cfg))))

(defun buffer-manage-mode-name-at-point ()
  "Return the name of the buffer at the current point if there is one."
  (buffer-manage-mode-assert)
  (save-excursion
    (beginning-of-line)
    (forward-char (+ (length (cdar buffer-manager-status-defs))
		     buffer-manager-list-col-space))
    (if (looking-at "\\(.+?\\)[ \t]")
	(match-string-no-properties 1))))

(defun buffer-manage-mode-mouse-down (event)
  "Call back for mouse down events.
EVENT mouse event data."
  (interactive "e")
  (mouse-set-point event)
  (setq buffer-manage-on-mouse-down (buffer-manage-mode-name-at-point)))

(defun buffer-manage-mode-mouse-up (event)
  "Call back for mouse down events.
EVENT mouse event data."
  (interactive "e")
  (mouse-set-point event)
  (let ((name (buffer-manage-mode-name-at-point)))
    (if (string= name buffer-manage-on-mouse-down)
	(buffer-manage-mode-activate-buffer name))))

(defun buffer-manage-mode-first-buffer ()
  "Go to the first buffer entry in the buffer listing."
  (goto-char (point-min))
  (forward-line 2))

(defun buffer-manage-mode-next ()
  "Called by pressing the `tab' key in `buffer-manage-mode'."
  (interactive)
  (buffer-manage-mode-assert)
  (beginning-of-line)
  (unless (save-excursion (end-of-line) (eobp))
    (forward-line)))

(defun buffer-manage-mode-previous ()
  "Called by pressing the `tab' key in `buffer-manage-mode'."
  (interactive)
  (buffer-manage-mode-assert)
  (beginning-of-line)
  (if (> (line-number-at-pos (point)) 3)
      (forward-line -1)))

(defun buffer-manage-mode-activate-buffer (&optional name)
  "Activates the buffer entry with name NAME."
  (interactive)
  (buffer-manage-mode-assert)
  (setq name (or name (buffer-manage-mode-name-at-point)))
  (let ((this buffer-manager-instance))
    (buffer-manage-mode-assert)
    ;;(buffer-manage-mode-quit)
    (buffer-manager-switch this name)))

(defun buffer-manage-mode-view (&optional name)
  "Activates the buffer entry with name NAME."
  (interactive)
  (buffer-manage-mode-assert)
  (setq name (or name (buffer-manage-mode-name-at-point)))
  (let ((this buffer-manager-instance))
    (buffer-manage-mode-assert)
    (buffer-manager-switch this name nil 'split)))

(defun buffer-manage-mode-set-status (status)
  "Set the mode status to STATUS for the mode."
  (buffer-manage-mode-assert)
  (let ((name (buffer-manage-mode-name-at-point)))
    (when name
      (puthash name status buffer-entry-status)
      (buffer-manage-mode-refresh)
      (buffer-manage-mode-next))))

(defun buffer-manage-mode-refresh ()
  "Refresh the buffer entry listing buffer."
  (interactive)
  (buffer-manage-mode-assert)
  (let ((line (1+ (count-lines (point-min) (point)))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (buffer-manager-list-entries buffer-manager-instance)
    (setq buffer-read-only t)
    (goto-char (point-min))
    (forward-line (max 3 line))
    (beginning-of-line)
    (set-window-point (get-buffer-window (current-buffer)) (point))))

(defun buffer-manage-mode-mark-delete ()
  "Delete a buffer (terminate)."
  (interactive)
  (buffer-manage-mode-set-status 'to-delete))

(defun buffer-manage-mode-mark-show ()
  "Display \(show) a buffer."
  (interactive)
  (buffer-manage-mode-set-status 'to-show))

(defun buffer-manage-mode-mark-undelete ()
  "Unmark a buffer for deletion."
  (interactive)
  (buffer-manage-mode-set-status 'alive))

(defun buffer-manage-mode-apply-selected (status replace func)
  "Apply STATUS to the selection.
Replace status for REPLACE and the selection uses the return
value of FUNC."
  (buffer-manage-mode-assert)
  (let ((this buffer-manager-instance))
    (maphash #'(lambda (key val)
		 (when (eq status val)
		   (let ((entry (buffer-manager-entry this key)))
		     (and entry (funcall func this entry)))
		   (if replace
		       (puthash key replace buffer-entry-status)
		     (remhash key buffer-entry-status))))
	     buffer-entry-status)
    (buffer-manage-mode-refresh)))

(defun buffer-manage-mode-delete-selected ()
  "Delete all entries that are selected for delete."
  (interactive)
  (buffer-manage-mode-assert)
  (buffer-manage-mode-apply-selected 'to-delete nil
				     'buffer-manager-remove-entry))

(defun buffer-manage-mode-show-selected ()
  "Show all entries in one frame that are selected."
  (interactive)
  (buffer-manage-mode-assert)
  (let ((this buffer-manager-instance)
	entries)
    (cl-flet ((collect
	       (inst entry)
	       (setq entries (append entries (list entry)))))
      (buffer-manage-mode-apply-selected 'to-show 'to-show 'collect)
      (buffer-manager-display-given-entries this entries))))

(defun buffer-manage-mode-rename (new-name)
  "Rename a buffer to NEW-NAME."
  (interactive
   (progn
     (buffer-manage-mode-assert)
     (let ((this buffer-manager-instance))
       (list (buffer-manager-read-new-name this "Rename")))))
  (buffer-manage-mode-assert)
  (let ((name (buffer-manage-mode-name-at-point))
	(this buffer-manager-instance))
    (buffer-entry-rename (buffer-manager-entry this name) new-name)
    (buffer-manage-mode-refresh)))

(defun buffer-manage-mode-new ()
  "Create a new entry."
  (interactive)
  (let* ((this buffer-manager-instance)
	 (name (buffer-manager-read-new-name this)))
    (buffer-manager-new-entry this name)
    (buffer-manage-mode-refresh)))

(define-derived-mode buffer-manage-mode fundamental-mode "Buffer Manager"
  "Major mode for displaying and buffer entries.
Special commands:
\\{buffer-manage-mode-map}"
  (if (not (featurep 'xemacs))
      (set (make-local-variable 'font-lock-defaults)
	   '(buffer-manage-font-lock-keywords t)))
  (font-lock-mode (if buffer-manage-highlight 1 0))
  (set (make-local-variable 'org-window-config)
       (current-window-configuration)))

(define-key buffer-manage-mode-map "q" 'buffer-manage-mode-quit)
(define-key buffer-manage-mode-map [down-mouse-2] 'buffer-manage-mode-mouse-down)
(define-key buffer-manage-mode-map [mouse-2] 'buffer-manage-mode-mouse-up)
(define-key buffer-manage-mode-map [return] 'buffer-manage-mode-activate-buffer)
(define-key buffer-manage-mode-map "n" 'buffer-manage-mode-next)
(define-key buffer-manage-mode-map "p" 'buffer-manage-mode-previous)
(define-key buffer-manage-mode-map [(control down)] 'buffer-manage-mode-next)
(define-key buffer-manage-mode-map [(control up)] 'buffer-manage-mode-previous)
(define-key buffer-manage-mode-map "d" 'buffer-manage-mode-mark-delete)
(define-key buffer-manage-mode-map "s" 'buffer-manage-mode-mark-show)
(define-key buffer-manage-mode-map "u" 'buffer-manage-mode-mark-undelete)
(define-key buffer-manage-mode-map "i" 'buffer-manage-mode-new)
(define-key buffer-manage-mode-map "x" 'buffer-manage-mode-delete-selected)
(define-key buffer-manage-mode-map "z" 'buffer-manage-mode-show-selected)
(define-key buffer-manage-mode-map "g" 'buffer-manage-mode-refresh)
(define-key buffer-manage-mode-map "r" 'buffer-manage-mode-rename)
(define-key buffer-manage-mode-map "v" 'buffer-manage-mode-view)

(defvar buffer-manage-mode-menu-definition
  (list "Buffer Manager"
	["Create New" buffer-manage-mode-new t]
	["Goto Entry" buffer-manage-mode-activate-buffer t]
	"-"
	["Mark Delete" buffer-manage-mode-mark-delete t]
	["Unmark" buffer-manage-mode-mark-undelete t]
	["Rename" buffer-manage-mode-rename t]
	["Delete Selected" buffer-manage-mode-delete-selected t]
	["Show Selected" buffer-manage-mode-show-selected t]
	"-"
	["Refresh" buffer-manage-mode-refresh t]
	["Quit" buffer-manage-mode-quit t]))

(defun buffer-manage-list (manager-instance buffer-name)
  "Create a listing of buffers used for viewing, renameing, deleting, adding.
MANAGER-INSTANCE is the `buffer-manager' singleton instance.
BUFFER-NAME is the name of the buffer holding the entries for the mode."
  (let* ((buf (get-buffer buffer-name))
	 (newp (not buf))
	 (buf (or buf (get-buffer-create buffer-name))))
    (save-excursion
      (eval-and-compile
	(let ((msg (concat "we need `save-excursion' since interactively "
			   "called `buffer-manage-mode-refresh' sets "
			   "the window point")))
	  (display-warning 'buffer-manage msg :debug)))
      (set-buffer buf)
      (if (not newp)
	  (buffer-manage-mode-refresh)
	(setq buffer-read-only nil)
	(erase-buffer)
	(buffer-manage-mode)
	(buffer-manager-list-entries manager-instance)
	(buffer-manage-mode-first-buffer)
	(set-window-point (get-buffer-window (current-buffer)) (point))
	(set (make-local-variable 'buffer-manager-instance) manager-instance)
	(set-buffer-modified-p nil)
	(setq buffer-read-only t)
	(easy-menu-define buffer-manage-mode-menu buffer-manage-mode-map
	  "Menu for Buffer Manage." buffer-manage-mode-menu-definition)))
    (switch-to-buffer buf)))

(provide 'buffer-manage)

;;; buffer-manage.el ends here
