;;; register-list.el ---  Interactively list/edit registers
;;
;; Copyright (C) 2011  Free Software Foundation, Inc.
;;
;; Filename: register-list.el
;; Author: Bastien Guerry <bzg AT altern DOT org>
;; Maintainer: Bastien Guerry <bzg AT altern DOT org>
;; Keywords: register
;; Description: List and edit the register
;; Version: 0.1
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;; This is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This library lets you list and edit registers.  M-x `register-list'
;; displays a list of currently set registers.

;; This list is similar to that of `bookmark-bmenu-list': you can set
;; registers to delete with `d' and delete them with `x'.  If you want
;; to concatenate the content of registers, mark them with `c' and
;; process with `x'.

;; You can also edit the register's key with `k' and its value with `v'
;; Hitting RET on a value string will jump to the register's location or
;; add the text to the kill ring.  Hitting RET on a register's type will
;; restrict the list to registers of this type.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'register-list)
;; 
;;; Todo:
;; 
;; - better sorting (interactive)
;; - overlay register when deleting duplicates
;; - more useful message when selecting a type
;; - concatenation -> merge
;; - support merging rectangles
;; - add numbers when "merging" them
;; - C-k to kill a register
;; 
;;; History: 
;; 
;; - [2008-03-09] Released v0.1
;;   http://article.gmane.org/gmane.emacs.sources/2832
;; 
;;; Code:

(eval-when-compile
  (require 'cl))

(defgroup register-list nil
  "Interactively list/edit registers."
  :tag "Register List"
  :group 'register)

(defcustom register-list-string-width nil
  "Maximum width for the register value string."
  :type 'integer
  :group 'register-list)

(defcustom register-list-preserve-fontification nil
  "Non-nil means keep the value strings fontified."
  :type 'integer
  :group 'register-list)

(defcustom register-list-default-types "[FNMRSW]"
  "A regexp matching the default register types to list.

The available types are: [F]rame [N]umber [M]arkers [R]ectangle
\[S]string and [W]window.  [FW] will list markers, frame and
window configuration, [SM] will list strings and markers, etc."
  :type 'regexp
  :group 'register-list)

(defface register-list-off-rectangle
  '((t (:inverse-video t)))
  "Face used to show what falls out of a rectangle."
  :group 'register-list)

;;; Variables, map, mode

(defvar register-list-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "q" 'quit-window)
    (define-key map "Q" 'register-list-quit)
    (define-key map [(tab)] 'register-list-tab)
    (define-key map "d" 'register-list-mark-delete)
    (define-key map "D" 'register-list-delete-duplicates)
    (define-key map "c" 'register-list-mark-concat)
    (define-key map "x" 'register-list-execute)
    (define-key map "+" 'register-list-increment-key)
    (define-key map "-" 'register-list-decrement-key)
    (define-key map "e" 'register-list-edit-key)
    (define-key map "E" 'register-list-edit-value)
    (define-key map "f" 'register-list-toggle-fontification)
    (define-key map " " 'next-line)
    (define-key map "n" 'next-line)
    (define-key map "p" 'previous-line)
    (define-key map "u" 'register-list-unmark)
    (define-key map "U" 'register-list-unmark-all)
    (define-key map "g" 'register-list-refresh)
    (define-key map "F"
      (lambda () (interactive) (register-list-refresh "F")))
    (define-key map "N"
      (lambda () (interactive) (register-list-refresh "N")))
    (define-key map "M"
      (lambda () (interactive) (register-list-refresh "M")))
    (define-key map "R"
      (lambda () (interactive) (register-list-refresh "R")))
    (define-key map "S"
      (lambda () (interactive) (register-list-refresh "S")))
    (define-key map "W"
      (lambda () (interactive) (register-list-refresh "W")))
    (define-key map "G"
      (lambda() (interactive) (register-list-refresh "[FNMRSW]")))
    (define-key map "?" 'describe-mode)

    (define-key map [follow-link] 'mouse-face)
    (define-key map [mouse-2] 'register-list-call-handler-at-mouse)
    (define-key map [(return)] 'register-list-call-handler-at-point)
    map)
  "Keymap for `register-list-mode'.")
(defvar register-list-edit-value-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'register-list-send-value)
    map)
  "Keymap for editing the value of a register.")
(defvar register-list-current-type nil
  "The current type for the register menu.")
(defvar register-list-current-fontification nil
  "Whether the value strings are currently fontified.")
(defvar register-list-temp-pos nil
  "Temporary store the line the cursor is on.")
(defvar register-list-temp-window-cfg nil
  "Temporary window configuration.
Saved before editing the value of a register.")
(defvar register-list-temp-register nil
  "Temporary value of the edited register.")
(defvar register-list-edit-value-type nil
  "The type of the edited value.")
(defvar register-list-rectangle-column nil
  "End of a rectangle line.")

;;; Marks

(defmacro register-list-preserve-pos (force-line &rest body)
  "Preserve the position and execute BODY.
If FORCE-LINE is non-nil, force moving to this line."
  `(let ((line (line-number-at-pos (point)))
	 (col (current-column)))
     ,@body
     (goto-char (point-min))
     (line-move ,(or (eval force-line) '(1- line)) t)
     (line-move-to-column col)))

(defmacro register-list-map-lines (let-vals &rest body)
  "Execute BODY inside a let form with LET-VALS on all lines."
  `(save-excursion
     (goto-char (point-min))
     (while (not (eobp))
       (let* ,let-vals
	 ,@body
	 (forward-line 1)))))

(defvar register-list-concat-separator "\n"
  "Default separator when merging.")

(defvar register-list-concat-key-select 'last)

;; FIXME skip rectangle (or handle them separatly
(defun register-list-execute nil
  "Delete/concatenate registers marker for deletion/concatenation."
  (interactive)
  (let ((line (line-number-at-pos (point))) newreg concat)
    (goto-char (point-min))
    (while (re-search-forward "^[DC]" nil t)
      (let* ((reg-point (next-single-property-change (point) 'register))
	     (reg (get-text-property reg-point 'register)))
	(if (string= (match-string 0) "D")
	    (setq register-alist (delete reg register-alist))
	  (push reg concat))))
    (when concat
      ;; set the new register 
      (setq newreg 
	    (cons (cond ((eq register-list-concat-key-select 'first)
			 (caar concat))
			((eq register-list-concat-key-select 'last)
			 (caar (reverse concat)))
			(t (read-char 
			    (format "Key [%s]: " 
				    (mapconcat (lambda(x) (char-to-string (car x)))
					       concat "")))))
		  (mapconcat (lambda (i) (cdr i)) (reverse concat)
			     (cond ((eq register-list-concat-separator 'ask)
				    (read-from-minibuffer "Separator: "))
				   ((stringp register-list-concat-separator) 
				    register-list-concat-separator)
				   (t "")))))
      ;; delete old registers
      (dolist (r concat)
	(setq register-alist (delete r register-alist)))
      ;; push the new register
      (push newreg register-alist))
    (register-list register-list-current-type
		   register-list-current-fontification)
    ;; move the cursor back
    (goto-char (point-min))
    (line-move (- line 2) t)))

(defun register-list-set-mark (mark)
  "Set mark at the beginning of the line."
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (unless (get-text-property (point) 'intangible)
      (delete-char 1)
      (save-excursion (insert mark))
      (unless (save-excursion (forward-line 1) (eobp))
	(forward-line 1)))))

(defun register-list-mark-delete nil
  "Mark the register at point for deletion."
  (interactive)
  (register-list-set-mark "D"))

(defun register-list-mark-concat nil
  "Mark the register at point for further concatenation."
  (interactive)
  (register-list-set-mark "C"))

(defun register-list-unmark nil
  "Unmark the register at point."
  (interactive)
  (register-list-set-mark " "))

(defun register-list-unmark-all nil
  "Unmark all registers."
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (while (and (forward-line 1) (not (eobp)))
	(delete-char 1)
	(insert " ")))))

(defun register-list-refresh (&optional type)
  "Refresh the list of registers.
An optional TYPE argument restrict the list these types."
  (interactive "P")
  (register-list-preserve-pos
   (1- (line-number-at-pos (point)))
   (register-list (or type register-list-current-type)
		  register-list-current-fontification)))

(defun register-list-quit nil
  "Quit the register list and kill its buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun register-list-toggle-fontification nil
  "Toggle fontification of the value strings."
  (interactive)
  (register-list-preserve-pos
   nil
   (setq register-list-current-fontification
	 (not register-list-current-fontification))
   (register-list register-list-current-type
		  register-list-current-fontification)))

(define-derived-mode register-list-mode special-mode "Register List"
  "Major mode for editing a list of register keys.

Each line is of the form:

\[Delete-flag] Key Type Value

The leftmost column displays a `D' character if the register key
is flagged for further deletion.  You can add such flag by hitting
\\[register-list-delete].

The Key column displays the character used for this register.
Hitting \\[register-list-call-handler-at-point] on the key will
prompt for a replacement.

The Type column displays the type of the register, either [F]rame
\[N]umber [M]arkers [R]ectangle [S]string or [W]window.  Hitting
\\[register-list-call-handler-at-point] on this column will
restrict the register list to this type of registers.  To quickly
list a specific type, hit the type character among [FNMRSW].

The Value column displays information about the value of the
register: either a string if the register's value is a string, a
number or a rectangle, or the location of the marker or some
information about window and frame configuration.  Hitting
\\[register-list-call-handler-at-point] on this column will
copy the string to the kill ring or jump to the location.

\\[register-list-edit-key] -- edit the key for this register.
\\[register-list-edit-value] -- edit the value for this register.
\\[register-list-increment-key] -- increment key at point.
\\[register-list-decrement-key] -- decrement key at point.
\\[register-list-mark-delete] -- mark the register at point for deletion.
\\[register-list-mark-concat] -- mark the register at point for concatenation.
\\[register-list-unmark] -- unmark the register at point.
\\[register-list-unmark-all] -- unmark all registers.
\\[register-list-execute] -- execute deletions or concatenations.
\\[register-list-toggle-fontification] -- toggle fontification of value strings.
\\[register-list-refresh] -- refresh the register menu display.
\\[register-list-tab] -- cycle between the key, the type and the value.
\\[register-list-quit] -- quit the register menu."
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;\\[register-list-edit-key-or-value] -- edit the key for this register.

(defun register-list-tab nil
  "Cycle between the register key, the type and the value."
  (interactive)
  (let* ((eol (save-excursion (end-of-line) (point)))
	 (m-f-chg (next-single-property-change (point) 'mouse-face nil eol))
	 (m-f-pos (text-property-any m-f-chg eol 'mouse-face 'highlight))
	 (r-f-chg (next-single-property-change (point) 'register nil eol))
	 (r-f-prop (get-text-property r-f-chg 'register)) point)
    (cond (r-f-prop (goto-char r-f-chg))
	  (m-f-pos (goto-char m-f-pos))
	  (t  (beginning-of-line 2)
	      (if (setq point (next-single-property-change
			       (point) 'register))
		  (goto-char point))))))

;;;###autoload
(defun register-list (&optional type fontify)
  "Display a list of registers.
An optional argument TYPE defines a regexp to restrict the
register menu to.  A second optional argument FONTIFICATION
decides if the display preserves original fontification for
values.

The default types are defined in `register-list-default-types',
which see.

The list is displayed in a buffer named `*Register List*' in
`register-list-mode', which see."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Register List*"))
  (let ((inhibit-read-only t) reg-alist)
    (setq type (or type register-list-default-types))
    (setq register-list-current-fontification
	  (or fontify register-list-preserve-fontification))
    (setq register-list-current-type type)

    (setq register-alist ;; TODO better sorting.
	  (sort register-alist (lambda (a b) (< (car a) (car b)))))
    (erase-buffer)
    ;; FIXME: Why `intangible'?
    (insert (concat (propertize "% Key  Type  Value\n"
				'face 'font-lock-type-face
				'intangible t) ;; 'front-sticky t)
		    (propertize "- ---  ----  -----\n"
				'intangible t
				'face 'font-lock-comment-delimiter-face)))
    (mapc
     (lambda (register)
       (let* ((key (char-to-string (car register)))
	      (val (cdr register))
	      (typ (register-list-get-type val))
	      (hdl (register-list-get-handler register typ)))
	 (when (string-match typ type)
	   (insert
	    (format "  %s    %s   %s\n"
		    (propertize key 'face 'bold 'register register
				'register-handler hdl)
		    (propertize (concat "[" typ "]")
				'mouse-face 'highlight
				'help-echo "mouse-2: restrict to this type"
				'register-handler
				`(lambda()
				   (register-list-preserve-pos nil
				    (register-list
				     ,typ ,register-list-current-fontification))))
		    (propertize (register-list-prepare-string
				 (register-list-value-to-string val typ) fontify)
				'mouse-face 'highlight
				'register-handler hdl
				'help-echo "mouse-2: use this register"))))))
     register-alist))
  (register-list-mode)
  (goto-char (point-min))
  (line-move 2 t)
  (if (called-interactively-p)
      (message "[d]elete  [e/E]dit key/value  RET:jump/copy  [FNRSW]:select type  ?:help")
    (message "Register type: %s" register-list-current-type)))

(defun register-list-call-handler-at-mouse (ev)
  "Call the register handler at point.
See `register-list-call-handler-at-point' for details."
  (interactive "e")
  (mouse-set-point ev)
  (register-list-call-handler-at-point))

(defun register-list-call-handler-at-point nil
  "Call the register handler at point.
If the point is on a register key, edit the key.  If the point is
on a register type, rebuild the list restricting to registers of
this type.  If the point is on a register value, either jump to
the register or copy its value into the kill ring."
  (interactive)
  (let ((handler (get-text-property (point) 'register-handler)))
    (if handler
	(condition-case nil
	    (funcall (get-text-property (point) 'register-handler))
	  (error (message "Can't jump to register location"))))))

(defun register-list-get-handler (register type)
  "Return a handler function for a REGISTER with TYPE."
  (cond ((string= "?" type)
	 `(lambda() (message "No action with this type")))
	((string= "S" type)
	 `(lambda()
	    (kill-new ,(cdr register))
	    (message "String copied to the kill ring")))
	((string= "N" type)
	 `(lambda()
	    (kill-new ,(number-to-string (cdr register)))
	    (message "Number copied to the kill ring as a string")))
	((string= "R" type)
	 `(lambda()
	    (kill-new ,(mapconcat 'identity (cdr register) "\n"))
	    (message "Rectangle copied to the kill ring")))
	((string-match "[FMW]" type)
	 `(lambda()
	    (jump-to-register ,(car register))
	    (message (format "Jumped to register %s"
			     ,(char-to-string (car register))))))))

(defun register-list-value-to-string (value type)
  "Convert a register VALUE into a string according to its TYPE."
  (cond ((string= "M" type)
	 (cond ((marker-position value)
		(format "[Marker at point %d in buffer %s]"
			(marker-position value)
			(buffer-name (marker-buffer value))))
	       ((marker-buffer value)
		(format "[Marker in buffer %s]"
			(buffer-name (marker-buffer value))))
	       (t (format "[Marker gone?]"))))
	((string= "N" type)
	 (format "Number: %s" (number-to-string value)))
	((string= "S" type)
	 (replace-regexp-in-string "[\n\r\t]" " " value))
	((string= "R" type)
	 (mapconcat 'identity value "\\ "))
	((string= "W" type)
	 (format "[Window configuration in frame \"%s\"]"
		  (frame-parameter
		   (window-configuration-frame (car value)) 'name)))
	((string= "F" type)
	 (format "[Frame configuration]"))
	(t "[Error: unknow type]")))

(defun register-list-get-type (key)
  "Get the type for register's KEY."
  (if (atom key)
      (cond ((stringp key) "S")
	    ((markerp key) "M")
	    ((numberp key) "N")
	    (t "error"))
    (cond ((window-configuration-p (car key)) "W")
	  ((frame-configuration-p (car key)) "F")
	  ((stringp (car key)) "R")
	  ((string= "Unprintable entity" (car key)) "?")
	  (t "error"))))

;;; Edit key/value of the register

;; FIXME delete?
;; (defun register-list-edit-key-or-value nil
;;   "Edit the register key or value depending on the point."
;;   (interactive)
;;   (if (get-text-property (point) 'register)
;;       (register-list-edit-key)
;;     (register-list-edit-value)))

(defun register-list-edit-key nil
  "Edit the key of the register at point."
  (interactive)
  (register-list-set-key 
   (lambda (v) (read-char (format "New key (%s): " 
				  (char-to-string v))))))

(defun register-list-increment-key nil
  "Increment the key of the register at point."
  (interactive)
  (register-list-set-key '1+))

(defun register-list-delete-duplicates nil
  "Interactively delete duplicates."
  (interactive)
  (mapc (lambda (r)
	  (mapc (lambda(rr)
		  (if (and (eq (car r) (car rr))
			   (y-or-n-p 
			    (format "Delete register with key `%s'? "
				    (char-to-string (car rr)))))
		      (setq register-alist (delete rr register-alist))))
		(cdr (member r register-alist))))
	register-alist))

;; (defun register-list- (register)
;;   "Overline the register with KEY."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward (concat "^[ DC]" (char-to-string key)) nil t)
;;       (goto-char
;;     (while (next-single-property-change (point) 'register)

(defun register-list-decrement-key nil
  "Decrement the key of the register at point."
  (interactive)
  (register-list-set-key '1-))

(defun register-list-set-key (function)
  "Update the regsiter key by applying FUNCTION."
  (register-list-preserve-pos
   2 ;; go back to top of the sorted list
   (beginning-of-line)
   (let* ((reg-point (next-single-property-change (point) 'register))
	  (reg (get-text-property reg-point 'register))
	  (val (car reg)))
     (setq register-alist (delete reg register-alist))
     (add-to-list 'register-alist
		  (cons (setcar reg (funcall function val)) (cdr reg)))
     (register-list register-list-current-type
		    register-list-current-fontification))))

(defun register-list-edit-value nil
  "Edit the value of the register at point."
  (interactive)
  (let* ((reg-at-point
	  (save-excursion
	    (beginning-of-line)
	    (next-single-property-change (point) 'register)))
	 (reg (get-text-property reg-at-point 'register))
	 (val (cdr reg))
	 new-val)
    (if (not (or (stringp val) (numberp val)
		 (and (listp val) (stringp (car val)))))
	(message "Can't edit this type of register")
      (setq register-list-temp-window-cfg (current-window-configuration))
      (setq register-list-temp-register reg)
      (setq register-list-temp-pos
	    (cons (line-number-at-pos (point)) (current-column)))
      (setq register-list-edit-value-type
	    (cond ((numberp val) 'number)
		  ((listp val) 'rectangle)
		  (t 'string)))
      (pop-to-buffer (get-buffer-create "*Register Edit*"))
      (erase-buffer)
      (insert (cond ((numberp val) (number-to-string val))
		    ((listp val) (mapconcat 'identity val "\n"))
		    (t val)))
      (setq register-list-rectangle-column
	    (if (eq register-list-edit-value-type 'rectangle)
		(length (car val)) nil))
      (register-list-edit-value-mode)
      (message "Press C-c C-c when you're done"))))

(define-derived-mode register-list-edit-value-mode text-mode
  "Edit Register Value"
  "Mode for editing the value of a register.
When you are done editing the value, store it with \\[register-list-send-string].

\\{register-list-edit-value-mode-map}")

(defun register-list-add-rectangle-overlays (column)
  "Add overlays to display strings beyond COLUMN.
Do this on all lines in the current buffer."
  (register-list-map-lines
   ((beg (progn (forward-char column) (point)))
    (end (progn (end-of-line) (point))))
   (unless (eq beg end)
     (overlay-put (make-overlay beg end)
		  'face 'register-list-off-rectangle))))

(defun register-list-add-trailing-whitespace (column)
  "Add trailing whitespaces to fill to COLUMN.
Do this on all lines in the current buffer."
  (register-list-map-lines
   ((eol (save-excursion (end-of-line) (point)))
    (rem (% eol (1+ column))))
   (if (and (not (eq rem 0))
	    (< eol (* (1+ column) (line-number-at-pos (point)))))
       (save-excursion
	 (end-of-line)
	 (insert (make-string (- (1+ column) rem) 32))))))

(defun register-list-send-value nil
  "Use the buffer to store the new value of a register.
Convert the buffer to a number or a rectangle if required."
  (interactive)
  (catch 'cancel
    (when register-list-rectangle-column
      ;; fix whitespace before sending a rectangle
      (register-list-add-trailing-whitespace
       register-list-rectangle-column)
      ;; cut off trailing string before sending a rectangle
      (register-list-add-rectangle-overlays
       register-list-rectangle-column)
      (if (and (delq nil (overlay-lists))
	       (not (y-or-n-p "Cut off the fontified part of the rectangle? ")))
	  (throw 'cancel (message "Back to editing"))))
    ;; now send the value
    (set-register (car register-list-temp-register)
		  (cond ((eq register-list-edit-value-type 'number)
			 (string-to-number (buffer-string)))
			((eq register-list-edit-value-type 'rectangle)
			 (mapcar (lambda (l) (truncate-string-to-width
					      l register-list-rectangle-column
					      0 32))
				 (split-string (buffer-string) "\n")))
			(t (buffer-string))))
    (kill-buffer (current-buffer))
    (register-list register-list-current-type
		   register-list-current-fontification)
    (set-window-configuration register-list-temp-window-cfg)
    (line-move (1- (car register-list-temp-pos)) t)
    (line-move-to-column (cdr register-list-temp-pos)))
  ;; remove overlays if sending was cancelled
  (mapc (lambda(ovs) (mapc (lambda(o) (delete-overlay o)) ovs))
	(overlay-lists))
  (message "New value stored"))

(defun register-list-prepare-string (string &optional fontify)
  "Prepare STRING for the register list.
An optional argument FONTIFY takes precedence over
`register-list-preserve-fontification' to decide whether the
string should keep its original fontification.  Also shorten the
output string to `register-list-string-width'."
  (if (and register-list-string-width
	   (> (length string) register-list-string-width))
      (setq string (substring string 0 register-list-string-width)))
  (when (or fontify register-list-preserve-fontification)
    (remove-text-properties 0 (length string) '(face nil) string))
  string)

;;;; ChangeLog:

;; 2012-10-31  Stefan Monnier  <monnier@iro.umontreal.ca>
;; 
;; 	* admin/update-archive.sh: Keep old packages.
;; 
;; 2011-07-12  Chong Yidong  <cyd@stupidchicken.com>
;; 
;; 	Fix version numbers of sisu-mode, register-list, and windresize.
;; 
;; 2011-07-08  Bastien Guerry  <bastien1@free.fr>
;; 
;; 	Add packages/register-list/register-list.el
;; 


(provide 'register-list)

;;; register-list.el ends here
