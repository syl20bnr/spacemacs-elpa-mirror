;;; vhdl-capf.el --- Completion at point function (capf) for vhdl-mode.

;; Copyright (C) 2015 sh-ow
;;
;; Author: sh-ow <sh-ow@users.noreply.github.com>
;; URL: https://github.com/sh-ow/vhdl-capf
;; Package-Version: 20160221.934
;; Version: 0.1
;; Keywords: convenience, usability, vhdl, completion

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; A copy of the GNU General Public License can be obtained from this
;; program's author or from the Free Software Foundation, Inc., 675 Mass Ave,
;; Cambridge, MA 02139, USA.

;;; Commentary:
;; VHDL-completion at point function provided by this module. Enable by just
;; calling (vhdl-capf-enable) in your .emacs.

;;; Code:

(defconst vhdl-capf-search-vhdl-buffers-for-candidates 3
  "If t, search in _all_ other vhdl-buffers for completions.
When number, search in the last opened (number+1) vhdl-buffers.")

(defvar vhdl-capf-completion-cache nil
  "Cache for completion candidates per vhdl-buffer: alist with form (buffername . candidates).")

(defconst vhdl-capf-exclude-common-vhdl-syntax '("signal" "variable" "downto" "to" "if" "then"
												  "begin" "end" "in" "out" "std_logic" "std_logic_vector")
  "Some often occuring VHDL syntax constructs to exclude from the possible completions-list.")

(defun vhdl-capf-flatten (l)
  "Convert a list of lists into a single list.
Argument L is the list to be flattened."
  (when l
    (if (atom (first l))
		(cons (first l) (vhdl-capf-flatten (rest l)))
      (append (vhdl-capf-flatten (first l)) (vhdl-capf-flatten (rest l))))))

(defun vhdl-capf-get-vhdl-buffers (&optional nfirst)
  "Returns a list with all buffers that are in vhdl major mode.
Optional argument NFIRST is the amount of buffers to return."
  (let ((vhdl-buffers ())
		(cnt 0))
    (dolist (name (buffer-list))
      (with-current-buffer name
		(when (and (eq major-mode 'vhdl-mode)
				   (or (not nfirst) (<= cnt nfirst)))
		  (setq vhdl-buffers (append vhdl-buffers (list name)))
		  (setq cnt (+ cnt 1)))))
    vhdl-buffers))

(defun vhdl-capf-line-is-comment ()
  "Returns t if current line contains nothing but a comment."
  (save-excursion
    (beginning-of-line 1)
    (looking-at (concat "^[\s-]*" comment-start-skip))))

(defun vhdl-capf-get-vhdl-symbols (&optional limit buffer)
  "Get all vhdl symbols  of a certain BUFFER.
Optional argument LIMIT specifies the point where search for symbols shall be stopped."
  (let ((complist ())
		(regpat "\\<[A-Za-z]+\\(\\sw\\|\\s_\\)+")
		(whichbuffer (if (eq buffer nil) (current-buffer) buffer))
		(result ""))
    (with-current-buffer whichbuffer
      (save-excursion
		(goto-char (point-min))
		(while (re-search-forward regpat limit t)
		  (let ((result (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
			;; exclude: vhdl syntax-stuff, stuff that is in a comment, already captured stuff
			(when (and (not (or (member result vhdl-capf-exclude-common-vhdl-syntax) (vhdl-capf-line-is-comment)))
					   (not (member result complist)))
			  (push result complist))))))
    complist))

;;;###autoload
(defun vhdl-capf-main ()
  "Handling the completion at point for vhdl mode."
  (when (eq major-mode 'vhdl-mode)
	(let* ((pos (point))
		   ;; find the word boundary (vhdl-expressions can only follow on the chars in following regexp)
		   (beg (if (re-search-backward "[=(,+-/\*\s-]" (line-beginning-position) t)
					(match-end 0)
				  (line-beginning-position)))
		   (end (goto-char pos)) ;; goto on purpose: search for 'beg' eventually moves cursor backwards!
		   (table-etc (list nil
							(completion-table-merge
							 vhdl-abbrev-list
							 (let* ((vhdl-abbrevs ())
									(didchanges nil)
									(vhdl-buffers (if (eq vhdl-capf-search-vhdl-buffers-for-candidates t)
													  (vhdl-capf-get-vhdl-buffers)
													(vhdl-capf-get-vhdl-buffers vhdl-capf-search-vhdl-buffers-for-candidates))))
							   (dotimes (idx (length vhdl-buffers))
								 (when (not (equal (car (nth idx vhdl-capf-completion-cache)) (nth idx vhdl-buffers)))
								   (add-to-ordered-list 'vhdl-capf-completion-cache
														(cons (nth idx vhdl-buffers)
															  (delete (buffer-substring-no-properties beg end)
																	  (vhdl-capf-get-vhdl-symbols (point-max) (nth idx vhdl-buffers))))
														idx)
								   (setq didchanges t)))
							   ;; cut the cache list, do save ram (the now deleted elements would have been updated anyway)
							   (when (> (length vhdl-capf-completion-cache) (length vhdl-buffers))
								 (nbutlast vhdl-capf-completion-cache (- (length vhdl-capf-completion-cache) (length vhdl-buffers))))
							   ;; if the active buffer is still the same, just do the cache update for this buffer
							   (unless didchanges
								 (setcdr (car vhdl-capf-completion-cache) (delete (buffer-substring-no-properties beg end)
																				   (vhdl-capf-get-vhdl-symbols (point-max) (nth 0 vhdl-buffers)))))
							   (vhdl-capf-flatten
								(dolist (bufcomps vhdl-capf-completion-cache vhdl-abbrevs)
								  (push (cdr bufcomps) vhdl-abbrevs))))))))
	  (when end
		(let ((tail (if (null (car table-etc))
						(cdr table-etc)
					  (cons
					   (if (memq (char-syntax (or (char-after end) ?\s))
								 '(?\s ?>))
						   (cadr table-etc)
						 (apply-partially 'completion-table-with-terminator
										  " " (cadr table-etc)))
					   (cddr table-etc)))))
		  `(,beg ,end ,@tail))))))

;;;###autoload
(defun vhdl-capf-enable ()
  "Add `vhdl-completion-at-point' function to capf's when visiting a vhdl-file."
  (add-hook 'vhdl-mode-hook (lambda () (make-local-variable 'completion-at-point-functions)
							  (add-to-list 'completion-at-point-functions 'vhdl-capf-main))))

(provide 'vhdl-capf)

;;; vhdl-capf.el ends here
