;;; grep-context.el --- Increase context in compilation and grep buffers  -*- lexical-binding: t; -*-

;;
;; Author: Michał Kondraciuk <k.michal@zoho.com>
;; URL: https://github.com/mkcms/grep-context
;; Package-Version: 20180409.1227
;; Package-Requires: ((emacs "24.4") (dash "2.12.0") (cl-lib "0.5.0"))
;; Version: 0.0.1
;; Keywords: convenience, search, grep, compile

;; Copyright (C) 2017 Michał Kondraciuk

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
;;
;; This package provides commands to show and hide lines of context around
;; errors in compilation buffers or around matches in grep buffers
;; (e.g. M-x grep).  Works with `wgrep', `ag-mode', `ivy-occur-grep-mode',
;; `ack-mode' and `ripgrep'.
;;
;; Usage:
;;
;;   (progn
;;     (require 'grep-context)
;;     (dolist (elt '((compile . compilation-mode-map)
;;                    (grep . grep-mode-map)
;;                    (ivy . ivy-occur-grep-mode-map)
;;                    (ripgrep . ripgrep-search-mode-map)
;;                    (ag . ag-mode-map)
;;                    (ack . ack-mode-map)))
;;       (eval-after-load (car elt)
;;         `(progn
;;             (define-key ,(cdr elt) (kbd "+")
;;                #'grep-context-more-around-point)
;;             (define-key ,(cdr elt) (kbd "-")
;;                #'grep-context-less-around-point)))))
;;
;; After evaluating that you can open a grep buffer and navigate to a match,
;; then hit "+" to insert a line of context before and after that match.
;; This is almost the same as running grep with `-A 1 -B 1` flags, except
;; the context is inserted only around match at point, not everywhere.
;; It is also much faster than re-running grep with those flags.
;; Hitting "+" again will insert more context lines and "-" will kill
;; outermost context lines.
;;
;; This package will work with any *compilation* buffer except it needs to
;; know how to format context lines.  If you want to use it in your mode,
;; you can add an entry to `grep-context-line-format-alist'.
;; You can also add an entry to `grep-context-separator-alist' to specify
;; a separator for non-contiguous regions of context.
;;

;;; Code:

(require 'compile)
(require 'dash)
(eval-when-compile
  (require 'cl-lib))

(defgroup grep-context nil "More context in compilation buffers."
  :group 'compilation
  :group 'grep)

(defcustom grep-context-line-format-alist
  (list (cons 'grep-mode "%s-%d-")
	(cons 'ivy-occur-grep-mode "%s-%d-")
	(cons 'ripgrep-search-mode "%s-%d-")
	(cons 'ag-mode #'grep-context-ag-format)
	(cons 'ack-mode #'grep-context-ag-format))
  "Alist that associates major modes with line formatters.
Each value is a string passed to `format' to format a prefix for a context
line.  It should contain two %-sequences, for a filename and a line number,
e.g. \"%s:%d:\".
Value can also be a function callable with a filename and a line number
and should return a formatted prefix string."
  :type '(alist :key-type (symbol :tag "Major mode")
		:value-type (choice string function))
  :group 'grep-context)

(defcustom grep-context-separator-alist
  (list (cons 'grep-mode "--")
	(cons 'ripgrep-search-mode "--")
	(cons 'ivy-occur-grep-mode "--"))
  "Alist that associates major modes with separators.
Each value is a string to be inserted between non-contiguous regions of
context.  If an entry is missing for a major mode, separators are not
used in that mode."
  :type '(alist :key-type (symbol :tag "Major mode")
		:value-type (choice string (const :tag "No separator" nil)))
  :group 'grep-context)

(defcustom grep-context-default-format "%s:%d:"
  "Default format for context lines.
Used if `grep-context-line-format-alist' contains no entry for current major
mode."
  :type '(choice string function)
  :group 'grep-context)

(defvar-local grep-context--temp-file-buffer nil
  "A cell (file . buffer) where BUFFER is a buffer with contents of FILE.")

(defun grep-context-ag-format (_file line-number)
  "Formatter for context lines in `ag-mode'."
  (concat (number-to-string line-number) "-"))

(defun grep-context--kill-temp-buffer ()
  "Kill buffer in `grep-context--temp-file-buffer'."
  (when (buffer-live-p (cdr grep-context--temp-file-buffer))
    (kill-buffer (cdr grep-context--temp-file-buffer))))
(add-hook 'kill-buffer-hook #'grep-context--kill-temp-buffer)

(defun grep-context--next-error (&optional n)
  "Move point to the next error, ignoring context lines."
  (or n (setq n 0))
  (let ((res (compilation-next-error n)))
    (if (get-text-property (point) 'grep-context-context-line)
	(if (= n 0)
	    (error "No match here")
	  (grep-context--next-error (if (< n 0) -1 1)))
      res)))

(defun grep-context--match-location (&optional n)
  "In current compilation buffer, get location for match at point.
If N is non-nil, call `grep-context--next-error' with N as argument first.
Return value is a cell (file . line)."
  (save-excursion
    (let* ((msg (grep-context--next-error (or n 0)))
	   (loc (compilation--message->loc msg))
	   (fs (compilation--loc->file-struct loc))
	   (file (car (compilation--file-struct->file-spec fs)))
	   (line (compilation--loc->line loc)))
      (cons file line))))

(defun grep-context--at-match (&optional n)
  "Get number of lines of context around match at point.
If N is non-nil, call `grep-context--next-error' with N as argument first.
Return value is a cell (context-before . context-after) that can be modified."
  (save-excursion
    (grep-context--next-error (or n 0))
    (or (get-text-property (point) 'grep-context)
	(let ((cell (cons 0 0))
	      (inhibit-read-only t))
	  (put-text-property (point-at-bol) (point-at-eol) 'grep-context cell)
	  cell))))

(defun grep-context--format-line (format file line-number line)
  (propertize (if (stringp format)
		  (concat (format format file line-number) line)
		(concat (funcall format file line-number) line))
	      'grep-context-context-line t))

;;;###autoload
(defun grep-context-more-around-point (&optional n)
  "Insert N context lines around point.
If N is negative, kill -N lines of context.
N defaults to 1."
  (interactive "p")
  (unless (compilation-buffer-p (current-buffer))
    (error "Current buffer is not compilation buffer"))

  (or n (setq n 1))
  (-let* (((file . line) (grep-context--match-location))
	  (ctx (grep-context--at-match))

	  ;; File, line, context around previous/next match
	  ((prev-file . prev-line) (ignore-errors
				     (grep-context--match-location -1)))
	  ((next-file . next-line) (ignore-errors
				     (grep-context--match-location 1)))
	  ((_ . prev-ctx) (ignore-errors (grep-context--at-match -1)))
	  ((next-ctx . _) (ignore-errors (grep-context--at-match 1)))

	  ;; Number of lines that can be inserted before/after match at point
	  (avail-before
	   (min n (or (and (equal file prev-file) (< prev-line line)
			   (- line 1 (car ctx) (+ prev-line prev-ctx)))
		      n)))
	  (avail-after
	   (min n (or (and (equal file next-file) (< line next-line)
			   (- next-line 1 next-ctx (+ line (cdr ctx))))
		      n)))

	  (format (or (cdr (assoc major-mode grep-context-line-format-alist))
		      grep-context-default-format))
	  (separator (cdr (assoc major-mode grep-context-separator-alist)))
	  (buffer (current-buffer))
	  (inhibit-read-only t))

    ;; Remove separator before and after this match
    (dolist (line-outside (list (1+ (cdr ctx)) (- (1+ (car ctx)))))
      (save-excursion
	(forward-line line-outside)
	(when (get-text-property (point) 'grep-context-separator)
	  (kill-whole-line))))

    (if (< n 0)
	(progn
	  (let ((n n))
	    (save-excursion
	      (forward-line (- (car ctx)))
	      (while (and (<= (cl-incf n) 0) (> (car ctx) 0))
		(kill-whole-line)
		(cl-decf (car ctx)))))
	  (let ((n n))
	    (save-excursion
	      (forward-line (cdr ctx))
	      (while (and (<= (cl-incf n) 0) (> (cdr ctx) 0))
		(kill-whole-line -1)
		(cl-decf (cdr ctx))))))

      ;; Prepare a buffer with file contents.
      ;; It's cached so next calls to this function will be faster.
      (unless (and grep-context--temp-file-buffer
		   (equal (car grep-context--temp-file-buffer) file))
	(when (buffer-live-p (cdr grep-context--temp-file-buffer))
	  (kill-buffer (cdr grep-context--temp-file-buffer)))
	(let ((b (generate-new-buffer
		  (generate-new-buffer-name " *tempbuffer*"))))
	  (with-current-buffer b
	    (insert-file-contents file))
	  (setq grep-context--temp-file-buffer (cons file b))))

      (with-current-buffer (cdr grep-context--temp-file-buffer)
	(goto-char (point-min))
	(unless (= (forward-line (1- line)) 0)
	  (error "Line %s is out of bounds for this file" line))

	;; Insert context lines before
	(save-excursion
	  (forward-line (- (car ctx)))

	  (while (and (>= (cl-decf avail-before) 0) (= (forward-line -1) 0))
	    (let ((string (buffer-substring (point-at-bol) (point-at-eol))))
	      (with-current-buffer buffer
		(forward-line (- (car ctx)))
		(beginning-of-line)
		(open-line 1)
		(insert (grep-context--format-line
			 format file (- line 1 (car ctx)) string))
		(cl-incf (car ctx))
		(forward-line (car ctx))))))

	;; Insert context lines after
	(save-excursion
	  (forward-line (cdr ctx))

	  (while (and (>= (cl-decf avail-after) 0) (= (forward-line 1) 0))
	    (let ((string (buffer-substring (point-at-bol) (point-at-eol))))
	      (with-current-buffer buffer
		(save-excursion
		  (forward-line (1+ (cdr ctx)))
		  (beginning-of-line)
		  (insert (grep-context--format-line
			   format file (+ line 1 (cdr ctx)) string))
		  (open-line 1)
		  (cl-incf (cdr ctx)))))))))

    ;; Insert separator before and after this match
    (when separator
      (unless (or (and (equal file prev-file) (< prev-line line)
		       (= (+ prev-line prev-ctx (car ctx) 1) line))
		  (and (= (car ctx) 0) (or (null prev-ctx) (= prev-ctx 0))))
	(forward-line (- (car ctx)))
	(beginning-of-line)
	(open-line 1)
	(insert (propertize separator 'grep-context-separator t))
	(forward-line (1+ (car ctx))))
      (unless (or (and (equal file next-file) (< line next-line)
		       (= (+ line (cdr ctx) next-ctx 1) next-line))
		  (and (= (cdr ctx) 0) (or (null next-ctx) (= next-ctx 0))))
	(save-excursion
	  (forward-line (1+ (cdr ctx)))
	  (beginning-of-line)
	  (open-line 1)
	  (insert (propertize separator 'grep-context-separator t)))))

    (save-excursion
      (forward-line (1+ (cdr ctx)))
      (compilation--ensure-parse (point-at-bol)))

    ;; Tell wgrep to reparse buffer.
    ;; TODO: Find a way to tell wgrep to reparse context around this match only
    (when (boundp 'wgrep-prepared)
      (setq wgrep-prepared nil))))

;;;###autoload
(defun grep-context-less-around-point (&optional n)
  "Kill N context lines around point.
N defaults to 1."
  (interactive "p")
  (grep-context-more-around-point (- (or n 1))))

(provide 'grep-context)

;;; grep-context.el ends here
