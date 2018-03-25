;;; company-suggest.el --- Company-mode back-end for search engine suggests  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@archlinux.org>
;; URL: https://github.com/juergenhoetzel/company-suggest
;; Package-Version: 20180325.931
;; Keywords: completion convenience
;; Package-Requires: ((company "0.9.0") (emacs "25.1"))

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

;; This package provides a company-mode back-end for auto-completing using search engine suggestions.

;;; Code:

(require 'company)
(require 'xml)
(require 'mm-url)
(require 'cl-lib)
(require 'thingatpt)

(defgroup company-suggest '()
  "Customization group for `company-suggest'."
  :link '(url-link "http://github.com/juergenhoetzel/company-suggest")
  :group 'convenience
  :group 'comm)

(defcustom company-suggest-complete-sentence nil
  "When non-nil, use sentence to complete current word."
  :type 'boolean)

(defvar company-suggest-google-url
  "https://suggestqueries.google.com/complete/search?q=%s&client=toolbar")

(defvar company-suggest-wiktionary-url
  "https://en.wiktionary.org/w/api.php?action=opensearch&format=json&formatversion=2&search=%s&namespace=0&limit=10&suggest=true")

(defun company-suggest--google-candidates (prefix)
  "Return a list of Google suggestions matching PREFIX."
  (with-temp-buffer
    ;; FIXME: Error checking
    (delete-region (point-min) (point-max))
    (mm-url-insert (format company-suggest-google-url prefix))
    (mapcar (lambda (node) (xml-get-attribute (car (xml-get-children node 'suggestion)) 'data))
	    (xml-get-children (car (xml-parse-region (point-min) (point-max))) 'CompleteSuggestion))))

(defun company-suggest--sentence-at-point ()
  "Return the sentence at point."
  (let* ((current-line (line-number-at-pos))
	 sentence-line
	 (sentence
	  (save-excursion
	    (backward-sentence 1)
	    (setq sentence-line (line-number-at-pos))
	    ;; don't span prefix over following lines
	    (when (thing-at-point 'sentence)
	      (replace-regexp-in-string
	       ".*?\\([[:alnum:]][[:space:][:alnum:]]*\\)"
	       "\\1"
	       (replace-regexp-in-string "\\(.*\\)[ \t\n]*.*" "\\1" (thing-at-point 'sentence)))))))
    (or (if (eq sentence-line current-line) sentence) (thing-at-point 'word)))) ;fallback to word

;;;###autoload
(defun company-suggest-google (command &optional arg &rest ignored)
  "`company-mode' completion backend for Google suggestions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-suggest-google))
    (prefix (when (derived-mode-p 'text-mode)
	      (if company-suggest-complete-sentence
		  ;; FIXME (thing-at-point 'sentence) doesn't work reliable
		  (company-suggest--sentence-at-point)
		(thing-at-point 'word))))
    (ignore-case t)
    (candidates (mapcar (lambda  (s)
			  (if (string-prefix-p arg s t)
			      (concat arg (substring s (length arg)))
			    s))
			(company-suggest--google-candidates arg)))))

(defun company-suggest--wiktionary-candidates (prefix)
  "Return a list of Wiktionary suggestions matching PREFIX."
  (with-temp-buffer
    (delete-region (point-min) (point-max))
    (mm-url-insert (format company-suggest-wiktionary-url prefix))
    (let ((json-array-type 'list)
	  (json-object-type 'hash-table)
	  (json-key-type 'string))
      ;; FIXME: Error checking
      (cadr (json-read)))))

;;;###autoload
(defun company-suggest-wiktionary (command &optional arg &rest ignored)
  "`company-mode' completion backend for Wiktionary suggestions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-suggest-wiktionary))
    (prefix (when (derived-mode-p 'text-mode)
	      (thing-at-point 'word)))
    (candidates (company-suggest--wiktionary-candidates arg))))

(provide 'company-suggest)
;;; company-suggest.el ends here
