;;; omniref.el --- Omniref Ruby documentation search engine interface

;; Filename: omniref.el
;; Description: Oniref Ruby documentation search engine interface
;; Author: "Aleksandar Simić" <asimic@gmail.com>
;; License: BSD
;; Created: 2014-02-22 17:22
;; Version: 1.0.1
;; Package-Version: 20160225.1424
;; URL: http://github.org/dotemacs/omniref.el
;; Keywords: docs, help, tools

;; This file in not part of GNU Emacs.

;; Commentary:

;; M-x omniref - if a region is selected, then that region is searched
;; otherwise you'll be prompted for a search term

;; Shout out to Dimitri Fontaine for dim-google.el:
;; http://www.emacswiki.org/emacs/dim-google.el from which this code
;; borrows

;;; Code:

(require 'browse-url)

(defconst omniref-url "http://docs.omniref.com/?q="
  "Omniref query URL")

(defun omniref-formatted-search-term (terms)
  "Format the term for searching"
  (replace-regexp-in-string "[[:space:]]+" "+" terms))

(defun omniref-search-for (term)
  "Browse to Omniref with the search term"
  (browse-url
   (concat omniref-url (omniref-formatted-search-term term))))

(defun omniref-region-or-word ()
  "Determine what should be searched, a highlighted region of
   text or a word the cursor is on"
   (list
    (if (use-region-p)
	(buffer-substring (region-beginning) (region-end))
      (read-string "Search omniref for: " (thing-at-point 'word)))))

;;;###autoload
(defun omniref (keywords)
  "Search Omniref, Ruby documentation search engine"
  (interactive (omniref-region-or-word))
  (omniref-search-for keywords))

(provide 'omniref)

;;; omniref.el ends here
