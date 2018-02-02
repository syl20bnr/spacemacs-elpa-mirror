;;; ox-minutes.el --- Plain text backend for Org for Meeting Minutes -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Kaushal Modi

;; Author: Kaushal Modi <kaushal.modi@gmail.com>
;; URL: https://github.com/kaushalmodi/ox-minutes
;; Package-Version: 20180202.934
;; Version: 0.1
;; Keywords: org, exporter, notes
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

;; The aim of this exporter to generate meeting minutes plain text that is
;; convenient to send via email.
;;  - Unnecessary blank lines are removed from the final exported plain text.
;;  - Header decoration and section numbers done in the default ASCII exports
;;    is prevented.
;;  - Also TOC and author name are not exported.
;;
;; This is an ox-ascii derived backed for org exports.
;;
;; This backend effectively sets the `org-export-headline-levels' to 0 and,
;; `org-export-with-section-numbers', `org-export-with-author' and
;; `org-export-with-toc' to nil time being for the exports.  That is equivalent
;; to manually putting the below in the org file:
;;
;;     #+options: H:0 num:nil author:nil toc:nil
;;
;; This package has been tested to work with the latest version of org built
;; from the master branch ( http://orgmode.org/cgit.cgi/org-mode.git ) as of
;; Aug 10 2016.

;; EXAMPLE ORG FILE:
;;
;;     #+title: My notes
;;
;;     * Heading 1
;;     ** Sub heading
;;     *** More nesting
;;     - List item 1
;;     - List item 2
;;     - List item 3
;;     * Heading 2
;;     ** Sub heading
;;     - List item 1
;;     - List item 2
;;     - List item 3
;;     *** More nesting

;; MINUTES EXPORT:
;;
;;                                    __________
;;
;;                                     MY NOTES
;;                                    __________
;;
;;     * Heading 1
;;       + Sub heading
;;         - More nesting
;;           - List item 1
;;           - List item 2
;;           - List item 3
;;
;;     * Heading 2
;;       + Sub heading
;;         - List item 1
;;         - List item 2
;;         - List item 3
;;         - More nesting

;; REQUIREMENTS:
;;
;; - Emacs 24 is required at minimum for lexical binding support.
;; - Emacs 24.4 is required as ox-ascii got added to org-mode in that Emacs
;;   release.

;;; Code:

(require 'ox-ascii)

(org-export-define-derived-backend 'minutes 'ascii
  :menu-entry
  '(?M "Export to Plain Text meeting minutes"
       ((?M "To temporary buffer" org-minutes-export-as-ascii)
        (?m "To file" org-minutes-export-to-ascii)
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-minutes-export-to-ascii t s v b)
                (org-open-file (org-minutes-export-to-ascii nil s v b)))))))
  ;; See `org-export-options-alist' for details on the structure of the below
  ;; value.
  :options-alist
  '((:with-toc nil "toc" nil) ; No TOC by default
    (:with-author nil "author" nil) ; Do not print the author by default
    (:headline-levels nil "H" 0) ; Count all headlines as list items by default
    (:section-numbers nil "num" nil)) ; Do not print section numbers by default
  :filters-alist '((:filter-final-output . org-minutes-final-function)))

;;;; Filter functions

(defun org-minutes-final-function (contents _backend _info)
  "Filter to remove extra blank lines from the final output CONTENTS."
  ;; (message "dbg: info: %S" info)
  (with-temp-buffer
    (insert contents)

    (goto-char (point-min))
    (while (re-search-forward (concat
                               ;; 0 or more newlines
                               "\n*"
                               ;; followed by the newline before the
                               ;; line containing list items
                               "\\(\n\\s-*[*+-]\\)")
                              nil :noerror)
      ;; Make sure that only one new line exists only before the topmost
      ;; level list items.
      (if (string-match-p "^\\*" (match-string-no-properties 1))
          (replace-match "\n\\1")
        ;; Remove all blank lines before all other list items
        (replace-match "\\1")))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;; End-user functions

;;;###autoload
(defun org-minutes-export-as-ascii
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Minutes Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'minutes "*Org Minutes Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (text-mode))))

;;;###autoload
(defun org-minutes-export-to-ascii
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a text file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, strip title and
table of contents from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (let ((file (org-export-output-file-name "_minutes.txt" subtreep)))
    (org-export-to-file 'minutes file
      async subtreep visible-only body-only ext-plist)))


(provide 'ox-minutes)
;;; ox-minutes.el ends here
