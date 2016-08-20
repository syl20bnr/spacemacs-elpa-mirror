;;; ox-tiddly.el --- org TiddlyWiki exporter

;; Copyright (C) 2013 Derek Feichtinger
 
;; Author: Derek Feichtinger <derek.feichtinger@psi.ch>
;; Keywords: org
;; Package-Version: 20151206.240
;; Homepage: https://github.com/dfeich/org8-wikiexporters
;; Package-Requires: ((org "8") (cl-lib "0.5"))
;; Version: 0.1.20131124

;; This file is not part of GNU Emacs.

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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ox-tiddly.el lets you convert Org files to tiddly buffers
;; using the ox.el export engine.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-tiddly)
;;
;; Export Org files to tiddly:
;; M-x org-tiddly-export-as-tiddly RET
;;
;; based on ox-confluence by Sébastien Delafond
;;; Code:
(require 'ox)
(require 'ox-ascii)
(require 'cl-lib)

;; Define the backend itself
(org-export-define-derived-backend 'tiddly 'ascii
  :translate-alist '((bold . org-tiddly-bold)
		     (example-block . org-tiddly-example-block)
		     (export-block . org-tiddly-export-block)
		     (fixed-width . org-tiddly-fixed-width)
		     (footnote-definition . org-tiddly-empty)
		     (footnote-reference . org-tiddly-empty)
		     (headline . org-tiddly-headline)
		     (italic . org-tiddly-italic)
		     (item . org-tiddly-item)
		     (link . org-tiddly-link)
		     (paragraph . org-tiddly-paragraph)
;;		     (plain-list . org-tiddly-plain-list)
		     (section . org-tiddly-section)
		     (src-block . org-tiddly-src-block)
		     (strike-through . org-tiddly-strike-through)
		     (table . org-tiddly-table)
		     (table-cell . org-tiddly-table-cell)
		     (table-row . org-tiddly-table-row)
		     (template . org-tiddly-template)
		     (underline . org-tiddly-underline)
		     (verbatim . org-tiddly-verbatim))
  :export-block "HTML" 
  ;; :menu-entry '(?w "Export to Wiki"
  ;; 		   ((?t "As TiddlyWiki buffer" org-tiddly-export-as-tiddly)))
)

;;;;;;;;;;
;; debugging helpers
(defun org-enclose-element-property (plist property tag)
  (format "<%s>%s</%s>" tag (org-element-property property plist) tag)
)

(defun plist-get-keys (pl)
  (let (result)
      (cl-loop for (key val) on pl by #'cddr
               do (push key result))
      result)
)
;;;;;;;;;;

;; All the functions we use
(defun org-tiddly-bold (bold contents info)
  (format "''%s''" contents))

(defun org-tiddly-empty (empty contents info)
  "")

(defun org-tiddly-plain-list (plain-list contents info)
  ;; (format "<TYPE>%s</TYPE>\n<STRUCTURE>%s</STRUCTURE>\n<CONTENTS>%s</CONTENTS>\n<PLAIN-LIST>%s</PLAIN-LIST>"
  ;; 	  (org-element-property :type plain-list)
  ;; 	  (org-element-property :structure plain-list)
  ;; 	  contents plain-list)

  ;; (concat (org-enclose-element-property plain-list :type "TYPE")
  ;; 	  (org-enclose-element-property plain-list :structure "STRUCTURE")
  ;; 	  (format "\n<CONTENTS>%s</CONTENTS>" contents))
  contents
  )

(defun org-tiddly-item (item contents info)
  ;; (message "<ITEM_PROPS>%s</ITEM_PROPS>" (plist-get-keys (cadr item)))
  ;; (message "<ITEM_CONTENTS>%s</ITEM_CONTENTS>" contents)
  (let* ((beg (org-element-property :begin item))
	 (struct (org-element-property :structure item))
	 (itemstruct (assoc beg struct))
	 (parent (org-element-property :parent item))
	 (ltype (org-element-property :type parent))
	 (indices (org-list-get-item-number
					(org-element-property :begin item)
					struct
					(org-list-prevs-alist struct)
					(org-list-parents-alist struct))))
    (concat 
     ;; (format "beg: %s; " beg)
     ;; (format "str: %s; " itemstruct)
     ;; (format "type: %s; " ltype)
     ;; (format "\n  olpa: %s; " (org-list-prevs-alist struct))
     ;; (format "\n  olpara: %s; " (org-list-parents-alist struct))
     ;; (format "number: %s;\n" indices)	    
     (if (eq ltype 'ordered) (make-string (length indices) ?#)
       (make-string (length indices) ?*))
     " " contents))
)

(defun org-tiddly-example-block (example-block contents info)
  (format "\n\{\{\{\n%s\}\}\}\n"
	  (org-export-format-code-default example-block info))
  )

(defun org-tiddly-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to HTML.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (when (string= (org-element-property :type export-block) "HTML")
    (org-element-property :value export-block)))

(defun org-tiddly-italic (italic contents info)
  (format "//%s//" contents))

(defun org-tiddly-fixed-width (fixed-width contents info)
  (format "\{\{\{\n%s\}\}\}"
	 (org-element-property :value fixed-width)))

(defun org-tiddly-verbatim (verbatim contents info)
  (format "\{\{\{%s\}\}\}" (org-element-property :value verbatim)))

(defun org-tiddly-headline (headline contents info)
  (let ((low-level-rank (org-export-low-level-p headline info))
        (text (org-export-data (org-element-property :title headline)
                               info))
        (level (org-export-get-relative-level headline info)))
    ;; Else: Standard headline.
    (format "%s %s\n%s" (make-string level ?\!) text
            (if (org-string-nw-p contents) contents
              ""))))

;; TODO: if there is whitespace in desc, the line gets sometimes broken apart. Why?
(defun org-tiddly-link (link desc info)
  (let ((raw-link (org-element-property :raw-link link)))
    (concat "[["
            (when (org-string-nw-p desc) (format "%s|" desc))
	    raw-link
            "]]")))

;; replace all newlines in paragraphs (includes list item text, which
;; also constitutes a paragraph
(defun org-tiddly-paragraph (paragraph contents info)
  (replace-regexp-in-string "\n" " " contents))

(defun org-tiddly-section (section contents info)
  contents)

(defun org-tiddly-src-block (src-block contents info)
  (org-tiddly-example-block src-block contents info)
  )

(defun org-tiddly-strike-through (strike-through contents info)
  (format "==%s==" contents))

(defun org-tiddly-table (table contents info)
  contents)

(defun org-tiddly-table-row  (table-row contents info)
  (concat
   (if (org-string-nw-p contents) (format "|%s" contents)
     "")
   (when (org-export-table-row-ends-header-p table-row info)
     "|")))

(defun org-tiddly-table-cell  (table-cell contents info)
  (let ((table-row (org-export-get-parent table-cell)))
    (concat
     (when (org-export-table-row-starts-header-p table-row info)
       "|")
     contents "|")))

(defun org-tiddly-template (contents info)
  (let ((depth (plist-get info :with-toc)))
    (concat (when depth "<<ToC>>\n\n") contents)))

(defun org-tiddly-underline (underline contents info)
  (format "__%s__" contents))

;; (defun org-tiddly--block (language theme contents)
;;   (concat "\{code:theme=" theme
;;           (when language (format "|language=%s" language))
;;           "}\n"
;;           contents
;;           "\{code\}\n"))

;; main interactive entrypoint
;;;###autoload
(defun org-tiddly-export-as-tiddly
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

When optional argument BODY-ONLY is non-nil, strip title, table
of contents and footnote definitions from output.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Export is done in a buffer named \"*Org Tiddly Export*\", which
will be displayed when `org-export-show-temporary-export-buffer'
is non-nil."
  (interactive)
  (org-export-to-buffer 'tiddly "*Org Tiddly Export*"
    async subtreep visible-only body-only ext-plist (lambda () (text-mode))))

(provide 'ox-tiddly)

;;; ox-tiddly.el ends here
