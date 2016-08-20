;;; ox-nikola.el --- Export Nikola articles using org-mode.

;; Copyright (C) 2014,2015  IGARASHI Masanao

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Author: IGARASHI Masanao <syoux2@gmail.com>
;; Keywords: org, nikola
;; Package-Version: 20151114.316
;; Version: 0.1
;; URL: https://github.com/masayuko/ox-nikola
;; Package-Requires: ((emacs "24.4") (org "8.2.4") (ox-rst "0.2"))

;;; Commentary:

;;; Code:

;;; Dependencies

(eval-when-compile (require 'cl))
(require 'ox)
(require 'ox-publish)
(require 'ox-rst)


;;; User Configurable Variables

(defgroup org-export-nikola nil
  "Options for exporting Org mode files to Nikola reStructuredText."
  :tag "Org Nikola"
  :group 'org-export)

(defcustom org-nikola-nikola-template ""
  "Default template in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-type "text"
  "Default type in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-password ""
  "Default password in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-section ""
  "Default section in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-category ""
  "Default category in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-annotations ""
  "Default annotations metadata field in a Nikola article. True or other"
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-noannotations ""
  "Default noannotations metadata field in a Nikola article. True or other"
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-nocomments ""
  "Default nocomments metadata field in a Nikola article. True or other"
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-hidetitle ""
  "Default hidetitle metadata field in a Nikola article. True or other"
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-previewimage ""
  "Default previewimage in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

(defcustom org-nikola-enclosure ""
  "Default enclosure in a Nikola article."
  :group 'org-export-nikola
  :type 'string)

;;; Define Back-End

(org-export-define-derived-backend 'nikola 'rst
  :menu-entry
  '(?n "Export to reStructuredText for Nikola"
       ((?R "As reStructuredText buffer" org-nikola-export-as-rst)
        (?r "As reStructuredText file" org-nikola-export-to-rst)))
  :translate-alist
  '((template . org-nikola-template))
  :options-alist
  '((:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
	(:nikola-slug "NIKOLA_SLUG" nil "")
	(:nikola-link "NIKOLA_LINK" nil "")
	(:nikola-type "NIKOLA_TYPE" nil org-nikola-type)
	(:nikola-password "NIKOLA_PASSWORD" nil org-nikola-password)
	(:nikola-template "NIKOLA_TEMPLATE" nil org-nikola-nikola-template)
    (:nikola-section "NIKOLA_SECTION" nil org-nikola-section)
	(:nikola-updated "NIKOLA_UPDATED" nil "")
    (:nikola-category "NIKOLA_CATEGORY" nil org-nikola-category)
    (:nikola-annotations "NIKOLA_ANNOTATIONS" nil org-nikola-annotations)
    (:nikola-annotations "NIKOLA_NOANNOTATIONS" nil org-nikola-noannotations)
    (:nikola-nocomments "NIKOLA_NOCOMMENTS" nil org-nikola-nocomments)
    (:nikola-hidetitle "NIKOLA_HIDETITLE" nil org-nikola-hidetitle)
    (:nikola-previewimage "NIKOLA_PREVIEWIMAGE" nil org-nikola-previewimage)
    (:nikola-enclosure "NIKOLA_ENCLOSURE" nil org-nikola-enclosure)))


;;; Template

(defun org-nikola-template (contents info)
  "Return complete document string after reStructuredText conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (concat
   (org-nikola--front-matter info)
   "\n"
   contents))

(defun org-nikola--get-option (info property-name &optional default)
  (let ((property (org-export-data (plist-get info property-name) info)))
    (if (string= "" property) default property)))

(defun org-nikola--get-true-option (info property-name)
  (let ((property (org-export-data (plist-get info property-name) info)))
	(if	(or (string= (downcase property) "true")
			(string= (downcase property) "t"))
		"True" "")))

(defun org-nikola--front-matter (info)
  (let* ((title
		  (org-nikola--get-option info :title ""))
		 (author
		  (org-nikola--get-option info :author ""))
		 (email
		  (org-nikola--get-option info :email ""))
		 (date
		  (org-nikola--get-option info :date ""))
		 (title
		  (if (string= title "")
			  (cond
			   ((and (org-string-nw-p date) (org-string-nw-p author))
				(concat
				 author
				 " "
				 date
				 (when (org-string-nw-p email) (concat " " email))))
			   ((and (org-string-nw-p date) (org-string-nw-p email))
				(concat
				 email
				 " "
				 date))
			   ((org-string-nw-p date)
				date)
			   ((and (org-string-nw-p author) (org-string-nw-p email))
				(concat author " " email))
			   ((org-string-nw-p author) author)
			   ((org-string-nw-p email) email)) title))
		 (slug
		  (org-nikola--get-option info :nikola-slug title))
		 (keywords
		  (org-nikola--get-option info :keywords ""))
		 (link
		  (org-nikola--get-option info :nikola-link ""))
		 (description
		  (org-nikola--get-option info :description ""))
		 (type
		  (org-nikola--get-option info :nikola-type ""))
		 (password
		  (org-nikola--get-option info :nikola-password ""))
		 (template
		  (org-nikola--get-option info :nikola-template ""))
		 (section
		  (org-nikola--get-option info :nikola-section ""))
		 (updated
		  (org-nikola--get-option info :nikola-updated ""))
		 (category
		  (org-nikola--get-option info :nikola-category ""))
		 (annotations
		  (org-nikola--get-true-option info :nikola-annotations))
		 (noannotations
		  (org-nikola--get-true-option info :nikola-noannotations))
		 (nocomments
		  (org-nikola--get-true-option info :nikola-nocomments))
		 (hidetitle
		  (org-nikola--get-true-option info :nikola-hidetitle))
		 (previewimage
		  (org-nikola--get-option info :nikola-previewimage ""))
		 (enclosure
		  (org-nikola--get-option info :nikola-enclosure "")))
    (concat
     ".. title: "      title
     "\n.. slug: "     (replace-regexp-in-string "[　]+" "-"
												 (replace-regexp-in-string
												  "[\s-]+" "-" slug))
     "\n.. date: "     date
     (cond ((not (string= "" updated)) (concat "\n.. updated: " updated)))
     "\n.. tags: "     keywords
     "\n.. link: "     link
     "\n.. description: " description
     (cond ((not (string= "" type)) (concat "\n.. type: " type)))
     (cond ((and (not (string= "" author)) (plist-get info :with-author))
			(concat "\n.. author: " author)))
     (cond ((and (not (string= "" email)) (plist-get info :with-email))
			(format " <%s>" email)))
     (cond ((not (string= "" password)) (concat "\n.. password: " password)))
	 (cond ((not (string= "" template)) (concat "\n.. template: " template)))
     (cond ((not (string= "" section)) (concat "\n.. section: " section)))
     (cond ((not (string= "" category)) (concat "\n.. category: " category)))
     (cond ((not (string= "" annotations))
			(concat "\n.. annotations: " annotations)))
     (cond ((not (string= "" noannotations))
			(concat "\n.. noannotations: " noannotations)))
     (cond ((not (string= "" nocomments))
			(concat "\n.. nocomments: " nocomments)))
     (cond ((not (string= "" hidetitle))
			(concat "\n.. hidetitle: " hidetitle)))
     (cond ((not (string= "" previewimage))
            (concat "\n.. previewimage: " previewimage)))
     (cond ((not (string= "" enclosure))
            (concat "\n.. enclosure: " enclosure)))
     "\n")))


;;; End-User functions

;;;###autoload
(defun org-nikola-export-as-rst
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reStructuredText buffer."
  (interactive)
  (org-export-to-buffer 'nikola "*Org nikola RST Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (rst-mode))))


;;;###autoload
(defun org-nikola-export-to-rst
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reStructuredText file"
  (interactive)
  (let ((outfile (org-export-output-file-name ".rst" subtreep)))
    (org-export-to-file 'nikola outfile
      async subtreep visible-only body-only ext-plist)))


;;;###autoload
(defun org-nikola-publish-to-rst (plist filename pub-dir)
  "Publish an org file to reStructuredText.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'nikola filename ".rst" plist pub-dir))


;;; provide

(provide 'ox-nikola)

;;; ox-nikola.el ends here
