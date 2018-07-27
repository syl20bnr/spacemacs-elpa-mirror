;;; emacsagist.el --- Search Packagist.org packages without leaving Emacs

;; Copyright (C) 2014  Brian Zwahr

;; Version: 1.0.0
;; Package-Version: 20140331.1830
;; Author: Brian Zwahr <echosa@icloud.com>
;; Keywords: tools
;; URL: http://github.com/echosa/emacsagist
;; Package-Requires: ((cl-lib "0.5"))

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

;; This is a package for searching Packagist.org, a PHP library repository, from
;; within emacs.
;; 
;; To run a search, simply execute M-x emacsagist-search
;;
;; TAB/Shift-TAB, n/p, and j/k all jump between links.

(require 'cl-lib)
(require 'cl)
(require 'json)
(require 'url)

;;; Code:

(defvar emacsagist-packagist-url "https://packagist.org")
(defvar emacsagist-packagist-results-buffer "*Packagist*")

(cl-defstruct emacsagist-packagist-search
  query page results next-page)

(cl-defstruct emacsagist-search-result
  name description url downloads favers)

(cl-defstruct emacsagist-packagist-package
  name description maintainers versions type repository downloads favers)

(cl-defstruct emacsagist-packagist-package-version
  name description homepage version license authors type require suggest)

(defun emacsagist-parse-package-versions (data)
  "Parse the DATA into a list of structs."
  (loop for version in (reverse data)
        collect
        (make-emacsagist-packagist-package-version
         :name (cdr (assoc 'name (cdr version)))
         :description (cdr (assoc 'description (cdr version)))
         :homepage (cdr (assoc 'homepage (cdr version)))
         :version (cdr (assoc 'version (cdr version)))
         :license (cdr (assoc 'license (cdr version)))
         :authors (cdr (assoc 'authors (cdr version)))
         :type (cdr (assoc 'type (cdr version)))
         :require (cdr (assoc 'require (cdr version)))
         :suggest (cdr (assoc 'suggest (cdr version))))))

(defun emacsagist-parse-package (json)
  "Parse JSON data into a struct."
  (let ((parsed-data (cdr (assoc 'package (json-read-from-string json)))))
    (make-emacsagist-packagist-package
     :name (cdr (assoc 'name parsed-data))
     :description (cdr (assoc 'description parsed-data))
     :maintainers (cdr (assoc 'maintainers parsed-data))
     :versions (emacsagist-parse-package-versions
                (cdr (assoc 'versions parsed-data)))
     :type (cdr (assoc 'type parsed-data))
     :repository (cdr (assoc 'repository parsed-data))
     :downloads (cdr (assoc 'downloads parsed-data))
     :favers (cdr (assoc 'favers parsed-data)))))

(defun emacsagist-make-package-url (package)
  "Generate a url for the PACKAGE."
  (concat emacsagist-packagist-url "/packages/"
          (emacsagist-packagist-package-name package)
          ".json"))

(defun emacsagist-get-packagist-package (package)
  "Get the PACKAGE information from Packagist."
  (save-current-buffer
    (let ((http-buffer (url-retrieve-synchronously
                        (emacsagist-make-package-url package))))
      (switch-to-buffer http-buffer)
      (let ((result (buffer-substring url-http-end-of-headers (point-max))))
        (kill-buffer http-buffer)
        result))))

(defun emacsagist-display-package-version (version)
  "Display single package VERSION."
  (insert (emacsagist-packagist-package-version-version version))
  (newline)
  (insert (concat "\t" (emacsagist-packagist-package-version-name version)))
  (newline)
  (insert (concat "\t" (emacsagist-packagist-package-version-description version)))
  (newline)
  (insert "\t")
  (let ((url (emacsagist-packagist-package-version-homepage version))
        (start (point))
        (map (make-sparse-keymap)))
    (insert url)
    (define-key map (kbd "RET") 'emacsagist-goto-url-property)
    (add-text-properties start (point) `(keymap ,map
                                         face underline
                                         url ,url)))
  (insert " ")
  (newline 2)
  (insert "\tLicense: ")
  (loop for license across
        (emacsagist-packagist-package-version-license version)
        do (insert (concat license " ")))
  (newline)
  (insert "\tAuthors:")
  (newline)
  (loop for author across (emacsagist-packagist-package-version-authors version)
        do
        (insert (concat "\t\t" (cdr (assoc 'name author))
                        " <" (cdr (assoc 'email author)) ">\n")))
  (newline)
  (insert "\tRequire:")
  (newline)
  (loop for require in (reverse
                        (emacsagist-packagist-package-version-require version))
        do
        (insert (concat "\t\t" (symbol-name (car require)) ": " (cdr require)
                        "\n")))
  (newline)
  (insert "\tSuggest:")
  (newline)
  (loop for suggest in (reverse
                        (emacsagist-packagist-package-version-suggest version))
        do
        (insert (concat "\t\t" (symbol-name (car suggest)) ": " (cdr suggest)
                        "\n")))
  (newline 2))

(defun emacsagist-display-package (package-name query page)
  "Display package information for PACKAGE-NAME, with a link to QUERY PAGE."
  (let* ((package (emacsagist-parse-package
                   (emacsagist-get-packagist-package
                    (make-emacsagist-packagist-package :name package-name))))
         (total-downloads
          (cdr (assoc 'total (emacsagist-packagist-package-downloads package))))
         (favorites (emacsagist-packagist-package-favers package)))
    (switch-to-buffer emacsagist-packagist-results-buffer)
    (read-only-mode -1)
    (kill-region (point-min) (point-max))
    (insert
     (concat (emacsagist-packagist-package-name package) " ("
             (number-to-string total-downloads)
             " download" (when (> total-downloads 1) "s") ", "
             (number-to-string favorites)
             " favorite" (when (> favorites 1) "s") ")"))
    (newline 2)
    (let ((map (make-sparse-keymap))
          (start (point)))
      (insert "[Back]")
      (define-key map (kbd "RET") 'emacsagist-back-to-search)
      (add-text-properties start (point) `(keymap ,map
                                           face underline
                                           page ,page
                                           query ,query)))
    (insert " ")
    (newline 2)
    (insert (emacsagist-packagist-package-description package))
    (newline 2)
    (insert (concat "Type: " (emacsagist-packagist-package-type package)))
    (newline)
    (insert (concat "Repository: "
                    (emacsagist-packagist-package-repository package)))
    (newline 2)
    (insert "Maintainers: ")
    (newline)
    (loop for maintainer across
          (emacsagist-packagist-package-maintainers package)
          do
          (insert (concat "\t" (cdr (assoc 'name maintainer))
                          " <" (cdr (assoc 'email maintainer)) ">\n")))
    (newline)
    (insert "Versions:")
    (newline 2)
    (loop for version in (emacsagist-packagist-package-versions package)
          do (emacsagist-display-package-version version))
    (let ((map (make-sparse-keymap))
          (start (point)))
      (insert "[Back]")
      (define-key map (kbd "RET") 'emacsagist-back-to-search)
      (add-text-properties start (point) `(keymap ,map
                                           face underline
                                           page ,page
                                           query ,query)))
    (insert " ")
    (read-only-mode 1)
    (goto-char (point-min))
    (emacsagist-mode)))

(defun emacsagist-back-to-search ()
  "Return the user to the previous search page."
  (interactive)
  (emacsagist-search (get-text-property (point) 'query)
                     (get-text-property (point) 'page)))

(defun emacsagist-make-search-url (search)
  "Generate a url for the SEARCH query."
  (let ((page (emacsagist-packagist-search-page search)))
    (concat emacsagist-packagist-url "/search.json?q="
            (emacsagist-packagist-search-query search)
            (when page (concat "&page=" (number-to-string page))))))

(defun emacsagist-search-packagist (search)
  "Search Packagist for the given SEARCH, returning the resulting JSON."
  (save-current-buffer
    (let ((http-buffer (url-retrieve-synchronously
                        (emacsagist-make-search-url search))))
      (switch-to-buffer http-buffer)
      (let ((result (buffer-substring url-http-end-of-headers (point-max))))
        (kill-buffer http-buffer)
        result))))

(defun emacsagist-parse-search (search results)
  "Parse the SEARCH struct's RESULTS."
  (let ((parsed-results (json-read-from-string results)))
    (setf (emacsagist-packagist-search-results search)
          (loop for result across (cdr (assoc 'results parsed-results))
                collect (make-emacsagist-search-result
                         :name (cdr (assoc 'name result))
                         :description (cdr (assoc 'description result))
                         :url (cdr (assoc 'url result))
                         :downloads (cdr (assoc 'downloads result))
                         :favers (cdr (assoc 'favers result)))))
    (setf (emacsagist-packagist-search-next-page search)
          (when (assoc 'next parsed-results)
            (string-to-number
             (nth 1 (split-string (cdr (assoc 'next parsed-results))
                                  "page=")))))
    search))

(defun emacsagist-add-page-link (start end target-page query)
  "Add link between START and END to view TARGET-PAGE of QUERY search results."
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'emacsagist-display-page)
    (add-text-properties start end `(keymap ,map
                                            face underline
                                            page ,target-page
                                            query ,query))))

(defun emacsagist-display-page-links (search)
  "Display previous/next page links for SEARCH results."
  (let ((query (emacsagist-packagist-search-query search))
        (page (emacsagist-packagist-search-page search))
        (next-page (emacsagist-packagist-search-next-page search)))
    (when (> page 1)
      (let ((start (point)))
        (insert "[Previous Page]")
        (emacsagist-add-page-link start (point) (- page 1) query))
      (when next-page (insert " ")))
    (when next-page
      (let ((start (point)))
        (insert "[Next Page]")
        (emacsagist-add-page-link start (point) next-page query)))
    (insert " ")))

(defun emacsagist-display-header (search)
  "Display a header for the SEARCH results."
  (insert (concat "Packagist results for: "
                  (emacsagist-packagist-search-query search)))
  (newline)
  (insert (concat "Page "
                  (number-to-string (emacsagist-packagist-search-page search))))
  (newline)
  (emacsagist-display-page-links search)
  (newline 2))

(defun emacsagist-goto-url-property ()
  "Open the URL text property in a web browser."
  (interactive)
  (browse-url (get-text-property (point) 'url)))

(defun emacsagist-next-link ()
  "Move cursor to the next link in the buffer."
  (interactive)
  (let* ((next-position (next-single-property-change (point) 'keymap))
         (next-position
          (when next-position
            (if (get-text-property next-position 'keymap)
                next-position
              (unless (eobp)
                (next-single-property-change next-position 'keymap))))))
    (when next-position (goto-char next-position))))

(defun emacsagist-previous-link ()
  "Move cursor to the previous link in the buffer."
  (interactive)
  (let* ((previous-position (previous-single-property-change (point) 'keymap))
         (previous-position
          (when previous-position
            (if (get-text-property previous-position 'keymap)
                previous-position
              (unless (bobp)
                (previous-single-property-change previous-position 'keymap))))))
    (when previous-position (goto-char previous-position))))

(defun emacsagist-goto-package ()
  "Show the package information."
  (interactive)
  (emacsagist-display-package (get-text-property (point) 'name)
                              (get-text-property (point) 'query)
                              (get-text-property (point) 'page)))

(defun emacsagist-display-result (result search)
  "Display the RESULT entry in the SEARCH results list."
  (let ((start (point))
        (map (make-sparse-keymap))
        (name (emacsagist-search-result-name result))
        (query (emacsagist-packagist-search-query search))
        (page (emacsagist-packagist-search-page search)))
    (insert name)
    (define-key map (kbd "RET") 'emacsagist-goto-package)
    (add-text-properties start (point) `(keymap ,map
                                         face underline
                                         name ,name
                                         query ,query
                                         page ,page)))
  (insert (concat  " ("
                  (number-to-string (emacsagist-search-result-downloads result))
                  " download"
                  (when (> (emacsagist-search-result-downloads result) 1) "s")
                  ", "
                  (number-to-string (emacsagist-search-result-favers result))
                  " favorite"
                  (when (> (emacsagist-search-result-favers result) 1) "s")
                  ")"))
  (newline)
  (let ((desc (emacsagist-search-result-description result)))
    (unless (string= desc "")
      (insert desc)
      (newline)))
  (let ((url (emacsagist-search-result-url result))
        (start (point))
        (map (make-sparse-keymap)))
    (insert url)
    (define-key map (kbd "RET") 'emacsagist-goto-url-property)
    (add-text-properties start (point) `(keymap ,map
                                         face underline
                                         url ,url)))
  (insert " ")
  (newline 2))

(defun emacsagist-display-results (search)
  "Display the SEARCH results in a user interface buffer."
  (switch-to-buffer emacsagist-packagist-results-buffer)
  (read-only-mode -1)
  (kill-region (point-min) (point-max))
  (let ((matches (emacsagist-packagist-search-results search)))
    (emacsagist-display-header search)
    (if (= 0 (length matches))
        (insert "No packages found.")
      (dolist (result matches)
        (emacsagist-display-result result search)))
    (emacsagist-display-header search))
  (read-only-mode 1)
  (goto-char (point-min))
  (emacsagist-mode))

(defun emacsagist-display-page ()
  "Display search results page stored in the page text-property."
  (interactive)
  (emacsagist-search (get-text-property (point) 'query)
                     (get-text-property (point) 'page)))

;;;###autoload
(defun emacsagist-search (query &optional page)
  "Search Packagist for QUERY, then display results for PAGE (default 1)."
  (interactive "sSearch Packagist for: ")
  (let* ((page (if (stringp page) (string-to-number page) (or page 1)))
         (search (make-emacsagist-packagist-search :query query :page page)))
    (emacsagist-display-results
     (emacsagist-parse-search search (emacsagist-search-packagist search)))))


(define-derived-mode emacsagist-mode special-mode "Emacsagist"
  "Major mode for the emacsagist results buffer.
\\{emacsagist-mode-map}"
  (auto-fill-mode))

(define-key emacsagist-mode-map (kbd "TAB") 'emacsagist-next-link)
(define-key emacsagist-mode-map [backtab] 'emacsagist-previous-link)
(define-key emacsagist-mode-map "n" 'emacsagist-next-link)
(define-key emacsagist-mode-map "p" 'emacsagist-previous-link)
(define-key emacsagist-mode-map "j" 'emacsagist-next-link)
(define-key emacsagist-mode-map "k" 'emacsagist-previous-link)



(provide 'emacsagist)

;;; emacsagist.el ends here
