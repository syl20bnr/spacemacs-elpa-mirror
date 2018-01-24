;;; relax.el --- For browsing and interacting with CouchDB

;; Copyright (C) 2009 Phil Hagelberg
;;
;; Author: Phil Hagelberg
;; URL: http://github.com/technomancy/relax.el
;; Package-Version: 20131029.1434
;; Version: 0.2
;; Keywords: database http
;; Created: 2009-05-11
;; Package-Requires: ((json "1.2"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Interact with CouchDB databases from within Emacs, with ease!

;; M-x relax to get started. From there hit C-h b.

;;; Installation:

;; Use ELPA. (http://tromey.com/elpa/install.html)

;; If installing by hand for some reason, place on your load path and
;; put this in your init file:

;; (autoload 'relax "relax" "Connect to the CouchDB database at db-url." t)

;; It depends on json.el and js.el. These are available
;; through ELPA or if you need to get them manually download from
;; http://edward.oconnor.cx/elisp/json.el and
;; http://download.savannah.gnu.org/releases-noredirect/espresso/espresso.el

;; Tested with CouchDB 0.9.

;;; TODO:

;; * enforce pagination range
;; * display current page
;; * better error reporting
;; * attachment handling?

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'thingatpt)
(require 'url)
(require 'json)
(require 'js)
(require 'mm-util) ;; for replace-regexp-in-string
(require 'cl)

(defvar relax-host "127.0.0.1") ;; Can't use localhost due to IPv6 sometimes
(defvar relax-port 5984)
(defvar relax-db-path "")

(defvar relax-docs-per-page 25
  "How many documents to show on a given page.")

;;; Utilities

(defun relax-url (&optional id)
  "Return a URL for the given ID using relax- host, port, and db-path."
  ;; remove double slashes that sneak in
  (replace-regexp-in-string "\\([^:]\\)//*" "\\1/"
                            (format "http://%s:%s/%s/%s"
                                    relax-host (number-to-string relax-port)
                                    relax-db-path (or id ""))))

(defun relax-trim-headers ()
  "Remove HTTP headers from the current buffer."
  (goto-char (point-min))
  (search-forward "\n\n")
  (delete-region (point-min) (point)))

(defun relax-json-encode (obj)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-encode obj)))

(defun relax-json-decode (str)
  (let ((json-array-type 'list)
        (json-object-type 'plist))
    (json-read-from-string str)))

(defun relax-load-json-buffer (json-buffer)
  (with-current-buffer json-buffer
    (relax-json-decode
     (buffer-substring (point-min) (point-max)))))

(defun relax-kill-http-buffer ()
  (kill-buffer http-buffer))

(defun relax-kill-document (doc rev &optional callback)
  (let ((url-request-method "DELETE")
        (url (concat (relax-url doc) "?rev=" rev)))
    (url-retrieve url (or callback 'message))))

(defun relax-parse-db-line ()
  "Return the id and rev of the document at point."
  (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
    (unless (string-match "\\[\\(.*\\) @rev \\(.*\\)\\]" line)
      (error "Not on a document line"))
    (list (match-string 1 line) (match-string 2 line))))

(defun relax-db-buffer-name (url)
  (concat "*relax " url "*"))

;;; DB-level

(defvar relax-mode-hook nil)

(defun relax-create-db-menu (&rest ignore)
  (let ((menu (make-sparse-keymap "Databases")))
    (dolist (db (relax-url-completions))
      (define-key-after menu (vector (intern (concat "relax-db-" db))) (cons db `(lambda () (interactive)
                                                                                   (relax ,db)))))
    menu))

(defvar relax-menu-bar
  (let ((menu (make-sparse-keymap "Relax")))
    (define-key-after menu [relax-doc] '("Open doc" . relax-doc))
    (define-key-after menu [relax-new-doc] '("New doc" . relax-new-doc))
    (define-key-after menu [relax-kill-doc] '("Remove doc" . relax-kill-doc-from-db))
    (define-key-after menu [relax-sp1] '("---"))
    (define-key-after menu [relax-refresh] '("Update doclist" . relax-update-db))
    (define-key-after menu [relax-sp2] '("---"))
    (define-key-after menu [relax-prompt-db] '("Open database..." . relax))
    (define-key-after menu [relax-new-db] '("Open new database..." . relax-new-db))
    (define-key-after menu [relax-databases] '(menu-item "Switch to database" t
                                                         :filter relax-create-db-menu))
    (define-key-after menu [relax-delete-db] '("Delete database..." . relax-delete-db))
    menu))

(defvar relax-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'relax-doc)
    (define-key map (kbd "C-o") 'relax-new-doc)
    (define-key map (kbd "g") 'relax-update-db)

    (define-key map (kbd "SPC") 'scroll-up)
    (define-key map (kbd "<backspace>") 'scroll-down)
    (define-key map "q" 'quit-window)
    (define-key map (kbd "C-k") 'relax-kill-doc-from-db)
    (define-key map "[" 'relax-prev-page)
    (define-key map "]" 'relax-next-page)

    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [menu-bar relax] (cons "Relax" relax-menu-bar))

    map))

(defun relax-url-completions ()
  "A list of all DB URLs at the server given by relax-host:relax-port."
  (mapcar (lambda (db-name) (let ((relax-db-path ""))
                         (relax-url db-name)))
          (with-current-buffer (url-retrieve-synchronously
                                (let ((relax-db-path ""))
                                  (relax-url "_all_dbs")))
            (relax-trim-headers)
            (relax-json-decode (buffer-substring (point-min) (point-max))))))

;;;###autoload
(defun relax (db-url)
  "Connect to the CouchDB database at DB-URL."
  (interactive (list (completing-read "CouchDB URL: " (relax-url-completions)
                                      nil nil (relax-url))))
  (let ((url (url-generic-parse-url db-url)))
    (setq relax-host (url-host url)
          relax-port (url-port url)
          relax-db-path (url-filename url)))
  (if (not (get-buffer (relax-db-buffer-name db-url)))
      (url-retrieve (relax-url (format "_all_docs?descending=false&limit=%s"
                                       relax-docs-per-page))
                    'relax-mode (list db-url))
    ;; buffer has been initialized; needs refresh
    (switch-to-buffer (relax-db-buffer-name db-url))
    (relax-update-db)))

(defun relax-mode (status database-url)
  "Major mode for interacting with CouchDB databases."
  (let ((json-buffer (current-buffer)))
    (relax-trim-headers)
    (switch-to-buffer (relax-db-buffer-name database-url))
    (buffer-disable-undo)
    (kill-all-local-variables)

    (set (make-local-variable 'http-buffer) json-buffer)
    (set (make-local-variable 'kill-buffer-hook) '(relax-kill-http-buffer))
    (set (make-local-variable 'db-url) database-url)
    (set (make-local-variable 'doc-list)
         (relax-load-json-buffer json-buffer)))

  (use-local-map relax-mode-map)
  (setq mode-name "relax")
  (setq major-mode 'relax-mode)

  (save-excursion
    (insert "== " db-url "\n")
    (insert (format "Total documents: %s\n\n"
                    (getf doc-list :total_rows)))
    (relax-insert-doc-list (getf doc-list :rows)))
  (setq buffer-read-only t)

  (run-hooks 'relax-mode-hook))

(defun relax-insert-doc-list (docs)
  "Given a list of documents, insert them into the buffer sorted by key."
  (dolist (doc (sort* docs (lambda (doc1 doc2)
                             (string< (getf doc1 :id) (getf doc2 :id)))))
    ;; If this changes, change relax-parse-db-line to match.
    (insert (format "  [%s @rev %s]\n" (getf doc :id)
                                       (getf (getf doc :value) :rev)))))

(defun relax-new-db (url)
  "Create a new database at URL."
  (interactive (list (completing-read "CouchDB URL: " (relax-url-completions)
                                      nil nil (relax-url))))
  (when url
    (message url)
    (let ((url-request-method "PUT"))
      (url-retrieve url (lambda (status url)
                          (if status
                              (message (format "%S" status))
                            (relax url))) (list url)))))

(defun relax-delete-db (url)
  "Delete database at URL."
  (interactive (list (completing-read "CouchDB URL: " (relax-url-completions)
                                      nil nil (relax-url))))
  (when url
    (message url)
    (let ((url-request-method "DELETE"))
      (url-retrieve url (lambda (status url)
                          (if status
                              (message (format "%S" status))
                            (message "Ok"))) (list url)))))

(defun relax-new-doc (choose-id)
  "Create a new document. With prefix arg CHOOSE-ID, prompt for a document ID."
  (interactive "P")
  (let ((url-request-method (if choose-id "PUT" "POST"))
        (url-request-extra-headers
         `(("content-type" . "application/json")))
        (url-request-data "{}")
        (id (if choose-id (read-from-minibuffer "Document ID: "))))
    (url-retrieve (relax-url id) 'relax-visit-new-doc)))

(defun relax-visit-new-doc (status)
  "Open a buffer for a newly-created document. Used as a callback."
  (goto-char (point-min))
  (search-forward "Location: ")
  (let ((doc-url (buffer-substring (point) (progn (end-of-line) (point)))))
    (url-retrieve doc-url 'relax-doc-load (list doc-url))))

(defun relax-update-db (&optional docs)
  "Update the DB buffer with the current list of DOCS."
  (interactive)
  ;; TODO: remain on the same page
  (setq buffer-read-only nil)
  (delete-region (point-min) (point-max))
  (url-retrieve (relax-url (or docs (format "_all_docs?descending=false&limit=%s"
                                            relax-docs-per-page)))
                'relax-mode (list db-url)))

(defun relax-kill-doc-from-db ()
  "Issue a delete for the document under point."
  (interactive)
  (let* ((line (relax-parse-db-line))
         (id (car line))
         (rev (cadr line)))
    (lexical-let ((db-buffer (current-buffer)))
      (relax-kill-document id rev (lambda (status)
                                    (switch-to-buffer db-buffer)
                                    (relax-update-db))))))

(defun relax-next-page ()
  "Go forward a page.  With prefix arg, go back n pages.  See `relax-docs-per-page'."
  (interactive)
  ;; TODO: display pagination in DB buffer, prevent paging out of range
  (relax-update-db (format "_all_docs?descending=false&limit=%s&startkey=\"%s\"&skip=1"
                           relax-docs-per-page
                           (save-excursion
                             (goto-char (point-max))
                             (forward-line -1)
                             (car (relax-parse-db-line))))))

(defun relax-prev-page ()
  "Go back a page.  With prefix arg, go back n pages.  See `relax-docs-per-page'."
  (interactive)
  (relax-update-db (format "_all_docs?descending=true&limit=%s&startkey=\"%s\"&skip=1"
                           relax-docs-per-page
                           (save-excursion
                             (goto-char (point-min))
                             (forward-line 3)
                             (car (relax-parse-db-line))))))

;;; Document-level

(defvar relax-doc-mode-hook nil)

(defvar relax-doc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x C-s") 'relax-submit)
    (define-key map (kbd "C-c C-u") 'relax-update-doc)
    (define-key map (kbd "C-c C-k") 'relax-kill-doc)
    map))

(defun relax-doc-load (status document-url)
  "Create and switch to a buffer for a newly-retrieved document."
  (let ((json-buffer (current-buffer)))
    (relax-trim-headers)
    (let ((doc-string (buffer-substring-no-properties (point-min) (point-max))))
      (switch-to-buffer (concat "*relax " document-url "*"))

      (js-mode)
      (relax-doc-mode t)
      (set (make-local-variable 'http-buffer) json-buffer)
      (set (make-local-variable 'kill-buffer-hook) '(relax-kill-http-buffer))
      (set (make-local-variable 'doc-url) document-url)
      (set (make-local-variable 'doc)
           (relax-load-json-buffer json-buffer))
      (insert doc-string)))

  (save-excursion ;; prettify
    (goto-char (point-min))
    (replace-string "\",\"" "\",\n\"")
    (indent-region (point-min) (point-max))
    (font-lock-fontify-buffer))
  (message "Loaded %s" doc-url))

(define-minor-mode relax-doc-mode
  "Minor mode for interacting with CouchDB documents."
  nil
  " relax-doc")

(defun relax-doc ()
  "Open a buffer viewing the document at point."
  (interactive)
  (let ((doc-url (relax-url (car (relax-parse-db-line)))))
    (url-retrieve doc-url 'relax-doc-load (list doc-url))))

(defun relax-submit ()
  "Save the contents of the buffer to the server.  Enforces valid JSON."
  (interactive)
  ;; Verify valid JSON before submitting.
  (relax-json-decode (buffer-substring (point-min) (point-max)))
  (let ((url-request-method "PUT")
        (url-request-data (buffer-substring (point-max) (point-min))))
    (lexical-let ((doc-buffer (current-buffer)))
      (url-retrieve doc-url (lambda (status)
                              (switch-to-buffer doc-buffer)
                              (relax-update-doc))))))

(defun relax-update-doc ()
  "Update the current buffer with the latest version of the document."
  (interactive)
  (delete-region (point-min) (point-max))
  (url-retrieve doc-url 'relax-doc-load (list doc-url)))

(defun relax-kill-doc ()
  "Delete this revision of the current document from the server."
  (interactive)
  (lexical-let ((target-buffer (current-buffer)))
    (relax-kill-document (getf doc :_id) (getf doc :_rev)
                         (lambda (status)
                           (kill-buffer target-buffer)
                           (relax-update-db)))))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

(provide 'relax)
;;; relax.el ends here
