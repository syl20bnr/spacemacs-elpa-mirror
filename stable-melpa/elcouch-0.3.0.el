;;; elcouch.el --- View and manipulate CouchDB databases  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Url: https://gitlab.petton.fr/DamienCassou/elcouch
;; Package-Version: 0.3.0
;; Package-requires: ((emacs "25.1") (json-mode "1.0.0") (libelcouch "0.8.0"))
;; Version: 0.3.0
;; Keywords: data, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; View and manipulate CouchDB databases.

;;; Code:

(require 'tabulated-list)
(require 'json-mode)

(require 'libelcouch)


;;; Customization

(defgroup elcouch nil
  "View and manipulate CouchDB databases."
  :group 'externa)


;;; Variables

(defvar-local elcouch-entity nil
  "Remember the CouchDB entity of current buffer.")


;;; Helper code

(cl-defgeneric elcouch--entity-buffer-name (entity)
  "Return a buffer name approapriate for listing the content of ENTITY.")

(cl-defmethod elcouch--entity-buffer-name ((instance libelcouch-instance))
  (format "*elcouch-dbs: %s" (libelcouch-entity-full-name instance)))

(cl-defmethod elcouch--entity-buffer-name ((database libelcouch-database))
  (format "*elcouch-docs: %s" (libelcouch-entity-full-name database)))

(cl-defmethod elcouch--entity-buffer-name ((document libelcouch-document))
  (format "*elcouch-doc: %s" (libelcouch-entity-full-name document)))

(cl-defgeneric elcouch--default-tabulated-list-format (_entity)
  "Return `tabulated-list-format' value to list children of _ENTITY."
  (vector (list "Name" 0 t)))

(cl-defgeneric elcouch--entity-to-list-entry ((entity libelcouch-named-entity))
  "Convert ENTITY to a format suitable for the tabulated list."
  (list entity
        (vector (libelcouch-entity-name entity))))


;;; Entity listing code

(defvar elcouch-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'elcouch-open)
    map)
  "Keybindings for `elcouch-list-mode'.")

(define-derived-mode elcouch-list-mode tabulated-list-mode "elcouch"
  "Major mode for all elcouch listing modes.")

;;;###autoload
(defun elcouch-open (entity)
  "Open a new buffer showing CouchDB ENTITY.

Interactively, ENTITY is either the element at point or the user
is asked for an INSTANCE among `elcouch-couchdb-instances'."
  (interactive (list (let ((entity (tabulated-list-get-id)))
                       (if (and entity (libelcouch-named-entity-p entity))
                           entity
                         (libelcouch-choose-instance)))))
  (if (libelcouch-document-p entity)
      (elcouch-view-document entity)
    (elcouch-list entity)))

;;;###autoload
(defun elcouch-open-url (url)
  "Open entity pointed to by URL, a string."
  (interactive (list (read-from-minibuffer "URL: ")))
  (elcouch-open (libelcouch-entity-from-url url)))

(defun elcouch-list (entity)
  "Open a buffer showing children of ENTITY."
  (libelcouch-entity-list
   entity
   (lambda (children)
     (with-current-buffer (get-buffer-create (elcouch--entity-buffer-name entity))
       (elcouch-list-mode)
       (setq tabulated-list-format (elcouch--default-tabulated-list-format entity))
       (tabulated-list-init-header)
       (setq-local elcouch-entity entity)
       (setq tabulated-list-entries (mapcar #'elcouch--entity-to-list-entry children))
       (tabulated-list-print)
       (switch-to-buffer (current-buffer))))))


;;; Document view mode

(defvar elcouch-document-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'elcouch-document-save)
    map)
  "Keybindings for `elcouch-document-view-mode'.")

(define-derived-mode elcouch-document-view-mode json-mode "elcouch document"
  "Major mode to view and edit a CouchDB document.")

(defun elcouch--document-prepare-buffer (content)
  "Insert json CONTENT into current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert content)
    (json-pretty-print-buffer))
  ;; Give more keybindings to buffer navigation:
  (setq buffer-read-only t)
  (goto-char (point-min))
  (font-lock-ensure))

(defun elcouch-view-document (document &optional saved-point saved-mark)
  "Display a CouchDB DOCUMENT in a JSON read-write buffer.
If SAVED-POINT and/or SAVED-MARK are provided, the point and/or
mark are changed to those ones."
  (interactive (list (tabulated-list-get-id)))
  (libelcouch-document-content
   document
   (lambda (json-document)
     (with-current-buffer (get-buffer-create (elcouch--entity-buffer-name document))
       (elcouch-document-view-mode)
       (setq-local elcouch-entity document)
       (elcouch--document-prepare-buffer json-document)
       (switch-to-buffer (current-buffer))
       (when saved-point
         (goto-char saved-point))
       (when saved-mark
         (save-mark-and-excursion--restore saved-mark))))))

(defun elcouch-document-refresh (&optional buffer)
  "Refresh BUFFER with new document content.
Use current buffer if BUFFER is nil."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (let ((saved-point (point))
          (saved-mark (save-mark-and-excursion--save)))
      (elcouch-view-document elcouch-entity saved-point saved-mark))))

(defun elcouch-document-save ()
  "Save buffer's document to CouchDB."
  (interactive)
  (libelcouch-document-save elcouch-entity nil #'elcouch-document-refresh))

(defun elcouch-document-delete (document)
  "Delete the CouchDB DOCUMENT."
  (interactive (list elcouch-entity))
  (when (yes-or-no-p (format "Really delete %s? " (libelcouch-entity-full-name document)))
    (let* ((json-object (save-excursion
                          (goto-char (point-min))
                          (json-read)))
           (revision (map-elt json-object '_rev)))
      (libelcouch-document-delete
       document
       revision
       (lambda () (elcouch-list (libelcouch-entity-parent document)))))))

(provide 'elcouch)
;;; elcouch.el ends here
