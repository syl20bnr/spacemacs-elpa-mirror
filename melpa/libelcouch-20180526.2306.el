;;; libelcouch.el --- Communication with CouchDB  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Damien Cassou

;; Author: Damien Cassou <damien@cassou.me>
;; Keywords: tools
;; Package-Version: 20180526.2306
;; Url: https://gitlab.petton.fr/elcouch/libelcouch/
;; Package-requires: ((emacs "25.1") (request "0.3.0"))
;; Version: 0.6.0

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

;; The package libelcouch is an Emacs library client to communicate with
;; CouchDB (https://couchdb.apache.org/), a database focusing on ease of
;; use and having a scalable architecture.  For a user interface, please
;; check the elcouch project instead (which depends on this one).

;;; Code:
(require 'cl-lib)
(require 'request)
(require 'json)
(require 'map)


;;; Customization

(defgroup libelcouch nil
  "View and manipulate CouchDB databases."
  :group 'external)

(defcustom libelcouch-couchdb-instances nil
  "List of CouchDB instances."
  :type 'list)

(defcustom libelcouch-timeout 10
  "Timeout in seconds for calls to the CouchDB instance.
Number of seconds before a call to CouchDB without answer is
considered to have failed."
  :type 'number)


;;; Structures

(cl-defstruct (libelcouch-named-entity
               (:constructor libelcouch--named-entity-create)
               (:conc-name libelcouch--named-entity-))
  (name nil :read-only t))

(cl-defstruct (libelcouch-instance
               (:include libelcouch-named-entity)
               (:constructor libelcouch--instance-create)
               (:conc-name libelcouch--instance-))
  (url nil :read-only t))

(cl-defstruct (libelcouch-database
               (:include libelcouch-named-entity)
               (:constructor libelcouch--database-create)
               (:conc-name libelcouch--database-))
  (instance nil :read-only t))

(cl-defstruct (libelcouch-document
               (:include libelcouch-named-entity)
               (:constructor libelcouch--document-create)
               (:conc-name libelcouch--document-))
  (database nil :read-only t))


;;; Accessors

(cl-defgeneric libelcouch-entity-name ((entity libelcouch-named-entity))
  "Return the name of ENTITY."
  (libelcouch--named-entity-name entity))

(cl-defgeneric libelcouch-entity-full-name ((entity libelcouch-named-entity))
  "Return the full name of ENTITY's parent followed by ENTITY name."
  (format "%s/%s"
          (libelcouch-entity-name (libelcouch-entity-parent entity))
          (libelcouch-entity-name entity)))

(cl-defmethod libelcouch-entity-full-name ((entity libelcouch-instance))
  (libelcouch-entity-name entity))

(cl-defgeneric libelcouch-entity-parent (entity)
  "Return the entity containing ENTITY.")

(cl-defmethod libelcouch-entity-parent ((database libelcouch-database))
  (libelcouch--database-instance database))

(cl-defmethod libelcouch-entity-parent ((document libelcouch-document))
  (libelcouch--document-database document))

(cl-defgeneric libelcouch-entity-instance (entity)
  "Return the CouchDB instance of ENTITY.")

(cl-defmethod libelcouch-entity-instance ((instance libelcouch-instance))
  instance)

(cl-defmethod libelcouch-entity-instance ((database libelcouch-database))
  (libelcouch--database-instance database))

(cl-defmethod libelcouch-entity-instance ((document libelcouch-document))
  (libelcouch-entity-instance (libelcouch--document-database document)))

(cl-defgeneric libelcouch-entity-url (entity)
  "Return the url of ENTITY."
  (format "%s/%s"
          (libelcouch-entity-url (libelcouch-entity-parent entity))
          (libelcouch-entity-name entity)))

(cl-defmethod libelcouch-entity-url ((instance libelcouch-instance))
  (libelcouch--instance-url instance))


;;; Private helpers

(cl-defgeneric libelcouch--entity-create-children-from-json (entity json)
  "Create and return children of ENTITY from a JSON object.")

(cl-defmethod libelcouch--entity-create-children-from-json ((instance libelcouch-instance) json)
  (mapcar
   (lambda (database-name) (libelcouch--database-create :name database-name :instance instance))
   json))

(cl-defmethod libelcouch--entity-create-children-from-json ((database libelcouch-database) json)
  (let ((documents-json (map-elt json 'rows)))
    (mapcar
     (lambda (document-json)
       (libelcouch--document-create
        :name (map-elt document-json 'id)
        :database database))
     documents-json)))

(cl-defgeneric libelcouch--entity-children-url (entity)
  "Return the path to query all children of ENTITY.")

(cl-defmethod libelcouch--entity-children-url ((instance libelcouch-instance))
  (format "%s/%s" (libelcouch-entity-url instance) "_all_dbs"))

(cl-defmethod libelcouch--entity-children-url ((database libelcouch-database))
  (format "%s/%s" (libelcouch-entity-url database) "_all_docs"))

(cl-defun libelcouch--request-error (&rest args &key error-thrown &allow-other-keys)
  "Report an error when communication with an instance fails."
  (message "Got error: %S" error-thrown))


;;; Navigating

(defun libelcouch-instances ()
  "Return a list of couchdb instances built from `libelcouch-couchdb-instances'."
  (mapcar
   (lambda (instance-data)
     (libelcouch--instance-create
      :name (car instance-data)
      :url (cadr instance-data)))
   libelcouch-couchdb-instances))

(cl-defgeneric libelcouch-entity-list (entity function)
  "Evaluate function with the children of ENTITY as parameter."
  (request
   (url-encode-url (libelcouch--entity-children-url entity))
   :timeout libelcouch-timeout
   :headers '(("Content-Type" . "application/json")
              ("Accept" . "application/json"))
   :parser 'json-read
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (let* ((children (libelcouch--entity-create-children-from-json entity data)))
                 (funcall function children))))
   :error #'libelcouch--request-error)
  nil)

(defun libelcouch-document-content (document function)
  "Evaluate FUNCTION with the content of DOCUMENT as parameter."
  (request
   (url-encode-url (libelcouch-entity-url document))
   :timeout libelcouch-timeout
   :parser (lambda () (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8))
   :headers '(("Accept" . "application/json"))
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (funcall function data)))
   :error #'libelcouch--request-error)
  nil)

(defun libelcouch-document-save (document content function)
  "Evaluate FUNCTION when CONTENT is saved as new value for DOCUMENT."
  (request
   (url-encode-url (libelcouch-entity-url document))
   :type "PUT"
   :headers '(("Content-Type" . "application/json"))
   :data (or content (encode-coding-string (buffer-substring-no-properties (point-min) (point-max)) 'utf-8))
   :success (cl-function (lambda (&rest _args) (funcall function)))
   :error #'libelcouch--request-error)
  nil)

(provide 'libelcouch)
;;; libelcouch.el ends here
