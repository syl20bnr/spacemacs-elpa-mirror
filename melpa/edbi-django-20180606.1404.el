;;; edbi-django.el --- Run edbi with django settings

;; Copyright (C) 2014-2018 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/edbi-django
;; Package-Version: 20180606.1404
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (pythonic "0.1.0") (edbi "0.1.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'pythonic)
(require 'json)
(require 'edbi)
(require 'f)

(defvar edbi-django-command "
from __future__ import print_function
from django.conf import settings
from json import dumps

print(dumps(settings.DATABASES))
" "Python code to read django settings.")

(defvar edbi-django-engines
  '(("django.db.backends.postgresql_psycopg2" . "Pg")
    ("django.db.backends.sqlite3" . "SQLite")
    ("django.db.backends.oracle" . "Oracle")
    ("django.db.backends.mysql" . "mysql"))
  "Django to DBI engines mapping.")

(defun edbi-django-completing-read (prompt collection)
  "Ask with PROMPT for COLLECTION element."
  (cond
   ((eq (length collection) 1)
    (car collection))
   ((> (length collection) 1)
    (completing-read prompt collection))))

(defun edbi-django-settings ()
  "Read django settings."
  (let ((json-array-type 'list)
        (json-key-type 'string))
    (json-read-from-string
     (with-output-to-string
       (with-current-buffer
           standard-output
         (pythonic-call-process :buffer standard-output
                                :args (list "-c" edbi-django-command)))))))

(defun edbi-django-add-test-databases (settings)
  "Add test databases to the Django SETTINGS."
  (--mapcat
   (list it (cons (concat "test_" (car it))
                  (--map (if (s-equals-p "NAME" (car it))
                             (cons "NAME" (concat "test_" (cdr it)))
                           it)
                         (cdr it))))
   settings))

(defun edbi-django-databases (settings)
  "Databases list defined in SETTINGS."
  (mapcar 'car settings))

(defun edbi-django-uri (options)
  "Generate DBI connection uri from Django OPTIONS."
  (format "dbi:%s:%s"
          (edbi-django-engine options)
          (s-join "" (list (edbi-django-dbname options)
                           (edbi-django-host options)
                           (edbi-django-port options)))))

(defun edbi-django-engine (options)
  "Get ENGINE from Django OPTIONS."
  (cdr (assoc (cdr (assoc "ENGINE" options)) edbi-django-engines)))

(defun edbi-django-dbname (options)
  "Get NAME from Django OPTIONS."
  (format "dbname=%s;" (cdr (assoc "NAME" options))))

(defun edbi-django-host (options)
  "Get HOST from Django OPTIONS."
  (let* ((db-hostname (cdr (assoc "HOST" options)))
         (db-port (cdr (assoc "PORT" options)))
         (hostname (if (pythonic-remote-docker-p)
                       "127.0.0.1"
                     db-hostname)))
    (when (pythonic-remote-docker-p)
      (let* ((container-raw-description
              (with-output-to-string
                (with-current-buffer
                    standard-output
                  (call-process "docker" nil t nil "inspect"
                                ;; FIXME: Get DB container name from
                                ;; its host name inside internal
                                ;; project network.
                                db-hostname))))
             (container-description
              (let ((json-array-type 'list))
                (json-read-from-string container-raw-description)))
             (container-ip
              (cdr (assoc 'IPAddress
                          (cdadr (assoc 'Networks
                                        (cdr (assoc 'NetworkSettings
                                                    (car container-description))))))))
             (process
              (start-process "edbi-django-socat"
                             "*edbi-django-socat*"
                             "socat"
                             (format "TCP4-LISTEN:%d" db-port)
                             (format "TCP4:%s:%d" container-ip db-port))))
        ;; TODO: Stop this process after EDBI exits.
        (set-process-query-on-exit-flag process nil)))
    (format "host=%s;" hostname)))

(defun edbi-django-port (options)
  "Get PORT from Django OPTIONS."
  (format "port=%s;" (cdr (assoc "PORT" options))))

(defun edbi-django-user (options)
  "Get USER from Django OPTIONS."
  (cdr (assoc "USER" options)))

(defun edbi-django-password (options)
  "Get PASSWORD from Django OPTIONS."
  (cdr (assoc "PASSWORD" options)))

(defun edbi-django-data-source (options)
  "Make `edbi' data source from Django OPTIONS."
  (edbi:data-source (edbi-django-uri options)
                    (edbi-django-user options)
                    (edbi-django-password options)))

;;;###autoload
(defun edbi-django ()
  "Connect to Django databases."
  (interactive)
  (let* ((settings (edbi-django-add-test-databases (edbi-django-settings)))
         (databases (edbi-django-databases settings))
         (database (edbi-django-completing-read "Database: " databases))
         (options (cdr (assoc database settings)))
         (connection (edbi:start))
         (source (edbi-django-data-source options)))
    (edbi:connect connection source)
    (edbi:dbview-open connection)))

(provide 'edbi-django)

;;; edbi-django.el ends here
