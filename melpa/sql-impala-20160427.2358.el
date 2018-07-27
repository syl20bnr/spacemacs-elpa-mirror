;;; sql-impala.el --- comint support for Cloudera Impala -*- lexical-binding: t -*-

;; Copyright 2015-2016 Jason Terk <jason@goterkyourself.com>

;; Author: Jason Terk <jason@goterkyourself.com>
;; Version: 1.0
;; Package-Version: 20160427.2358
;; Keywords: sql, impala
;; URL: https://github.com/jterk/sql-impala

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds comint support for the 'impala-shell' interface to Cloudera
;; Impala.  Depends on an installed and functional 'impala-shell'.  Installation
;; of 'impala-shell' is out of scope of this package.

;;; Code:
(require 'sql)

(defcustom sql-impala-program "impala-shell"
  "Command to start the Cloudera Impala command interpreter."
  :type 'file
  :group 'SQL)

(defcustom sql-impala-login-params '(server database)
  "Parameters needed to connect to Cloudera Impala."
  :type 'sql-login-params
  :group 'SQL)

(defcustom sql-impala-options '("--quiet")
  "List of additional options for `sql-impala-program'."
  :type '(repeat string)
  :group 'SQL)

(defun sql-comint-impala (product options)
  "Connect to Cloudera Impala in a comint buffer.

PRODUCT is the sql product (impala).  OPTIONS are any additional
options to pass to impala-sehll."
  (let ((params
         (append
          (if (not (string= "" sql-server))
              (list "-i" sql-server))
          (if (not (string= "" sql-database))
              (list "-d" sql-database))
          options)))
    (sql-comint product params)))

(defun sql-impala (&optional buffer)
  "Run Cloudera Impala as an inferior process.

The buffer with name BUFFER will be used or created."
  (interactive "P")
  (sql-product-interactive 'impala buffer))

(sql-add-product 'impala "Cloudera Impala"
                 :prompt-regexp "^[^>]*> "
                 :sqli-comint-func 'sql-comint-impala
                 :sqli-login sql-impala-login-params
                 :sqli-program 'sql-impala-program
                 :sqli-options 'sql-impala-options)

(provide 'sql-impala)
;;; sql-impala.el ends here
