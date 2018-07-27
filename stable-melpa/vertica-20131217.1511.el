;;; vertica.el --- Vertica SQL mode extension

;; Copyright (C) 2013 Roman Scherer

;; Author: Roman Scherer <roman@burningswell.com>
;; Version: 0.1.0
;; Package-Version: 20131217.1511
;; Package-Requires: ((sql "3.0"))
;; Keywords: sql vertica

;;; Commentary:

;; This package adds Vertica to the SQL mode product list.

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

(require 'sql)

(defcustom sql-vertica-program "vsql"
  "Command to start the Vertica client."
  :type 'file
  :group 'SQL)

(defcustom sql-vertica-options '()
  "List of additional options for `sql-vertica-program'."
  :type '(repeat string)
  :group 'SQL)

(defcustom sql-vertica-login-params '(user password database server)
  "List of login parameters needed to connect to Vertica."
  :type 'sql-login-params
  :group 'SQL)

(defun sql-comint-vertica (product options)
  "Create comint buffer and connect to Vertica."
  (let ((params options))
    (if (not (string= "" sql-server))
        (setq params (append (list "-h" sql-server) params)))
    (if (not (string= "" sql-database))
        (setq params (append (list "-d" sql-database) params)))
    (if (not (string= "" sql-password))
        (setq params (append (list "-w" sql-password) params)))
    (if (not (string= "" sql-user))
        (setq params (append (list "-U" sql-user) params)))
    (sql-comint product params)))

;;;###autoload
(defun sql-vertica (&optional buffer)
  "Run vsql as an inferior process."
  (interactive "P")
  (sql-product-interactive 'vertica buffer))

(eval-after-load "sql"
  '(sql-add-product
    'vertica "Vertica"
    :sqli-program 'sql-vertica-program
    :sqli-options 'sql-vertica-options
    :sqli-login 'sql-vertica-login-params
    :sqli-comint-func 'sql-comint-vertica
    :prompt-regexp "^\\w*=[#>] "
    :prompt-length 5
    :prompt-cont-regexp "^\\w*[-(][#>] "))

(provide 'vertica)

;;; vertica.el ends here
