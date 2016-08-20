;;; sql-mssql.el --- Connect Microsoft SQL Server with sql.el in Linux/Unix environment

;; * Header
;; Copyright (c) 2015, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/sql-mssql
;; Package-Version: 20160512.137
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; * README                                                             :README:
;; ** Introduce
;; sql-mssql is a sql.el extension, which can let Linux/Unix user connect
;; Microsoft SQL Server with the help of sqsh, this extension may useless
;; for Windows user.

;; ** Install
;; 1. Install sqsh, unixODBC and freetds (For example: apt-get install sqsh unixodbc freetds)
;; 2. Install sql-mssql (You can config Melpa and install it with `package-install' command)

;; ** Configure
;; 1. Config unixODBC, you can view the below documents as reference
;;    1. http://richbs.org/post/43142767072/connecting-to-microsoft-sql-server-from-unix
;;    2. http://askubuntu.com/questions/167491/connecting-ms-sql-using-freetds-and-unixodbc-isql-no-default-driver-specified
;;    3. http://help.interfaceware.com/kb/904
;;    4. Google: unixodbc mssql
;; 2. Config sql.el
;;    #+BEGIN_EXAMPLE
;;    (require 'sql)
;;    (require 'sql-mssql)
;;    #+END_EXAMPLE

;; ** Usage
;; 1. Method1: M-x sql-mssql
;; 2. Method2: M-x sql-mssql-connect (you must set `sql-connection-alist' before)

;;; Code:
;; * Code                                                               :code:
;; #+BEGIN_SRC emacs-lisp
;; require
(require 'sql)

(defcustom sql-mssql-program "sqsh"
  "Use sqsh to connect Microsoft SQL Server.

sqsh is isql's enhanced edition，which can connect to sybase server
or Microsoft SQL Server with the following command:

   sqsh -s SERVER -U USER -P PASSWORD -D DB -L bcp_colsep=','

When login succes, you can see the prompt like below:

   1> select * from dbo.tzd_brly
   1> go

Starts `sql-interactive-mode' after doing some setup."
  :type 'file
  :group 'SQL)

(defcustom sql-mssql-options '("-w" "80" "-m" "pretty")
  "List of additional options for `sql-mssql-program'."
  :type '(repeat string)
  :version "20.8"
  :group 'SQL)

(defcustom sql-mssql-login-params '(server user password database)
  "List of login parameters needed to connect to Microsoft SQL Server
by sqsh."
  :type 'sql-login-params
  :version "24.1"
  :group 'SQL)

(defalias 'sql-mssql-comint 'sql-comint-sybase
  "sqsh is compatible with isql, so we can let it work with `sql-sybase'，
defalias it as `sql-mssql-comint' for maintain reason.")

(push '(mssql
        :name "mssql"
        :font-lock sql-mode-ms-font-lock-keywords
        :sqli-program sql-mssql-program
        :sqli-options sql-mssql-options
        :sqli-login sql-mssql-login-params
        :sqli-comint-func sql-mssql-comint
        ;; sqsh's prompt is like "1>" or "2>"，so we need to redefine `:prompt-regexp'.
        ;; PLEASE NOTE： Don't delete the *space* at the end of regexp， or you will
        ;; get an empty prompt :-), More details can be found at:
        ;; `sql-interactive-remove-continuation-prompt'
        :prompt-regexp "^[0-9]+> "
        :prompt-length 5
        :syntax-alist ((?@ . "_"))
        :terminator ("^go" . "go"))
      sql-product-alist)

(defun sql-mssql (&optional buffer)
  "Run sqsh as an inferior process to connect Microsoft SQL Server.

If buffer `*SQL*' exists but no process is running, make a new process.
If buffer exists and a process is running, just switch to buffer
`*SQL*'.

Interpreter used comes from variable `sql-mssql-program'.  Login uses the
variables `sql-user', `sql-password', `sql-database', and `sql-server'
as defaults, if set.  Additional command line parameters can be stored
in the list `sql-mssql-options'.

The buffer is put in SQL interactive mode, giving commands for sending
input.  See `sql-interactive-mode'.

To set the buffer name directly, use \\[universal-argument]
before \\[sql-mssql].  Once session has started,
\\[sql-rename-buffer] can be called separately to rename the
buffer.

To specify a coding system for converting non-ASCII characters
in the input and output to the process, use \\[universal-coding-system-argument]
before \\[sql-mssql].  You can also specify this with \\[set-buffer-process-coding-system]
in the SQL buffer, after you start the process.
The default comes from `process-coding-system-alist' and
`default-process-coding-system"
  (interactive "P")
  (sql-product-interactive 'mssql buffer))

(defun sql-mssql--pretty-output (orig-fun oline)
  "Sqsh pretty table is too complicate and with too much split line,
simplify it."
  (let ((output (funcall orig-fun oline)))
    (if (and (boundp 'sql-mssql-program)
             (stringp sql-mssql-program)
             (string-match-p "sqsh$" sql-mssql-program))
        (sql-mssql--clean-sqsh-pretty-output output)
      output)))

(defun sql-mssql--clean-sqsh-pretty-output (output)
  "Internal clean function used by `sql-mssql--pretty-output'."
  (with-temp-buffer
    (insert output)
    ;; Delete pretty table's horizontal line
    (goto-char (point-min))
    (while (re-search-forward "^+[-+]+\n|" nil t)
      (replace-match "|" nil t))
    ;; Pretty table's head, use "-" as sep char instead of "=".
    (goto-char (point-min))
    (while (re-search-forward "=" nil t)
      (replace-match "-" nil t))
    ;; Sqsh's table can align well when it include Chinese, hack it by
    ;; add space.
    (goto-char (point-min))
    (while (re-search-forward "|[^|]+" nil t)
      (let* ((str (match-string 0))
             (char-list (string-to-list str))
             (count 0))
        (dolist (x char-list)
          (when (string-match-p "\\cc" (char-to-string x))
            (setq count (+ 1 count))))
        (replace-match
         (concat str (make-string count ?\ )))))
    (buffer-string)))

(advice-add 'sql-interactive-remove-continuation-prompt'
            :around #'sql-mssql--pretty-output)

(defun sql-mssql-cache-dabbrev ()
  (interactive)
  "Generate column-names and table-names buffers. by which user can
use `company-dabbrev' or `auto-complete' to complete column names or
 table names when code, it is poor man's sql complete :-)."
  (when (null sql-buffer)
    (error "No SQLi buffer available"))
  (let ((str (replace-regexp-in-string
              "[ *]" "" sql-buffer)))
    ;; Create a buffer which contain all column-names.
    (sql-redirect
     sql-buffer
     '("select distinct COLUMN_NAME from information_schema.columns" "go")
     (concat "sql-mssql-column-list-" str))

    ;; Create a buffer which contain all table-names.
    (sql-redirect
     sql-buffer
     '("select distinct TABLE_NAME from information_schema.columns" "go")
     (concat "sql-mssql-table-list-" str))

    ;; Create a buffer which contain table-names which include schema.
    (sql-redirect
     sql-buffer
     '("select distinct TABLE_SCHEMA + '.' + TABLE_NAME from information_schema.columns" "go")
     (concat "sql-mssql-schema-table-list-" str))))

(defun sql-mssql-connect ()
  "Connect a mssql database in `sql-connection-alist'
with `sql-connect', user should set `sql-connection-alist'
before run this command."
  (interactive)
  (setq sql-product 'mssql)
  (let ((connect-name
         (completing-read "Which database do you want to connect: "
                          (mapcar #'(lambda (x)
                                      (symbol-name (car x)))
                                  sql-connection-alist))))
    (sql-connect (intern connect-name))
    (message "Cache all table-names and column-names...")
    (sit-for 3)
    (sql-mssql-cache-dabbrev)
    (message "Table-names and column-names cache finished!")))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'sql-mssql)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; sql-mssql.el ends here
;; #+END_SRC
