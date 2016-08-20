;;; mysql2sqlite.el --- Convert mysql databases into sqlite databases.
;; Package-Version: 20151123.1339

;;; Commentary:

;; mysql2sqlite is an Emacs package for converting a mysql database into an sqlite
;; database.

;; Installation:

;; The package can be installed with M-x package-list-packages (requires
;; package.el, which is included for Emacs 24 but availble for 23). The MELPA
;; repository must be added to your packages setup first.  Instructions can be found
;; at https://melpa.org/

;; Alternatively, the source can be pulled direclty from github:
;; https://github.com/echosa/emacs-mysql2sqlite

;; Customize

;; There are some customizable options:

;; mysql2sqlite-sqlite-executable    - The sqlite executable to run.
;; mysql2sqlite-mysqldump-executable - The mysqldump executable to run.
;; mysql2sqlite-mysqldump-host       - The default host to connect to with mysqldump.
;; mysql2sqlite-mysqldump-user       - The default user to use to connect to mysqldump.
;; mysql2sqlite-mysqldump-database   - The default database to convert

;; Usage

;; Usage is as simple as running M-x mysql2sqlite.  You will be prompted for the
;; necessary values, with the customized defaults as default values.  Running the
;; function will result in several files in the target directory:

;; <output-file>.sql - The actual SQL file generated via mysqldump.
;; <output-file>.db  - The generated sqlite database.
;; <output-file>.err - The output of the conversion process.

;; mysql2sqlite is written and maintained by Brian Zwahr <echosa@gmail.com>

;;; Code:

(require 'thingatpt)

(defgroup mysql2sqlite nil
  "Customizations for mysql2sqlite."
  :group 'external)

(defcustom mysql2sqlite-sqlite-executable "sqlite"
  "Sqlite executable to run."
  :group 'mysql2sqlite
  :type 'string)

(defcustom mysql2sqlite-mysqldump-executable "mysqldump"
  "Mysqldump executable to run."
  :group 'mysql2sqlite
  :type 'string)

(defcustom mysql2sqlite-mysqldump-host "localhost"
  "Mysqldump default host."
  :group 'mysql2sqlite
  :type 'string)

(defcustom mysql2sqlite-mysqldump-user ""
  "Mysqldump default user."
  :group 'mysql2sqlite
  :type 'string)

(defcustom mysql2sqlite-mysqldump-database ""
  "Mysqldump default database."
  :group 'mysql2sqlite
  :type 'string)

(defun mysql2sqlite (directory)
  "This function convert a mysql dump file into an sqlite database.
The dump file should be generated with
mysqldump -u root -p --compatible=ansi --skip-opt DATABASE > DUMPFILE

Several files will be generated:
- DUMPFILE.sql is the DUMPFILE after full conversion
- DUMPFILE.db is the generated sqlite database
- DUMPFILE.err is the output of the conversion
Argument DIRECTORY Directory where all output will be generated."
  (interactive "DTarget directory: ")
  (let* ((keep-inserts (y-or-n-p "Keep INSERT statements? "))
         (dumpfile (mysql2sqlite-get-mysqldump directory keep-inserts)))
    (when dumpfile
      (find-file dumpfile)
      (goto-char (point-min))
      (mysql2sqlite-cleanup-buffer)
      (mysql2sqlite-remove-comments)
      
      ;; add blank line at the top to keep search-forward from infinite loop
      (goto-char (point-min))
      (newline)
      (goto-char (point-min))

      (mysql2sqlite-convert-primary-keys)
      (mysql2sqlite-convert-constraints)
      (mysql2sqlite-remove-extra-keys)
      (mysql2sqlite-convert-types)
      (mysql2sqlite-delete-sets)
      (mysql2sqlite-convert-newlines)
      (mysql2sqlite-convert-quotes)
      (mysql2sqlite-remove-trailing-commas)
      (mysql2sqlite-convert-inserts)
      (unless keep-inserts
        (mysql2sqlite-move-inserts-to-end))
      (mysql2sqlite-wrap-in-transaction)
      (mysql2sqlite-cleanup-buffer)

      (mysql2sqlite-save-and-convert dumpfile))))

(defun mysql2sqlite-get-mysqldump (directory &optional keep-inserts)
  "Retrieves a dump from mysql.
Argument DIRECTORY Directory where all output will be generated.
Optional argument KEEP-INSERTS Whether or not data should be included in the dump."
  (let ((host (read-string "Host: " mysql2sqlite-mysqldump-host))
        (database (read-string "Database: " mysql2sqlite-mysqldump-database))
        (user (read-string "User: " mysql2sqlite-mysqldump-user))
        (password (read-passwd "Password: ")))
    (when (and (not (equal host ""))
               (not (equal database ""))
               (not (equal user ""))
               (not (equal password "")))
      (let* ((dumpfile (convert-standard-filename
                        (concat directory "/" database ".dump")))
             (cmd (concat mysql2sqlite-mysqldump-executable " -u" user
                         " --password=" password " -h " host
                         (unless keep-inserts
                           " --no-data")
                         " --compatible=ansi --skip-opt " database
                         " > " dumpfile)))
        (shell-command cmd)
        dumpfile))))

(defun mysql2sqlite-cleanup-buffer ()
  "Remove whitespace and blanklines."
  (delete-trailing-whitespace)
  (delete-matching-lines "^$"))

(defun mysql2sqlite-save-and-convert (dumpfile)
  "Write the updated buffer to a .sql file and create the sqlite database.
Argument DUMPFILE FIle containing the mysqldump output."
  (let* ((basefilename (concat (substring dumpfile 0 -5)))
         (sqlfile (concat basefilename ".sql"))
         (dbfile (concat basefilename ".db")))
    (write-file sqlfile)
    (kill-buffer)
    (let* ((cmd (concat "cat " sqlfile " | "
                        mysql2sqlite-sqlite-executable " " dbfile)))
      (shell-command cmd))))

(defun mysql2sqlite-remove-comments ()
  "Remove comments from the SQL dump."
    (save-excursion
      (goto-char (point-min))
      (delete-matching-lines "^--")
      (delete-matching-lines "^/\\*!")
      (while (search-forward "comment '" nil t)
        (backward-char 9)
        (kill-line)
        (insert ","))))

(defun mysql2sqlite-convert-primary-keys ()
  "Convert primary keys for sqlite.

Given:

CREATE TABLE \"animalSpecies\" (
  \"pk\" int(11) NOT NULL,
  \"species\" text NOT NULL,
  \"genus\" int(11) NOT NULL,
  PRIMARY KEY (\"pk\"),
  KEY \"animalGenusFk\" (\"genus\"),
  CONSTRAINT \"animalGenusFk\" FOREIGN KEY (\"genus\")
    REFERENCES \"animalGenus\" (\"pk
\") ON DELETE NO ACTION ON UPDATE NO ACTION);

The output will be:

CREATE TABLE \"animalSpecies\" (
  \"pk\" INTEGER PRIMARY KEY,
  \"species\" text NOT NULL,
  \"genus\" int(11) NOT NULL,
  KEY \"animalGenusFk\" (\"genus\"),
  CONSTRAINT \"animalGenusFk\" FOREIGN KEY (\"genus\")
    REFERENCES \"animalGenus\" (\"pk
\") ON DELETE NO ACTION ON UPDATE NO ACTION);"
  (save-excursion
    (goto-char (point-min))
    (let (bounds key)
      (while (setq bounds (mysql2sqlite-next-table-definition))
        (let ((beg (car bounds))
              (end (cdr bounds)))
          (goto-char beg)
          (when (search-forward "PRIMARY KEY (" end t)
            (forward-char)
            (setq key (word-at-point))
            (beginning-of-line)
            (kill-line)
            (kill-line)
            (goto-char beg)
            (search-forward key)
            (forward-char)
            (kill-line)
            (insert " INTEGER PRIMARY KEY,")))))
    (goto-char (point-min))
    (while (search-forward "auto_increment" nil t)
      (replace-match "PRIMARY KEY AUTOINCREMENT" nil t))))

(defun mysql2sqlite-remove-extra-keys ()
  "Remove extra keys from the dump.

Given:

CREATE TABLE \"animalSpecies\" (
  \"pk\" INTEGER PRIMARY KEY,
  \"species\" text NOT NULL,
  \"genus\" int(11) NOT NULL,
  KEY \"animalGenusFk\" (\"genus\"),
  CONSTRAINT \"animalGenusFk\" FOREIGN KEY (\"genus\")
    REFERENCES \"animalGenus\" (\"pk
\") ON DELETE NO ACTION ON UPDATE NO ACTION);

The output will be:

CREATE TABLE \"animalSpecies\" (
  \"pk\" INTEGER PRIMARY KEY,
  \"species\" text NOT NULL,
  \"genus\" int(11) NOT NULL,
  CONSTRAINT \"animalGenusFk\" FOREIGN KEY (\"genus\")
    REFERENCES \"animalGenus\" (\"pk
\") ON DELETE NO ACTION ON UPDATE NO ACTION);

This should be the equivalent of:

grep -v ' KEY \"'"
  (save-excursion
    (goto-char (point-min))
    (delete-matching-lines " KEY \"")))

(defun mysql2sqlite-convert-types ()
  "Convert column type declarations.

This should be the eqivalent of:

sed 's/ unsigned / /g'
sed 's/ smallint([0-9]*) / integer /g'
sed 's/ tinyint([0-9]*) / integer /g'
sed 's/ int([0-9]*) / integer /g'
sed 's/ character set [^ ]* / /g'
sed 's/ enum([^)]*) / varchar(255) /g'"
  (save-excursion
    (let ((types '((" unsigned " . " ")
                   (" smallint([0-9]*) " . " INTEGER ")
                   (" tinyint([0-9]*) " . " INTEGER ")
                   (" int([0-9]*) " . " INTEGER ")
                   (" character set [^ ]* " . " ")
                   (" enum([^)]*) " . " VARCHAR(255) "))))
      (dolist (type types)
        (goto-char (point-min))
        (while (re-search-forward (car type) nil t)
          (replace-match (cdr type) nil nil))))))

(defun mysql2sqlite-convert-constraints ()
  "This function coverts constraints."
  (save-excursion
    (goto-char (point-min))
    (let (bounds)
      (while (setq bounds (mysql2sqlite-next-table-definition))
        (let ((beg (car bounds))
              (end (cdr bounds))
              key
              table
              column
              actions)
          (goto-char beg)
          (while (search-forward "constraint " end t)
            (search-forward " FOREIGN KEY (\"")
            (setq key (word-at-point))
            (search-forward " REFERENCES \"")
            (setq table (word-at-point))
            (search-forward "(\"")
            (setq column (word-at-point))
            (search-forward "\") ON")
            (backward-char 3)
            (setq actions (buffer-substring-no-properties
                           (point)
                           (line-end-position)))
            (beginning-of-line)
            (kill-line)
            (kill-line)
            (goto-char beg)
            (search-forward (concat "\"" key "\" "))
            (kill-line)
            (unless (equal "," (substring actions -1))
              (setq actions (concat actions ",")))
            (insert (concat "INTEGER REFERENCES " table "(" column ")" actions))))))))

(defun mysql2sqlite-delete-sets ()
  "This function deletes SET statements.

This should be the equivalent of:

sed '/^SET/d'"
  (save-excursion
    (goto-char (point-min))
    (delete-matching-lines "^SET")))

(defun mysql2sqlite-convert-newlines ()
  "Convert newlines from \r\n to \n.

This should be the equivalent of:

sed 's/\\r\\n/\\n/g'"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\r\\\\n" nil t)
      (replace-match "\\\\n" nil nil))))

(defun mysql2sqlite-convert-quotes ()
  "Convert quotes from escaped to not escaped.

This should be the equivalent of:

sed 's/\\\"/\"/g'"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\\\\"" nil t)
      (replace-match "\"" nil nil))))

(defun mysql2sqlite-remove-trailing-commas ()
  "Remove trailing commas.

This should be the equivalent of:

s/,\\n\\)/\\n\\)/gs"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ",\n)" nil t)
      (replace-match "
)"))))

(defun mysql2sqlite-wrap-in-transaction ()
  "This function wraps the whole script in a transaction.

This should be the equivalent of:

print \"begin;\n\";print;print \"commit\n\";"
  (save-excursion
    (goto-char (point-min))
    (insert "begin;")
    (newline)
    (goto-char (point-max))
    (newline)
    (insert "commit;")
    (newline)))

(defun mysql2sqlite-convert-inserts ()
  "Convert INSERT statements to sqlite format.

This should be the equivalent of:

perl -pe '
if (/^(INSERT.+?)\(/) {
  $a=$1;
  s/\\'\''/'\'\''/g;
  s/\\n/\n/g;
  s/\),\(/\);\n$a\(/g;
}"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^INSERT" nil t)
      (search-forward "(")
      (backward-char)
      (let ((statement (buffer-substring-no-properties
                        (line-beginning-position)
                        (point))))
        (beginning-of-line)
        (while (re-search-forward "\\\\'\\''" nil t)
          (replace-match "'\\'\\''" nil nil))
        (while (re-search-forward "\\\\n" nil t)
          (replace-match "\\n" nil nil))
        (while (re-search-forward "\),\(" nil t)
          (replace-match (concat ")
 " statement "(")))
      (forward-line)))))

(defun mysql2sqlite-move-inserts-to-end ()
  "Move all insert statements to the end of the script."
  (save-excursion
    (let ((end (buffer-end 1)))
      (goto-char (point-min))
      (while (re-search-forward "^INSERT" end t)
        (beginning-of-line)
        (save-excursion
          (kill-line)
          (goto-char (point-max))
          (newline)
          (yank)
          (newline))))))

(defun mysql2sqlite-get-table-definition-bounds ()
  "Return the bounds of the current create table definition.
The bounds are returned as a cons of (beginning . end)"
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (search-forward "(")
      (backward-char)
      (forward-sexp)
      (setq end (point)))
    (cons beg end)))

(defun mysql2sqlite-next-table-definition ()
  "Move point to the next table definition and return its bounds.
The bounds are the result of mysql2sqlite-get-table-defiition-bounds."
  (let ((current-line (line-number-at-pos)))
    (if (search-forward "CREATE TABLE" nil t)
        (if (equal (line-number-at-pos) current-line)
            (progn
              (end-of-line)
              (mysql2sqlite-next-table-definition))
          (mysql2sqlite-get-table-definition-bounds))
      (goto-char (point-max))
      nil)))
    
(provide 'mysql2sqlite)

;;; mysql2sqlite.el ends here
