mysql2sqlite is an Emacs package for converting a mysql database into an sqlite
database.

Installation:

The package can be installed with M-x package-list-packages (requires
package.el, which is included for Emacs 24 but availble for 23). The MELPA
repository must be added to your packages setup first.  Instructions can be found
at https://melpa.org/

Alternatively, the source can be pulled direclty from github:
https://github.com/echosa/emacs-mysql2sqlite

Customize

There are some customizable options:

mysql2sqlite-sqlite-executable    - The sqlite executable to run.
mysql2sqlite-mysqldump-executable - The mysqldump executable to run.
mysql2sqlite-mysqldump-host       - The default host to connect to with mysqldump.
mysql2sqlite-mysqldump-user       - The default user to use to connect to mysqldump.
mysql2sqlite-mysqldump-database   - The default database to convert

Usage

Usage is as simple as running M-x mysql2sqlite.  You will be prompted for the
necessary values, with the customized defaults as default values.  Running the
function will result in several files in the target directory:

<output-file>.sql - The actual SQL file generated via mysqldump.
<output-file>.db  - The generated sqlite database.
<output-file>.err - The output of the conversion process.

mysql2sqlite is written and maintained by Brian Zwahr <echosa@gmail.com>
