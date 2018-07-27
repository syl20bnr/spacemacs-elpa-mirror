Indent SQL statements.

As the indentation of SQL statements depends not only on the previous
line but also on the current line, empty lines cannot always be
indented correctly.

Usage note: Loading this file will make all SQL mode buffers created
from then on use `sql-indent-line' for indentation.  A possible way
to install sql-indent.el would be to add the following to your
.emacs:

(eval-after-load "sql"
  '(load-library "sql-indent"))

Thanks:
Arcady Genkin <antipode@thpoon.com>
