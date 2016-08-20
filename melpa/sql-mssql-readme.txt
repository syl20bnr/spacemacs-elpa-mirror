* README                                                             :README:
** Introduce
sql-mssql is a sql.el extension, which can let Linux/Unix user connect
Microsoft SQL Server with the help of sqsh, this extension may useless
for Windows user.

** Install
1. Install sqsh, unixODBC and freetds (For example: apt-get install sqsh unixodbc freetds)
2. Install sql-mssql (You can config Melpa and install it with `package-install' command)

** Configure
1. Config unixODBC, you can view the below documents as reference
   1. http://richbs.org/post/43142767072/connecting-to-microsoft-sql-server-from-unix
   2. http://askubuntu.com/questions/167491/connecting-ms-sql-using-freetds-and-unixodbc-isql-no-default-driver-specified
   3. http://help.interfaceware.com/kb/904
   4. Google: unixodbc mssql
2. Config sql.el
   #+BEGIN_EXAMPLE
   (require 'sql)
   (require 'sql-mssql)
   #+END_EXAMPLE

** Usage
1. Method1: M-x sql-mssql
2. Method2: M-x sql-mssql-connect (you must set `sql-connection-alist' before)
