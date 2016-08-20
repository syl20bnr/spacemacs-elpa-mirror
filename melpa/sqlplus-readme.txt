 Facilitates interaction with Oracle via SQL*Plus (GNU Emacs only).
 Moreover, this package complements plsql.el (Kahlil Hodgson)
 upon convenient compilation of PL/SQL source files.

 This package was inspired by sqlplus-mode.el (Rob Riepel, Peter
 D. Pezaris, Martin Schwenke), but offers more features:
   - tables are parsed, formatted and rendered with colors, like in
     many GUI programs; you can see raw SQL*Plus output also,
     if you wish
   - table will be cutted if you try to fetch too many rows
     (SELECT * FROM MY_MILLION_ROWS_TABLE); current SQL*Plus command
     will be automatically interrupted under the hood in such cases
   - you can use many SQL*Plus processes simultaneously,
   - font locking (especially if you use Emacs>=22), with database
     object names highlighting,
   - history (log) of executed commands - see` sqlplus-history-dir`
     variable,
   - commands for fetching any database object definition
     (package, table/index/sequence script)
   - query result can be shown in HTML,
   - input buffer for each connection can be saved into file on
     disconnect and automatically restored on next connect (see
     'sqlplus-session-cache-dir' variable); if you place some
     SQL*Plus commands between '/* init */' and '/* end */'
     comments in saved input buffer, they will be automatically
     executed on every connect
   - if you use plsql.el for editing PL/SQL files, you can compile
     such sources everytime with C-cC-c; error messages will be
     parsed and displayed for easy source navigation
   - M-. or C-mouse-1 on database object name will go to definition
     in filesystem (use arrow button on toolbar to go back)

 The following commands should be added to a global initialization
 file or to any user's .emacs file to conveniently use
 sqlplus-mode:

   (require 'sqlplus)
   (add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))

 If you want PL/SQL support also, try something like this:

 (require 'plsql)
 (setq auto-mode-alist
   (append '(("\\.pls\\'" . plsql-mode) ("\\.pkg\\'" . plsql-mode)
		("\\.pks\\'" . plsql-mode) ("\\.pkb\\'" . plsql-mode)
		("\\.sql\\'" . plsql-mode) ("\\.PLS\\'" . plsql-mode)
		("\\.PKG\\'" . plsql-mode) ("\\.PKS\\'" . plsql-mode)
		("\\.PKB\\'" . plsql-mode) ("\\.SQL\\'" . plsql-mode)
		("\\.prc\\'" . plsql-mode) ("\\.fnc\\'" . plsql-mode)
		("\\.trg\\'" . plsql-mode) ("\\.vw\\'" . plsql-mode)
		("\\.PRC\\'" . plsql-mode) ("\\.FNC\\'" . plsql-mode)
		("\\.TRG\\'" . plsql-mode) ("\\.VW\\'" . plsql-mode))
	      auto-mode-alist ))

 M-x sqlplus will start new SQL*Plus session.

 C-RET   execute command under point
 S-C-RET execute command under point and show result table in HTML
         buffer
 M-RET   explain execution plan for command under point
 M-. or C-mouse-1: find database object definition (table, view
         index, synonym, trigger, procedure, function, package)
         in filesystem
 C-cC-s  show database object definition (retrieved from database)

 Use describe-mode while in sqlplus-mode for further instructions.

 Many useful commands are defined in orcl-mode minor mode, which is
 common for input and otput SQL*Plus buffers, as well as PL/SQL
 buffers.

 For twiddling, see 'sqlplus' customization group.

 If you find this package useful, send me a postcard to address:

   Peter Karpiuk
   Scott Tiger S.A.
   ul. Gawinskiego 8
   01-645 Warsaw
   Poland
