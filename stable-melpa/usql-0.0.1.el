;;; usql.el --- U-SQL support for sql-mode
;; Copyright (C) 2018 Nicholas Barnwell

;; Author: Nicholas Barnwell <nb@ul.io>
;; Homepage: https://github.com/nickbarwell/usql.el
;; Version: 0.0.1
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages
;;; Commentary:

;; This package adds U-SQL to the sql-mode product list.
;; It does not yet support a comint buffer

;;; Code:
(require 'sql)

(defvar usql-mode-font-lock-keywords
  (eval-when-compile (list
   '("^\/\/.+" . font-lock-comment-face)
   '("@[a-zA-Z0-9_]*" . font-lock-variable-name-face)
   (sql-font-lock-keywords-builder 'font-lock-type-face nil
    "bigint" "bigserial"
    "bit" "boolean" "box" "bytea" "cidr" "circle" "date" "datetime" "string"
    "double\sprecision" "inet" "int" "integer" "line" "lseg" "macaddr" "money"
    "oid" "path" "point" "polygon" "real" "serial" "smallint" "sysdate" "text")
   (sql-font-lock-keywords-builder 'font-lock-keyword-face nil
    "add" "additionalfiles" "agg" "algorithm" "all" "alter" "and" "antisemijoin"
    "any" "apply" "array" "as" "asc" "assembly" "begin" "between" "bigint"
    "binary" "bit" "broadcast" "broadcastleft" "broadcastright" "buckets" "by"
    "cluster" "clustered" "colsize" "column" "columngroups" "columnset"
    "columnstore" "combine" "const" "create" "created" "credential" "cross"
    "csharp" "current" "data" "database" "datetime2" "decimal" "declare"
    "default" "deploy" "desc" "direct" "distinct" "distinctvalue" "distribute"
    "distributed" "donotcollapse" "donotspill" "drop" "else" "elseif" "end"
    "error" "except" "execute" "exists" "explode" "explrule" "external"
    "extract" "fetch" "file" "first" "float" "following" "for" "force" "from"
    "full" "fullcross" "fullscan" "function" "group" "hash" "having" "identity"
    "ignore" "if" "in" "incremental" "index" "indexlookup" "inner" "insert"
    "int" "integrity" "intersect" "into" "is" "join" "keeppartition" "keys"
    "lcid" "left" "length" "location" "loop" "lowdistinctness" "map" "max"
    "maxbytespersssextractcombinevertex" "maxdop" "merge" "modified" "money"
    "move" "name" "next" "nonclustered" "nvarchar" "null" "off" "offset" "on"
    "only" "or" "order" "option" "outer" "output" "over" "package" "pair"
    "partition" "partitioned" "pattern" "pivot" "preceding" "presort"
    "procedure" "process" "produce" "range" "readonly" "real" "rebuild" "reduce"
    "reference" "required" "resource" "returns" "return" "right" "robin" "round"
    "row" "rows" "rowcount" "rowsize" "sample" "schema" "script" "select"
    "semijoin" "serial" "set" "skewfactor" "skewjoin" "skip" "smallint"
    "sortcolumns" "source" "sql" "stageboundaryoutput" "statistics" "statpath"
    "struct" "switch" "system" "table" "then" "tinyint" "truncate" "to" "top"
    "type" "unbounded" "uniform" "union" "unique" "uniqueidentifier" "universe"
    "unpivot" "update" "use" "username" "using" "values" "varbinary" "varchar"
    "view" "violation" "weight" "where" "with" "within" "withindex"
    "authorization" "backup" "break" "browse" "bulk" "cascade" "case" "check"
    "checkpoint" "close" "coalesce" "collate" "commit" "compute" "contains"
    "containstable" "continue" "convert" "current_date" "current_time"
    "current_timestamp" "current_user" "cursor" "dbcc" "deallocate" "delete"
    "deny" "disk" "dump" "errlvl" "exec" "exit" "fillfactor" "foreign"
    "freetext" "freetexttable" "goto" "grant" "holdlock" "identity_insert"
    "identitycol" "key" "kill" "last" "lineno" "load" "national" "nocheck"
    "nullif" "of" "offsets" "open" "opendatasource" "openquery" "openrowset"
    "openxml" "plan" "precision" "primary" "print" "public" "raiserror" "read"
    "readtext" "reconfigure" "replication" "restore" "restrict" "revert"
    "revoke" "rollback" "rowguidcol" "rule" "save" "securityaudit"
    "semantickeyphrasetable" "semanticsimilaritydetailstable"
    "semanticsimilaritytable" "session_user" "setuser" "shutdown" "some"
    "system_user" "tablesample" "textsize" "tran" "transaction" "trigger"
    "try_convert" "tsequal" "updatetext" "user" "varying" "waitfor" "when"
    "while" "writetext"))))

(eval-after-load "sql"
  '(sql-add-product
    'usql "USQL"
    :font-lock 'usql-mode-font-lock-keywords
    :syntax-alist '((?\" . "\""))))

(add-to-list 'auto-mode-alist '("\\.usql\\'" . (lambda ()
                                                 (sql-mode)
                                                 (sql-set-product 'usql))))
(provide 'usql)
;;; usql.el ends here
