;;; picolisp-mode.el --- Major mode for PicoLisp programming.

;; Copyright (C) 2014-2015  Alexis <flexibeast@gmail.com>

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2014-11-18
;; URL: https://github.com/flexibeast/picolisp-mode
;; Package-Version: 20150516.155
;; Keywords: picolisp, lisp, programming

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; `picolisp-mode' provides a major mode for PicoLisp programming.

;; This package is not based on, nor connected with, the PicoLisp support for Emacs provided in [the PicoLisp distribution](http://software-lab.de/down.html), or the more recently [updated version of that support](https://github.com/tj64/picolisp-mode). At this stage, the main advantages provided by this package are:

;; * an actively maintained and supported system;

;; * access to the PicoLisp reference documentation, including via ElDoc;

;; * basic Imenu support;

;; * ease of customisability; and

;; * a cleaner codebase.

;; ## Table of Contents

;; - [Features](#features)
;; - [Installation](#installation)
;; - [Usage](#usage)
;;  - [Syntax highlighting](#usage-highlighting)
;;  - [REPL](#repl)
;;  - [Documentation](#documentation)
;;  - [Commenting](#commenting)
;;  - [Indentation](#indentation)
;;  - [Miscellaneous](#miscellanous)
;; - [TODO](#todo)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Features

;; * Syntax highlighting of PicoLisp code. (But please read the below [note on syntax highlighting](#note-highlighting).)

;; * Comint-based `pil' REPL buffers.

;; * Quick access to documentation for symbol at point.

;; ## Installation

;; Install [picolisp-mode from MELPA](http://melpa.org/#/picolisp-mode), or put the `picolisp-mode' folder in your load-path and do a `(require 'picolisp-mode)'.

;; ## Usage

;; <a name='usage-highlighting'></a>

;; ### Syntax highlighting

;; Enable syntax highlighting for a PicoLisp source buffer with `M-x picolisp-mode'. 

;; ### REPL

;; Start a `pil' REPL session with `M-x picolisp-repl' or, from a `picolisp-mode' buffer, with `C-c C-r' (`picolisp-repl').

;; ### Documentation

;; Access documentation for the function at point with `C-c C-d' (`picolisp-describe-symbol'). By default, documentation will be displayed via the `lynx' HTML browser. However, one can set the value of `picolisp-documentation-method' to either a string containing the absolute path to an alternative browser, or - for users of Emacs 24.4 and above - to the symbol `picolisp--shr-documentation'; this function uses the `shr' library to display the documentation in an Emacs buffer. The absolute path to the documentation is specified via `picolisp-documentation-directory', and defaults to `/usr/share/picolisp/doc/'.

;; ElDoc support is available; note, however, that documentation is not yet accessible for some symbols - in particular, the `c[ad]*ar' functions - due to edge-cases in the reference documentation structure.

;; ### Commenting

;; Comment a region in a `picolisp-mode' buffer with `C-c C-;' (`picolisp-comment-region'); uncomment a region in a `picolisp-mode' buffer with `C-c C-:' (`picolisp-uncomment-region'). By default one '#' character is added/removed; to specify more, supply a numeric prefix argument to either command.

;; ### Indentation

;; Indent a region in a `picolisp-mode' buffer with `C-c M-q' (`picolisp-indent-region'). Indentation is done via the `pilIndent' script provided with the current PicoLisp distribution; the path to the script is specified via the `picolisp-pilindent-executable' variable.

;; ### Miscellaneous

;; SLIME users should read the below [note on SLIME](#note-slime).

;; The various customisation options, including the faces used for syntax highlighting, are available via the `picolisp' customize-group.

;; <a name="note-highlighting"></a>

;; ### A note on syntax highlighting

;; PicoLisp's creator is opposed to syntax highlighting of symbols in PicoLisp, for [good reasons](http://www.mail-archive.com/picolisp@software-lab.de/msg05019.html). However, some - such as the author of this package! - feel that, even taking such issues into consideration, the benefits can outweigh the costs. (For example, when learning PicoLisp, it can be useful to get immediate visual feedback about unintentionally redefining a PicoLisp 'builtin'.) To accommodate both views, syntax highlighting can be enabled or disabled via the `picolisp-syntax-highlighting-p' variable; by default, it is set to `t' (enabled).

;; <a name="note-slime"></a>

;; ### A note on [SLIME](https://github.com/slime/slime)

;; The design of SLIME is such that it can override `picolisp-mode' functionality. (The documentation for `picolisp--disable-slime-modes' provides details.) The user-customisable variable `picolisp-disable-slime-p' specifies whether to override these overrides, and defaults to `t'.

;; ## TODO

;; * Implement `pilIndent' in Emacs Lisp, make available as a fallback when `pilIndent' is not available.

;; * Fix misalignment of single-'#' comments upon newline.

;; * Handle edge-cases in reference documentation structure:

;;  * `picolisp-describe-symbol' failures;

;;  * `picolisp--eldoc-function' fails on e.g. `c[ad]*ar'.

;; <a name="issues"></a>

;; ## Issues / bugs

;; If you discover an issue or bug in `picolisp-mode' not already noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on GitHub](https://github.com/flexibeast/picolisp-mode/issues),

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `picolisp-mode'.

;; ## License

;; [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.

;;; Code:


;;
;; User-customisable settings.
;;

(defgroup picolisp nil
  "PicoLisp support."
  :group 'languages)

(defcustom picolisp-picolisp-executable "/usr/bin/picolisp"
  "Absolute path of the `picolisp' executable."
  :type '(file :must-match t)
  :group 'picolisp)

(defcustom picolisp-pil-executable "/usr/bin/pil"
  "Absolute path of the `pil' executable."
  :type '(file :must-match t)
  :group 'picolisp)

(defcustom picolisp-pilindent-executable "/usr/bin/pilIndent"
  "Absolute path of the `pilIndent' executable."
  :type '(file :must-match t)
  :group 'picolisp)

(defcustom picolisp-documentation-directory "/usr/share/picolisp/doc/"
  "Absolute path of the PicoLisp HTML documentation directory."
  :type 'directory
  :group 'picolisp)

(defcustom picolisp-documentation-method 'picolisp--shr-documentation
  "System to be used to display PicoLisp documentation."
  :type '(radio (function :tag "Function - must already be defined" :value 'picolisp--shr-documentation)
                (file :tag "HTML browser - absolute path" :value "/usr/bin/lynx"))
  :group 'picolisp)

(defcustom picolisp-repl-debug-p t
  "Whether to enable debug mode in the REPL.
Must be `t' to access documentation via `picolisp-describe-symbol'."
  :type 'boolean
  :group 'picolisp)

(defcustom picolisp-syntax-highlighting-p t
  "Whether to enable syntax highlighting."
  :type 'boolean
  :group 'picolisp)

(defcustom picolisp-disable-slime-p t
  "Whether to disable SLIME modes in `picolisp-mode' buffers."
  :type 'boolean
  :group 'picolisp)

(defgroup picolisp-faces nil
  "Faces for PicoLisp syntax highlighting."
  :group 'picolisp)

(defface picolisp-abstract-class-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp abstract classes."
  :group 'picolisp-faces)

(defface picolisp-builtin-face
  '((((background light)) :foreground "Purple"))
  "Face for PicoLisp builtins."
  :group 'picolisp-faces)

(defface picolisp-comment-face
  '((((background light)) :foreground "green"))
  "Face for PicoLisp comments."
  :group 'picolisp-faces)

(defface picolisp-global-constant-face
  '((((background light)) :foreground "Purple"))
  "Face for PicoLisp global constants."
  :group 'picolisp-faces)

(defface picolisp-global-variable-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp global variables."
  :group 'picolisp-faces)

(defface picolisp-local-function-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp local functions."
  :group 'picolisp-faces)

(defface picolisp-method-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp methods."
  :group 'picolisp-faces)

(defface picolisp-normal-class-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp normal classes."
  :group 'picolisp-faces)

(defface picolisp-transient-symbol-face
  '((((background light)) :foreground "blue"))
  "Face for PicoLisp transient symbols."
  :group 'picolisp-faces)


;;
;; Internal variables.
;;

(defvar picolisp-mode-map (make-sparse-keymap))
(define-key picolisp-mode-map (kbd "C-c C-;") 'picolisp-comment-region)
(define-key picolisp-mode-map (kbd "C-c C-:") 'picolisp-uncomment-region)
(define-key picolisp-mode-map (kbd "C-c C-d") 'picolisp-describe-symbol)
(define-key picolisp-mode-map (kbd "C-c C-r") 'picolisp-repl)
(define-key picolisp-mode-map (kbd "C-c M-q") 'picolisp-indent-region)

(defvar picolisp-repl-mode-map (make-sparse-keymap))
(define-key picolisp-repl-mode-map (kbd "C-c C-d") 'picolisp-describe-symbol)

;; http://software-lab.de/doc/ref.html#fun

(defvar picolisp-builtins
  '("!" "$" "$dat" "$tim" "%" "&" "*" "**" "*/" "*Allow" "*Bye" "*CPU" "*Class" "*Class" "*DB" "*Dbg" "*Dbg" "*Dbs" "*EAdr" "*Err" "*Fork" "*Hup" "*Led" "*Msg" "*OS" "*PPid" "*Pid" "*Prompt" "*Run" "*Scl" "*Sig1" "*Sig2" "*Solo" "*Tsm" "*Uni" "*Zap" "+" "+Alt" "+Any" "+Aux" "+Bag" "+Blob" "+Bool" "+Date" "+Dep" "+Entity" "+Fold" "+Hook" "+Hook2" "+Idx" "+IdxFold" "+Joint" "+Key" "+Link" "+List" "+Mis" "+Need" "+Number" "+Ref" "+Ref2" "+Sn" "+String" "+Swap" "+Symbol" "+Time" "+UB" "+index" "+relation" "-" "->" "/" ":" "::" ";" "<" "<=" "<>" "=" "=0" "=:" "==" "====" "=T" ">" ">=" ">>" "?" "@" "@@" "@@@" "This" "^" "abort" "abs" "accept" "accu" "acquire" "adr" "alarm" "align" "all" "allow" "allowed" "and" "any" "append" "append/3" "apply" "arg" "args" "argv" "as" "asoq" "assert" "asserta" "asserta/1" "assertz" "assertz/1" "assoc" "at" "atom" "aux" "balance" "be" "beep" "bench" "bin" "bind" "bit?" "blob" "blob!" "bool" "bool/3" "box" "box?" "by" "bye" "bytes" "caaaar" "caaadr" "caaar" "caadar" "caaddr" "caadr" "caar" "cache" "cadaar" "cadadr" "cadar" "caddar" "cadddr" "caddr" "cadr" "call" "call/1" "can" "car" "case" "casq" "catch" "cd" "cdaaar" "cdaadr" "cdaar" "cdadar" "cdaddr" "cdadr" "cdar" "cddaar" "cddadr" "cddar" "cdddar" "cddddr" "cdddr" "cddr" "cdr" "center" "chain" "char" "chdir" "chkTree" "chop" "circ" "circ?" "class" "clause" "clause/2" "clip" "close" "cmd" "cnt" "co" "collect" "commit" "con" "conc" "cond" "connect" "cons" "copy" "count" "ctl" "ctty" "curry" "cut" "d" "daemon" "dat$" "datStr" "datSym" "date" "day" "db" "db/3" "db/4" "db/5" "db:" "dbSync" "dbck" "dbs" "dbs+" "de" "debug" "dec" "def" "default" "del" "delete" "delete/3" "delq" "dep" "depth" "diff" "different/2" "dir" "dirname" "dm" "do" "doc" "e" "echo" "edit" "em" "env" "eof" "eol" "equal/2" "err" "errno" "eval" "expDat" "expTel" "expr" "ext?" "extend" "extern" "extra" "extract" "fail" "fail/0" "fetch" "fifo" "file" "fill" "filter" "fin" "finally" "find" "fish" "flg?" "flip" "flush" "fmt64" "fold" "fold/3" "for" "fork" "forked" "format" "free" "from" "full" "fully" "fun?" "gc" "ge0" "genKey" "get" "getd" "getl" "glue" "goal" "group" "gt0" "hash" "hax" "hd" "head" "head/3" "heap" "hear" "here" "hex" "host" "id" "idx" "if" "if2" "ifn" "import" "in" "inc" "inc!" "index" "info" "init" "insert" "intern" "ipid" "isa" "isa/2" "iter" "job" "journal" "key" "kids" "kill" "last" "later" "ld" "le0" "leaf" "length" "let" "let?" "lieu" "line" "lines" "link" "lint" "lintAll" "list" "listen" "lit" "load" "loc" "local" "locale" "lock" "loop" "low?" "lowc" "lst/3" "lst?" "lt0" "lup" "macro" "made" "mail" "make" "map" "map/3" "mapc" "mapcan" "mapcar" "mapcon" "maplist" "maps" "mark" "match" "max" "maxKey" "maxi" "member" "member/2" "memq" "meta" "meth" "method" "min" "minKey" "mini" "mix" "mmeq" "money" "more" "msg" "n0" "n==" "nT" "name" "nand" "native" "need" "new" "new!" "next" "nil" "nil/1" "nond" "nor" "not" "not/1" "nth" "num?" "obj" "object" "oct" "off" "offset" "on" "onOff" "once" "one" "open" "opid" "opt" "or" "or/2" "out" "pack" "pad" "pair" "part/3" "pass" "pat?" "patch" "path" "peek" "permute/2" "pick" "pico" "pilog" "pipe" "place" "poll" "pool" "pop" "port" "pp" "pr" "prEval" "pre?" "pretty" "prin" "prinl" "print" "println" "printsp" "prior" "proc" "prog" "prog1" "prog2" "prop" "protect" "prove" "prune" "push" "push1" "put" "put!" "putl" "pwd" "qsym" "query" "queue" "quit" "quote" "rand" "range" "range/3" "rank" "raw" "rc" "rd" "read" "recur" "recurse" "redef" "rel" "release" "remote/2" "remove" "repeat" "repeat/0" "replace" "request" "rest" "retract" "retract/1" "reverse" "rewind" "rollback" "root" "rot" "round" "rules" "run" "same/3" "scan" "scl" "script" "sect" "seed" "seek" "select" "select/3" "send" "seq" "set" "set!" "setq" "show" "show/1" "sigio" "size" "skip" "solve" "sort" "sp?" "space" "split" "sqrt" "stack" "stamp" "state" "stem" "step" "store" "str" "str?" "strDat" "strip" "sub?" "subr" "sum" "super" "sym" "sym?" "symbols" "sync" "sys" "t" "tab" "tail" "task" "telStr" "tell" "test" "text" "throw" "tick" "till" "tim$" "time" "timeout" "tmp" "tolr/3" "touch" "trace" "traceAll" "trail" "tree" "trim" "true/0" "try" "type" "u" "ubIter" "udp" "ultimo" "unbug" "undef" "unify" "uniq" "uniq/2" "unless" "until" "untrace" "up" "upd" "update" "upp?" "uppc" "use" "useKey" "usec" "val" "val/3" "var" "var:" "version" "vi" "view" "wait" "week" "what" "when" "while" "who" "wipe" "with" "wr" "wrap" "xchg" "xor" "x|" "yield" "yoke" "zap" "zapTree" "zero" "|"))

(defvar picolisp-builtins-by-length
  (let ((bs (copy-sequence picolisp-builtins)))
    (sort bs #'(lambda (e1 e2)
                 (> (length e1) (length e2)))))
  "List of PicoLisp builtins, sorted by length for use by
`picolisp-builtins-regex'.")

(defvar picolisp-builtins-regex
  (let ((s "")
        (firstp t))
    (dolist (b picolisp-builtins-by-length)
      (if (not firstp)
          (setq s (concat s "\\|" (regexp-quote b)))
        (progn
          (setq s (regexp-quote b))
          (setq firstp nil))))
    s)
  "Regex for use by `picolisp-font-lock-keywords'.")

;; http://software-lab.de/doc/ref.html#conv

(defvar picolisp-font-lock-keywords
  `(("\\_<\\(T\\|NIL\\)\\_>"
     (1 'picolisp-global-constant-face t))
    ("\\(\\*[[:alpha:]]+\\)"
     (1 'picolisp-global-variable-face t))
    ("\\(\\+[a-z]\\S-*\\)"
     (1 'picolisp-abstract-class-face t))
    ("\\(\\+[A-Z][[:alpha:]]*\\)"
     (1 'picolisp-normal-class-face t))
    (,(concat "\\((\\)\\_<\\(" picolisp-builtins-regex "\\)\\_>")
     (1 'default t)
     (2 'picolisp-builtin-face t))
    ("(\\(_\\S-+\\)"
     (1 'picolisp-local-function-face t))
    ("(\\([[:alpha:]]\\S-+>\\s-\\)"
     (1 'picolisp-method-face t))
    ("\\(\".+?\"\\)"
     (1 'picolisp-transient-symbol-face t))
    ("^.*?\\(#+.*\\)$"
     (1 'picolisp-comment-face)))
  "Regexes for syntax-highlighting `picolisp-mode' buffers.")

;;
;; http://software-lab.de/doc/ref.html#symbol:
;;
;; Internal symbol names can consist of any printable (non-whitespace)
;; character, except for the following meta characters:
;;
;; "  '  (  )  ,  [  ]  `  ~ { }
;;

(defvar picolisp-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;;;
    ;;; Symbol syntax.
    ;;;

    (modify-syntax-entry ?* "_   " table)
    (modify-syntax-entry ?+ "_   " table)
    (modify-syntax-entry ?- "_   " table)
    (modify-syntax-entry ?: "_   " table)
    (modify-syntax-entry ?> "_   " table)
    (modify-syntax-entry ?< "_   " table)
    (modify-syntax-entry ?? "_   " table)
    (modify-syntax-entry ?@ "_   " table)
    (modify-syntax-entry ?A "_   " table)
    (modify-syntax-entry ?B "_   " table)
    (modify-syntax-entry ?C "_   " table)
    (modify-syntax-entry ?D "_   " table)
    (modify-syntax-entry ?E "_   " table)
    (modify-syntax-entry ?F "_   " table)
    (modify-syntax-entry ?G "_   " table)
    (modify-syntax-entry ?H "_   " table)
    (modify-syntax-entry ?I "_   " table)
    (modify-syntax-entry ?J "_   " table)
    (modify-syntax-entry ?K "_   " table)
    (modify-syntax-entry ?L "_   " table)
    (modify-syntax-entry ?M "_   " table)
    (modify-syntax-entry ?N "_   " table)
    (modify-syntax-entry ?O "_   " table)
    (modify-syntax-entry ?P "_   " table)
    (modify-syntax-entry ?Q "_   " table)
    (modify-syntax-entry ?R "_   " table)
    (modify-syntax-entry ?S "_   " table)
    (modify-syntax-entry ?T "_   " table)
    (modify-syntax-entry ?U "_   " table)
    (modify-syntax-entry ?V "_   " table)
    (modify-syntax-entry ?W "_   " table)
    (modify-syntax-entry ?X "_   " table)
    (modify-syntax-entry ?Y "_   " table)    
    (modify-syntax-entry ?Z "_   " table)
    (modify-syntax-entry ?a "_   " table)
    (modify-syntax-entry ?b "_   " table)
    (modify-syntax-entry ?c "_   " table)
    (modify-syntax-entry ?d "_   " table)
    (modify-syntax-entry ?e "_   " table)
    (modify-syntax-entry ?f "_   " table)
    (modify-syntax-entry ?g "_   " table)
    (modify-syntax-entry ?h "_   " table)
    (modify-syntax-entry ?i "_   " table)
    (modify-syntax-entry ?j "_   " table)
    (modify-syntax-entry ?k "_   " table)
    (modify-syntax-entry ?l "_   " table)
    (modify-syntax-entry ?m "_   " table)
    (modify-syntax-entry ?n "_   " table)
    (modify-syntax-entry ?o "_   " table)
    (modify-syntax-entry ?p "_   " table)
    (modify-syntax-entry ?q "_   " table)
    (modify-syntax-entry ?r "_   " table)
    (modify-syntax-entry ?s "_   " table)
    (modify-syntax-entry ?t "_   " table)
    (modify-syntax-entry ?u "_   " table)
    (modify-syntax-entry ?v "_   " table)
    (modify-syntax-entry ?w "_   " table)
    (modify-syntax-entry ?x "_   " table)
    (modify-syntax-entry ?y "_   " table)    
    (modify-syntax-entry ?z "_   " table)
    
    ;; { and } delimit external symbol names.
    (modify-syntax-entry ?\{ "_   " table)
    (modify-syntax-entry ?\} "_   " table)

    ;; . can be used in a symbol name, even though,
    ;; when surrounded by white space, it's
    ;; a metacharacter indicating a dotted pair.
    (modify-syntax-entry ?. "_   " table)

    ;; " primarily indicates a transient symbol, even
    ;; though it can also be used to indicate strings.
    (modify-syntax-entry ?\" "_   " table)

    ;;;
    ;;; Whitespace syntax.
    ;;;

    (modify-syntax-entry ?\n "    " table)
    (modify-syntax-entry ?\s "    " table)
    (modify-syntax-entry ?\x8a0 "    " table)
    (modify-syntax-entry ?\t "    " table)
    (modify-syntax-entry ?\f "    " table)

    ;;;
    ;;; Comment syntax.
    ;;;
    
    (modify-syntax-entry ?# "<   " table)

    ;;;
    ;;; Quote syntax.
    ;;;
    
    (modify-syntax-entry ?` "'   " table)
    (modify-syntax-entry ?' "'   " table)
    (modify-syntax-entry ?, "'   " table)
    (modify-syntax-entry ?~ "'   " table)

    ;;;
    ;;; Parenthesis syntax.
    ;;;
    
    (modify-syntax-entry ?\( "()  " table)
    (modify-syntax-entry ?\) ")(  " table)
    (modify-syntax-entry ?\[ "(]  " table)
    (modify-syntax-entry ?\] ")[  " table)

    ;;;
    ;;; Escape syntax.
    ;;;

    (modify-syntax-entry ?\\ "\\   " table)
    
    table)
  
  "Syntax table used in `picolisp-mode'.")


;;
;; Internal functions.
;;

(defun picolisp--create-picolisp-mode-menu ()
  "Internal function to create or recreate the picolisp-mode menu."
  (easy-menu-define picolisp-menu picolisp-mode-map "Menu bar entry for `picolisp-mode'"
    `("PicoLisp"
      ["Comment region" (picolisp-comment-region) :keys "C-c C-;"]
      ["Uncomment region" (picolisp-uncomment-region) :keys "C-c C-:"]
      ["Indent region" (picolisp-indent-region) :keys "C-c M-q"]
      ["PicoLisp REPL" (picolisp-repl) :keys "C-c C-r"]
      ["Customize" (customize-group 'picolisp) t])))

(defun picolisp--disable-slime-modes ()
  "Function to add to `lisp-mode-hook' if `picolisp-disable-slime-p'
is set to `t'.

SLIME adds the function `slime-lisp-mode-hook' to the
`lisp-mode-hook' variable. Since `picolisp-mode' is defined as
being derived from `lisp-mode', the effect of this is to enable
various SLIME features in `picolisp-mode' buffers, overriding
`picolisp-mode' functionality.

This function thus overrides those overrides, and:

* disables `slime-mode';

* disables `slime-autodoc-mode'; and

* ensures that the value of `eldoc-documentation-function' is
  `picolisp--eldoc-function'."
  (slime-mode 0)
  (slime-autodoc-mode 0)
  (make-local-variable 'eldoc-documentation-function)
  (setq eldoc-documentation-function #'picolisp--eldoc-function))

(defun picolisp--eldoc-function ()
  "Function for use by `eldoc-documentation-function'."
  (let* ((sym (symbol-name (symbol-at-point)))
         (dl (picolisp--extract-reference-documentation sym))
         (result nil))
    (unless (string= "nil" sym)
      (dotimes (i (/ (length dl) 2))
        (let ((fst (nth (* i 2) dl))
              (snd (nth (1+ (* i 2)) dl)))
          (if (eq 'dt (car-safe fst))
              (cond
               ((eq 'cons (type-of (nth 2 fst)))
                (if (string= sym (cdaadr (nth 2 fst)))
                    (setq result (concat (propertize (concat sym ", ") 'face 'picolisp-builtin-face)
                                         (nth 2 (car (cdr (cdr (nth 2 fst)))))))))
               ;; Ignore edge-cases in the documentation structure, such
               ;; as the documentation for `c[ad]*ar'.
               ((eq 'string (type-of (nth 2 fst)))
                (setq result nil))))))
      result)))

(defun picolisp--extract-reference-documentation (sym)
  "Helper function to extract the 'Function Reference' definition
list from the PicoLisp documentation, where SYM is the symbol being
looked up."
  (let* ((char (progn
                 (string-match "^[[:punct:]]*\\([[:punct:]]\\|[[:alpha:]]\\)" sym)
                 (upcase (match-string 1 sym))))
         (doc (if (string-match "[[:alpha:]]" char)
                  (concat picolisp-documentation-directory "ref" char ".html")
                (concat picolisp-documentation-directory "ref_.html")))
         (bfr (generate-new-buffer " *PicoLisp documentation source*"))
         (dom (progn
                (switch-to-buffer bfr)
                (insert-file-contents doc)
                (libxml-parse-html-region (point-min) (point-max))))
         (dl (nth 5 (nth 3 dom))))
    (kill-buffer bfr)
    dl))

(defun picolisp--font-lock-syntactic-face-function (state)
  "No-op function to prevent font-lock from trying to fontify
comments and strings.

Since strings in PicoLisp are fundamentally (transient)
symbols, they are marked as such in the PicoLisp syntax-table.
However, this makes it complicated for Emacs to determine if
a '#' character is a comment delimiter or merely a constituent
of a string / transient symbol. So we override syntactic
fontification with this no-op function, and fontify comments
via `picolisp-font-lock-keywords'."
  nil)

(defun picolisp--imenu-create-index ()
  "Internal function to create an Imenu index."
  (let ((index '()))
    (setq index (append index (picolisp--imenu-find-classes-and-members)))
    (setq index (append index (picolisp--imenu-find-database-objects)))
    (setq index (append index (picolisp--imenu-find-facts-and-rules)))
    (setq index (append index (picolisp--imenu-find-functions)))
    (setq index (append index (picolisp--imenu-find-global-variables)))
    index))

(defun picolisp--imenu-find-classes-and-members ()
  "Internal function to find PicoLisp classes and their
associated methods and/or relations."
  (let ((classes '()))
    (goto-char (point-min))
    (while (re-search-forward "^[[:space:]]*(class \\([+][[:alnum:]]+\\)" nil t)
      (let ((class (match-string 1))
            (class-index (match-beginning 1))
            (members '())
            (methods '())
            (relations '())
            (next-class-index 0))
        (setq members `(("Definition" . ,class-index)))
        (save-excursion
          (setq next-class-index
                (if (re-search-forward "^[[:space:]]*(class \\([+][[:alnum:]]+\\)" nil t)
                    (match-beginning 1)
                  (point-max))))
        (save-excursion
          (while (re-search-forward "^[[:space:]]*(dm \\([[:alnum:]]+>\\)" next-class-index t)
            (setq methods (append methods `((,(match-string 1) . ,(match-beginning 1)))))))
        (setq methods `(("Methods" . ,methods)))
        (save-excursion
          (while (re-search-forward "^[[:space:]]*(rel \\([[:alnum:]]+\\)" next-class-index t)
            (setq relations (append relations `((,(match-string 1) . ,(match-beginning 1)))))))
        (setq relations `(("Relations" . ,relations)))
        (setq members (append members methods relations))
        (setq classes (append classes `((,class . ,members))))))
    (setq classes `(("Classes" . ,classes)))
    classes))

(defun picolisp--imenu-find-database-objects ()
  "Internal function to find PicoLisp database objects."
  (let ((re (concat "^[[:space:]]*(obj[[:space:]]+((\\([^)]+\\))[[:space:]]+"
                    "\\(?:[^[:space:]]+[[:space:]]+\\)?\\([^[:space:]]+\\))"))
        (objs '()))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (let ((obj-class (match-string 1))
            (obj-identifier (match-string 2))
            (obj-position (match-beginning 2)))
        (if (assoc obj-class objs)
            (setcdr (assoc obj-class objs)
                    (append (cdr (assoc obj-class objs)) `((,obj-identifier . ,obj-position))))
          (setq objs
                (append objs `((,obj-class . ((,obj-identifier . ,obj-position)))))))))
    (setq objs `(("Database objects" . ,objs)))
    objs))

(defun picolisp--imenu-find-facts-and-rules ()
  "Internal function to find PicoLisp facts and/or rules."
  (picolisp--imenu-find-things "Facts and rules" "^[[:space:]]*(be \\([[:alnum:]]+\\))"))

(defun picolisp--imenu-find-functions ()
  "Internal function to find PicoLisp functions."
  (picolisp--imenu-find-things "Functions" "^[[:space:]]*(de \\([^*][[:alnum:]*+_]+\\)[[:space:]]+("))

(defun picolisp--imenu-find-global-variables ()
  "Internal function to find PicoLisp global variables."
  (picolisp--imenu-find-things "Global variables" "^[[:space:]]*(de \\([*][[:alnum:]*+]+\\)[[:space:]]+"))

(defun picolisp--imenu-find-things (name re)
  "Internal function to find PicoLisp components of type NAME
that can be identified by a simple regular expression RE."
  (let ((things '()))
    (goto-char (point-min))
    (while (re-search-forward re nil t)
      (setq things (append things `((,(match-string 1) . ,(match-beginning 1))))))
    (setq things `((,name . ,things)))
    things))

(defun picolisp--shr-documentation (sym)
  "Use `shr' to display documentation for symbol SYM at point."
  (unless (or (> emacs-major-version 24)
              (and (= emacs-major-version 24)
                   (> emacs-minor-version 3)))
    (error "Emacs 24.4 or greater required"))
  (let ((dl (picolisp--extract-reference-documentation sym)))
    (dotimes (i (/ (length dl) 2))
      (let ((fst (nth (* i 2) dl))
            (snd (nth (1+ (* i 2)) dl)))
        (if (eq 'dt (car-safe fst))
            (cond
             ((eq 'cons (type-of (nth 2 fst)))
              (if (string= sym (cdr (car (car (cdr (nth 2 fst))))))
                  (progn
                    (switch-to-buffer (generate-new-buffer (concat "*PicoLisp documentation - '" sym "' *")))
                    (insert (concat (propertize "Symbol:" 'face '(foreground-color . "ForestGreen")) " " (propertize sym 'face 'picolisp-builtin-face) "\n\n"))
                    (shr-insert-document snd)
                    (goto-char (point-min))
                    (help-mode))))
             ;; Ignore edge-cases in the documentation structure, such
             ;; as the documentation for `class'.
             ((eq 'string (type-of (nth 2 fst)))
              nil)))))))


;;
;; User-facing functions.
;;

(defun picolisp-comment-region (&optional n)
  "Comment lines in region using N '#' characters. N can be
specified by providing a numeric prefix argument; otherwise,
N defaults to 1."
  (interactive "p")
  (if n
      (comment-region (region-beginning) (region-end) n)
    (comment-region (region-beginning) (region-end) 1)))

(defun picolisp-uncomment-region (&optional n)
  "Uncomment lines in region by removing N '#' characters. N can
be specified by providing a numeric prefix argument; otherwise,
N defaults to 1."
  (interactive "p")
  (if n
      (uncomment-region (region-beginning) (region-end) n)
    (comment-region (region-beginning) (region-end) 1)))

(defun picolisp-indent-region ()
  "Indent the active region using the `pilIndent' script."
  (interactive)
  (unless (region-active-p)
    (user-error "Region is not active"))
  (let* ((beginning (region-beginning))
         (end (region-end)))
    (shell-command-on-region
     beginning end
     picolisp-pilindent-executable
     nil t)))

(defun picolisp-describe-symbol ()
  "Display documentation for symbol at point, via method
specified by `picolisp-documentation-method'."
  (interactive)
  (let ((process-environment
         (if (eq 'string (type-of picolisp-documentation-method))
             (add-to-list 'process-environment
                          (concat "BROWSER=" picolisp-documentation-method))
           process-environment))
        (sym (symbol-name
              (symbol-at-point))))
    (if (member sym picolisp-builtins)
        (cond
         ((eq 'symbol (type-of picolisp-documentation-method))
          (picolisp--shr-documentation sym))
         ((eq 'string (type-of picolisp-documentation-method))
          (start-process-shell-command "picolisp-doc" nil
                                       (concat "pil -\"doc (car (nth (argv) 3)\" -bye - '" sym "' +")))
         (t
          (error "Unexpected value type in picolisp-documentation-method")))
      (message "No PicoLisp builtin at point."))))

;;;###autoload
(define-derived-mode picolisp-mode lisp-mode "PicoLisp"
  "Major mode for PicoLisp programming. Derived from lisp-mode.

\\{picolisp-mode-map}"
  :group 'picolisp
  :syntax-table picolisp-mode-syntax-table

  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (if picolisp-syntax-highlighting-p
      (setq-local font-lock-defaults '(picolisp-font-lock-keywords
                                       nil nil nil nil
                                       (font-lock-syntactic-face-function
                                        . picolisp--font-lock-syntactic-face-function))))
  (setq-local eldoc-documentation-function #'picolisp--eldoc-function)
  (picolisp--create-picolisp-mode-menu)
  (setq-local imenu-create-index-function 'picolisp--imenu-create-index)
  (setq-local imenu-sort-function 'imenu--sort-by-name)
  (imenu-add-menubar-index)
  (if picolisp-disable-slime-p
      (progn
        (make-local-variable 'lisp-mode-hook)
        (add-hook 'lisp-mode-hook 'picolisp--disable-slime-modes))))

;;;###autoload
(define-derived-mode picolisp-repl-mode comint-mode "PicoLisp REPL"
  "Major mode for `pil' REPL sessions. Derived from comint-mode.

\\{picolisp-repl-mode-map}"
  :group 'picolisp
  :syntax-table picolisp-mode-syntax-table

  (setq-local comment-start "#")
  (setq-local comment-start-skip "#+ *")
  (if picolisp-syntax-highlighting-p
      (setq-local font-lock-defaults '(picolisp-font-lock-keywords
                                       nil nil nil nil
                                       (font-lock-syntactic-face-function
                                        . picolisp--font-lock-syntactic-face-function))))
  (setq-local eldoc-documentation-function #'picolisp--eldoc-function))

;;;###autoload
(defun picolisp-repl ()
  "Start a `pil' session in a new `picolisp-repl-mode' buffer."
  (interactive)
  (let ((process-environment
         (if (eq 'string (type-of picolisp-documentation-method))
             (add-to-list 'process-environment
                          (concat "BROWSER=" picolisp-documentation-method))
           process-environment)))
    (make-comint "picolisp-repl" picolisp-pil-executable nil (if picolisp-repl-debug-p "+" nil))
    (switch-to-buffer "*picolisp-repl*")
    (picolisp-repl-mode)))


;; --

(provide 'picolisp-mode)

;;; picolisp-mode.el ends here
