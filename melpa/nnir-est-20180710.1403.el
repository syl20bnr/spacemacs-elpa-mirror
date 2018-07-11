;;; nnir-est.el --- Gnus nnir interface for HyperEstraier  -*- lexical-binding: t; -*-

;; Filename: nnir-est.el
;; Description: Gnus nnir interface for HyperEstraier
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2014-02-01
;; Version: 1.180705
;; Package-Version: 20180710.1403
;; Keywords: mail
;; Human-Keywords: gnus nnir
;; URL: https://github.com/kawabata/nnir-est

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; -*- mode: org -*-
;;
;; * HyperEstraier Interface for Gnus nnir Method.
;;
;; This file provides a Gnus nnir interface for [[http://fallabs.com/hyperestraier/index.html][HyperEstraier]].
;; HyperEstraier is powerful, fast, multi-lingual, mail-friendly search engine.
;; You can use this search engine with `nnmh', `nnml', and `nnmaildir'.
;;
;; ** Indexing
;;
;; You can create the index by the following command.
;;
;; : % mkdir ~/News/casket                              # if necessary
;; : % estcmd gather -cl -fm -cm ~/News/casket ~/Mail   # for nnml/nnmh
;;
;; Sometimes, it is recommended to optimize the index.
;;
;; : % estcmd optimize ~/News/casket
;;
;; You may choose any directory for index or target.
;;
;; : % mkdir ~/.index                                   # if necessary
;; : % estcmd gather -cl -fm -cm ~/.index ~/Maildir     # for nnmaildir
;;
;; Target directory can be specified by `nnir-est-prefix' or directory
;; property of `nnmaildir'.
;; Index directory can be specified by `nnir-est-index-directory'.
;;
;; ** Emacs Setup
;;
;; In .emacs, you should set as the following. (`gnus-select-method' is
;; used in the following examples, but you can specify it in
;; `gnus-secondary-select-methods`, too.)
;;
;; First of all, you should choose which method to use HyperEstraier.
;;
;; : (require 'nnir-est)
;; : (setq nnir-method-default-engines
;; :        '((nnmaildir . est)
;; :          (nnml . est)
;; :          (nntp . gmane)))
;;
;; If you are using `nnml', specifying mail directory may be sufficient.
;;
;; : ;; nnml/nnmh
;; : (setq gnus-select-method '(nnml ""))
;; : (setq nnir-est-prefix "/home/john/Mail/")
;;
;; Or you can specify it as attribute.
;;
;; : (setq gnus-select-method '(nnml "" (nnir-est-prefix "/home/john/Mail/")))
;;
;; If `directory' attribute is specified, it will be used for prefix.
;; Otherwise, `nnir-est-prefix' will be used.
;;
;; : ;; nnmaildir
;; : (setq gnus-select-method '(nnmaildir "" (directory "~/Maildir"))
;; :                            (nnir-est-index-directory "~/.index"))
;;
;;
;; * Query Format
;;
;; In `gnus-group-make-nnir-group' query, you can specify following
;; attributes in addition to query word.
;;
;; - @cdate>, @cdate< :: Mail date
;; - @author= :: sender
;; - @size>, @size< :: mail size
;; - @title= :: mail subject
;;
;; You can also specify 'AND', 'NOT', 'ANDNOT' or 'OR' for content query word.
;; Queries are case-insensitive.
;;
;; For example, the following query will search for the mails whose subject
;; include 'foobar' and 'foo' in the content, but not 'bar'.
;;
;; : @title=foobar foo ANDNOT bar
;;
;; The following query will search for the mails whose date is between
;; 2013/01/01 and 2013/01/03.
;;
;; : @cdate>2013/01/01 @cdate<2013/01/03

;;; Code:

(require 'nnir)
(eval-when-compile
  (require 'cl))

(defgroup nnir-est nil
  "nnir interface for HyperEstraier."
  :group 'nnir)

(defcustom nnir-est-program "estcmd"
  "*Name of HyperEstraier search executable."
  :type '(string)
  :group 'nnir-est)

(defcustom nnir-est-index-directory (expand-file-name "~/News/casket/")
  "*Index directory for HyperEstraier."
  :type '(directory)
  :group 'nnir-est)

(defcustom nnir-est-additional-switches '("-ord" "@cdate NUMD")
  "*A list of strings, to be given as additional arguments to HyperEstraier.
Default value is to sort by date.  If you want to sort by score, try setting
the value to (\"-ord\" \"@weight NUMD\")."
  :type '(repeat (string))
  :group 'nnir-est)

(defcustom nnir-est-prefix (concat (getenv "HOME") "/Mail/")
  "*The prefix to remove from each file name returned by HyperEstraier.
in order to get a group name (albeit with / instead of .).

For example, suppose that HyperEstraier returns file names such as
\"/home/john/Mail/mail/misc/42\".  For this example, use the following
setting:  (setq nnir-est-prefix \"/home/john/Mail/\")
Note the trailing slash.  Removing this prefix gives \"mail/misc/42\".
`nnir' knows to remove the \"/42\" and to replace \"/\" with \".\" to
arrive at the correct group name, \"mail.misc\"."
  :type '(directory)
  :group 'nnir-est)

(defcustom nnir-est-max -1
  "Maximum number of search output.  Negative number means infinite."
  :type 'integer
  :group 'nnir-est)

(defconst nnir-est-field-keywords
  '("@cdate=" "@cdate>" "@cdate<" "@author=" "@size>" "@size<" "@title=" "@uri=")
  "*List of keywords to do field-search.")

(defvar nnir-est-field-keywords-regexp
  (concat "^" (regexp-opt nnir-est-field-keywords t)))

;; Query converter
(defun nnir-est-query-to-attrs (query)
  "Replace field specs in QUERY to attribute specs of HyperEstraier.
e.g.  '@title=foo @cdate>2011/01/01 foo AND bar'
→ '(\"foo AND bar\" \"-attr\" \"@title STRINC foo\" \"-attr\" \"@cdate NUMLE 2011/01/01\")"
  (let ((query-split (split-string-and-unquote query))
        attr-list query-list)
    (dolist (segment query-split)
      (if (string-match nnir-est-field-keywords-regexp segment)
          (let ((attr (substring (match-string 1 segment) 0 -1))
                (oper (substring (match-string 1 segment) -1))
                (subj (substring segment (match-end 1))))
            (setq attr-list
                  (nconc
                   attr-list
                   (list "-attr"
                         (concat attr
                                 (cond ((and (equal oper "=") (equal attr "cdate"))
                                        " NUMEQ ")
                                       ((equal oper "=") " STRINC ")
                                       ((equal oper "<") " NUMLE ")
                                       (t " NUMGE "))
                                 subj)))))
        (push segment query-list)))
    (cons (mapconcat 'identity (nreverse query-list) " ") attr-list)))

;; HyperEstraier interface
(defun nnir-run-est (query server &optional _group)
  "Run given QUERY against HyperEstraier for SERVER.
Returns a vector of (_GROUP name, file name)
pairs (also vectors, actually).

Tested with HyperEstraier 1.4.13 on a GNU/Linux and MacOS X 10.9 systems."
  (save-excursion
    (let* ((article-pattern (if (string-match "\\`nnmaildir:"
                                              (gnus-group-server server))
                                ":[0-9]+"
                              "^[0-9]+$"))
           artlist
           (qstring (cdr (assq 'query query)))
           (directory (nnir-read-server-parm 'directory server))
           (prefix
            (if directory (file-name-as-directory (expand-file-name directory))
              (nnir-read-server-parm 'nnir-est-prefix server)))
           (score 1) group article
           (process-environment (copy-sequence process-environment))
           (qlist (nnir-est-query-to-attrs qstring))
           (qwords (car qlist))
           (qattrs (cdr qlist))
           )
      ;;
      ;;(message "debug qwords=%s" qwords)
      ;;(setq qwords (encode-coding-string qwords 'shift_jis))
      (setenv "LC_MESSAGES" "C")
      (set-buffer (get-buffer-create nnir-tmp-buffer))
      (erase-buffer)
      (let* ((cp-list
              `( ,nnir-est-program
                 nil			; input from /dev/null
                 t			; output
                 nil			; don't redisplay
                 "search"               ; search command
                 "-vu"                  ; output will be 'ID <TAB> URI'
                 "-max"                 ; maximum output
                 ,(number-to-string
                   (nnir-read-server-parm 'nnir-est-max server))
                 ,@(nnir-read-server-parm 'nnir-est-additional-switches server)
                 ,@qattrs               ; query attributes
                 ;; trailing "/" fails Windows version of Hyperestraier.
                 ,(substring (expand-file-name
                              (nnir-read-server-parm 'nnir-est-index-directory server))
                             0 -1)
                                        ; index directory
                 ,qwords                ; query words
                 ))
             (exitstatus
              (progn
                (message "%s args: %s" nnir-est-program
                         (mapconcat 'identity (cddddr cp-list) " "))
                (apply 'call-process cp-list))))
        (unless (or (null exitstatus)
                    (zerop exitstatus))
          (nnheader-report 'nnir "Couldn't run HyperEstraier: %s" exitstatus)
          ;; HyperEstraier failure reason is in this buffer, show it if
          ;; the user wants it.
          (when (> gnus-verbose 6)
            (display-buffer nnir-tmp-buffer))))

      ;; HyperEstraier output looks something like this:
      ;; % estcmd search -vu -max -1 -ord '@cdate NUMA' ~/News/casket/ foobar
      ;; --------[02D18ACF0353065C]--------
      ;; VERSION	1.0
      ;; NODE	local
      ;; HIT	134
      ;; HINT#1	foobar	134
      ;; TIME	0.000467
      ;; DOCNUM	123161
      ;; WORDNUM	2683336
      ;; VIEW	URI
      ;;
      ;; --------[02D18ACF0353065C]--------
      ;; 69384	file:///home/john/Mail/mail/from-myself/3809
      ;; 75340	file:///home/john/Mail/mail/test/1937
      ;; 44645	file:///home/john/Mail/mail/linux/935
      ;; ....
      ;; For windows,
      ;; --------[02D18ACF5B0A7757]--------
      ;; 28984   file:///C|/Users/john/Mail/mail/misc/7787
      ;; 9226    file:///C|/Users/john/Mail/mail/etsi/3270
      ;; --------[02D18ACF5B0A7757]--------:END

      (goto-char (point-min))
      ;; debug
      ;; (message "buffer=%s" (buffer-string))
      (while (re-search-forward
              "^\\([0-9]+\\)	+file://\\([^ ]+?\\)$" nil t)
        (setq score (1+ score))
        (let ((filename (url-unhex-string (match-string 2))))
          ;; for windows
          (when (string-match "^/\\(.\\)|/" filename)
            (setq filename (concat (match-string 1 filename) ":" (substring filename 3))))
          (setq group (file-name-directory filename)
                article (file-name-nondirectory filename)))
        ;; debug
        ;; (message "score=%s,group=%s,article=%s" score group article)
        ;; make sure article and group is sane
        (when (and (string-match article-pattern article)
                   (not (null group)))
	  (nnir-add-result group article (number-to-string score)
                           prefix server artlist)))
      ;; debug
      ;; (message "prefix=%s,server=%s,artlist=%s" prefix server artlist)
      ;; sort artlist by score
      (apply 'vector
             (sort artlist
                   (function (lambda (x y)
                               (> (nnir-artitem-rsv x)
                                  (nnir-artitem-rsv y)))))))))

(add-to-list 'nnir-engines '(est nnir-run-est nil))

(provide 'nnir-est)

;;; nnir-est.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:
