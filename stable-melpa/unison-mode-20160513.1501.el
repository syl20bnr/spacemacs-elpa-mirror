;;; unison-mode.el --- Syntax highlighting for unison file synchronization program

;; Copyright (C) 2013  Karl Fogelmark

;; Author: Karl Fogelmark <karlfogel@gmail.com>
;; URL: https://github.com/impaktor/unison-mode
;; Package-Version: 20160513.1501
;; Version: 0.1
;; Keywords: symchronization, unison

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
;; Install by putting this file in ~/.emacs.d/elisp/ and this code in your
;; emacs configuration (~/.emacs):
;;
;; (add-to-list 'load-path "~/.emacs.d/elisp/")
;; (autoload 'unison-mode "unison-mode" "my unison mode" t)
;; (setq auto-mode-alist (append '(("\\.prf$" . unison-mode)) auto-mode-alist))

;;  Resources used when making this mode:
;;  http://www.emacswiki.org/emacs/ModeTutorial
;;  http://www.ergoemacs.org/emacs/elisp_syntax_coloring.html
;;  http://www.cis.upenn.edu/~bcpierce/unison/download/releases/stable/unison-manual.html#tutorial


;;; Code:

;; allow users to have hooks with this mode
(defvar unison-mode-hook nil "Hook run after `unison-mode'.")

(defvar unison-command "unison-gtk2" "Command to run graphical unison")

(defvar unison-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'unison-run)
    map)
  "Keymap for `unison-mode'.")


(defvar unison-basic
  (concat "^[ \t]*"
          (regexp-opt
           '("auto" "batch" "fat" "group" "ignore" "ignorenot" "nocreation"
             "nodeletion" "noupdate" "owner" "path" "perms" "root"
             "silent" "terse" "testserver" "times" "version")
           'words))
  "A list of words that indicate basic Unison options.")


(defvar unison-advanced
  (concat "^[ \t]*"
          (regexp-opt
           '("addprefsto" "addversionno" "backup" "backupcurr"
             "backupcurrnot" "backupdir" "backuploc" "backupnot"
             "backupprefix" "backups" "backupsuffix" "confirmbigdel"
             "confirmmerge" "contactquietly" "copymax" "copyprog"
             "copyprogrest" "copyquoterem" "copythreshold" "debug"
             "diff" "dontchmod" "dumbtty" "fastcheck" "follow" "force"
             "forcepartial" "halfduplex" "height" "host"
             "ignorearchives" "ignorecase" "ignoreinodenumbers"
             "ignorelocks" "immutable" "immutablenot" "key"
             "killserver" "label" "links" "log" "logfile" "maxbackups"
             "maxerrors" "maxthreads" "merge" "nocreationpartial"
             "nodeletionpartial" "noupdatepartial" "numericids"
             "prefer" "preferpartial" "repeat" "retry" "rootalias"
             "rshargs" "rsrc" "rsync" "selftest" "servercmd" "socket"
             "showarchive" "sortbysize" "sortfirst" "sortlast"
             "sortnewfirst" "sshargs" "sshcmd" "stream" "ui" "unicode"
             "xferbycopying")
           'words))
  "A list of words that include advanced Unison options.")


(defvar unison-profile
  (concat "^[ \t]*"
          (regexp-opt
           '("include")
           'words))
  "Words that modify profile information.")


(defvar unison-matcher
  (concat "=[ \t]*"
          (regexp-opt
           '("Path" "Name" "Regex" "BelowPath")
           'words))
  "List of words that heads a Unison matcher.

A Unison matcher contains one of these keywords followed by a glob.
It is only meaningful as the first word in certain options
\(such as ignore, ignorenot &c.\)
but the syntax highlighting does not reflect this fully.")


;; "<>" is used to match only whole words, i.e. preceeded and/or
;; followed by new line, or space.
(defvar unison-font-lock-keywords
  `(;; Unison-modes comment only works
    ;; when # is the first non-whitespace character.
    ("^[ \t]*\\(#.*\\)$" . font-lock-comment-face)
    (,unison-basic    . font-lock-function-name-face)
    (,unison-advanced . font-lock-builtin-face)
    (,unison-matcher  1 font-lock-keyword-face)
    (,unison-profile  . font-lock-type-face)
    ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)))


;; Tell emacs what is a word, etc. Usied by syntax highlighting
;; package font-lock function will read this. Character (to modify)
;; are prefixed with a "?".
(defvar unison-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_  "w"  st)  ; word constituent
    st)
  "Syntax table for unison-mode")


;; entry function, to be called by emacs when we enter the mode:
;;;###autoload
(define-derived-mode unison-mode prog-mode "Unison"
  "Major mode for font-lcoking unison configuration files"
  :syntax-table unison-mode-syntax-table

  (set (make-local-variable 'font-lock-defaults)      ; font lock
       '(unison-font-lock-keywords))

  ;; comment syntax for `newcomment.el'
  (set (make-local-variable 'comment-start)      "# ")
  (set (make-local-variable 'comment-end)        "")
  (set (make-local-variable 'comment-start-skip) "#+\\s-*")

  (run-hooks 'unison-mode-hook))  ; run user hooks last.


(defun unison-run ()
  "Run an instance of Unison"
  (interactive)
  (shell-command unison-command))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.prf$" . unison-mode))

(provide 'unison-mode)
;;; unison-mode.el ends here
