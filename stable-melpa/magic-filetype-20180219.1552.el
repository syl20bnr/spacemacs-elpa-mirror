;;; magic-filetype.el --- Enhance filetype major mode -*- mode: emacs-lisp; lexical-binding: t -*-
;;; vim: set ft=lisp:

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 9 Aug 2015
;; Version: 0.2.0
;; Package-Version: 20180219.1552
;; Keywords: emulations vim ft file magic-mode
;; Homepage: https://github.com/zonuexe/magic-filetype.el
;; Package-Requires: ((emacs "24") (s "1.9.0"))

;; This file is NOT part of GNU Emacs.

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

;; `magic-filetype' parse Vim-style file type header.
;; For example, in executable JavaScript(node) file is...

;;   #!/usr/bin/env node
;;   // vim:set ft=javascript:
;;   (function(){
;;       "use strict";
;;        ....

;; put into your own .emacs file (init.el)

;;   (magic-filetype-enable-vim-filetype)

;; `magic-filetype-exemplary-filename-alist' have dummy filename that is delegate of major-mode.

;;; Code:

(require 's)
(eval-when-compile
  (require 'rx))

(defcustom  magic-filetype-vim-filetype-line-re
  "vim: *set +\\(?:.*?\\)\\(?:ft\\|filetype\\)=\\([^ :]+\\)\\(?:.*?\\):$"
  "Regexp of Vim filetype line."
  :group 'magic-filetype
  :type  'regexp)

(defcustom magic-filetype-exemplary-filename-alist
  '((applescript  . ("/dir/file.scpt"))
    (basic        . ("/dir/file.bas"))
    (c            . ("/dir/file.c"))
    (caml         . ("/dir/file.ml"))
    (changelog    . ("/dir/Changelog"))
    (clojure      . ("/dir/file.clj"))
    (coffee       . ("/dir/file.coffee"))
    (commonlisp   . ("/dir/file.lisp"))
    (cpp          . ("/dir/file.cpp"))
    (crystal      . ("/dir/file.cr"))
    (cs           . ("/dir/file.cs"))
    (csh          . (sh-mode . (lambda () (sh-set-shell "csh"))))
    (css          . ("/dir/file.css"))
    (csv          . ("/dir/file.csv"))
    (dart         . ("/dir/file.dart"))
    (debchangelog . ("/dir/debian/changelog"))
    (debcontrol   . ("/dir/debian/control"))
    (debsources   . ("/dir/sources.list"))
    (delphi       . ("/dir/file.pas"))
    (dosbatch     . ("/dir/file.bat"))
    (dosini       . ("/dir/file.ini"))
    (elixir       . ("/dir/file.exs"))
    (emacslisp    . ("/dir/file.el"))
    (erlang       . ("/dir/file.erl"))
    (es6          . ("/dir/file.es6"))
    (esqlc        . (sql-mode . (lambda () (sql-set-product 'informix))))
    (freebasic    . ("/dir/file.fb"))
    (fsharp       . ("/dir/file.fs"))
    (gdb          . (gdb-script-mode))
    (go           . ("/dir/file.go"))
    (hack         . ("/dir/file.hh"))
    (haml         . ("/dir/file.haml"))
    (haskell      . ("/dir/file.hs"))
    (html         . ("/dir/file.html"))
    (java         . ("/dir/file.java"))
    (javascript   . ("/dir/file.js"))
    (json         . ("/dir/file.json"))
    (lisp         . ("/dir/file.lisp"))
    (nadeshiko    . ("/dir/file.nako"))
    (nim          . ("/dir/file.nim"))   ;; Nim https://nim-lang.org/
    (m4           . ("/dir/file.m4"))
    (markdown     . ("/dir/file.md"))
    (msql         . (sql-mode))
    (mysql        . (sql-mode . (lambda () (sql-set-product 'mysql))))
    (ocaml        . ("/dir/file.ocaml"))
    (org          . ("/dir/file.org"))
    (pascal       . ("/dir/file.pas"))
    (perl         . ("/dir/file.pl"))
    (perl6        . ("/dir/file.p6"))
    (php          . ("/dir/file.php"))
    (plsql        . (sql-mode . (lambda () (sql-set-product 'oracle))))
    (python       . ("/dir/file.py"))
    (rst          . ("/dir/file.rst"))
    (rust         . ("/dir/file.rs"))    ;; Rust https://www.rust-lang.org/
    (ruby         . ("/dir/file.rb"))
    (sass         . ("/dir/file.sass"))
    (scala        . ("/dir/file.scala"))
    (scheme       . ("/dir/file.scm"))
    (scss         . ("/dir/file.scss"))
    (standardml   . ("/dir/file.sml"))
    (sql          . ("/dir/file.sql"))
    (sqlinformix  . (sql-mode . (lambda () (sql-set-product 'informix))))
    (sqloracle    . (sql-mode . (lambda () (sql-set-product 'oracle))))
    (swift        . ("/dir/file.swift"))
    (tcsh         . (sh-mode . (lambda () (sh-set-shell "tcsh"))))
    (texinfo      . ("/dir/file.texi"))
    (text         . ("/dir/file.txt"))
    (typescript   . ("/dir/file.ts"))
    (vb           . ("/dir/file.vb"))
    (vim          . ("/dir/file.vim"))
    (xhtml        . ("/dir/file.xhtml"))
    (xml          . ("/dir/file.xml"))
    (yaml         . ("/dir/file.yml"))
    (zsh          . (sh-mode . (lambda () (sh-set-shell "zsh")))))
  "Alist of Vim-filetype vs dummy filename."
  :group 'magic-filetype
  :type  '(alist :key-type symbol :value-type list))

(defcustom magic-filetype-auto-mode-alist
  (eval-when-compile
    `((caml       . ("\\.ml[iyl]?\\'"))
      (cpp        . (,(rx (or ".cc" ".cxx" ".c++") string-end)))
      (delphi     . ("\\.dp[kr]\\'"))
      (json       . (("/composer.lock" "/.jshintrc" "/.stylintrc")))
      (markdown   . ("\\.mk?dn?\\'" "\\.m\\(ark\\)?do?wn\\'"))
      (perl       . ("\\.pm\\'"))
      (python     . (("/SConstruct" "/SConscript" "/wscript")))
      (ruby       . (,(rx (or ".gemspec" ".thor" ".rabl" ".ru" ".jbuilder" ".podspec") string-end)
                     ("/Appraisals" "/Berksfile" "/Brewfile" "/Buildfile" "/Capfile" "/Dangerfile"
                      "/Deliverfile" "/Fastfile" "/Gemfile" "/Guardfile" "/Jarfile" "/Mavenfile"
                      "/Puppetfile" "/Podfile" "/Rakefile" "/Snapfile" "/Thorfile" "/Vagrantfile"
                      "/buildfile")))
      (scheme     . (,(rx (or ".rkt" ".ss" ".sls" ".sld") string-end)))
      (vb         . (,(rx (or ".frm" ".bas" ".cls" ".vb" ".rvb") string-end)))
      (xml        . ("\\.plist\\'"))
      (yaml       . ("\\.yaml\\'"))))
  "Alist of Vim-filetype vs auto-mode patterns."
  :group 'magic-filetype
  :type  '(alist :key-type symbol :value-type list))

(defun magic-filetype-collect-major-modes ()
  "Retturn list of MAJOR-MODEs by `auto-mode-alist'."
  (cl-loop for elm in auto-mode-alist
           if (symbolp (cdr elm))
           collect (cdr elm)))

;;;###autoload
(defun magic-filetype-major-mode-from-language-name (lang-name)
  "Invoke `major-mode' from `LANG-NAME'."
  (interactive
   (list
    (completing-read "Choose language: " magic-filetype-exemplary-filename-alist)))
  (when lang-name
    (let* ((data (cdr (assq (intern lang-name) magic-filetype-exemplary-filename-alist)))
           (file (car data))
           (new-major-mode
            (if (symbolp file) file
              (assoc-default file auto-mode-alist #'string-match))))
      (when new-major-mode
        (funcall new-major-mode)
        (when (cdr data)
          (funcall (cdr data)))
        new-major-mode))))

;;;###autoload
(defun magic-filetype-vim-filetype-magic-mode (&optional ft)
  "Invoke `major-mode' by Vim-style `FT' file header."
  (interactive)
  (let* ((bufs (buffer-substring-no-properties (point-min) (point-max)))
         (lang (or ft (cadr (s-match magic-filetype-vim-filetype-line-re bufs)))))
    (when lang
      (magic-filetype-major-mode-from-language-name lang))))

;;;###autoload
(defun magic-filetype-enable-vim-filetype (&optional force)
  "Turn on magic-mode by Vim-style file header."
  (interactive)
  (add-to-list
   (if force 'magic-mode-alist 'magic-fallback-mode-alist)
   '(magic-filetype-vim-filetype-magic-mode . magic-filetype-vim-filetype-magic-mode)))

;;;###autoload
(defun magic-filetype-major-mode-of (lang-name)
  "Get MAJOR-MODE from `LANG-NAME'."
  (let* ((data (cdr (assq lang-name magic-filetype-exemplary-filename-alist)))
         (file (car data))
         (new-major-mode
          (if (symbolp file) file
            (assoc-default file auto-mode-alist #'string-match))))
    (unless new-major-mode (error "Unknown LANG-NAME"))
    (if (cdr data)
        (lambda () (funcall new-major-mode) (funcall (cdr data)))
      new-major-mode)))

;;;###autoload
(defun magic-filetype-set-auto-mode (lang-name)
  "Set `auto-mode-alist' by `LANG-NAME'."
  (let* ((data           (assq lang-name magic-filetype-auto-mode-alist))
         (new-major-mode (magic-filetype-major-mode-of (car data))))
    (mapc
     (lambda (ext)
       (add-to-list 'auto-mode-alist
                    (cons (if (listp ext) (concat (regexp-opt ext) "\\'") ext)
                          new-major-mode)))
     (cdr data))))

;;;###autoload
(defun magic-filetype-reload-major-mode ()
  "Reload current major mode."
  (interactive)
  (let ((current-mode major-mode))
    (fundamental-mode)
    (funcall current-mode)
    current-mode))

(provide 'magic-filetype)
;;; magic-filetype.el ends here
