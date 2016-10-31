;;; gxref.el --- xref backend using GNU Global. -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Dedi Hirschfeld

;; Author: Dedi Hirschfeld
;; URL: https://github.com/dedi/gxref
;; Package-Version: 20161030.1223
;; Keywords: xref, global
;; Version: 0.1
;; Package-Requires: ((emacs "25"))

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
;; A pretty simple (but, at least for me, effective) backend for xref
;; library, using GNU Global.
;;
;; Prerequisites:
;;
;; * GNU Global.
;; * Emacs version >= 25.1
;;
;; Disclaimers:
;;
;; Because the xref API in Emacs 25.1 is experimental, it's likely to
;; change in ways that will break this package.  I will try to
;; keep up with API changes.
;;
;; Installing:
;;
;; Add something like the following to your init.el file:
;; (add-to-list 'xref-backend-functions 'gxref-xref-backend)
;;
;; TODO:
;;
;; Generate DB at root
;; project.el integration.
;; Use process-environment to pass GTAGSROOT/GTAGSLABEL/etc.
;;

;;; Code:

(require 'cl-lib)
(require 'xref)

(defgroup gxref nil
  "XRef backend using GNU Global."
  :group 'xref)

(defcustom gxref-global-exe "global"
  "Path to GNU Global executable."
  :type 'string
  :group 'gxref)

(defcustom gxref-gtags-exe "gtags"
  "Path to GTAGS executable."
  :type 'string
  :group 'gxref)

(defcustom gxref-update-db-on-save t
  "A flag indicating whether to update the GTAGS database when a file is saved."
  :type 'boolean
  :group 'gxref)


(defun gxref--global-to-list (args)
  "Run GNU Global in an external process.  Return the output as a
list of strings.  Return nil if an error occured.  ARGS is the
list of arguments to use when running global"
  (condition-case nil
      (apply #'process-lines gxref-global-exe args)
    (error nil)))


(defun gxref--db-path ()
  "Return the GTAGS DB path for the current project.  Return nil if none."
  (car (gxref--global-to-list '("-p"))))


(defun gxref--make-xref-from-file-loc (file line column desc)
  "Create an xref object pointing to the given file location.
FILE, LINE, and COLUMN point to the location of the xref, DESC is
a description of it."
  (xref-make desc (xref-make-file-location file line column)))


(defun gxref--make-xref-from-gtags-x-line (ctags-x-line)
  "Create and return an xref object pointing to a file location,
based on global -x output line CTAGS-X-LINE.  If the line does
not match the expected format, return nil."
  (if (string-match "^\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\]+\\)[ \t]+\\(.*\\)" ctags-x-line)
      (gxref--make-xref-from-file-loc (match-string 3 ctags-x-line)
            (string-to-number (match-string 2 ctags-x-line))
            0
            (match-string 4 ctags-x-line))
    nil))


(defun gxref--find-symbol (symbol &rest args)
  "Run GNU Global to find a symbol SYMBOL.  Return the results as a list
of xref location objects.  ARGS are any additional command line
arguments to pass to GNU Global."
  (let* ((process-args
          (append args
                  (list "-x" "-a" (shell-quote-argument symbol))))
         (global-output (gxref--global-to-list process-args)))
    (mapcar #'gxref--make-xref-from-gtags-x-line global-output)
    ))


(defun gxref-update-db ()
  "Update GTAGS project database for current project."
  (interactive)
  (unless (gxref--db-path) (error "Not under a GTAGS project"))
  ;; TODO: We shoule probably call `call-process' directly, not
  ;; `gxref--global-to-list'`
  (gxref--global-to-list '("-u")))

(defun gxref-single-update-db ()
  "Update GTAGS project database for the current file."
  (interactive)
  (unless (gxref--db-path) (error "Not under a GTAGS project"))
  (unless (buffer-file-name) (error "Buffer has no file associated with it"))
  ;; `gxref--global-to-list'`
  (gxref--global-to-list (list "--single-update" (buffer-file-name))))


(defun gxref--after-save-hook ()
  "After save hook to maybe update the GTAGS database with changed data."
  (when (and gxref-update-db-on-save (buffer-file-name) (gxref--db-path))
    (gxref-single-update-db)))

(add-hook 'after-save-hook 'gxref--after-save-hook)


;;
;; gxref backend definition.
;;

;;;###autoload
(defun gxref-xref-backend ()
  "Gxref backend for Xref."
  'gxref)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql gxref)))
  (symbol-name (symbol-at-point)))

(cl-defmethod xref-backend-definitions ((_backend (eql gxref)) symbol)
  (gxref--find-symbol symbol "-d"))

(cl-defmethod xref-backend-references ((_backend (eql gxref)) symbol)
  (gxref--find-symbol symbol "-r"))

(cl-defmethod xref-backend-apropos ((_backend (eql gxref)) symbol)
  (gxref--find-symbol symbol "-g"))

(cl-defmethod
    xref-backend-identifier-completion-table ((_backend (eql gxref)))
  "Return a list of terms for completions taken from the symbols in the current buffer.
The current implementation returns all the words in the buffer,
which is really sub optimal."
  (gxref--global-to-list '("-c")))


(provide 'gxref)
;;; gxref.el ends here
