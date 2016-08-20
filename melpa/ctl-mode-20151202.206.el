;;; ctl-mode.el --- Major mode for editing GrADS script files

;; Author: Joe Wielgosz <joew@cola.iges.org>
;; Created: 2 Oct 2003
;; Keywords: GrADS script major-mode
;; Package-Version: 20151202.206
;; Version: 0.1

;; Copyright (C) Joe Wielgosz <joew@cola.iges.org>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; Based on wpdl-mode-el, a major mode for editing WPDL files
;; by Scott Andrew Borton <scott@pp.htv.fi>,
;; which is an example used in a tutorial about Emacs
;; mode creation. The tutorial can be found here:
;; http://two-wugs.net/emacs/mode-tutorial.html

;;; Code:
(defvar ctl-mode-hook nil)
(defvar ctl-mode-map nil
  "Keymap for ctl-mode.")

(if ctl-mode-map nil
  (setq ctl-mode-map (make-keymap)))

(setq auto-mode-alist
      (append
       '(("\\.ctl\\'" . ctl-mode))
       auto-mode-alist))

(defconst ctl-font-lock-keywords-1
  (list
   ;; generated with (regexp-opt '("dset" "dtype" "index" "title"
   ;; "undef" "options" "xdef" "ydef" "zdef" "tdef" "vars" "endvars"
   ;; "fileheader" "theader" "xyheader" "unpack") t)

   '("\\<\\(d\\(?:set\\|type\\)\\|endvars\\|fileheader\\|index\\|stnmap\\|options\\|t\\(?:def\\|header\\|itle\\)\\|un\\(?:def\\|pack\\)\\|vars\\|x\\(?:def\\|yheader\\)\\|[yz]def\\|D\\(?:SET\\|TYPE\\)\\|ENDVARS\\|FILEHEADER\\|INDEX\\|STNMAP\\|OPTIONS\\|T\\(?:DEF\\|HEADER\\|ITLE\\)\\|UN\\(?:DEF\\|PACK\\)\\|VARS\\|X\\(?:DEF\\|YHEADER\\)\\|[YZ]DEF\\)\\>" . font-lock-keyword-face)



   ;; builtins: say prompt pull sublin subwrd substr read write close
   '("\\<\\(365_day_calendar\\|b\\(?:ig_endian\\|yteswapped\\)\\|cray_32bit_ieee\\|grib\\|hdfsds\\|l\\(?:evels\\|i\\(?:near\\|ttle_endian\\)\\)\\|netcdf\\|s\\(?:equential\\|tation\\)\\|template\\|[yz]rev\\|365_DAY_CALENDAR\\|B\\(?:IG_ENDIAN\\|YTESWAPPED\\)\\|CRAY_32BIT_IEEE\\|GRIB\\|HDFSDS\\|L\\(?:EVELS\\|I\\(?:NEAR\\|TTLE_ENDIAN\\)\\)\\|NETCDF\\|S\\(?:EQUENTIAL\\|TATION\\)\\|TEMPLATE\\|[YZ]REV\\)\\>" . font-lock-builtin-face)

   ;; unused faces:
   ;;   '("\\('\\d*'\\)" . font-lock-variable-name-face)
   '("\\<\\([-e.0-9]+\\)\\>" . font-lock-constant-face)
   "Highlighting expressions for ctl-mode."))

(defvar ctl-font-lock-keywords ctl-font-lock-keywords-1
  "Default highlighting expressions for ctl-mode.")

(defvar ctl-mode-syntax-table nil
  "Syntax table for ctl-mode.")

(defun ctl-create-syntax-table ()
  (if ctl-mode-syntax-table
      ()
    (setq ctl-mode-syntax-table (make-syntax-table))

    ;; This is added so entity names with underscores and periods can be more easily parsed
    (modify-syntax-entry ?_ "w" ctl-mode-syntax-table)

    ;; Comment syntax
    (modify-syntax-entry ?* "<" ctl-mode-syntax-table)
    (modify-syntax-entry ?\n ">" ctl-mode-syntax-table))

  (set-syntax-table ctl-mode-syntax-table))

;;;###autoload
(defun ctl-mode ()
  "Major mode for editing GrADS descriptor files."
  (interactive)
  (kill-all-local-variables)
  (ctl-create-syntax-table)

  ;; Set up font-lock
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
        '(ctl-font-lock-keywords))

  ;; Register our indentation function
  ;; (make-local-variable 'indent-line-function)
  ;; (setq indent-line-function 'ctl-indent-line)

  (setq major-mode 'ctl-mode)
  (setq mode-name "GrADS descriptor file")
  (run-hooks 'ctl-mode-hook))

(provide 'ctl-mode)

;;; ctl-mode.el ends here
