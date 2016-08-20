;;; flymake-cppcheck.el --- Flymake work with Cppcheck for C/C++

;; Copyright 2014 Akiha Senda

;; Author: Akiha Senda <senda.akiha@gmail.com>
;; URL: https://github.com/senda-akiha/flymake-cppcheck/
;; Package-Version: 20140415.557
;; Created: 13 January 2014
;; Version: 1.0.0
;; Keywords: flymake, cppcheck, C, C++
;; Package-Requires: ((flymake-easy "0.9"))

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Please check the GitHub
;; (https://github.com/senda-akiha/flymake-cppcheck/)
;; for more information.

;;; Code:

(require 'flymake-easy)

(defconst flymake-cppcheck-err-line-patterns
  '(("\\(.*\\)::\\([[:digit:]]+\\)::\\(.*\\)\r?\n"
     1 2 nil 3)))

(defconst flymake-cppcheck-template "{file}::{line}::{message}"
  "Output format template.")

(defcustom flymake-cppcheck-enable "error"
  "By default only error messages are shown.
Through the --enable command more checks can be enabled."
  :type 'string
  :group 'flymake-cppcheck)

(defcustom flymake-cppcheck-command (or (executable-find "cppcheck") "")
  "The name of the cppcheck executable."
  :type 'string
  :group 'flymake-cppcheck)

(defcustom flymake-cppcheck-location 'inplace
  "Where to create the temporary copy: one of 'tempdir or 'inplace (default)."
  :type `(choice
          (const :tag "In place" inplace)
          (const :tag "Temporary location" tempdir))
  :group 'flymake-cppcheck)

(defun flymake-cppcheck-build-command-line (filename)
  "Construct a command that flymake can use to check C/C++ source."
  (list flymake-cppcheck-command "--quiet"
        (concat "--template=" flymake-cppcheck-template)
        (if (string-match "error" flymake-cppcheck-enable)
            "" (concat "--enable=" flymake-cppcheck-enable))
        filename))

;;;###autoload
(defun flymake-cppcheck-load ()
  "Configure flymake mode to check the current buffer's C/C++ source."
  (interactive)
  (flymake-easy-load 'flymake-cppcheck-build-command-line
                     flymake-cppcheck-err-line-patterns
                     flymake-cppcheck-location
                     "cpp"))

(provide 'flymake-cppcheck)

;;; flymake-cppcheck.el ends here
