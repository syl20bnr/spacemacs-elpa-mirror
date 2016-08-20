;;; flymake-google-cpplint.el --- Help to comply with the Google C++ Style Guide

;; Copyright 2014 Akiha Senda

;; Author: Akiha Senda <senda.akiha@gmail.com>
;; URL: https://github.com/senda-akiha/flymake-google-cpplint/
;; Package-Version: 20140205.525
;; Created: 02 February 2014
;; Version: 1.0.0
;; Keywords: flymake, C, C++
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

;; If you're want to write code according to the [Google C++ Style Guide](http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml), this will help a great deal.

;; I recommend that the package [google-c-style](http://melpa.milkbox.net/#/google-c-style) also installed with.

;; For more infomations, please check the GitHub
;; https://github.com/senda-akiha/flymake-google-cpplint/

;;; Code:

(require 'flymake-easy)

(defconst flymake-google-cpplint-err-line-patterns
  '(("\\(.*\\):\\([[:digit:]]+\\): *\\(.*\\)\r?\n"
     1 2 nil 3)))

(defcustom flymake-google-cpplint-verbose ""
  "verbose=#
     Specify a number 0-5 to restrict errors to certain verbosity levels."
  :type 'string
  :group 'flymake-google-cpplint)

(defcustom flymake-google-cpplint-filter ""
  "filter=-x,+y,...
     Specify a comma-separated list of category-filters to apply: only
     error messages whose category names pass the filters will be printed.
     (Category names are printed with the message and look like
     \"[whitespace/indent]\".)  Filters are evaluated left to right.
     \"-FOO\" and \"FOO\" means \"do not print categories that start with FOO\".
     \"+FOO\" means \"do print categories that start with FOO\".

     Examples: --filter=-whitespace,+whitespace/braces
               --filter=whitespace,runtime/printf,+runtime/printf_format
               --filter=-,+build/include_what_you_use

     To see a list of all the categories used in cpplint, pass no arg:
        --filter="
  :type 'string
  :group 'flymake-google-cpplint)

(defcustom flymake-google-cpplint-counting ""
  "counting=total|toplevel|detailed
     The total number of errors found is always printed. If
     'toplevel' is provided, then the count of errors in each of
     the top-level categories like 'build' and 'whitespace' will
     also be printed. If 'detailed' is provided, then a count
     is provided for each category like 'build/class'."
  :type 'string
  :group 'flymake-google-cpplint)

(defcustom flymake-google-cpplint-root ""
  "root=subdir
     The root directory used for deriving header guard CPP variable.
     By default, the header guard CPP variable is calculated as the relative
     path to the directory that contains .git, .hg, or .svn.  When this flag
     is specified, the relative path is calculated from the specified
     directory. If the specified directory does not exist, this flag is
     ignored.

     Examples:
       Assuing that src/.git exists, the header guard CPP variables for
       src/chrome/browser/ui/browser.h are:

       No flag => CHROME_BROWSER_UI_BROWSER_H_
       --root=chrome => BROWSER_UI_BROWSER_H_
       --root=chrome/browser => UI_BROWSER_H_"
  :type 'string
  :group 'flymake-google-cpplint)

(defcustom flymake-google-cpplint-linelength ""
  "linelength=digits
     This is the allowed line length for the project. The default value is
     80 characters.

     Examples:
       --linelength=120"
  :type 'string
  :group 'flymake-google-cpplint)

(defcustom flymake-google-cpplint-extensions ""
  "extensions=extension,extension,...
     The allowed file extensions that cpplint will check

     Examples:
       --extensions=hpp,cpp"
  :type 'string
  :group 'flymake-google-cpplint)

(defcustom flymake-google-cpplint-command (executable-find "cpplint.py")
  "The name of the cpplint executable."
  :type 'string
  :group 'flymake-google-cpplint)

(defcustom flymake-google-cpplint-location 'inplace
  "Where to create the temporary copy: one of 'tempdir or 'inplace (default)."
  :type `(choice
          (const :tag "In place" inplace)
          (const :tag "Temporary location" tempdir))
  :group 'flymake-google-cpplint)

(defun flymake-google-cpplint-build-command-line (filename)
  "Construct a command that flymake can use to check C/C++ source."
  (list flymake-google-cpplint-command
        flymake-google-cpplint-verbose
        flymake-google-cpplint-filter
        flymake-google-cpplint-counting
        flymake-google-cpplint-root
        flymake-google-cpplint-linelength
        flymake-google-cpplint-extensions
        filename))

;;;###autoload
(defun flymake-google-cpplint-load ()
  "Configure flymake mode to check the current buffer's C/C++ source."
  (interactive)
  (flymake-easy-load 'flymake-google-cpplint-build-command-line
                     flymake-google-cpplint-err-line-patterns
                     flymake-google-cpplint-location
                     "cpp"))

(provide 'flymake-google-cpplint)

;;; flymake-google-cpplint.el ends here
