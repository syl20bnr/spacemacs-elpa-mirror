;;; ghc-imported-from.el --- Haskell documentation lookup with ghc-imported-from  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  David Raymond Christiansen, Galois, Inc

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Maintainer: David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: languages
;; Package-Version: 20141124.1132
;; Package-Requires: ((emacs "24.1"))
;; Version: 0.1.2

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

;; This is an Emacs frontend for ghc-imported-from, a command-line
;; tool for finding Haddock documentation for symbols in Haskell
;; source files. Because it uses cabal and ghc, it's sandbox-aware.

;;; Code:

(defgroup ghc-imported-from nil
  "Customization for `ghc-imported-from'"
  :group 'haskell
  :prefix 'ghc-imported-from)

(defcustom ghc-imported-from-command-name "ghc-imported-from"
  "The command to run for ghc-imported-from"
  :type 'file
  :group 'ghc-imported-from)

;;;###autoload
(defun ghc-imported-from-haddock-for-symbol-at-point ()
  "Look up the Haddock for the symbol under point in a Haskell
buffer using the ghc-imported-from command-line utility.

The standard function `browse-url' is used to open the Haddock
documentation URL. Customize `browse-url-browser-function' to
change this.

If ghc-imported-from is not on your $PATH, customize
`ghc-imported-from-command-name' "
  (interactive)
  (let* ((symbol (thing-at-point 'symbol))
         (dir (file-name-directory (buffer-file-name)))
         (file (file-name-nondirectory (buffer-file-name)))
         (line (line-number-at-pos))
         (column (current-column))
         (module (save-excursion
                   (goto-char (point-min))
                   (if (re-search-forward "^module\\s-+\\([A-Za-z0-9.]+\\)\\s-"
                                          nil t)
                       (match-string 1)
                     (error "Couldn't determine module name for buffer")))))
    (cond ((null symbol)
           (error "There is no Haskell symbol at point"))
          ((null dir)
           (error "Couldn't determine the current working directory"))
          ((null file)
           (error "Couldn't determine file name"))
          (t (with-temp-buffer
               (erase-buffer)
               (cd dir)
               (call-process ghc-imported-from-command-name
                             nil
                             t
                             nil
                             file
                             module
                             symbol
                             (number-to-string line)
                             (number-to-string column))
               
               (cond ((progn (goto-char (point-min))
                             (re-search-forward "^SUCCESS:\\s-+\\(.+\\)$" nil t))
                      (browse-url (match-string 1)))
                     ((progn (goto-char (point-min))
                             (re-search-forward "^FAIL:\\s-+\\(.+\\)$" nil t))
                      (error "Couldn't find Haddock: %s" (match-string 1)))
                     (t (error "Couldnt' find Haddock."))))))))




(provide 'ghc-imported-from)
;;; ghc-imported-from.el ends here
