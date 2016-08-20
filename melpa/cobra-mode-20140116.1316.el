;;; cobra-mode.el --- Major mode for .NET-based Cobra language

;; Copyright (C) 2014  Taylor "Nekroze" Lawson

;; Author: Taylor "Nekroze" Lawson
;; Keywords: languages
;; Package-Version: 20140116.1316
;; URL: http://github.com/Nekroze/cobra-mode
;; Version: 1.0.1

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

;; Provides a major mode for the .NET-based Cobra language. The mode
;; is based on `python-mode'.

;;; Code:

;; Load python-mode if available, otherwise use builtin emacs python package
(eval-when-compile
  (or (require 'python-mode nil t)
      (require 'python)))

(defvar cobra-font-lock-keywords
  `((,(regexp-opt '("vari" "as" "require" "ensure" "body" "test" "use" "catch" "ref" "shared" "cue"
                    "get" "set" "throw" "as" "using" "namespace" "of" "stop" "out" "branch" "on"
                    "pro" "inherits" "protected" "final" "override" "public" "constant" "invariant"
                    "implies" "var" "success" "enum" "interface" "implements" "<>" "result" "old"
                    ) 'words)
     1 font-lock-keyword-face)

    (,(regexp-opt '("String" "nil" "decimal" "number" "dynamic") 'words)
     1 font-lock-builtin-face))
  "Additional font lock keywords for Cobra mode.")

;;;###autoload
(define-derived-mode cobra-mode python-mode "Cobra"
  "Major mode for Cobra development, derived from Python mode."
  (setcar font-lock-defaults
          (append python-font-lock-keywords cobra-font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cobra\\'" . cobra-mode))

(provide 'cobra-mode)
;;; cobra-mode.el ends here
