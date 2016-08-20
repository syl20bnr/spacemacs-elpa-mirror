;;; neon-mode.el --- Simple major mode for editing neon files

;; Copyright (C) 2015 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 1.0.0
;; Package-Version: 20160811.216
;; Created: 26th March 2015
;; Keywords: conf

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing neon files: http://ne-on.org/

;; Currently only provides syntax highlighting, otherwise derives from
;; `conf-colon-mode'.

;;; Code:

(require 'conf-mode)

(defvar conf-neon-font-lock-keywords
  `(
    (,(concat "\\<" (regexp-opt '("true" "True" "TRUE" "yes" "Yes"
                                  "YES" "on" "On" "ON" "false" "False"
                                  "FALSE" "no" "No" "NO" "off" "Off" "OFF"))
              "\\>")
     0 'font-lock-constant-face)
    ("\\<%\\(.*?\\)%\\>" 0 'font-lock-function-name-face)
    ("@\\_<\\(.*?\\)\\_>" 0 'font-lock-type-face)
    ,@conf-colon-font-lock-keywords))

;;;###autoload
(define-derived-mode neon-mode conf-colon-mode "Conf[Neon]"
  "Conf Mode starter for Neon files.
\"Assignments\" are with `:'.
For details see `conf-mode'."
  (conf-mode-initialize "#" 'conf-neon-font-lock-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.neon\\'" . neon-mode))

(provide 'neon-mode)
;;; neon-mode.el ends here
