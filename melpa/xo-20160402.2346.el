;;; xo.el --- XO linter integration with compilation mode

;; Copyright (C) 2016 J.A

;; Author: J.A <jer.github@gmail.com>
;; Keywords: processes
;; Package-Version: 20160402.2346
;; Version: 1.0

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

;; Need to call xo-setup for this to work as intented.
;; This only works when launching xo with the --compact flag.

;;; Code:

(provide 'xo)

(defun xo-setup ()
  (add-to-list 'compilation-error-regexp-alist-alist
	       '(xo "\\(/\\(?:\\w+/\\)+\\w*\\.js\\): line \\([1-9]*\\), col \\([1-9]*\\), \\w+ .*" 1 2 3))

  (add-to-list 'compilation-error-regexp-alist 'xo))

;;; xo.el ends here
