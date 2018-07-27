;;; edbi-sqlite.el --- Open sqlite files with edbi

;; Copyright (C) 2014-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/edbi-sqlite
;; Package-Version: 20160221.1923
;; Version: 1.0.0
;; Package-Requires: ((emacs "24") (edbi "0.1.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the README for more details.

;;; Code:

(require 'edbi)

;;;###autoload
(defun edbi-sqlite (file)
  "Open sqlite FILE with `edbi'."
  (interactive (list (read-file-name "SQLite: ")))
  (let* ((uri (format "dbi:SQLite:dbname=%s" (file-truename file)))
         (data-source (edbi:data-source uri nil nil))
         (conn (edbi:start)))
    (edbi:connect conn data-source)
    (edbi:dbview-open conn)))

(provide 'edbi-sqlite)

;;; edbi-sqlite.el ends here
