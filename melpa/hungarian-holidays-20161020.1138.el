;;; hungarian-holidays.el --- Adds a list of Hungarian public holidays to Emacs calendar

;; Copyright (C) 2016 Gergely Polonkai <gergely@polonkai.eu>

;; Author: Gergely Polonkai <gergely@polonkai.eu>
;; Maintainer: Gergely Polonkai <gergely@polonkai.eu>
;; Version: 0.0.1
;; Package-Version: 20161020.1138
;; Created: 20th October 2016
;; Keywords: calendar

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

;;; This package adds Hungarian (https://en.wikipedia.org/wiki/Hungary)
;;; public holidays to the Emacs calendar.
;;;
;;; If you have `org-agenda-include-diary` set to `t`, these will be
;;; also listed in the `org-agenda` view.

;;; Installation

;;; This package is available on MELPA, just `M-x` `package-install`
;;; `hungarian-holidays`. If you want to install it manually, clone
;;; this repository somewhere, add it to `load-path`, and add
;;; `(require 'hungarian-holidays)` to `.emacs`.

;;; Configuration

;;; Add a call to `(hungarian-holidays-add)` somewhere in your
;;; `.emacs`.  Note that this must be called *before* Emacs calendar
;;; is loaded.

;;; Attribution

;;; This package is based on David Chkhikvadze’s `czech-holidays'
;;; package.

;;; Code:

(defvar hungarian-holidays-list
  '((holiday-fixed 1 1 "Újév")
    (holiday-fixed 3 15 "Az 1848-as forradalom ünnepe")
    (holiday-easter-etc 0 "Húsvét Vasárnap")
    (holiday-easter-etc 1 "Húsvét Hétfő")
    (holiday-fixed 5 1 "A munka ünnepe")
    (holiday-easter-etc 49 "Pünkösd vasárnap")
    (holiday-easter-etc 50 "Pünkösd hétfő")
    (holiday-fixed 8 20 "Az Államalapítás ünnepe")
    (holiday-fixed 10 23 "Az 1956-os forradalom ünnepe")
    (holiday-fixed 11 1 "Mindenszentek")
    (holiday-fixed 12 25 "Karácsony napja")
    (holiday-fixed 12 26 "Karácsony másnapja"))
  "List of Hungarian public holidays.")

;;;###autoload
(defun hungarian-holidays-add ()
  "Add Hungarian public holidays to Emacs calendar."
  (mapc (lambda (d) (add-to-list 'holiday-other-holidays d t))
        hungarian-holidays-list))

(provide 'hungarian-holidays)
;;; hungarian-holidays.el ends here
