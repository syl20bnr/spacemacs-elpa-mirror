;;; flycheck-dtrace.el --- Flycheck: DTrace support  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jürgen Hötzel

;; Author: Jürgen Hötzel <juergen@hoetzel.info>
;; Keywords: languages, convenience, tools
;; Package-Version: 20180126.1935
;; Package-Requires: ((emacs "25.1") (flycheck "0.22"))

;; This file is not part of GNU Emacs.

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

;;; Code:

(require 'flycheck)

(flycheck-define-checker dtrace
  "A DTrace syntax checker using the DTrace compiler.

See URL `https://www.freebsd.org/doc/handbook/dtrace.html'."
  :command ("dtrace"  "-e" "-s" source)
  :error-patterns
  ((error line-start
	  "dtrace: failed to compile script " (file-name) ": line " line ": " (message)
          line-end))
  :modes dtrace-script-mode)
(provide 'flycheck-dtrace)
;;; flycheck-dtrace ends here



;;; flycheck-dtrace.el ends here
