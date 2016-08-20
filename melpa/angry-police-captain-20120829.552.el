;;; angry-police-captain.el --- Show quote from http://theangrypolicecaptain.com in the minibuffer

;; Copyright (C) 2012  Rolando Pereira

;; Author: Rolando Pereira <rolando_pereira@sapo.pt>
;; Keywords: games, web, fun
;; Package-Version: 20120829.552

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

;; The website "http://theangrypolicecaptain.com" shows a quote about
;; a angry police captain.
;;
;; The function `angry-police-captain' shows you a quote from that
;; webpage in the minibuffer.

;;; Code:

;;;###autoload
(defun angry-police-captain ()
  "Display a quote from \"http://theangrypolicecaptain.com\" in the minibuffer."
  (interactive)
  (url-retrieve "http://theangrypolicecaptain.com"
		(lambda (x)
		  (re-search-forward "http://theangrypolicecaptain.com\">")
		  (let ((start-point (point))
			end-point
			quote)
		    (re-search-forward "</a>")
		    (backward-char 5)
		    (setq end-point (point))
		    (setq quote (buffer-substring-no-properties start-point end-point))
		    (kill-this-buffer)
		    (message "%s" quote)))))

(provide 'angry-police-captain)

;;; angry-police-captain.el ends here
