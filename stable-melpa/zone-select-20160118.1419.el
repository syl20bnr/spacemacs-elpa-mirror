;;; zone-select.el --- Select zone programs.

;; Filename: zone-select.el
;; Description: Select zone programs.
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2016-01-02
;; Version: 1.160116
;; Package-Version: 20160118.1419
;; Package-Requires: ((emacs "24.3") (dash "2.8"))
;; Keywords: games
;; URL: https://github.com/kawabata/zone-select

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

;; Simple program to select and review zone programs.

;;; Code:

(eval-when-compile (require 'cl))
(require 'zone)
(require 'tabulated-list)
(require 'dash)

(defgroup zone-select nil
  "Zone Select"
  :tag "Zone Select"
  :group 'games)

(defcustom zone-select-programs
  (-if-let (zone-el (locate-library "zone.el"))
      (with-temp-buffer
        (insert-file-contents zone-el)
        (search-forward "(defvar zone-programs ")
        (read (current-buffer)))
    (copy-sequence zone-programs))
  "Zone program selections."
  :group 'zone-select)

(defvar zone-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "i" 'zone-select-mark)
    (define-key map "u" 'zone-select-unmark)
    (define-key map "I" 'zone-select-mark-all)
    (define-key map "U" 'zone-select-unmark-all)
    (define-key map "v" 'zone-select-view)
    map))

(define-derived-mode zone-select-mode tabulated-list-mode "Zone Select"
  "Major mode for browsing a list of zone programs.
\\<zone-select-mode-map>
\\{zone-select-mode-map}"
  (setq tabulated-list-format
        [("Zone Programs" 18 zone-pgm-name-predicate)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun zone-select ()
  "UI to select zone programs."
  (interactive)
  (let ((buf (get-buffer-create "*Zone Select*"))
        (selected-zones (mapcar 'identity zone-programs)))
    (with-current-buffer buf
      (zone-select-mode)
      (setq tabulated-list-entries
            (mapcar (lambda (pgm)
                      (list pgm (vector (symbol-name pgm))))
                    zone-select-programs))
      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-char (point-min))
      (while (not (eobp))
        (if (memq (tabulated-list-get-id) selected-zones)
            (tabulated-list-put-tag "I" t)
          (forward-line)))
      (goto-char (point-min))
      (switch-to-buffer buf))))

(defmacro zone-select-with-programs-as-list (&rest body)
  "Execute BODY as if `zone-select-programs' is a list.
It automatically kills *Zone Select* buffer."
  `(progn
     (callf2 mapcar 'identity zone-select-programs)
     (callf2 mapcar 'identity zone-programs)
     ,@body
     (callf2 apply 'vector zone-select-programs)
     (callf2 apply 'vector zone-programs)
     (-when-let (buf (get-buffer "*Zone Select*"))
       (kill-buffer buf))))

;;;###autoload
(defun zone-select-add-program (program)
  "Add PROGRAM to zone selections."
  (zone-select-with-programs-as-list
   (if (not (functionp program))
       (error "Zone program %s is not loaded" program))
   (add-to-list 'zone-select-programs program)
   (add-to-list 'zone-programs program)))

(defun zone-select-remove-program (program)
  "Remove PROGRAM to zone selections."
  (zone-select-with-programs-as-list
   (if (not (memq program zone-select-programs))
       (error "Zone program %s does not exist" program))
   (callf2 remove program zone-select-programs)
   (callf2 remove program zone-programs)))

(defun zone-select-tag (tag num)
  "Select TAG for NUM times."
  (dotimes (_i num)
    (tabulated-list-put-tag tag t))
  (zone-select-execute))

(defun zone-select-mark (&optional arg)
  "Mark zone program with prefix ARG."
  (interactive "p")
  (zone-select-tag "I" arg))

(defun zone-select-mark-all ()
  "Mark zone program with prefix ARG."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (zone-select-tag "I" (length zone-select-programs))))

(defun zone-select-unmark (&optional arg)
  "Unmark zone program with prefix ARG."
  (interactive "p")
  (zone-select-tag " " arg))

(defun zone-select-unmark-all ()
  "Mark zone program with prefix ARG."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (zone-select-tag " " (length zone-select-programs))))

(defun zone-select-view ()
  "View zone program."
  (interactive)
  (-when-let (zone-program (tabulated-list-get-id))
    (let* ((zone-programs (vector zone-program)))
      (zone))))

(defun zone-select-execute ()
  "Execute zone selection."
  (save-excursion
    (goto-char (point-min))
    (let (pgms)
      (while (not (eobp))
        (when (eq (char-after) ?I)
          (push (tabulated-list-get-id) pgms))
        (forward-line))
      (setq zone-programs
            (apply 'vector (nreverse pgms))))))

(provide 'zone-select)

;;; zone-select.el ends here

;; Local Variables:
;; time-stamp-pattern: "10/Version:\\\\?[ \t]+1.%02y%02m%02d\\\\?\n"
;; End:
