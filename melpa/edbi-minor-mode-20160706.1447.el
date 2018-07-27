;;; edbi-minor-mode.el --- Use edbi with regular SQL files.

;; Copyright (C) 2015-2016 by Artem Malyshev

;; Author: Artem Malyshev <proofit404@gmail.com>
;; URL: https://github.com/proofit404/edbi-minor-mode
;; Package-Version: 20160706.1447
;; Version: 0.1.0
;; Package-Requires: ((edbi "0.1.3"))

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

(defvar-local edbi-minor-mode-connection nil
  "Edbi minor mode connection.")

(defvar-local edbi-minor-mode-result-buffer nil
  "Target buffer to show current buffer queries result.")

(defvar edbi-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap sql-send-buffer] 'edbi-minor-mode-execute-buffer)
    (define-key map [remap sql-send-paragraph] 'edbi-minor-mode-execute-paragraph)
    (define-key map [remap sql-send-region] 'edbi-minor-mode-execute-region)
    (define-key map [remap sql-send-string] 'edbi-minor-mode-execute)
    (define-key map (kbd "C-c C-z") 'edbi-minor-mode-show-result-buffer)
    map)
  "Edbi minor mode keymap.")

;;;###autoload
(define-minor-mode edbi-minor-mode
  "Minor mode for using Edbi from current buffer."
  :lighter " Edbi")

(defun edbi-minor-mode-execute (sql)
  "Execute SQL statement."
  (interactive "sSQL: ")
  (unless edbi-minor-mode-connection
    (let ((buffer (get-buffer edbi:dbview-buffer-name)))
      (unless buffer
        (error "Unable to find %s buffer" edbi:dbview-buffer-name))
      (setq edbi-minor-mode-connection
            (buffer-local-value 'edbi:connection buffer))))
  (unless edbi-minor-mode-result-buffer
    (setq edbi-minor-mode-result-buffer
          (generate-new-buffer-name "*edbi-minor-mode-result*")))
  (let ((buf (get-buffer-create edbi-minor-mode-result-buffer)))
    (edbi:dbview-query-execute edbi-minor-mode-connection sql buf)))

(defun edbi-minor-mode-execute-paragraph ()
  "Execute SQL statement from current paragraph."
  (interactive)
  (let* ((start (save-excursion
                  (backward-paragraph)
                  (point)))
         (end (save-excursion
                (forward-paragraph)
                (point)))
         (sql (buffer-substring-no-properties start end)))
    (edbi-minor-mode-execute sql)))

(defun edbi-minor-mode-execute-region (start end)
  "Execute SQL statement from active region.
START and END are the region offsets."
  (interactive "r")
  (let ((sql (buffer-substring-no-properties start end)))
    (edbi-minor-mode-execute sql)))

(defun edbi-minor-mode-execute-buffer ()
  "Execute whole buffer as SQL statement."
  (interactive)
  (let ((sql (buffer-substring-no-properties (point-min) (point-max))))
    (edbi-minor-mode-execute sql)))

(defun edbi-minor-mode-show-result-buffer ()
  "Show query result buffer associated with current buffer."
  (interactive)
  (unless (and edbi-minor-mode-result-buffer
               (buffer-live-p (get-buffer edbi-minor-mode-result-buffer)))
    (error "Unable to find result buffer"))
  (pop-to-buffer edbi-minor-mode-result-buffer))

(defun edbi-minor-mode-list-dbviewers ()
  "DBViewer buffers list."
  (--filter (buffer-local-value 'edbi:connection it)
            (buffer-list)))

(defun edbi-minor-mode-list-connections ()
  "Edbi connections list."
  (--map (edbi:data-source-uri
          (edbi:connection-ds
           (buffer-local-value 'edbi:connection it)))
         (edbi-minor-mode-list-dbviewers)))

(provide 'edbi-minor-mode)

;;; edbi-minor-mode.el ends here
