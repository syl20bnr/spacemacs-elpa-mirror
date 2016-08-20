;;; wedge-ws.el --- Wedge whitespace between columns in text

;; Copyright (C) 2013 Anders Eurenius <aes@spotify.com>

;; Author: Anders Eurenius <aes@spotify.com>
;; Created: 2013-08-04
;; Keywords: formatting indentation
;; Package-Version: 20140714.1449
;; Version: 0.2.0

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(defun wedge-ws--ws-at-pos (&optional pos)
  (let ((c (or (char-after (or pos (point))) ?A)))
    (and (not (= 10 c)) (= 32 (char-syntax (or c ?A))) )))

(defun wedge-ws--ws-in-cols (beg &optional end)
  (save-excursion
    (let ((end (or end (1+ beg))))
      (move-to-column beg)
      (catch 'found
	(while (< (current-column) end)
	  (if (not (wedge-ws--ws-at-pos))
	      (throw 'found nil))
	  (forward-char))
	t))))

(defun wedge-ws--left-of-block ()
  (skip-chars-forward "[:space:]")
  (skip-chars-backward "^[:space:]")
  (if (and (not (wedge-ws--ws-at-pos))
	   (not (= 10 (char-before))))
      (backward-char)))


(defun wedge-ws--goto-top-of-ws-col (&optional beg end)
  (let* ((beg (or beg (current-column)))
	 (end (or end (1+ beg)))
	 (cont t))
    (while cont
      (forward-line -1)
      (when (bobp) (setq cont nil))
      (when (not (wedge-ws--ws-in-cols beg end))
        (forward-line)
        (setq cont nil))
      )
    (move-to-column beg)))

;;;###autoload
(defun wedge-ws-inc ()
  (interactive)
  (save-excursion
    (wedge-ws--left-of-block)
    (let ((beg (current-column)))
      (wedge-ws--goto-top-of-ws-col)
      (while (and (not (eobp)) (wedge-ws--ws-in-cols beg))
	(move-to-column beg)
	(if (< (current-column) (1+ beg))
	    (insert " "))
        (forward-line)
	 ))))

(defun wedge-ws--eat-one-space ()
  (let ((c (current-column)))
    (when (wedge-ws--ws-at-pos (1- (point)))
      (delete-backward-char 1)
      (while (< (current-column) (1- c))
	(insert " "))
      t)))

;;;###autoload
(defun wedge-ws-dec ()
  (interactive)
  (save-excursion
    (wedge-ws--left-of-block)
    (forward-char)
    (let* ((beg (- (current-column) 2))
	   (end (current-column)))
      (wedge-ws--goto-top-of-ws-col beg end)
      (while (and (not (eobp)) (wedge-ws--ws-in-cols beg end))
	(move-to-column end)
	(wedge-ws--eat-one-space)
        (forward-line) ))))

(provide 'wedge-ws)

;;; wedge-ws.el ends here
