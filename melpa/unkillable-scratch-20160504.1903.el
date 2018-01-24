;;; unkillable-scratch.el --- Disallow buffers from being killed by regexp -- default is *scratch* buffer
;; Version: 0.0.20140318

;; Copyright (C) 2015 Eric Crosson

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: scratch
;; Package-Version: 20160504.1903
;; Package-X-Original-Version: 0.1

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; TODO

;; unkillable-scratch-really-kill
;;   actually kill the selected buffer at point. If this buffer was
;;   the last matching buffer to the regexp(s) keeping him from being
;;   killed, remove said regexp(s) from `unkillable-buffers'.

;;; Commentary:

;; This package provides a minor mode that will disallow buffers from
;; being killed. Any buffer matching a regexp in the list
;; `unkillable-buffers' will not be killed.

;; Only one bufer is in `unkillable-buffers' by default: the *scratch*
;; buffer.

;; The *scratch* buffer is considered specially; in the event of a call to
;; `kill-buffer' the buffer will be replaced with
;; `initial-scratch-message'. Removing the regexp matching *scratch* from
;; `unkillable-buffers' disables this behavior.

;; Usage:

;; ; (optional): add regexp matching buffers to disallow killing to
;; ; list 'unkillable-scratch
;; (add-to-list 'unkillable-scratch "\\*.*\\*")

;; ; and activate the mode with
;; (unkillable-scratch 1)
;;   - or -
;; M-x unkillable-scratch

;; Conception thanks to
;; [[http://emacswiki.org/emacs/RecreateScratchBuffer][EmacsWiki:
;; Recreate Scratch Buffer]]

;; Idea to make the `unkillable-buffers' list thanks to
;; Donald Curtis (milkypostman)

;;; Code:
(defgroup scratch nil
  "*Scratch* buffer."
  :group 'scratch)

(defcustom unkillable-buffers '("^\\*scratch\\*$")
  "List of regexp's matching buffers that may not be killed."
  :type '(repeat string)
  :group 'scratch)

(defcustom unkillable-scratch-behavior 'do-nothing
  "The action which `unkillable-scratch-buffer' applies to
buffers matching regexp's in `unkillable-buffers'.

- 'do-nothing :: disallow the buffer from being killed (default)
- 'bury :: bury the buffer instead of killing it
- 'kill :: actually kill the buffer -- this is the same as disabling
           `unkillable-scratch'."
  :type 'symbol
  :group 'scratch)

(defun unkillable-scratch-matches (buf)
  "True if buffer name BUF matches any regexp contained in
variable `unkillable-buffers'."
  (let ((match t))
    (catch 'match
      (mapc (lambda (regexp) (when (string-match regexp buf) (throw 'match nil)))
	    unkillable-buffers)
      (setq match nil))
    match))

(defun unkillable-scratch-buffer ()
  "A hook designed to be added to hook
`kill-buffer-query-functions' to prevent buffers matching any
regexp in variable `unkillable-buffers' from ever being
killed. Instead of a successful kill, the *scratch* buffer will
be regenerated. All other buffers will simply not be killed."
  (let ((buf (buffer-name (current-buffer))))
    (if (not (unkillable-scratch-matches buf))
	t
      (cond ((eq unkillable-scratch-behavior 'kill)
	     (when (equal buf "*scratch*")
	       (delete-region (point-min) (point-max))
	       (insert (or initial-scratch-message "")))t)
	    ((eq unkillable-scratch-behavior 'bury) (bury-buffer) nil)
	    (t nil)))))

;;;###autoload
(define-minor-mode unkillable-scratch
  "A minor mode to disallow the *scratch* buffer from being killed."
  :init-value nil
  :global t
  :group 'scratch
  (if unkillable-scratch
      (add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)
    (remove-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)))

(provide 'unkillable-scratch)

;;; unkillable-scratch.el ends here
