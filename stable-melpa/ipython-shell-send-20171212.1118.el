;;; ipython-shell-send.el --- Send code (including magics) to ipython shell  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jack Kamm

;; Author: Jack Kamm <jackkamm@gmail.com>
;; Version: 1.0.2
;; Package-Version: 20171212.1118
;; Package-Requires: ((emacs "24"))
;; Keywords: tools, processes
;; URL: https://github.com/jackkamm/ipython-shell-send-el

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

;; This is a package for sending code to the IPython interpreter.
;; It provides functionality similar to the `python-shell-send-*'
;; functions in python.el, but is able to send code regions
;; containing IPython magic (such as `!ls' or `%timeit'),
;; whereas python.el only has limited support for this.
;;
;; The functions provided by ipython-shell-send are
;; `ipython-shell-send-region', `ipython-shell-send-buffer',
;; and `ipython-shell-send-defun'. They are essentially equivalent
;; to their `python-shell-send-*' equivalents in `python.el',
;; except better able to handle IPython magic.
;;
;; Note to use the ipython-shell-send, you must make sure
;; to start an IPython shell when calling `run-python'.

;;; Code:


(require 'python)

(defun ipython-shell-send--save-temp-file (string)
  "Send STRING to temp file with .ipy suffix.
Returns the tempfile name."
  (let* ((temporary-file-directory
          (if (file-remote-p default-directory)
              (concat (file-remote-p default-directory) "/tmp")
            temporary-file-directory))
         (temp-file-name (make-temp-file "ipy" nil ".ipy"))
         (coding-system-for-write (python-info-encoding)))
    (with-temp-file temp-file-name
      (insert string)
      (delete-trailing-whitespace))
    temp-file-name))

(defun ipython-shell-send-string (string &optional process msg)
  "Send STRING to inferior Python PROCESS.
When optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list (read-string "Python command: ") nil t))
  (let ((process (or process (python-shell-get-process-or-error msg))))
    (if (string-match ".\n+." string)   ;Multiline.
        (let* ((temp-file-name (ipython-shell-send--save-temp-file string))
               (file-name (or (buffer-file-name) temp-file-name)))
          (ipython-shell-send-file file-name process temp-file-name t))
      (comint-send-string process string)
      (when (or (not (string-match "\n\\'" string))
                (string-match "\n[ \t].*\n?\\'" string))
        (comint-send-string process "\n")))))

;;;###autoload
(defun ipython-shell-send-region (start end &optional send-main msg)
  "Send the region delimited by START and END to inferior IPython process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== \\='__main__\\=':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument.  When optional argument MSG is
non-nil, forces display of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive
   (list (region-beginning) (region-end) current-prefix-arg t))
  (let* ((string (python-shell-buffer-substring start end (not send-main)))
         (process (python-shell-get-process-or-error msg))
         (original-string (buffer-substring-no-properties start end))
         (_ (string-match "\\`\n*\\(.*\\)" original-string)))
    (message "Sent: %s..." (match-string 1 original-string))
    (ipython-shell-send-string string process)))

;;;###autoload
(defun ipython-shell-send-buffer (&optional send-main msg)
  "Send the entire buffer to inferior IPython process.
When optional argument SEND-MAIN is non-nil, allow execution of
code inside blocks delimited by \"if __name__== \\='__main__\\=':\".
When called interactively SEND-MAIN defaults to nil, unless it's
called with prefix argument.  When optional argument MSG is
non-nil, forces displa qqy of a user-friendly message if there's no
process running; defaults to t when called interactively."
  (interactive (list current-prefix-arg t))
  (save-restriction
    (widen)
    (ipython-shell-send-region (point-min) (point-max) send-main msg)))

;;;###autoload
(defun ipython-shell-send-defun (&optional arg msg)
  "Send the current defun to inferior IPython process.
When argument ARG is non-nil do not include decorators.  When
optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive (list current-prefix-arg t))
  (save-excursion
    (ipython-shell-send-region
     (progn
       (end-of-line 1)
       (while (and (or (python-nav-beginning-of-defun)
                       (beginning-of-line 1))
                   (> (current-indentation) 0)))
       (when (not arg)
         (while (and (forward-line -1)
                     (looking-at (python-rx decorator))))
         (forward-line 1))
       (point-marker))
     (progn
       (or (python-nav-end-of-defun)
           (end-of-line 1))
       (point-marker))
     nil  ;; noop
     msg)))

(defun ipython-shell-send-file (file-name &optional process temp-file-name
                                         delete msg)
  "Send FILE-NAME to inferior Python PROCESS.
If TEMP-FILE-NAME is passed then that file is used for processing
instead, while internally the shell will continue to use
FILE-NAME.  If TEMP-FILE-NAME and DELETE are non-nil, then
TEMP-FILE-NAME is deleted after evaluation is performed.  When
optional argument MSG is non-nil, forces display of a
user-friendly message if there's no process running; defaults to
t when called interactively."
  (interactive
   (list
    (read-file-name "File to send: ")   ; file-name
    nil                                 ; process
    nil                                 ; temp-file-name
    nil                                 ; delete
    t))                                 ; msg
  (let* ((process (or process (python-shell-get-process-or-error msg)))
         (file-name (expand-file-name
                     (or (file-remote-p file-name 'localname)
                         file-name)))
         (temp-file-name (when temp-file-name
                           (expand-file-name
                            (or (file-remote-p temp-file-name 'localname)
                                temp-file-name)))))
    (python-shell-send-string
     (format
      (concat
       "import IPython, os;"
       "IPython.get_ipython().magic('''run -i %s''');"
       (when (and delete temp-file-name)
         (format "os.remove('''%s''');" temp-file-name)))
      (or temp-file-name file-name))
     process)))

(provide 'ipython-shell-send)
;;; ipython-shell-send.el ends here



