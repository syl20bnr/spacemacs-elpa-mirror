;;; transfer-sh.el --- Simple interface for sending buffer contents to transfer.sh

;; Copyright (C) 2016 Steffen Roskamp

;; Author: S. Roskamp <steffen.roskamp@gmail.com>
;; Keywords: cloud, upload, share
;; Package-Version: 20160825.1025

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

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This package provides an interface to the transfer-sh website.
;; Calling the transfer-sh-upload function will upload either the
;; currently active region or the complete buffer (if no region is
;; active) to transfer.sh.  The remote file name is determined by the
;; buffer name and the prefix/suffix variables.

;;; Code:
(defgroup transfer-sh nil
  "Interface to transfer.sh uploading service."
  :group 'external)

(defcustom transfer-sh-temp-file-location "/tmp/transfer-sh.tmp"
  "The temporary file to use for uploading to transfer.sh."
  :type '(string)
  :group 'transfer-sh)

(defcustom transfer-sh-remote-prefix nil
  "A prefix added to each remote file name."
  :type '(string)
  :group 'transfer-sh)

(defcustom transfer-sh-remote-suffix nil
  "A suffix added to each remote file name."
  :type '(string)
  :group 'transfer-sh)

(defun transfer-sh-upload ()
  "Uploads either active region of complete buffer to transfer.sh.

If a region is active, that region is exported to a file and then
uploaded, otherwise the complete buffer is uploaded.  The remote
file name is determined by customize-variables and the buffer
name."
  (interactive)
  (if (use-region-p)
      (write-region (region-beginning) (region-end) transfer-sh-temp-file-location nil 0)
    (write-region (point-min) (point-max) transfer-sh-temp-file-location nil 0))

  (let* ((remote-filename (concat transfer-sh-remote-prefix (buffer-name) transfer-sh-remote-suffix))
         (transfer-link (substring
                         (shell-command-to-string (concat "curl --silent --upload-file "  transfer-sh-temp-file-location " \"https://transfer.sh/" remote-filename "\""))
                         0 -1)))
    (kill-new transfer-link)
    (message transfer-link)))

(provide 'transfer-sh)

;;; transfer-sh.el ends here
