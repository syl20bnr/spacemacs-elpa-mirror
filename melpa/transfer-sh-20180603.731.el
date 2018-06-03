;;; transfer-sh.el --- Simple interface for sending buffer contents to transfer.sh

;; Copyright (C) 2016 Steffen Roskamp

;; Author: S. Roskamp <steffen.roskamp@gmail.com>
;; Keywords: cloud, upload, share
;; Package-Version: 20180603.731
;; Package-Requires: ((async "1.0"))

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
  :type 'string
  :group 'transfer-sh)

(defcustom transfer-sh-remote-prefix nil
  "A prefix added to each remote file name."
  :type 'string
  :group 'transfer-sh)

(defcustom transfer-sh-remote-suffix nil
  "A suffix added to each remote file name."
  :type 'string
  :group 'transfer-sh)

(defcustom transfer-sh-gpg-args "-ac -o-"
  "Arguments given to gpg when using transfer-sh-upload-gpg."
  :type 'string
  :group 'transfer-sh)

(defcustom transfer-sh-upload-agent-command
  (cond
   ((executable-find "curl")
    "curl")
   ((executable-find "wget")
    "wget"))
  "Command used to upload files to transfer.sh"
  :type 'string
  :group 'transfer-sh)

(defcustom transfer-sh-upload-agent-arguments
  (cond
   ((executable-find "curl")
    (list "--silent" "--upload-file"))
   ((executable-find "wget")
    (list "--method" "PUT" "--output-document" "-"  "--no-verbose" "--quiet" "--body-file")))
  "Suffix arguments to `transfer-sh-upload-agent-command'"
  :type '(repeat string)
  :group 'transfer-sh)

;;;###autoload
(defun transfer-sh-upload-file-async (local-filename &optional remote-filename)
  "Upload file LOCAL-FILENAME to transfer.sh in background.

REMOTE-FILENAME is the name used in the transfer.sh link. If not
provided, query the user.

This function uses `transfer-sh-run-upload-agent'."
  (interactive "ffile: ")
  (or remote-filename
      (setq remote-filename (url-encode-url
                             (read-from-minibuffer
                              (format "Remote filename (default %s): "
                                      (file-name-nondirectory local-filename))
                              (file-name-nondirectory local-filename)))))
  (async-start
   `(lambda ()
      ,(async-inject-variables "local-filename")
      ,(async-inject-variables "remote-filename")
      ,(transfer-sh-run-upload-agent local-filename remote-filename))))

;;;###autoload
(defun transfer-sh-upload-file (local-filename &optional remote-filename)
  "Uploads file LOCAL-FILENAME to transfer.sh.

REMOTE-FILENAME is the name used in the transfer.sh link. If not
provided, query the user.

This function uses `transfer-sh-run-upload-agent'."
  (interactive "ffile: ")
  (transfer-sh-run-upload-agent
   local-filename
   (or remote-filename
       (url-encode-url
        (read-from-minibuffer
         (format "Remote filename (default %s): "
                 (file-name-nondirectory local-filename))
         (file-name-nondirectory local-filename))))))

(defun transfer-sh-run-upload-agent (local-filename  &optional remote-filename)
  "Upload LOCAL-FILENAME to transfer.sh using `transfer-sh-upload-agent-command'.

If no REMOTE-FILE is given, LOCAL-FILENAME is used."
  (let* ((filename-without-directory (file-name-nondirectory local-filename))
         (remote-filename (or remote-filename filename-without-directory))
         (transfer-link (with-temp-buffer
                          (apply 'call-process
                                 transfer-sh-upload-agent-command
                                 nil t nil
                                 (append transfer-sh-upload-agent-arguments
                                         (list local-filename
                                               (concat "https://transfer.sh/" remote-filename))))
                          (buffer-string))))
    (kill-new transfer-link)
    (minibuffer-message "File %S uploaded: %s" filename-without-directory transfer-link)))

;;;###autoload
(defun transfer-sh-upload (async)
  "Upload either active region or complete buffer to transfer.sh.

If a region is active, that region is exported to a file and then
uploaded, otherwise the complete buffer is uploaded.

This function uses `transfer-sh-upload-file' and
`transfer-sh-upload-file-async'."
  (interactive "P")
  (let* ((remote-filename (concat transfer-sh-remote-prefix (buffer-name) transfer-sh-remote-suffix))
         (local-filename (if (use-region-p)
                             (progn
                               (write-region (region-beginning) (region-end) transfer-sh-temp-file-location nil 0)
                               transfer-sh-temp-file-location)
                           (if (buffer-file-name)
                               buffer-file-name
                             (progn
                               (write-region (point-min) (point-max) transfer-sh-temp-file-location nil 0)
                               transfer-sh-temp-file-location)))))
    (if async
        (transfer-sh-upload-file-async local-filename)
      (transfer-sh-upload-file local-filename))))

;;;###autoload
(defun transfer-sh-upload-gpg (async)
  "Uploads the active region/complete buffer to transfer.sh with gpg encryption.

If a region is active, that region is encrypted using the
user-given passcode and then uploaded, otherwise the complete
buffer is encrypted and uploaded.  The argument given to gpg can
be modifed using the transfer-sh-gpg-args variable."
  (interactive "P")
  (let* ((text (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)) (buffer-string)))
    (cipher-text (substring
     (shell-command-to-string
      (concat
       "echo " (shell-quote-argument text) "|"
       "gpg --passphrase " (shell-quote-argument (read-passwd "Passcode: ")) " " transfer-sh-gpg-args))
     0 -1))
    (remote-filename (concat transfer-sh-remote-prefix (buffer-name) transfer-sh-remote-suffix)))
    (save-excursion
      (let* ((buf (find-file-noselect transfer-sh-temp-file-location)))
	(set-buffer buf)
	(erase-buffer)
	(princ cipher-text buf)
	(save-buffer)
	(kill-buffer)))
    (if async
	(transfer-sh-upload-file-async transfer-sh-temp-file-location remote-filename)
      (transfer-sh-upload-file transfer-sh-temp-file-location remote-filename))))

(provide 'transfer-sh)

;;; transfer-sh.el ends here
