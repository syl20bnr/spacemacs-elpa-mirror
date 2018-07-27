;;; save-visited-files.el --- save opened files across sessions

;; Copyright (C) 2009 Nathaniel Flath <nflath@gmail.com>

;; Author: Nathaniel Flath <nflath@gmail.com>
;; URL: http://github.com/nflath/save-visited-files
;; Package-Version: 20170301.650
;; Version: 1.4

;;; Commentary:

;; save-visited-files is a lightweight version of Desktop.el that
;; only save the files you have open(currently).  This was created because I
;; couldn't ever get Desktop to work and wanted to persist open files across
;; sessions.  This file is the result.

;;; Installation:

;; To install, put this file somewhere in your load-path and add the following
;; to your .emacs file:
;;
;; (require 'save-visited-files)
;; (turn-on-save-visited-files-mode)
;;
;; This will load the set of saved files on startup, as well as updating this
;; list whenever the auto-save-timer is run.  This does not wait to save on
;; closing emacs because I wanted it to be useful even if emacs crashed.  To
;; save the visited files at any time, you can call M-x save-visited-files-save.
;; M-x save-visited-files-restore will open all files saved this way.  To turn
;; off the saving of files, you need to run (turn-off-save-visited-files-mode)

;; Changelog:
;; 1.4
;;  * Add to after-init-hook if run during initialization instead of restoring
;;  * immediately.
;; 1.3
;;  * Allow saving of dired directories.
;;  * Add save-visited-files-ignore-directories configuration variable.
;; 1.2
;;  * Changed default value of save-visited-files-location to ~/.emacs.d/emacs-visisted-files
;;  * Improvements/rewriting by Jonathan Kotta
;;  ** Checks save-visited-files-location is writable, and gives a message if not
;;  ** Changed to use define-minor-mode
;;  ** Moved (setq save-visited-files-already-restored t) to the end of
;;  ** save-visited-files-restore from save-visited-files-mode.
;;  ** Doesn't print a message in the echo area every time it saves the file list.
;; 1.1
;;  * Improvements/rewriting by Ryan Thomson
;;  ** Use auto-save-hook instead of a periodic timer
;;  ** More consistent naming conventions
;;  ** Customization ability via M-x customize-group save-visited-files
;;  ** Better handling of the temp buffer
;; 1.0
;;  * Initial Release

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl-lib)
(require 'tramp)

(defcustom save-visited-files-location "~/.emacs.d/emacs-visited-files"
  "Location of the file that contains the list of previously visited files"
  :type 'file
  :group 'save-visited-files)

(defcustom save-visited-files-auto-restore t
  "If t, restore visited files the first time save-visited-files-mode is activated"
  :type 'boolean
  :group 'save-visited-files)

(defcustom save-visited-files-ignore-tramp-files nil
  "If non-nil, ignore tramp files when saving the list of files."
  :type 'boolean
  :group 'save-visited-files)

(defcustom save-visited-files-ignore-directories t
  "If non-nil, ignore dired buffers when saving the list of files."
  :type 'boolean
  :group 'save-visited-files)

(defvar save-visited-files-already-restored nil
  "If t, then files have already been restored")

(defun save-visited-files-list ()
  "Return a list of candidate files to remember."
  (cl-remove-if-not
   'stringp
   (append
    (mapcar (lambda (x) (with-current-buffer x dired-directory)) (buffer-list))
    (mapcar 'buffer-file-name (buffer-list)))))

(defun save-visited-files-ignore-p (file)
  "Returns non-nil if a file should not be in the list of files to save."
  (or (null file)
     (not (stringp file))
     (string-equal file save-visited-files-location)
     (not (file-exists-p file))
     (and save-visited-files-ignore-directories
        (file-directory-p file))
     (and save-visited-files-ignore-tramp-files
        (tramp-tramp-file-p file))))

;;;###autoload
(defun save-visited-files-save (&optional location)
  "Save the list of currently visited files"
  (interactive (list (read-file-name
                      "Save visited files to: "
                      (file-name-directory save-visited-files-location)
                      (file-name-nondirectory save-visited-files-location))))
  (let ((save-visited-files-location (or location save-visited-files-location))
        ;; save these anyway, -restore will ignore them
        (save-visited-files-ignore-directories nil)
        (save-visited-files-ignore-tramp-files nil))
    (with-temp-file save-visited-files-location
      (ignore-errors
        (erase-buffer)
        (mapc (lambda (x) (insert x "\n"))
              (cl-remove-if 'save-visited-files-ignore-p
                            (save-visited-files-list)))))
    nil))

;;;###autoload
(defun save-visited-files-restore (&optional location)
  "Restore all files that were saved by save-visited-files-save."
  (interactive (list (read-file-name
                      "Restore visited files from: "
                      (file-name-directory save-visited-files-location)
                      (file-name-nondirectory save-visited-files-location))))
  (with-temp-buffer
    (insert-file-contents (or location save-visited-files-location))
    (ignore-errors
      (goto-char (point-min))
      (dotimes-with-progress-reporter (line (count-lines (point-min) (point-max)))
          "Restoring previously visited files"
        (let ((filename (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position))))
          (unless (save-visited-files-ignore-p filename)
            (find-file-noselect filename 'nowarn nil nil))
          (forward-line)))))
  (setq save-visited-files-already-restored t))

;;;###autoload
(define-minor-mode save-visited-files-mode
  "Minor mode to automatically save a list of all open files, and
optionally open all files from such a list at startup."
  :init-value nil
  :global t
  :group 'save-visited-files

  (if save-visited-files-mode
      ;; activate
      (progn
        (add-hook 'auto-save-hook 'save-visited-files-save)
        (add-hook 'kill-emacs-hook 'save-visited-files-save)
        (unless save-visited-files-already-restored
          (when save-visited-files-auto-restore
            (if after-init-time
                (save-visited-files-restore)
              (add-hook 'after-init-hook 'save-visited-files-restore))))
        (message "Save visited files mode enabled"))
    ;; deactivate
    (progn
      (remove-hook 'auto-save-hook 'save-visited-files-save)
      (remove-hook 'kill-emacs-hook 'save-visited-files-save)
      (message "Save visited files mode disabled"))))

;;;###autoload
(defun turn-on-save-visited-files-mode ()
  "Turns save-visited-files-mode on"
  (interactive)
  (save-visited-files-mode 1))

;;;###autoload
(defun turn-off-save-visited-files-mode ()
  "Turns save-visited-files-mode off"
  (interactive)
  (setq save-visited-files-mode nil))

(provide 'save-visited-files)

;;; save-visited-files.el ends here
