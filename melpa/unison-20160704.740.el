;;; unison.el --- sync with Unison -*- lexical-binding: t -*-

;; Copyright (C) 2014-2016 Kevin Brubeck Unhammer

;; Author: Kevin Brubeck Unhammer <unhammer@fsfe.org>
;; Version: 0.1.0
;; Package-Version: 20160704.740
;; Package-Requires: ((emacs "24.1"))
;; Url: http://github.com/unhammer/unison.el
;; Keywords: sync

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple wrappers for running Unison to sync things; handy for
;; putting into midnight-hook's or similar.

;; To use, first set `unison-args' to the arguments you'd normally
;; call unison with (Unison won't do ), then call M-x unison (or put
;; #'unison into a hook).

;;; Code:

(defgroup unison nil
  "For syncing files with Unison"
  :tag "unison"
  :group 'communication)

(defcustom unison-sentinel-hook nil
  "Run by `unison' on process state change.
Same arguments as expected by `set-process-sentinel'."
  :group 'unison
  :type 'function)

(defcustom unison-program "unison"
  "Path to the Unison binary."
  :group 'unison
  :type 'string)

(defcustom unison-args nil
  "Arguments sent to Unison."
  :group 'unison
  :type '(repeat string))

;;;###autoload
(defun unison ()
  "Run Unison; only show buffer if there was output.
You need to set `unison-args' before calling this function."
  (interactive)
  (let ((proc (apply #'start-process
                     "Unison" "*unison*" unison-program
                     unison-args))
        (buffer-displayed nil))
    (set-process-sentinel proc (lambda (p s)
                                 (run-hook-with-args 'unison-sentinel-hook p s)
                                 (if (equal s "finished\n")
                                     (message "Unison %s" s)
                                   (when (buffer-live-p (process-buffer p))
                                     (display-buffer (process-buffer p)))
                                   (if (and (buffer-live-p (process-buffer p))
                                            (string-match "^Usage: unison.*" (with-current-buffer (process-buffer p)
                                                                               (buffer-string))))
                                       (error "Unison: please set `unison-args', %s" s)
                                     (error "Unison: %s" s)))))
    (set-process-filter proc (lambda (p s)
                               (if (buffer-live-p (process-buffer p))
                                   (progn
                                     (unless buffer-displayed
                                       (display-buffer (process-buffer p))
                                       (setq buffer-displayed t))
                                     (with-current-buffer (process-buffer p)
                                       (insert s)))
                                 (message "Unison: %s" s))))))


(provide 'unison)
;;; unison.el ends here
