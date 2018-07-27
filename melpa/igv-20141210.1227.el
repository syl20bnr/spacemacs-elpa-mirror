;;; igv.el --- Control Integrative Genomic Viewer within Emacs

;; Copyright (C) 2014 Stefano Barbi
;; Author: Stefano Barbi <stefanobarbi@gmail.com>
;; Version: 0.1
;; Package-Version: 20141210.1227

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
;; The igv package provides a minor mode to control remotely an
;; instance of Integrative Genomics Viewer (IGV).  It can load a track
;; file and position IGV at a specified location without leaving
;; Emacs.

;;; Code:

(require 'rx)

(defcustom igv-port
  60151
  "IGV port."
  :group 'igv-mode
  :type 'integer)

(defcustom igv-host
  "127.0.0.1"
  "IGV address."
  :group 'igv-mode :type 'string)

(defcustom igv-path
  "~/IGV/igv.sh"
  "Full path specification of the IGV launch script."
  :group 'igv-mode :type 'file)

(defcustom igv-search-location
  'igv-search-location-vcf
  "The function specified in igv-search-location is used by `igv-goto' to guess chromosomal locations near point.
The function must take no argument and must return nil if no location is found, or a string with location of type
\"chr1:nnnnnnn\" or \"chr1:nnnnnn-mmmmmm\" on success.  Two location-search functions are defined in this package:
`igv-search-location-vcf' returns the location of the current line in a vcf file;
`igv-search-location-backward' detects and returns locations of type \"chr1:nnnnnn-nnnnnn\" before point."
  :group 'igv-mode :type 'function)


(defvar igv-connection nil
  "Holds the current igv connection object.")

(defvar igv-location-history nil)

(defun igv-filter (process output)	;not used
  "Get answer from last command sent to igv PROCESS.
IGV does not give informative OUTPUT for most of the
commands.  E.g. after loading an existing file that does not
correspond to a track, IGV still answers with \"OK\"."
  (unless (string= output "OK\n")
    (error output)))

;;;###autoload
(defun igv-start ()
  "Start the IGV process."
  (interactive)
  (if (process-live-p "igv-process")
      (message "igv already started")
    (if (not (file-executable-p igv-path))
	(error (format "The file specified in igv-path does not exist or is not executable"))
     (start-process "igv-process" "igv-output" igv-path))))

(defun  igv-check-connection ()
  "Check that IGV connection is alive."
  (if igv-connection
      (string= (process-status igv-connection) "open")
    nil))

;;;###autoload
(defun igv-connect ()
  "Connect Emacs to an existing IGV process."
  (interactive)
  (condition-case err
      (if (igv-check-connection)
	  igv-connection
	(setq igv-connection (make-network-process :name "igv"
						   :host igv-host
						   :service igv-port
						   ;; :filter 'igv-filter
						   :buffer "*igv-process*")))
    (file-error
     (error "A connection to the server cannot be opened.
     Please, check that IGV is running or run `igv-start'"))))

(defun igv-send (str)
  "Helper function to send messages to IGV.
STR is a string without trailing newline."
  (cond ((igv-check-connection)
	 (process-send-string igv-connection (concat str "\n"))
	 (message str))
	(t (error "No connection established.  Please run `igv-connect' first"))))

(defun read-string-with-default (prompt default &optional history)
  "PROMPT is a string like \"Enter file (%s):\".
`%s' will be substituted by DEFAULT if not nil.
read-string will be called with the modified prompt and default
value.
HISTORY is the variable where specific history will be recorded."
  (let ((str (if default (substring-no-properties default) "")))
    (read-string (format prompt str) nil history str)))

;;; functions that search for a chromosome location near point
(defvar igv-vcf-re
  (rx (group line-start
	     (zero-or-one "chr")
	     (or (: "1" (any "0-9"))
		 (: "2" (any "0-2"))
		 (any "1-9MXY")))
      space
      (group word-start
	     (any "1-9")
	     (zero-or-more digit)
	     word-end)))

(defun igv-search-location-vcf ()
  "Search for a chromosome location in a vcf file."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward igv-vcf-re (point-at-eol) t)
      (format "%s:%s" (match-string-no-properties 1) (match-string-no-properties 2)))))

(defvar igv-search-location 'igv-search-location-vcf)

(defvar igv-location-regexp
  (rx word-start
      (zero-or-one "chr")
      (any "12MXY")
      (zero-or-one digit)
      ":"
      (one-or-more (any "0-9,"))
      (opt "-"
	   (one-or-more (any "0-9,")))
      word-end))

(defun igv-search-location-backward ()
  "Search backward for the regular expression `igv-location-regexp' and return the matching string."
  (save-excursion
    (skip-chars-forward "chrMXY0-9:,")
    (when (re-search-backward igv-location-regexp (point-at-bol) t)
      (match-string-no-properties 0))))

(defun igv-goto (location)
  "Set position of IGV to LOCATION."
  (interactive
   (list (read-string-with-default "Enter a location (%s):" (funcall igv-search-location) 'igv-location-history)))
  (igv-send (format "goto %s" location)))

(defun igv-load-file (fname)
  "Load a track file (e.g. .bam) into IGV.
FNAME is the path of file."
  (interactive
   (list (read-string-with-default "Enter filename (%s): " (thing-at-point 'filename))))
  (igv-send (format "load %s" fname)))

(defun igv-sort ()
  "Sort current IGV track by position."
  (interactive)
  (igv-send "sort position"))

(defun igv-load-url (url)
  "Open a remote url.
URL is an address pointing to a .bam file."
  (interactive
   (list (read-string-with-default "Enter url (%s):" (thing-at-point 'url))))
  (igv-send (format "load %s" url)))

(defun igv-set-snapshot-directory (dir)
  "Set the directory DIR where snapshots of IGV will be saved."
  (interactive "DEnter snapshot directory: ")
  (igv-send (format "snapshotDirectory %s" dir)))

(defun igv-snapshot (filename)
  "Take a snapshot of the current portview.
FILENAME is the path where the snapshot will be saved."
  (interactive "FEnter file name: ")
  (igv-send (format "snapshot %s" filename)))


(defvar igv-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map "f" 'igv-load-file)
    (define-key map "g" 'igv-goto)
    (define-key map "u" 'igv-load-url)
    (define-key map "s" 'igv-snapshot)
    (define-key map "d" 'igv-set-snapshot-directory)
    map))

;; (defvar igv-keymap-prefix (kbd "C-c i"))

(defvar igv-keymap-prefix  (kbd "C-c i")  "Prefix key of igv-mode.")

(defvar igv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map igv-keymap-prefix igv-command-map)
    map))

(define-minor-mode igv-mode
  "`igv-start' starts a IGV instance by calling the executable file at `igv-path'.
`igv-connect' establishes connection between Emacs and IGV.
`igv-load-file' loads a file into IGV.
`igv-goto' sets IGV position.
`igv-snapshot' take a snapshot of the current IGV portview.
"
  nil
  :keymap igv-mode-map
  :lighter " IGV")

(provide 'igv)

;;; igv.el ends here
