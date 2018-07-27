;;; smblog.el --- samba log viewer

;; Copyright (C) 2016 Aurélien Aptel <aaptel@suse.com>

;; Author: Aurélien Aptel <aaptel@suse.com>
;; URL: http://github.com/aaptel/smblog-mode
;; Package-Version: 20170419.1021
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Emacs major mode for samba-formated logs.
;; - hilighting of log meta data messages
;; - easy navigation (`n` and `p`)
;; - filter by log level (`+` and `-`), files&functions (`f`)
;; - go to the source file (`RET` on any part of the log message)
;; - hilight regexes (ip addresses, pointers, users, ...) with different colors (`h`)
;; - expand and collapse messages with `TAB`
;; - list and jump to SMB requests and results (`r`)

;;; License:

;; MIT License
;;
;; Copyright (c) 2016 Aurélien Aptel <aaptel@suse.com>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

;;; Code:

(require 'cl-lib)
(require 'rx)

;;;###autoload
(defconst smblog-time-rx (rx
			"["
			(group (+ (or num "/"))) ;; day (1)
			(+ " ")
			(group (+ (or num ":" "."))) ;; time (2)
			"," (+ " ")
			(group (+ num)) ;; debug level (3)
			(opt
			 "," (+ " ")
			 "pid=" (+ num)
			 "," (+ " ")
			 (+ (not (any "]")))) ;; effective/real uid/gid
			"]"
			" "
			(group (+ graphic)) ;; file (4)
			":"
			(group (+ num)) ;; line (5)
			"("
			(group (+ (not (any ")")))) ;; function (6)
			")"
			"\n")
  "Regex matching a log message header")

;;;###autoload
(add-to-list
 'auto-mode-alist
 (cons (rx (or "/" bos) "log." (or "nmbd" "smbd" "winbindd" "wb-"))
       'smblog-mode-from-file-buffer))

;;;###autoload
(add-to-list
 'magic-fallback-mode-alist
 (cons smblog-time-rx
       'smblog-mode-from-file-buffer))


;;;###autoload
(defcustom smblog-src-dir (expand-file-name "~/prog/samba-git")
  "Path to samba source")

(defface smblog-file-face
  '((t . (:foreground "#119911")))
  "Face used for the file path in a log message metadata.")

(defface smblog-metadata-face
  '((t . (:foreground "#999999")))
  "Face used for a log message metadata.")

(defface smblog-fun-face
  '((t . (:foreground "#5555ff")))
  "Face used for the function name in a message metadata.")

(defface smblog-date-face
  '((t . (:foreground "#999999")))
  "Face used for date and time in a log message metadata.")

(defface smblog-hl-1-face
  '((t . (:weight bold :background "orange")))
  "Face #1 used for message hilights.")

(defface smblog-hl-2-face
  '((t . (:weight bold :background "green")))
  "Face #2 used for message hilights.")

(defface smblog-hl-3-face
  '((t . (:weight bold :background "blue")))
  "Face #3 used for message hilights.")

(defface smblog-hl-4-face
  '((t . (:weight bold :background "yellow")))
  "Face #3 used for message hilights.")

(defface smblog-reqs-success-face
  '((t . (:inherit success)))
  "Face used for successful requests.")

(defface smblog-reqs-error-face
  '((t . (:inherit error)))
  "Face used for err requests.")

(defface smblog-reqs-op-face
  '((t . (:inherit bold)))
  "Face used for requests opcode.")

(defface smblog-hilight-msg-face
  '((((background dark)) (:background "#192B14"))
    (((background light)) (:background "#cceecc"))
    (t . (:inverse-video t)))
  "Face used to hilight whole message.")

(defcustom smblog-hl-face-list '(smblog-hl-1-face
				 smblog-hl-2-face
				 smblog-hl-3-face
				 smblog-hl-4-face)
  "Faces used for highlighting in messages.")

(defvar-local smblog-log-file nil "Current file being viewed in the buffer")
(defvar-local smblog-log-data nil "Vector of parsed log file")
(defvar-local smblog-log-reqs nil "List of requests")
(defvar-local smblog-pos-map nil "Vector mapping id to point position")
(defvar-local smblog-visible-map nil "Bool-vector mapping id to visibility (`t' for visible)")
(defvar-local smblog-filter-level 10 "Current log level being displayed")
(defvar-local smblog-filter-file nil "Current log file filter")
(defvar-local smblog-filter-fun nil "Current log function filter")
(defvar-local smblog-hl-list nil "Current log highlight-regex list")

(defvar-local smblog-reqs-win nil "Window used for displaying requests")
(defvar-local smblog-reqs-log-buf nil "smblog buffer associated with current request buffer")
(defvar-local smblog-reqs-log-win nil "smblog window associated with current request buffer")

(defvar-local smblog-msg-overlay nil "whole message overlay")
(defvar-local smblog-req-overlay nil "request line overlay")

(defconst smblog-reqs-success-rx (rx bos (or "NT_STATUS_OK") eos)
  "Regex matching successful request status.")

(defconst smblog-reqs-start-rx
  (rx "smbd_smb2_request_dispatch: opcode[" (group (+ (not (any "]")))) "] mid = ")
  "Regex matching the start of a SMB request (capture opcode).")

(defconst smblog-reqs-end-rx
  (rx "smbd_smb2_request_done_ex: idx[" (+ digit) "] status[" (group (+ (not (any "]")))) "]")
  "Regex matching the end of a SMB request (capture status).")

(defun smblog-buf-name (file)
  "Return buffer name to be used to view FILE."
  (format "*smblog: %s*" file))

;;;###autoload
(defun smblog-mode-from-file-buffer ()
  "Create a viewer buffer from current buffer.
The buffer must be visiting an actual file."
  (interactive)
  (smblog-open (buffer-file-name)))

;;;###autoload
(defun smblog-open (file)
  "Create a viewer buffer of FILE."
  (interactive "flog file: ")
  (let ((buf-name (smblog-buf-name file)))
    (with-current-buffer (get-buffer-create buf-name)
      (let ((buffer-read-only nil))
	(erase-buffer))
      (smblog-mode)
      (let ((buffer-read-only nil))
	(setq smblog-log-file file)
	(setq smblog-log-data (smblog-parse file))
	(setq smblog-visible-map (make-bool-vector (length smblog-log-data) t))
	(smblog-insert-log)
	(smblog-compute-reqs)
	(goto-char (point-min))))
    (switch-to-buffer buf-name)))

(defun smblog-parse (file)
  "Parse FILE and return a vector of log messages."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (msgs)
      (while (search-forward-regexp smblog-time-rx nil 'noerror)
	(let ((day (match-string 1))
	      (time (match-string 2))
	      (level (string-to-number (match-string 3)))
	      (file (match-string 4))
	      (nb (string-to-number (match-string 5)))
	      (fun (match-string 6))
	      (start (point))
	      txt)

	  (while (and (not (eobp)) (not (looking-at (rx bol "[20"))))
	    (forward-line))
	  (setq txt (buffer-substring start (point)))
	  (push (vector day time level file nb fun txt) msgs)))
      (apply 'vector (nreverse msgs)))))

(defun smblog-hl-propertize (txt hls)
  (let ((faces smblog-hl-face-list))
    (dolist (hl hls)
      (let ((i 0)
	    (face (car faces)))
	(while (string-match hl txt i)
	  (setq
	   txt (replace-match (propertize (match-string 0 txt) 'face face) nil t txt)
	   i (match-end 0))))
      (setq faces (if (cdr faces) (cdr faces) faces))))
  txt)

(defun smblog-insert-log (&optional filt-level filt-file filt-fun hl-list)
  "Insert filtered/processed log content."
  (setq filt-level (or filt-level 10))
  (let ((len (length smblog-log-data))
	(filt-file-rx (and filt-file (smblog-glob-to-rx filt-file)))
	(filt-fun-rx (and filt-fun (smblog-glob-to-rx filt-fun))))

    (setq smblog-pos-map (make-vector len nil))

    (dotimes (i len)
      (let* ((msg (aref smblog-log-data i))
	     (day (aref msg 0))
	     (time (aref msg 1))
	     (level (aref msg 2))
	     (file (aref msg 3))
	     (nb  (aref msg 4))
	     (fun (aref msg 5))
	     (txt (aref msg 6)))
	(when (and (<= level filt-level)
		   (or (null filt-file-rx) (string-match filt-file-rx file))
		   (or (null filt-fun-rx) (string-match  filt-fun-rx fun)))
	  (aset smblog-pos-map i (point))
	  (insert
	   (propertize
	    (concat
	     (propertize (format "[%2d " level) 'face 'smblog-metadata-face)
	     (propertize (concat day " " time) 'face 'smblog-date-face)
	     " "
	     (propertize file 'face 'smblog-file-face)
	     (propertize (format ":%d " nb) 'face 'smblog-metadata-face)
	     (propertize fun 'face 'smblog-fun-face)
	     (propertize "]" 'face 'smblog-metadata-face)
	     "\n" (propertize (if hl-list (smblog-hl-propertize txt hl-list) txt) 'invisible nil))
	    'smblog-index i)))))))

(defun smblog-next-msg ()
  "Move point to the beginning of the next message."
  (interactive)
  (let ((pos (next-single-property-change (point) 'smblog-index)))
    (when pos
      (goto-char pos)
      (forward-line))))

(defun smblog-prev-msg ()
  "Move point to the beginning of the previous message."
  (interactive)
  (let ((id (smblog-current-id)))
    (when (< 0 id)
      (smblog-move-close-to-id (1- id) -1)
      (forward-line))))

(defun smblog-current-id ()
  "Return the index in the parsed log vector of the message under the point."
  (save-excursion
    (when (= (point) (point-max))
      (backward-char))
    (get-text-property (point) 'smblog-index)))

(defun smblog-current-msg ()
  "Return the parsed message under the point."
  (aref smblog-log-data (or (smblog-current-id) (error "No log message under point"))))

(defun smblog-full-path (raw-file)
  "Return the full path of RAW-FILE."
  (let ((file (if (string-match
		   (rx bos (? "../") (group (+ any)) eos) raw-file)
		  (match-string 1 raw-file)
		(error "Invalid file %s" raw-file)))
	(dir (progn (string-match (rx bos (group (+ any)) (? "/") eos) smblog-src-dir)
		    (match-string 1 smblog-src-dir))))
    (concat dir "/" file)))

(defun smblog-goto-src ()
  "Open the file that emited the message under the point."
  (interactive)
  (let* ((msg (smblog-current-msg))
	 (file (aref msg 3))
	 (ln (aref msg 4))
	 (dir smblog-src-dir)
	 fullpath)

    (while (null (file-exists-p (setq fullpath (let ((smblog-src-dir dir)) (smblog-full-path file)))))
      (setq dir (read-directory-name
		 (format
		  (concat
		   "Cannot open file %s\n"
		   "smblog-src-dir: %s\n"
		   "          file: %s (any leading ../ removed)\n\n"
		   "New smblog-src-dir value: ")
		  fullpath dir file))))

    (when (not (string= dir smblog-src-dir))
      (if (y-or-n-p "Make setting global (yes) or buffer-local (no)")
	  (setq smblog-src-dir (expand-file-name dir))
	(setq-local smblog-src-dir (expand-file-name dir)))
      (message "You can set a buffer-local version of smblog-src-dir using M-x smblog-set-buffer-source-tree"))

    (find-file-other-window fullpath)
    (goto-char (point-min))
    (forward-line (1- ln))))

(defun smblog-inc-level ()
  "Increase verbosity of current log by 1 level."
  (interactive)
  (if (>= smblog-filter-level 10)
      (progn
	(setq smblog-filter-level 10)
	(message "Already at log level 10."))
    (cl-incf smblog-filter-level)
    (smblog-update)
    (message "Showing log up to level %d" smblog-filter-level)))

(defun smblog-dec-level ()
  "Decrease verbosity of current log by 1 level."
  (interactive)
  (if (<= smblog-filter-level 0)
      (progn
	(setq smblog-filter-level 0)
	(message "Already at log level 0."))
    (cl-decf smblog-filter-level)
    (smblog-update)
    (message "Showing log up to level %d" smblog-filter-level)))

(defun smblog-move-close-to-id (id step)
  "Move point to log ID if not filtered, otherwise move to closest log it in the direction of STEP."
  (let ((p (aref smblog-pos-map id)))
    (if p
	(goto-char p)
      (while (and (>= id 0) (< id (length smblog-pos-map)) (null (aref smblog-pos-map id)))
	(cl-incf id step))
      (when (>= id 0)
	(goto-char (aref smblog-pos-map id))))))

(defun smblog-update ()
  "Regenerate buffer content based on current filters."
  (let* ((buffer-read-only nil)
	 (id (smblog-current-id))
	 (offset (- (point) (aref smblog-pos-map id))))
    (erase-buffer)
    (when (overlayp smblog-msg-overlay) (delete-overlay smblog-msg-overlay))
    (smblog-insert-log smblog-filter-level smblog-filter-file smblog-filter-fun smblog-hl-list)

    ;; restore collapse state
    (dotimes (i (length smblog-log-data))
      (let ((p (aref smblog-pos-map i))
	    (v (not (aref smblog-visible-map i))))
	(when (and p v)
	  (goto-char p)
	  (forward-line)
	  (put-text-property (point)
			     (or
			      (next-single-property-change (point) 'smblog-index)
			      (point-max))
			     'invisible t))))

    (smblog-move-close-to-id id -1)
    (when (= id (smblog-current-id))
      (forward-char offset))))

(defun smblog-glob-to-rx (glob)
  (setq glob (replace-regexp-in-string (rx (+ "*")) "*" glob))
  (mapconcat 'regexp-quote (split-string glob (rx "*")) ".*"))

(defun smblog-hl-menu ()
  (interactive)
  (let ((c (read-char-choice "hilight:  [a]dd  [r]eset   [q]uit ? " '(?a ?r ?q))))
    (cond
     ((= c ?a)
      (add-to-list 'smblog-hl-list (read-regexp "regex? ") t)
      (smblog-update))
     ((= c ?r)
      (setq smblog-hl-list nil)
      (smblog-update)))))

(defun smblog-filter-menu ()
  (interactive)
  (let ((c (read-char-choice "log filter:   f[i]le    f[u]nction   [r]eset   [q]uit ? " '(?i ?u ?r ?q))))
    (cond
     ((= c ?i)
      (setq smblog-filter-file (read-string "file pattern (use * as wildcard)? "))
      (smblog-update))
     ((= c ?u)
      (setq smblog-filter-fun (read-string "func pattern (use * as wildcard)? "))
      (smblog-update))
     ((= c ?r)
      (setq smblog-filter-file nil
	    smblog-filter-fun nil)
      (smblog-update))))
  (message "filter set to %s"
	   (mapconcat 'identity (list
				 (format "level <= %d" smblog-filter-level)
				 (if smblog-filter-file (format "file <%s>" smblog-filter-file) "all file")
				 (if smblog-filter-fun (format "func <%s>" smblog-filter-fun) "all func"))
		      " and ")))

(defun smblog-set-buffer-source-tree (dir)
  "Change current buffer C source directory."
  (interactive "Ddir? ")
  (set (make-local-variable 'smblog-src-dir) dir))

(defun smblog-toggle-msg (&optional action)
  "Collapse or expand current msg under point or all msg in the region."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
	    (end (region-end))
	    (continue t))
	(save-excursion
	  (goto-char beg)
	  (while (and (< (point) end) continue)
	    (smblog-toggle-msg-1)
	    (setq continue (not (null (smblog-next-msg)))))))
    (smblog-toggle-msg-1)))

(defun smblog-toggle-msg-1 (&optional action)
  "Collapse or expand current message under point.
ACTION can be one of `collapse' or `expand'. Anything else will toggle the current state."
  (interactive)
  (when (bolp)
    (end-of-line))

  (let* ((id (smblog-current-id))
	 (reg (smblog-msg-region id))
	 (end (cdr reg))
	 (beg (car reg))
	 (beg-txt (progn (goto-char beg)(forward-line)(point)))
	 (new-val (cond
		   ((eq action 'collapse) t)
		   ((eq action 'expand) nil)
		   (t (not (get-text-property (point) 'invisible)))))
	 (buffer-read-only nil))
    (put-text-property beg-txt end 'invisible new-val)
    (aset smblog-visible-map id (not new-val))
    (goto-char beg)))

(defun smblog-expand-all ()
  "Expand all log in the current buffer."
  (interactive)
  (setq smblog-visible-map (make-bool-vector (length smblog-visible-map) t))
  (let ((buffer-read-only nil))
    (remove-list-of-text-properties (point-min) (point-max) '(invisible))))

(defun smblog-msg-region (id)
  (let* ((beg (aref smblog-pos-map id))
         (end (or (next-single-property-change beg 'smblog-index) (point-max))))
    (cons beg end)))

(defun smblog-hilight-msg-range (id-start id-end)
  (let* ((beg (car (smblog-msg-region id-start)))
	 (end (car (smblog-msg-region id-end))))
    (when smblog-msg-overlay
      (delete-overlay smblog-msg-overlay))
    (setq smblog-msg-overlay (make-overlay beg end))
    (overlay-put smblog-msg-overlay 'face 'smblog-hilight-msg-face)))

(defun smblog-hilight-req ()
  (let* ((beg (save-excursion (beginning-of-line)(point)))
	 (end (save-excursion (end-of-line)(point))))
    (when smblog-req-overlay
      (delete-overlay smblog-req-overlay))
    (setq smblog-req-overlay (make-overlay beg end))
    (overlay-put smblog-req-overlay 'face 'smblog-hilight-msg-face)))

(defun smblog-compute-reqs ()
  "Populate smblog-log-reqs variable with all the buffer SMB requests."
  (let (r
	reqs
	(len (length smblog-log-data))
	(i 0)
	m)
    (while (< i len)
      (setq m (aref smblog-log-data i))
      (let ((day (aref m 0))
	    (time (aref m 1))
	    (level (aref m 2))
	    (file (aref m 3))
	    (nb  (aref m 4))
	    (fun (aref m 5))
	    (txt (aref m 6)))
	(cond
	 ((string-match smblog-reqs-start-rx txt)
	  (setq r (vector i (match-string 1 txt))))
	 ((string-match smblog-reqs-end-rx txt)
	  (when r
	    (push (vconcat r (vector i (match-string 1 txt))) reqs)
	    (setq r nil))))
	(cl-incf i)))
    (setq smblog-log-reqs (vconcat (nreverse reqs)))))



(defun smblog-reqs-get-win (&optional vertical)
  "Return the (potentially new) window used to show requests."
  (when (null (window-live-p smblog-reqs-win))
    (setq smblog-reqs-win (if vertical (split-window-horizontally) (split-window-below))))
  (set-window-buffer smblog-reqs-win (smblog-reqs-get-buf))
  (set-window-dedicated-p smblog-reqs-win t)
  smblog-reqs-win)

(defun smblog-reqs-get-buf ()
  "Return the buffer used to show requests."
  (get-buffer-create (format " *smblog-reqs: %s*" smblog-log-file)))

(defun smblog-propertize-status (status)
  (propertize status
	      'face
	      (if (string-match smblog-reqs-success-rx status)
		  'smblog-reqs-success-face
		'smblog-reqs-error-face)))

(defun smblog-reqs-popup (&optional arg-vertical)
  (interactive "P")
  (let ((log-buf (current-buffer))
	(reqs smblog-log-reqs)
	(buf (smblog-reqs-get-buf))
	(win (selected-window)))
    (if (or (null smblog-log-reqs) (= 0 (length smblog-log-reqs)))
	(message "No SMB requests found. This might not be a smbd log.")
      (select-window (smblog-reqs-get-win arg-vertical))
      (with-current-buffer buf
	(smblog-reqs-mode)
	(setq smblog-reqs-log-buf log-buf
	      smblog-reqs-log-win win)
	(let ((buffer-read-only nil))
	  (erase-buffer)
	  (mapc (lambda (r)
		  (let ((i-start (aref r 0))
			(op      (aref r 1))
			(i-end  (aref r 2))
			(status  (aref r 3)))
		    (insert
		     (propertize
		      (format "SMB2 request %-18s ... response %-30s\n"
			      (propertize (replace-regexp-in-string (rx bos "SMB2_OP_") "" op)
					  'face 'smblog-reqs-op-face)
			      (smblog-propertize-status status))
		      'smblog-index (cons i-start i-end)))))
		reqs))
	(goto-char (point-min)))
      (smblog-reqs-help))))

(defun smblog-reqs-help ()
  (interactive)
  (message "Requests: [RET] cycle thru start/end of request   [n]ext  [p]rev [q]uit"))

(defun smblog-next-req ()
  "Move and jump to next request."
  (interactive)
  (forward-line)
  (when (get-text-property (point) 'smblog-index)
    (smblog-jump-to-req)))

(defun smblog-prev-req ()
  "Move and jump to previous request."
  (interactive)
  (forward-line -1)
  (when (get-text-property (point) 'smblog-index)
    (smblog-jump-to-req)))

(defun smblog-jump-to-req ()
  "Cycle thru the start and end of the request under point."
  (interactive)
  (let* ((ids (get-text-property (point) 'smblog-index))
	 (start (car ids))
	 (end (cdr ids)))
    (smblog-hilight-req)
    (with-selected-window smblog-reqs-log-win
      (let ((cur (smblog-current-id)))
	(smblog-move-close-to-id
	 (if (= cur start) end start) -1)
	(smblog-hilight-msg-range start end)))))

(define-derived-mode smblog-reqs-mode special-mode "Smblog Reqs"
  "Major mode for viewing smbd requests.
\\{smblog-reqs-mode-map}"
  (buffer-disable-undo)
  (define-key smblog-reqs-mode-map (kbd "n")   'smblog-next-req)
  (define-key smblog-reqs-mode-map (kbd "p")   'smblog-prev-req)
  (define-key smblog-reqs-mode-map (kbd "RET") 'smblog-jump-to-req)
  (define-key smblog-reqs-mode-map (kbd "h")   'smblog-reqs-help)
  (define-key smblog-reqs-mode-map (kbd "q")   'delete-window))

;;;###autoload
(define-derived-mode smblog-mode special-mode "Smblog"
  "Major mode for viewing samba log files.
\\{smblog-mode-map}"
  (buffer-disable-undo)
  (define-key smblog-mode-map (kbd "n")   'smblog-next-msg)
  (define-key smblog-mode-map (kbd "p")   'smblog-prev-msg)
  (define-key smblog-mode-map (kbd "s")   'smblog-goto-src)
  (define-key smblog-mode-map (kbd "RET") 'smblog-goto-src)
  (define-key smblog-mode-map (kbd "+")   'smblog-inc-level)
  (define-key smblog-mode-map (kbd "-")   'smblog-dec-level)
  (define-key smblog-mode-map (kbd "f")   'smblog-filter-menu)
  (define-key smblog-mode-map (kbd "h")   'smblog-hl-menu)
  (define-key smblog-mode-map (kbd "TAB") 'smblog-toggle-msg)
  (define-key smblog-mode-map (kbd "r")   'smblog-reqs-popup)
  (define-key smblog-mode-map (kbd "q")   'quit-window))

(provide 'smblog)
;;; smblog.el ends here
