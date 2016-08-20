;;; digistar-mode.el --- major mode for Digistar scripts

;; Copyright (C) 2014  John Foerch <jjfoerch@earthlink.net>

;; Author: John Foerch <jjfoerch@earthlink.net>
;; Version: 0.4
;; Package-Version: 0.4
;; Date: 2014-10-21
;; Keywords: languages

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides digistar-mode, a major mode for editing Digistar
;; scripts.  If installed via elpa, the auto-mode-list entry for this mode
;; will be setup automatically.  If installed manually, use a snippet like
;; the following to set it up:
;;
;;     (when (locate-library "digistar-mode")
;;      (add-to-list 'auto-mode-alist '("\\.ds\\'" . digistar-mode)))
;;

;;; Code:

(defvar digistar-indent 8
  "Indentation column for commands in a Digistar script.")

(defun digistar-seconds-to-timestamp (s)
  (let* ((str (number-to-string s))
         (sd (if (string-match "\\(\\.[[:digit:]]+\\)$" str)
                 (match-string 1 str)
               ""))
         (h (floor s 3600))
         (s (- s (* h 3600)))
         (m (floor s 60))
         (s (- s (* m 60))))
    (if (> h 0)
        (format "%d:%02d:%02d%s" h m (truncate s) sd)
      (format "%d:%02d%s" m (truncate s) sd))))

(defun digistar-timestamp-to-seconds (ts)
  (if (string-match (concat "\\`\\(?:\\([[:digit:]]+\\):\\)??"
                            "\\(?:\\([[:digit:]]+\\):\\)?"
                            "\\([[:digit:]]+\\(?:\\.[[:digit:]]+\\)?\\)\\'")
                    ts)
      (let ((h (string-to-number (or (match-string 1 ts) "0")))
            (m (string-to-number (or (match-string 2 ts) "0")))
            (s (string-to-number (match-string 3 ts))))
        (+ (* 3600 h) (* 60 m) s))
    (error "Not a valid timestamp")))

(defun digistar-absolute-time-at-point-1 ()
  "This procedure is for internal use by
`digistar-absolute-time-at-point'.  It assumes that the caller
has just used a regexp operation to find a timestamp.  If it is a
relative timestamp, this procedure returns its value in seconds.
If it is an absolute timestamp, it throws 'return with the value
in seconds."
  (let ((relativep (match-string 1))
        (s (digistar-timestamp-to-seconds (match-string 2))))
    (if relativep
        s
      (throw 'return s))))

(defun digistar-absolute-time-at-point ()
  (save-excursion
    (save-restriction
      (beginning-of-line)
      (let ((timestamp-regexp "^[[:blank:]]*\\(\\+\\)?\\([0-9:.]+\\)")
            (time 0))
        (let ((abstime
               (catch 'return
                 (when (looking-at timestamp-regexp)
                   (setq time (digistar-absolute-time-at-point-1)))
                 (while (re-search-backward timestamp-regexp nil t)
                   (setq time (+ time (digistar-absolute-time-at-point-1))))
                 0.0)))
          (+ abstime time))))))

(defun digistar-show-absolute-time ()
  (interactive)
  (let ((s (digistar-absolute-time-at-point)))
    (if (>= s 60)
        (message "%s (%s)" s (digistar-seconds-to-timestamp s))
      (message "%s" s))))

(defun digistar-show-lis-file ()
  "Show the .lis file that corresponds to the current Digistar
script file, if it exists."
  (interactive)
  (let* ((f (or (buffer-file-name) (error "Not visiting a file")))
         (sans-ds-ext (if (string-equal "ds" (file-name-extension f))
                          (file-name-sans-extension f)
                        f))
         (lisfile (concat sans-ds-ext ".lis")))
    (unless (file-exists-p lisfile)
      (error "LIS file does not exist (%s)" lisfile))
    (let ((buf (find-file-noselect lisfile)))
      (with-current-buffer buf
        (unless (eq major-mode 'digistar-mode)
          (digistar-mode)))
      (pop-to-buffer buf))))

(defvar digistar-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") 'digistar-show-lis-file)
    (define-key map (kbd "C-c C-t") 'digistar-show-absolute-time)
    map)
  "The keymap for digistar-mode.")

(defvar digistar-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)  ;; comment syntax
    (modify-syntax-entry ?\n ">" table)
    table)
  "The syntax table for font-lock in digistar-mode.")

(defvar digistar-font-lock-keywords
  `(;; timestamps
    ("^[[:blank:]]*\\(\\+?[0-9:.]+\\)"
     (1 font-lock-preprocessor-face))

    ;; errors in .lis files
    ("^!.*$" . font-lock-warning-face))
  "A font-lock-keywords table for digistar-mode.  See
`font-lock-defaults'.")

(defun digistar-indent-line-function ()
  "An indent-line-function for Digistar scripts.  Indents
timestamps to column 0 and commands to the value of
`digistar-indent'."
  (let ((col (current-column))
        (eol (point-at-eol))
        bol
        line-is-blank
        line-is-comment
        comment-start
        comment-column
        timestamp-start
        timestamp-end
        command-start)
    (save-excursion
      (beginning-of-line)
      (setq bol (point))
      (cond
       ((looking-at "[[:blank:]]*$")
        (setq line-is-blank t))
       ((looking-at "[[:blank:]]*\\(#\\)")
        (setq line-is-comment t)
        (setq comment-start (match-beginning 1))
        (goto-char comment-start)
        (setq comment-column (current-column)))
       ((looking-at "[[:blank:]]*\\(\\+?[0-9:.]+\\)?[[:blank:]]*\\(.+\\)?$")
        (setq timestamp-start (match-beginning 1)
              timestamp-end (match-end 1)
              command-start (match-beginning 2)))))
    (cond
     (line-is-blank
      (if (= digistar-indent col)
          (delete-region bol eol)
        (delete-region bol eol)
        (insert (make-string digistar-indent 32))))
     (line-is-comment
      (delete-region bol comment-start)
      (unless (= digistar-indent comment-column)
        (if (= (point) bol)
            (insert (make-string digistar-indent 32))
          (save-excursion
            (goto-char bol)
            (insert (make-string digistar-indent 32))))))
     ((and timestamp-start command-start)
      (delete-region timestamp-end command-start)
      (if (= (point) timestamp-end)
          (insert (make-string (- digistar-indent (- timestamp-end timestamp-start)) 32))
        (save-excursion
          (goto-char timestamp-end)
          (insert (make-string (- digistar-indent (- timestamp-end timestamp-start)) 32))))
      (delete-region bol timestamp-start))
     (timestamp-start
      (cond
       ((> timestamp-start bol)
        (let ((indent (>= (point) timestamp-end)))
          (delete-region bol timestamp-start)
          (when indent
            (insert (make-string (- digistar-indent (- (point) bol)) 32)))))
       ((= digistar-indent col)
        (delete-region timestamp-end eol))
       ((and (= bol timestamp-start)
             (>= col (- timestamp-end bol)))
        (delete-region timestamp-end eol)
        (insert (make-string (- digistar-indent (- timestamp-end timestamp-start)) 32)))))
     (command-start
      (delete-region bol command-start)
      (if (= (point) bol)
          (insert (make-string digistar-indent 32))
        (save-excursion
          (goto-char bol)
          (insert (make-string digistar-indent 32))))))))

(defalias 'digistar-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode digistar-mode digistar-parent-mode
  "Digistar"
  "A major mode for Digistar scripts.

\\{digistar-mode-map}"
  :syntax-table digistar-syntax-table

  ;; Indentation
  (set (make-local-variable 'indent-line-function)
       'digistar-indent-line-function)

  ;; Syntax Highlighting
  (setq font-lock-defaults (list digistar-font-lock-keywords nil t))
  (set (make-local-variable 'show-trailing-whitespace) t)

  ;; Comments
  (set (make-local-variable 'comment-start) "# ")
  (set (make-local-variable 'comment-end) "")

  ;; Whitespace
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'require-final-newline) t))


;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ds\\'" . digistar-mode))


(provide 'digistar-mode)
;;; digistar-mode.el ends here
