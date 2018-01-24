;;; uimage.el --- An iimage like mode with the ability to display url images

;; Copyright (C) 2004-2015 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; Keywords: lisp, url, image
;; Package-Version: 20160901.521

;; This file is NOT part of GNU Emacs.

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

;;; Source code
;;
;; uimage's code can be found here:
;;   http://github.com/lujun9972/uimage

;;; Commentary:

;; Uimage is a iimange like minor mode but could displays url images
;;
;; ** Display images in *Info* buffer.
;;
;; (add-hook 'info-mode-hook 'uimage-mode)
;;
;; .texinfo:   @file{file://foo.png}
;; .texinfo:   @file{http://xxx.com/foo.png}
;; .texinfo:   @file{https://xxx.com/foo.png}
;; .info:      `file://foo.png'
;; .info:      `http://xxx.com/foo.png'
;; .info:      `https://xxx.com/foo.png'
;;
;; ** Display images in Wiki buffer.
;;
;; (add-hook 'wiki-mode-hook 'uimage-mode)
;;
;; wiki-file:   [[file://foo.png]]
;; wiki-file:   [[http://xxx.com/foo.png]]
;; wiki-file:   [[https://xxx.com/foo.png]]


;;; Code:

(require 'image)
(require 'image-file)
(require 'url-queue)
(require 'url-file)
(require 'url-http)
(require 'url-ftp)
(require 'cl-lib)

(defgroup uimage nil
  "Support for url images."
  :version "22.1"
  :group 'image)



(defvar uimage-mode-image-filename-regex
  (concat "[-+./_0-9a-zA-Z:]+\\."
	  (regexp-opt (nconc (mapcar #'upcase
				     image-file-name-extensions)
			     image-file-name-extensions)
		      t)))

(defcustom uimage-mode-image-regex-alist
  `((,(concat "\\(`\\|\\[\\[\\|<)\\)?"
	      "\\(file:\\)?" "\\(\\(file://\\|ftp://\\|http://\\|https://\\)" uimage-mode-image-filename-regex "\\)"
	      "\\(\\]\\]\\|>\\|'\\)?") . 3)
	(,(concat "\\(`\\|\\[\\[\\|<)\\)"
	      "\\(" uimage-mode-image-filename-regex "\\)"
	      "\\(\\]\\]\\|>\\|'\\)?") . 2))
  "Alist of filename REGEXP vs NUM.
Each element looks like (REGEXP . NUM).
NUM specifies which parenthesized expression in the regexp.

Examples of image filename patterns to match:
    file://foo.png
    `file://foo.png'
    \\[\\[file://foo.gif]]
    <file://foo.png>
    `foo.png'
    \\[\\[foo.gif]]
    <foo.png>
"
  :type '(alist :key-type regexp :value-type integer)
  :group 'uimage)

(defvar uimage-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-l" 'uimage-recenter)
    map)
  "Keymap used in `uimage-mode'.")

(defun uimage-recenter (&optional arg start end)
  "Re-draw images and recenter."
  (interactive "P\nr")
  (when (and (called-interactively-p 'any)
			 (not (use-region-p)))
	(setq start (point-min))
	(setq end (point-max)))
  (uimage-mode-buffer nil start end)
  (uimage-mode-buffer t start end)
  (recenter arg))

;;;###autoload
(define-obsolete-function-alias 'turn-on-uimage-mode 'uimage-mode "24.1")

(defun turn-off-uimage-mode ()
  "Unconditionally turn off uimage mode."
  (interactive)
  (uimage-mode 0))

(defun uimage-display-images (&optional start end)
  "display url between START and END as image"
  (interactive "r")
  (when (and (called-interactively-p 'any)
			 (not (use-region-p)))
	(setq start (point-min))
	(setq end (point-max)))
  (uimage-mode-buffer t start end))

(defun uimage-no-images (&optional start end)
  "display url between START and END as url"
  (interactive "r")
  (when (and (called-interactively-p 'any)
			 (not (use-region-p)))
	(setq start (point-min))
	(setq end (point-max)))
  (uimage-mode-buffer nil start end))

(defun uimage-modification-hook (beg end)
  "Remove display property if a display region is modified."
  ;;(debug-print "ii1 begin %d, end %d\n" beg end)
  (let ((inhibit-modification-hooks t)
        (beg (previous-single-property-change end 'display
                                              nil (line-beginning-position)))
        (end (next-single-property-change     beg 'display
                                              nil (line-end-position))))
    (when (and beg end (plist-get (text-properties-at beg) 'display))
      ;;(debug-print "ii2 begin %d, end %d\n" beg end)
      (remove-text-properties beg end
                              '(display nil modification-hooks nil)))))


(defun uimage-display-inline-images-callback (status start end ori-buffer &optional guessed-image-type)
  (unwind-protect
      (let ((file-data)
            (inhibit-read-only t))
		(goto-char (point-min))
		(search-forward-regexp "^$")
		(unless (= (point) (point-max))
		  (setq file-data (buffer-substring-no-properties (+ (point) 1) (point-max))))
		(when file-data
		  (with-current-buffer ori-buffer
            (unless (text-property-any start end 'modification-hooks '(uimage-modification-hook))
              (add-text-properties start end
                                   `(display ,(or (create-image file-data nil t)
                                                  (create-image file-data guessed-image-type t)
                                                  (error "unsupported image type"))
                                             modification-hooks
                                             (uimage-modification-hook)))))))
	(kill-buffer)))

(defun uimage--url-readable-p (url)
  (save-match-data
    (let ((url-type (url-type (url-generic-parse-url url))))
      (cond ((equal url-type "ftp")
             (url-ftp-file-readable-p url))
            ((equal url-type "file")
             (url-file-file-readable-p url))
            ((equal url-type "http")
             (url-http-file-readable-p url))
            ((equal url-type "https")
             (url-https-file-readable-p url))
            (t
             (file-readable-p url))))))

(defun uimage--url-retrievable-p (url)
  (save-match-data
	(let ((url-type (url-type (url-generic-parse-url url))))
	  (member url-type '("ftp" "file" "http" "https")))))

(defun uimage--guess-image-type (url)
  "Return image type by URL's extension"
  (let* ((image-type-symbol (image-type-from-file-name url)))
    (cond ((image-type-available-p image-type-symbol)
           image-type-symbol)
          ((image-type-available-p 'imagemagick)
           'imagemagick))))

(defun uimage-mode-buffer (arg &optional start end)
  "Display images if ARG is non-nil, undisplay them otherwise."
  (let ((start (or start (point-min)))
        (end (or end (point-max)))
        url)
    (with-silent-modifications
      (save-excursion
        (dolist (pair uimage-mode-image-regex-alist)
          (goto-char start)
          (while (re-search-forward (car pair) end t)
            (setq url (match-string (cdr pair)))
            ;; FIXME: we don't mark our images, so we can't reliably
            ;; remove them either (we may leave some of ours, and we
            ;; may remove other packages's display properties).
            (let ((start (match-beginning 0))
                  (end (match-end 0))
                  (contain-image-p (lambda (start end)
                                     (cl-some (lambda (idx)
                                                  (eq 'image (car (get-text-property idx 'display))))
                                                (number-sequence start end)))))
              (if arg
                  (unless (funcall contain-image-p start end) 
                    (when (uimage--url-readable-p url)
                      (if (uimage--url-retrievable-p url)
                          (url-queue-retrieve url #'uimage-display-inline-images-callback `(,start ,end ,(current-buffer) ,(uimage--guess-image-type url)))
                        (add-text-properties start end
                                             `(display ,(or (create-image url)
                                                            (create-image url 'imagemagick))
                                                       modification-hooks
                                                       (uimage-modification-hook))))))
                (remove-text-properties start end
                                        '(display modification-hooks))))))))))

;;;###autoload
(define-minor-mode uimage-mode nil
  :group 'uimage :lighter " uImg" :keymap uimage-mode-map
  (uimage-mode-buffer uimage-mode))

(provide 'uimage)

;;; uimage.el ends here
