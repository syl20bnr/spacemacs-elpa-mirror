;;; thingopt.el --- Thing at Point optional utilities

;; Copyright (C) 2008-2015  Tomohiro Matsuyama

;; Author: Tomohiro Matsuyama <m2ym.pub@gmail.com>
;; Keywords: convenience
;; Package-Version: 20160520.2318

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

;; TODO documentation
;; TODO forward-string by syntax (?)

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'thingatpt)

(defvar thing-list-cache nil)
(defvar upward-mark-thing-index 0)
(defvar upward-mark-thing-trial 0)
(defvar upward-mark-thing-original-position)

(defvar upward-mark-thing-list '(string symbol (up-list . *))
  "List of types of things to mark for `upward-mark-thing'.")

(defvar upward-isearch-thing-list
  '(email url word symbol string filename)
  "Like `upward-mark-thing-list', but for `upward-isearch-thing'.")

(defun thingp (thing)
  (or (get thing 'bounds-of-thing-at-point)
      (get thing 'forward-op)
      (get thing 'beginning-op)
      (get thing 'end-op)
      (fboundp (intern-soft (concat "forward-" (symbol-name thing))))))

(defun list-thing ()
  (let (things)
    (mapatoms
     (lambda (atom)
       (if (thingp atom)
           (push atom things))))
    things))

(defun read-thing ()
  (or thing-list-cache
      (setq thing-list-cache (list-thing)))
  (completing-read "Thing: " (mapcar 'list thing-list-cache)
                   nil nil nil nil "sexp"))

;;;###autoload
(defun kill-thing (thing)
  (interactive (list (read-thing)))
  (if (stringp thing)
      (setq thing (intern thing)))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (kill-region (car bounds) (cdr bounds)))))

;;;###autoload
(defun copy-thing (thing)
  (interactive (list (read-thing)))
  (if (stringp thing)
      (setq thing (intern thing)))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (if bounds
        (copy-region-as-kill (car bounds) (cdr bounds)))))

;;;###autoload
(defun mark-thing (thing)
  (interactive (list (read-thing)))
  (if (stringp thing)
      (setq thing (intern thing)))
  (let ((bounds (bounds-of-thing-at-point thing)))
    (when bounds
      (goto-char (car bounds))
      (push-mark (cdr bounds) nil transient-mark-mode)
      (setq deactivate-mark nil))))

(defun reset-upward-bounds-of-thing ()
  "Helper function for `upward-bounds-of-thing'."
  (setq upward-bounds-of-thing-index 0
        upward-bounds-of-thing-trial 0
        upward-bounds-of-thing-original-position (point)
        upward-bounds-of-thing-prev nil))

(defun upward-bounds-of-thing (list-of-things)
  "Finds the location of the ends of a thing.  LIST-OF-THINGS is
  a list of types of things.  Repeatedly calling this will take
  successive types from the list and return the bounds of that
  thing at point."
  (interactive)
  (let ((len (length list-of-things))
        (index upward-bounds-of-thing-index)
        bounds)
    (while (and (null bounds)
              (< index len))
      (let ((thing (nth index list-of-things))
            (limit '*))
        (if (consp thing)
            (setq limit (cdr thing)
                  thing (car thing)))
        (setq bounds (bounds-of-thing-at-point thing))
        (if (not (or (null bounds)
                    (and (not (eq limit '*)) (>= upward-bounds-of-thing-trial limit))
                    (eq (car bounds) (cdr bounds))
                    (and upward-bounds-of-thing-prev
                       (equal bounds upward-bounds-of-thing-prev))))
            (setq upward-bounds-of-thing-name (format "%s" (symbol-name thing)))
          (setq bounds nil
                index (mod (1+ index) len)
                upward-bounds-of-thing-index (mod (1+ upward-bounds-of-thing-index) len)
                upward-bounds-of-thing-trial 0)
          (goto-char upward-bounds-of-thing-original-position))))
    (when bounds
      (setq upward-bounds-of-thing-trial (1+ upward-bounds-of-thing-trial))
      (setq upward-bounds-of-thing-prev bounds)
      bounds)))

;;;###autoload
(defun upward-mark-thing ()
  "Marks the first type of thing in `upward-mark-thing-list' at
point.  When called successively, it marks successive types of
things in `upward-mark-thing-list'.  It is recommended to put
smaller things (e.g. word, symbol) before larger
things (e.g. list, paragraph) in `upward-mark-thing-list'.  When
this is called enough times to get to the end of the list, it
wraps back to the first type of thing."
  (interactive)
  (unless (eq last-command this-command)
    (reset-upward-bounds-of-thing))
  (let ((bounds (upward-bounds-of-thing upward-mark-thing-list)))
    (when bounds
      (message "%s" upward-bounds-of-thing-name)
      (goto-char (car bounds))
      (push-mark (cdr bounds) t 'activate)
      (setq deactivate-mark nil))))

;;;###autoload
(defun upward-isearch-thing ()
  "Much like `upward-mark-thing', but adds THING to the isearch string.
This should be invoked while isearch is active.  Clobbers the current isearch string."
  (interactive)
  (unless (eq last-command this-command)
    (reset-upward-bounds-of-thing))
  (let ((bounds (upward-bounds-of-thing upward-isearch-thing-list)))
    (when bounds
      (setq isearch-initial-string (buffer-substring-no-properties
                                    (car bounds) (cdr bounds)))
      (setq isearch-string isearch-initial-string
            isearch-message isearch-initial-string)
      (isearch-update)
      (isearch-highlight (car bounds) (cdr bounds)))))

(defun define-thing-commands ()
  (dolist (thing (list-thing))
    (dolist (op '(mark kill copy))
      (let ((symbol (intern (format "%s-%s" op thing))))
        (if (and (fboundp symbol)
                 (not (get symbol 'thingopt)))
            (setq symbol (intern (format "%s-%s*" op thing))))
        (put symbol 'thingopt t)
        (fset symbol `(lambda () (interactive) (,(intern (format "%s-thing" op)) ',thing)))))))

(defvar kill-thing-map
  '((?w . word)
    (?e . sexp)
    (?s . symbol)
    (?t . sentence)
    (?p . paragraph)
    (?f . defun)
    (?F . filename)
    (?l . list)
    (?L . up-list)
    (?S . string)
    (?U . url)
    (?P . page)))

(defun kill-region-dwim-1 (function)
  (if (and transient-mark-mode mark-active)
      (call-interactively function)
    (let* ((c (read-char))
           (thing (assoc-default c kill-thing-map))
           (bounds (if thing (bounds-of-thing-at-point thing))))
      (cond
       (bounds
        (funcall function (car bounds) (cdr bounds))
        (message "Saved %s." thing))
       (thing
        (message "There is no %s here." thing))
       (t
        (message "Nothing here."))))))  

;;;###autoload
(defun kill-region-dwim ()
  (interactive)
  (kill-region-dwim-1 #'kill-region))

;;;###autoload
(defun kill-ring-save-dwim ()
  (interactive)
  (kill-region-dwim-1 #'kill-ring-save))

(defun string-face-p (face)
  (let (result)
    (or (consp face)
        (setq face (list face)))
    (while (and face (null result))
      (if (memq (car face) '(font-lock-string-face font-lock-doc-face))
          (setq result t)
        (setq face (cdr face))))
    result))

(defun forward-string (&optional arg)
  (interactive "p")
  (if (null arg)
      (setq arg 1))
  (ignore-errors
    (cond
     ((> arg 0)
      (dotimes (i arg)
        (while (and (re-search-forward "\\s\"")
                    (string-face-p (get-text-property (point) 'face))))))
     ((< arg 0)
      (dotimes (i (- arg))
        (while (and (re-search-backward "\\s\"")
                    (string-face-p (get-text-property (1- (point)) 'face)))))))))

(defun backward-string (&optional arg)
  (interactive "p")
  (forward-string (- (or arg 1))))

(defun bounds-of-up-list-at-point ()
  (ignore-errors
    (save-excursion
      (let ((pos (scan-lists (point) -1 1)))
        (goto-char pos)
        (forward-list)
        (cons pos (point))))))

(put 'up-list 'bounds-of-thing-at-point
     (symbol-function 'bounds-of-up-list-at-point))

(defun forward-defun (&optional arg)
  (interactive "p")
  (if (null arg)
      (setq arg 1))
  (ignore-errors
    (cond
     ((< arg 0)
      (beginning-of-defun (- arg)))
     ((> arg 0)
      (end-of-defun arg)))))

(defun backward-defun (&optional arg)
  (interactive "p")
  (forward-defun (- (or arg 1))))

(provide 'thingopt)
;;; thingopt.el ends here
