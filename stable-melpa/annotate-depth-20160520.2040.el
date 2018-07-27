;;; annotate-depth.el --- Annotate buffer if indentation depth is beyond threshold.

;; Copyright (C) 2016  Morten Slot Kristensen

;; Author: Morten Slot Kristensen <msk AT nullpointer DOT dk>
;; Keywords: convenience
;; Package-Version: 20160520.2040
;; URL: https://github.com/netromdk/annotate-depth
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify it under the terms of the
;; GNU General Public License as published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
;; even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License along with this program.  If
;; not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `annotate-depth-mode' annotates buffer if indentation depth is beyond threshold (see
;; `annotate-depth-threshold' which defaults to 5). An idle timer is started when entering the mode
;; and disabled when exiting it. The face `annotate-depth-face' is applied at indentation level and
;; to end-of-line for each line on or beyond threshold.
;;
;; Usage:
;;   (add-hook 'prog-mode-hook 'annotate-depth-mode)
;;
;; The threshold can be fine-tuned for specific modes if necessary:
;;   (add-hook 'annotate-depth-mode-hook
;;               (lambda ()
;;                 (if (equal major-mode 'emacs-lisp-mode)
;;                     (setq-local annotate-depth-threshold 10)
;;                   (when (equal major-mode 'c++-mode)
;;                     (setq-local annotate-depth-threshold 4)))))

;;; Code:

;;
;; Customizable variables
;;

(defgroup annotate-depth nil
  "Annotate buffer if indentation depth is beyond threshold."
  :version "0.1"
  :group 'convenience)

(defface annotate-depth
  '((t :background "#770000"))
  "Default face to highlight too deep indentation levels."
  :group 'annotate-depth)

(defcustom annotate-depth-face 'annotate-depth
  "Face to highlight too deep identation levels."
  :type 'face
  :group 'annotate-depth)

(defcustom annotate-depth-threshold 5
  "Depth threshold and beyond to annotate."
  :type 'integer
  :group 'annotate-depth)

(defcustom annotate-depth-idle-timeout 2
  "Perform annotation check when Emacs has been idle the
specified duration of seconds. To disable idle timers use `nil'
as value."
  :type 'integer
  :group 'annotate-depth)

;;
;; Local variables
;;

(defvar annotate-depth--overlays '())
(make-variable-buffer-local 'annotate-depth--overlays)

(defvar annotate-depth--idle-timer nil)
(make-variable-buffer-local 'annotate-depth--idle-timer)

;;
;; Mode
;;

(defcustom annotate-depth-lighter " Depth"
  "Text to display in the mode line when annotate depth mode is on."
  :type 'string
  :group 'annotate-depth)

(defvar annotate-depth-map (make-sparse-keymap)
  "Keymap used in `annotate-depth-mode' buffers.")

;;;###autoload
(define-minor-mode annotate-depth-mode
  "Minor mode for annotating indentation when too deep."
  nil
  annotate-depth-lighter
  annotate-depth-map
  :group annotate-depth
  (if annotate-depth-mode
      (annotate-depth-enter)
    (annotate-depth-exit)))

(defun annotate-depth-enter ()
  (annotate-depth--annotate)
  (annotate-depth--create-idle-timer))

(defun annotate-depth-exit ()
  (annotate-depth--stop-timer)
  (annotate-depth--clear-overlays))

;;
;; Main functions
;;

(defun annotate-depth--add-overlay ()
  "Add annotation overlay at current point and until end-of-line."
  (let ((overlay (make-overlay (point) (point-at-eol) nil t t)))
    (overlay-put overlay 'face annotate-depth-face)
    (add-to-list 'annotate-depth--overlays overlay)))

(defun annotate-depth--clear-overlays ()
  "Remove all annotations."
  (dolist (overlay annotate-depth--overlays)
    (delete-overlay overlay))
  (setq annotate-depth--overlays '()))

(defun annotate-depth--determine-indent ()
  "Determine tab width or indentation offset."
  (or (catch 'loop
        (dolist (indentf
                 '((lambda () (when (boundp 'c-basic-offset) c-basic-offset))
                   (lambda () (when (boundp 'lisp-body-indent) lisp-body-indent))
                   (lambda () (when (boundp 'sh-indentation) sh-indentation))
                   (lambda () (when (boundp 'js-indent-level) js-indent-level))
                   (lambda () (when (boundp 'cperl-indent-level) cperl-indent-level))
                   (lambda () (when (boundp 'css-indent-level) css-indent-level))
                   (lambda () (when (boundp 'sgml-basic-offset) sgml-basic-offset))
                   (lambda () (when (or (equal t tab-always-indent)
                                        (and (boundp 'c-tab-always-indent)
                                             (equal t c-tab-always-indent)))
                                tab-width))))
          (let ((val (funcall indentf)))
            (when (integerp val)
              (throw 'loop val)))))
      standard-indent))

(defun annotate-depth--annotate ()
  "Annotate depth when it gets beyond `annotate-depth-threshold'."
  (when (bound-and-true-p annotate-depth-mode)
    (save-excursion
      (annotate-depth--clear-overlays)
      (goto-char (point-min))
      (let ((indent-offset (annotate-depth--determine-indent)))
        (while (and (= 0 (forward-line 1))
                    (not (eobp)))
          (back-to-indentation)
          (when (>= (/ (current-indentation) indent-offset)
                    annotate-depth-threshold)
            (annotate-depth--add-overlay)))))))

(defun annotate-depth--create-idle-timer ()
  "Create idle timer for checking annotation depth. It is buffer-local."
  (when (and (not annotate-depth--idle-timer)
             annotate-depth-idle-timeout)
    (setq annotate-depth--idle-timer
          (run-with-idle-timer annotate-depth-idle-timeout t
                               'annotate-depth--annotate))))

(defun annotate-depth--stop-timer ()
  "Stop idle annotation depth timer, if active."
  (when annotate-depth--idle-timer
    (cancel-timer annotate-depth--idle-timer)
    (setq annotate-depth--idle-timer nil)))


(provide 'annotate-depth)
;;; annotate-depth.el ends here
