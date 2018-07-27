;;; ac-capf.el --- auto-complete source with completion-at-point

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-ac-capf
;; Package-Version: 20151101.217
;; Version: 0.01
;; Package-Requires: ((auto-complete "1.4") (cl-lib "0.5"))

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

;; An auto-complete source for completion-at-point.

;;; Code:

(require 'auto-complete)
(require 'cl-lib)

;; Most code is taken from `company-mode'

(defun ac-capf--candidates-response ()
  (cl-letf* (((default-value 'completion-at-point-functions)
              (remove 'tags-completion-at-point-function
                      (default-value 'completion-at-point-functions)))
             (data (run-hook-wrapped 'completion-at-point-functions
                                     #'completion--capf-wrapper 'optimist)))
    (when (and (consp (cdr data)) (numberp (nth 1 data)))
      data)))

(defun ac-capf--candidates ()
  (let ((res (ac-capf--candidates-response)))
    (when (and res ac-prefix)
      (let* ((table (nth 3 res))
             (pred (plist-get (nthcdr 4 res) :predicate))
             (meta (completion-metadata
                    (buffer-substring (nth 1 res) (nth 2 res))
                    table pred))
             (candidates (completion-all-completions ac-prefix table pred (length ac-prefix)))
             (last (last candidates))
             (base-size (and (numberp (cdr last)) (cdr last))))
        (when base-size
          (setcdr last nil))
        (if (not (zerop (or base-size 0)))
            (let ((before (substring arg 0 base-size)))
              (mapcar (lambda (candidate)
                        (concat before candidate))
                      candidates))
          (mapcar (lambda (c) (substring-no-properties c)) candidates))))))

(ac-define-source capf
  `((candidates . ac-capf--candidates)
    (requires . 0)
    (symbol . "s")))

;;;###autoload
(defun ac-capf-setup ()
  "Add `ac-source-capf' to `ac-sources' and enable `auto-complete' mode"
  (interactive)
  (add-to-list 'ac-sources 'ac-source-capf)
  (unless auto-complete-mode
    (auto-complete-mode +1)))

(provide 'ac-capf)

;;; ac-capf.el ends here
