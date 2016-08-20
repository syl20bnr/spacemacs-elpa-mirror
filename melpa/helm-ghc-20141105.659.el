;;; helm-ghc.el --- A Helm datasource for ghc-mod errors -*- lexical-binding: t -*-

;; Copyright (C) 2014 David Raymond Christiansen, Galois, Inc

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Maintainer: David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: languages, helm
;; Package-Version: 20141105.659
;; Package-Requires: ((emacs "24") (cl-lib "0.5") (helm "1.6.4") (ghc "5.2.1.0"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is a Helm datasource for GHC-mod errors. When ghc-mod places
;; at least one error overlay in the buffer, running `helm-ghc-errors'
;; will open a buffer that lists the errors.
;;
;; To use it, simply bind the command to a key in `haskell-mode-map'
;; (because ghc-mod isn't yet a proper minor mode). For example:
;; (add-hook 'haskell-mode-hook
;;           (lambda () (define-key haskell-mode-map (kbd "C-c ?") 'helm-ghc-errors)))


;;; Code:

(require 'helm)
(require 'cl-lib)
(require 'ghc-check)


(defun helm-ghc-errors-in-buffer ()
  "Return all the ghc-mod error overlays in the current buffer."
  (with-current-buffer helm-current-buffer
    (cl-remove-if-not #'(lambda (o) (overlay-get o 'ghc-check))
                      (overlays-in (point-min) (point-max)))))

(defun helm-ghc-describe-overlay (overlay)
  "Compute a user-visible description of OVERLAY for the helm
buffer.

Precondition: OVERLAY must be an error or warning overlay
produced by ghc-mod."
  (cl-flet ((abbreviate (str)
                        (if (> (length str) fill-column)
                            (concat (substring str 0 (- fill-column 3)) "...")
                          str)))
    (with-current-buffer (overlay-buffer overlay)
      (let* ((start (overlay-start overlay))
             (end (overlay-end overlay))
             (msg (overlay-get overlay 'ghc-msg)))
        (concat
         (propertize
          (format "%s (%s,%s)-(%s,%s):\n"
                  (buffer-name (overlay-buffer overlay))
                  (line-number-at-pos start)
                  (save-excursion (goto-char start) (current-column))
                  (line-number-at-pos end)
                  (save-excursion (goto-char end) (current-column)))
          'face 'italic)
         (format "%s\n\t%s"
                 (abbreviate (buffer-substring start end))
                 (if msg (abbreviate msg) "")))))))

(defun helm-ghc-candidates ()
  "Return a Helm-formatted list of ghc-mod errors."
  (with-current-buffer helm-current-buffer
    (mapcar (lambda (overlay) (cons (helm-ghc-describe-overlay overlay) overlay))
            (helm-ghc-errors-in-buffer))))


(defun helm-ghc-goto-overlay (overlay)
  "Hop to the GHC error at OVERLAY."
  (with-current-buffer (overlay-buffer overlay)
    (goto-char (overlay-start overlay))
    (ghc-display-errors)))

(defvar helm-ghc-errors-source
  '((name . "GHC notes")
    (candidates . helm-ghc-candidates)
    (action . (("Go to error" . helm-ghc-goto-overlay)))
    (multiline))
  "A helm datasource for ghc-mod errors in the current buffer.")

;;;###autoload
(defun helm-ghc-errors ()
  "Use Helm to browse the ghc-mod error annotations in the current buffer."
  (interactive)
  (helm :sources helm-ghc-errors-source
        :buffer "*helm-ghc*"))

(provide 'helm-ghc)
;;; helm-ghc.el ends here
