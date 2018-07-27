;;; i2b2-mode.el --- Highlights corresponding PHI data in the text portion of an i2b2 XML Document.

;; Copyright (C) 2014 Dan LaManna

;; Author: Dan LaManna <dan.lamanna@gmail.com>
;; Version: 1.0
;; Package-Version: 20140710.104
;; Keywords: xml,phi,i2b2,deidi2b2

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
;;  A minor mode applicable to maybe 5 people (being generous) worldwide.

(require 'cl)
(require 'xml)

;;; Code:

;;;###autoload
(define-minor-mode i2b2-mode
  "Matching highlighting of PHI data."
  nil
  nil
  nil
  (kill-all-local-variables)
  (set (make-local-variable 'i2b2-overlays) '())
  (i2b2--add-overlays))

(defcustom i2b2-phi-tag-colors '((NAME . "#9ACCCD")
                                 (PROFESSION . "#8FD8A0")
                                 (LOCATION . "#CBD890")
                                 (AGE . "#DACC8F")
                                 (DATE . "#D9A790")
                                 (CONTACT . "#D18FD9"))
  "Colors to use as the background for PHI tags.")

(defcustom i2b2-else-tag-color "#FFFAF0"
  "Color to use as background for any other PHI Tag not in `i2b2-phi-tag-colors'")

(defun i2b2-get-phi(&optional buffer)
  "Returns all PHI Tags in the form of a cons with the XML Attributes as properties."
  (remove-if 'stringp
             (xml-node-children
              (assq 'TAGS (car
                           (xml-parse-region nil nil (or buffer
                                                         (current-buffer))))))))

(defun i2b2-background-phi-tag(phi-tag &optional buffer)
  "Takes a PHI Tag and a buffer and takes it's start and end attributes and sets an overlay
   behind those points."
  (let* ((start-point (string-to-number (cdr (assq 'start (car (cdr phi-tag))))))
         (end-point (string-to-number (cdr (assq 'end (car (cdr phi-tag))))))
         (o (make-overlay (+ (i2b2-text-start-pos) start-point) (+ (i2b2-text-start-pos) end-point))))
    (setq-local i2b2-overlays (append `(,o) i2b2-overlays))
    (with-current-buffer (or buffer
                             (current-buffer))
      (overlay-put o 'face `((background-color . ,(or (cdr (assoc (car phi-tag) i2b2-phi-tag-colors))
                                                      i2b2-else-tag-color))
                             (foreground-color . "black"))))))

(defun i2b2--add-overlays(&rest args)
  "Adds all overlays from PHI Tags, removes any existing i2b2 overlays first, in case of repositioning."
  (i2b2-remove-overlays)
  (mapc (lambda (phi-tag)
          (i2b2-background-phi-tag phi-tag (current-buffer)))
        (i2b2-get-phi (current-buffer))))

(defun i2b2-remove-overlays()
  "Removes all existing i2b2 overlays."
  (mapc 'delete-overlay i2b2-overlays))

(defun i2b2-text-start-pos(&optional buffer)
  "Hack, gets actual starting position of TEXT node, assumes CDATA tag will be there."
  (with-current-buffer (or buffer
                           (current-buffer))
    (+ (string-match "\\(<TEXT>[[:space:]]?+<!\\[CDATA\\[\\)" (buffer-string))
       (length (match-string-no-properties 0 (buffer-string)))
       1)))

;;;###autoload
(defun i2b2-buffer-p(&optional buffer)
  "Checks if this is a valid i2b2 buffer."
  (string-equal
   "deIdi2b2"
   (ignore-errors
     (car (car (xml-parse-region nil nil (or buffer (current-buffer))))))))

(provide 'i2b2-mode)

;;; i2b2-mode.el ends here
