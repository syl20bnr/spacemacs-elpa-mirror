;;; sourcemap.el --- Sourcemap parser -*- lexical-binding: t; -*-

;; Copyright (C) 2014 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-sourcemap
;; Package-Version: 0.2
;; Version: 0.02
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))

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

;;; Code:

(require 'cl-lib)
(require 'json)

(cl-defstruct sourcemap-entry
  source generated-line generated-column original-line original-column name)

(defconst sourcemap--vlq-shift-width 5)

(defconst sourcemap--vlq-mask (1- (ash 1 5))
  "0b011111")

(defconst sourcemap--vlq-continuation-bit (ash 1 5)
  "0b100000")

(defvar sourcemap--char2int-table (make-hash-table :test 'equal))

(let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
  (cl-loop for char across chars
           for index = 0 then (1+ index)
           do
           (progn
             (puthash char index sourcemap--char2int-table))))

(defsubst sourcemap--vlq-continuation-value-p (value)
  (not (zerop (logand value sourcemap--vlq-continuation-bit))))

(defsubst sourcemap--vlq-value (value)
  (logand value sourcemap--vlq-mask))

(defun sourcemap--base64-decode (char)
  (let ((val (gethash char sourcemap--char2int-table 'not-found)))
    (if (eq val 'not-found)
        (error "Invalid input '%s'" (char-to-string char))
      val)))

(defun sourcemap--from-vlq-signed (value)
  (let ((negative-p (= (logand value 1) 1))
        (shifted (ash value -1)))
    (if negative-p
        (- shifted)
      shifted)))

(defun sourcemap-base64-vlq-decode (str start limit)
  (cl-loop for i from start below limit
           for index = 1 then (1+ index)
           for shift = 0 then (+ shift sourcemap--vlq-shift-width)
           for digit = (sourcemap--base64-decode (aref str i))
           for value = (sourcemap--vlq-value digit)
           for result = value then (+ result (ash value shift))
           unless (sourcemap--vlq-continuation-value-p digit)
           return (list :value (sourcemap--from-vlq-signed result)
                        :rest-index (+ index start))))

(defsubst sourcemap--starts-with-separator-p (char)
  (member char '(?\; ?,)))

(defun sourcemap--retrieve (sourcemap property)
  (let ((value (assoc-default property sourcemap)))
    (if (not value)
        (error "Can't found '%s' property" property)
      value)))

(defsubst sourcemap--sources-at (sourcemap index)
  (aref (sourcemap--retrieve sourcemap 'sources) index))

(defsubst sourcemap--names-at (sourcemap index)
  (aref (sourcemap--retrieve sourcemap 'names) index))

(defun sourcemap--parse-mappings (sourcemap)
  (let* ((str (sourcemap--retrieve sourcemap 'mappings))
         (generated-line 1) (previous-generated-column 0)
         (previous-source 0) (previous-original-line 0)
         (previous-original-column 0) (previous-name 0)
         (mappings '())
         temp (curpos 0) (limit (length str)))
    (while (< curpos limit)
      (let ((first-char (aref str curpos)))
        (cond ((= first-char ?\;)
               (cl-incf curpos)
               (setq generated-line (1+ generated-line)
                     previous-generated-column 0))
              ((= first-char ?,)
               (cl-incf curpos))
              (t
               (let ((mapping (make-sourcemap-entry :generated-line generated-line)))
                 ;; Generated Column
                 (setq temp (sourcemap-base64-vlq-decode str curpos limit))
                 (let ((current-column (+ previous-generated-column (plist-get temp :value))))
                   (setq previous-generated-column current-column)
                   (setf (sourcemap-entry-generated-column mapping) current-column))

                 ;; Original source
                 (setq curpos (plist-get temp :rest-index))
                 (when (and (< curpos limit)
                            (not (sourcemap--starts-with-separator-p (aref str curpos))))
                   (setq temp (sourcemap-base64-vlq-decode str curpos limit))
                   (let* ((source-value (plist-get temp :value))
                          (current-source (+ previous-source source-value)))
                     (setf (sourcemap-entry-source mapping)
                           (sourcemap--sources-at sourcemap current-source))
                     (cl-incf previous-source source-value))
                   (setq curpos (plist-get temp :rest-index))
                   (when (or (= (1- curpos) limit)
                             (sourcemap--starts-with-separator-p (aref str curpos)))
                     (error "Found a source, but no line and column"))

                   ;; Original line
                   (setq temp (sourcemap-base64-vlq-decode str curpos limit))
                   (let ((original-line (+ previous-original-line (plist-get temp :value))))
                     (setq previous-original-line original-line)
                     (setf (sourcemap-entry-original-line mapping) (1+ original-line)))
                   (setq curpos (plist-get temp :rest-index))

                   ;; Original column
                   (setq temp (sourcemap-base64-vlq-decode str curpos limit))
                   (let ((original-column (+ previous-original-column
                                             (plist-get temp :value))))
                     (setf (sourcemap-entry-original-column mapping) original-column)
                     (setq previous-original-column original-column))
                   (setq curpos (plist-get temp :rest-index))
                   ;; Original name
                   (when (and (< curpos limit)
                              (not (sourcemap--starts-with-separator-p (aref str curpos))))
                     (setq temp (sourcemap-base64-vlq-decode str curpos limit))
                     (let* ((name-value (plist-get temp :value))
                            (name-index (+ previous-name name-value)))
                       (setf (sourcemap-entry-name mapping)
                             (sourcemap--names-at sourcemap name-index))
                       (cl-incf previous-name name-value))
                     (setq curpos (plist-get temp :rest-index))))
                 (push mapping mappings))))))
    (reverse mappings)))

(defun sourcemap-generated-position-for (sourcemap &rest props)
  (let ((mappings (sourcemap--parse-mappings sourcemap))
        (original-source (plist-get props :source))
        (line (plist-get props :line))
        (column (plist-get props :column)))
    (cl-loop for map in mappings
             for source = (sourcemap-entry-source map)
             for original-line = (sourcemap-entry-original-line map)
             for original-column = (sourcemap-entry-original-column map)
             when (and (string= original-source source)
                       (= original-line line) (= original-column column))
             return (list :line (sourcemap-entry-generated-line map)
                          :column (sourcemap-entry-generated-column map)))))

(defun sourcemap-original-position-for (sourcemap &rest props)
  (let ((mappings (sourcemap--parse-mappings sourcemap))
        (line (plist-get props :line))
        (column (plist-get props :column)))
    (cl-loop for map in mappings
             for generated-line = (sourcemap-entry-generated-line map)
             for generated-column = (sourcemap-entry-generated-column map)
             when (and (= generated-line line) (= generated-column column))
             return (list :line (sourcemap-entry-original-line map)
                          :column (sourcemap-entry-original-column map)))))

(defun sourcemap--compare-mapping (target here)
  (let ((target-line (sourcemap-entry-original-line target))
        (target-column (sourcemap-entry-original-column target))
        (source-line (sourcemap-entry-original-line here))
        (source-column (sourcemap-entry-original-column here)))
    (cond ((< target-line source-line) 1)
          ((> target-line source-line) -1)
          ((< target-column source-column) 1)
          ((> target-column source-column) -1)
          (t 0))))

(defsubst sourcemap--line-distance (here a)
  (abs (- (sourcemap-entry-original-line here) (sourcemap-entry-original-line a))))

(defsubst sourcemap--column-distance (here a)
  (abs (- (sourcemap-entry-original-column here) (sourcemap-entry-original-column a))))

(defun sourcemap--select-nearest (here low high)
  (let ((distance-low (sourcemap--line-distance here low))
        (distance-high (sourcemap--line-distance here high)))
    (cond ((< distance-low distance-high) low)
          ((> distance-low distance-high) high)
          (t
           (let ((distance-low (sourcemap--column-distance here low))
                 (distance-high (sourcemap--column-distance here high)))
            (cond ((<= distance-low distance-high) low)
                  ((> distance-low distance-high) high)))))))

(defun sourcemap--binary-search (mappings here)
  (let ((v-mappings (vconcat mappings))
        (low 0) (high (1- (length mappings)))
        finish matched last-low last-high)
    (while (and (< low high) (not finish))
      (let* ((middle-index (/ (+ low high) 2))
             (middle (aref v-mappings middle-index))
             (compare-value (sourcemap--compare-mapping middle here)))
        (setq last-low low last-high high)
        (cond ((= compare-value -1)
               (setq high (1- middle-index)))
              ((= compare-value 1)
               (setq low (1+ middle-index)))
              (t
               (setq finish t
                     matched middle)))))
    (if finish
        matched
      (sourcemap--select-nearest here
                                 (aref v-mappings last-low)
                                 (aref v-mappings last-high)))))

(defsubst sourcemap--filter-same-file (mappings source)
  (cl-remove-if-not (lambda (a)
                      (string= source (sourcemap-entry-source a))) mappings))

;;;###autoload
(defun sourcemap-goto-corresponding-point (props)
  "hook function of coffee-mode. This function should not be used directly.
This functions should be called in generated Javascript file."
  (let ((sourcemap-file (plist-get props :sourcemap)))
    (unless sourcemap-file
      (error "Error: ':sourcemap' property is not set"))
    (let* ((sourcemap (sourcemap-from-file sourcemap-file))
           (mappings (sourcemap--parse-mappings sourcemap))
           (source-file (file-name-nondirectory (plist-get props :source)))
           (samefile-mappings (sourcemap--filter-same-file mappings source-file)))
      (if (not samefile-mappings)
          (message "Informations in '%s' are not found" source-file)
        (let* ((here (make-sourcemap-entry :original-line (plist-get props :line)
                                           :original-column (plist-get props :column)))
               (nearest (sourcemap--binary-search mappings here)))
          (forward-line (1- (sourcemap-entry-generated-line nearest)))
          (move-to-column (sourcemap-entry-generated-column nearest)))))))

;;;###autoload
(defun sourcemap-from-file (file)
  (interactive
   (list (read-file-name "Sourcemap File: " nil nil t)))
  (json-read-file file))

;;;###autoload
(defun sourcemap-from-string (str)
  (json-read-from-string str))

(provide 'sourcemap)

;;; sourcemap.el ends here
