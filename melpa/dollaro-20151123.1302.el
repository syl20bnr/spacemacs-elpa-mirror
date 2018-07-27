;;; dollaro.el --- simple text templates

;; Copyright (C) 2013  Alessandro Piras

;; Author: Alessandro Piras <laynor@gmail.com>
;; Keywords: tools, convenience
;; Package-Version: 20151123.1302
;; Package-Requires: ((s "1.6.0"))
;; Version: 0.2

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

;; This package provides a simple text template engine.
;; A template is a simple text containing $[variable-name] blocks.
;;
;; Literal $ symbols can be inserted normally unless they are followed by
;; an open square bracket, in which case they must be doubled.
;;
;; Example:
;; '$$[foo]' will be rendered as '$[foo]' after template substitution.
;;
;; Two functions are provided, `$:fill-template' and
;; `$:fill-template-from-file', that operate respectively on strings
;; and on files. The template variable values are passed as alists.
;;
;; To use these functions, just
;;  (require 'dollaro)
;;
;; Example Usage:
;; ($:fill-template "First Name: $[first-name], Last Name: $[last-name]"
;;                  '((first-name . "Giovanni") (last-name . "Cane")))
;;
;; ($:fill-template-from-file "/path/to/template/file" "/path/to/destination/file"
;;                            '((some-var . "some value") (another-var . "another value")))
;;
;;; Code:

;; Got this one from emacs wiki
(eval-when-compile (require 'cl))
(require 's)

(defun $::map-regex (buffer regex fn)
  "Map the REGEX over the BUFFER executing FN.
   FN is called with the match-data of the regex.
   Returns the results of the FN as a list."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (res)
        (save-match-data
          (while (re-search-forward regex nil t)
            (let ((f (match-data)))
              (setq res
                    (append res
                            (list
                             (save-match-data
                               (funcall fn f))))))))
        res))))

(defun $::$count-before (pos)
  "Returns the number of consecutive $ symbols before POS."
  (let ((i 0)
        (p pos))
    (while (and (> p 1)
                (= (char-before p) ?$))
      (decf p)
      (incf i))
    (list i p)))

(defun $:apply-template-in-current-buffer (var-alist &optional ignore-errors)
  "Performs template variable substitution in the current buffer,
using the bindings defined in VAR-ALIST.
If an empty $[] block or an unbound template variable is found in
the current buffer, an error is signaled, unless IGNORE-ERRORS is
not nil."
  ($::map-regex (current-buffer) "\\$\\[\\([[:word:]-]*\\)\\]"
               (lambda (match-data)
                 (let* (($pos (first match-data))
                        (var-start (third match-data))
                        (var-end (fourth match-data))
                        (end (second match-data))
                        ($$ ($::$count-before (1+ $pos))))
                   (save-excursion
                     (goto-char (second $$))
                     (delete-char (/ (first $$) 2)))
                   (unless (zerop (mod (first $$) 2))
                     (save-excursion
                       (let* ((var-name (s-trim (buffer-substring var-start var-end)))
                              (symbol (intern var-name))
                              (var-bound-p (assoc symbol var-alist))
                              (value (format "%s" (cdr var-bound-p))))
                         (cond (var-bound-p
                                (goto-char $pos)
                                (delete-char (- end $pos))
                                (insert value))
                               ((and (not ignore-errors)
                                     (not (string= "" var-name)))
                                (error "The variable '%S' has not been bound, line %s"
                                       symbol
                                       (line-number-at-pos $pos)))
                               ((not ignore-errors)
                                (error "Empty $[] block at line %s"
                                       (line-number-at-pos $pos)))))))))))


(defun $:fill-template (template-string var-alist &optional ignore-errors)
  "Fills in the template defined by TEMPLATE-STRING using the
bindings defined in VAR-ALIST. The result is returned as a string.
If an empty $[] block or an unbound template variable is found in
the current buffer, an error is signaled, unless IGNORE-ERRORS is
not nil."
  (with-temp-buffer
    (insert template-string)
    ($:apply-template-in-current-buffer var-alist ignore-errors)
    (buffer-string)))

;; add a parameter, signal-error, to signal an error when a variable is not bound
(defun $:fill-template-from-file (template-file destination-file var-alist &optional ignore-errors)
  "Fills in the template defined in the file TEMPLATE-FILE using the
bindings defined in VAR-ALIST. The result is saved to DESTINATION-FILE.
If an empty $[] block or an unbound template variable is found in
the current buffer, an error is signaled, unless IGNORE-ERRORS is
not nil."
  (with-temp-buffer
    (insert-file-contents-literally template-file)
    ($:apply-template-in-current-buffer var-alist ignore-errors)
    (write-file destination-file)))

;; Local Variables:
;; lexical-binding: t
;; End:

(provide 'dollaro)
;;; dollaro.el ends here
