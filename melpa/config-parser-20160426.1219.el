;;; config-parser.el --- a library for parsing config file

;; Copyright (C) 2004-2015 DarkSun <lujun9972@gmail.com>.

;; Author: DarkSun <lujun9972@gmail.com>
;; Created: 2016-01-06
;; Version: 0.1
;; Package-Version: 20160426.1219
;; Keywords: convenience, config
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/lujun9972/el-config-parser

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
;; config-parser's code can be found here:
;;   http://github.com/lujun9972/el-config-parser

;;; Commentary:

;; config-parser is a library for parsing config file with similar interface of config parser module in python

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defun config-parser--parse-section (line)
  "Parse \"[section]\" to '(\"section\")"
  (when (string-match "^\\[\\([^]]+\\)\\]$" line)
    (list (match-string 1 line))))

(defun config-parser--parse-option (line sep)
  "Parse \"key=val\" to '(\"key\". \"val\"). Here assume SEP is ="
  (when (string-match (format "^\\([^%s[:space:]]+\\)[[:space:]]*%s[[:space:]]*\\(.+\\)$" sep sep) line)
    (cons (match-string 1 line)
          (match-string 2 line))))

;;;###autoload
(defun config-parser-read (files &optional sep)
  "Read and parse a file(FILES is a string) or a list of files(FILES is a list of string), the result is an alist which car element is the section and cdr element is options in the section

options is also an alist like '(key . value) "
  (let ((sep (or sep ":"))
        (files (cond ((stringp files)
                      (list files))
                     ((listp files)
                      files)
                     (t (error "invalie files:%s" files))))
        (section '(""))
        result)
    (dolist (file files result)
      (let* ((file-content (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string)))
             (file-lines (mapcar #'string-trim (split-string file-content "[\r\n]+")))
             (valid-file-lines (cl-remove-if (lambda (line)
                                            (or (string-equal line "")
                                                (string-prefix-p "#" line)
                                                (string-prefix-p ";" line)))
                                          file-lines)))
        (dolist (line valid-file-lines)
          (cond ((config-parser--parse-section line)
                 (unless (equal section '("")) ;empty section
                   (push (reverse section) result))
                 (setq section (config-parser--parse-section line)))
                ((config-parser--parse-option line sep)
                 (push (config-parser--parse-option line sep) section))
                (t (error "invalid line:%s" line))))))
    (push (reverse section) result)
    (reverse result)))

(defun config-parser--insert-section (section)
  "Insert the SECTION data"
  (let ((section-name (cond ((stringp section)
                             section)
                            ((listp section)
                             (car section)))))
    (unless (string-equal section-name "")
      (insert (format "[%s]" section-name))
      (newline))))

(defun config-parser--insert-option (option sep)
  "Insert the OPTION data with SEP as the delimiter"
  (let ((key (car option))
        (value (cdr option)))
    (insert (format "%s%s%s" key sep value))
    (newline)))

;;;###autoload
(defun config-parser-write (file config-data &optional sep)
  "Write CONFIG-DATA in FILE with SEP as the delimiter"
  (let* ((sep (or sep ":")))
    (with-temp-file file
      (dolist (section config-data)
        (let ((section-name (car section))
              (options (cdr section)))
          (config-parser--insert-section section-name)
          (dolist (option options)
            (config-parser--insert-option option sep)))))))

;;;###autoload
(defun config-parser-sections (config-data)
  "Return all the configuration section names"
  (mapcar #'car config-data))

;;;###autoload
(defun config-parser-has-section (config-data section)
  "Return whether the given SECTION exists"
  (member section (config-parser-sections config-data)))

;;;###autoload
(defun config-parser-items (config-data section)
  "return a list (name . value) for each option in the section. "
  (let ((section-data (assoc section config-data)))
    (cdr section-data)))

;;;###autoload
(defun config-parser-options (config-data section)
  "Return list of configuration options for the named SECTOIN"
  (let* ((options-data (config-parser-items config-data section)))
    (mapcar #'car options-data)))

;;;###autoload
(defun config-parser-has-option (config-data section option)
  "Return whether the given optioin exists in the given section"
  (member option (config-parser-options config-data section)))

;;;###autoload
(defun config-parser-get-section (config-data section)
  "Return section data"
  (assoc section config-data))

;;;###autoload
(defun config-parser-get (config-data section option)
  "Return a string value for the named option"
  (let* ((options-data (config-parser-items config-data section))
         (option-data (assoc option options-data)))
    (cdr option-data)))

;;;###autoload
(defun config-parser-get-number (config-data section option &optional base)
  "like `config-parser-get' but convert valut to an number. If BASE, interpret STRING as a number in that base, default to be 10"
  (ignore-errors (string-to-number (config-parser-get config-data section option) base)))

;;;###autoload
(defun config-parser-get-boolean (config-data section option)
  "like `config-parser-get' but convert valut to a boolean.

currently case insensitively defined as 0, false, no, off, nil \"\" for nil, 
otherwise for t).  Returns nil or otherwise. "
  (let ((value (config-parser-get config-data section option)))
    (if (member (downcase value) '("0" "false" "no" "off" "nil" ""))
        nil
      value)))

;;;###autoload
(defun config-parser-delete-section! (config-data section)
  "remove the given file section and all its options"
  (cl-delete-if (lambda (section-data)
                  (string-equal section (car section-data)))
                config-data))

;;;###autoload
(defun config-parser-delete-option! (config-data section option)
  "Remove the given option from the given section"
  (when (config-parser-has-option config-data section option)
    (let ((section-data (assoc section config-data)))
      (setf (cdr section-data) (cl-delete-if (lambda (option-data)
                                               (string-equal (car option-data) option))
                                             (cdr section-data)))))
  config-data)

;;;###autoload
(defun config-parser-make-section (section &rest options)
  ""
  (cl-check-type section string)
  (append (list section) options))

;;;###autoload
(defun config-parser-make-option (key value)
  ""
  (cons (format "%s" key) (format "%s" value)))

;;;###autoload
(defun config-parser-set! (config-data section option value &optional create-p)
  "set the given option"
  (if (null config-data)
      (setq config-data
            (when create-p
              (list (config-parser-make-section section
                                                (config-parser-make-option option value)))))
    (if (and create-p
             (not (config-parser-has-option config-data section option)))
        (progn
          (let ((new-option (config-parser-make-option option value)))
            (if (config-parser-has-section config-data section)
                (nconc (config-parser-get-section config-data section) (list new-option))
              (nconc config-data (list (config-parser-make-section section new-option))))))
      (let* ((options-data (config-parser-items config-data section))
             (option-data (assoc option options-data)))
        (when option-data
          (setf (cdr option-data) value)))))
  config-data)

(provide 'config-parser)

;;; config-parser.el ends here
