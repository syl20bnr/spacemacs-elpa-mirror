;;; bbdb-china.el --- BBDB utils, which let Chinese BBDB users feel easy

;; Copyright (c) 2015, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/bbdb-china
;; Package-Version: 20150615.1856
;; Version: 0.0.1
;; Package-Requires: ((bbdb-vcard "0.4.1")(chinese-pyim "0.0.1"))

;;; License:

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ## Introduce ##
;;
;; bbdb-china includes some utils, which let Chinese BBDB users feel easy.
;;
;; 1. Search contacts by pinyin
;;
;;
;; ## Download ##
;;
;;     https://github.com/tumashu/bbdb-china
;;
;; ## Install ##
;;
;; 1. Config melpa: http://melpa.org/#/getting-started
;; 2. M-x package-install RET bbdb-china RET
;; 3. Add code to your emacs config file:（for example: ~/.emacs）：
;;
;; ```lisp
;; (require 'bbdb-china)
;; ```

;;; Code:
(require 'bbdb)
(require 'bbdb-vcard)
(require 'chinese-pyim)

(defun bbdb-china-bbdb-puthash (orig-fun key record)
  (funcall orig-fun key record)
  (when (and key (not (string= "" key))
             (string-match-p "\\cc" key)
             (featurep 'chinese-pyim))
    (let ((key-pinyin-1  (pyim-hanzi2pinyin key))
          (key-pinyin-2  (pyim-hanzi2pinyin key t)))
      (funcall orig-fun key-pinyin-1 record)
      (funcall orig-fun key-pinyin-2 record))))

(defun bbdb-china-bbdb-remhash (orig-fun key record)
  (funcall orig-fun key record)
  (when (and key (not (string= "" key))
             (string-match-p "\\cc" key)
             (featurep 'chinese-pyim))
    (let ((key-pinyin-1  (pyim-hanzi2pinyin key))
          (key-pinyin-2  (pyim-hanzi2pinyin key t)))
      (funcall orig-fun key-pinyin-1 record)
      (funcall orig-fun key-pinyin-2 record))))

;; Select contact record by pinyin when merge records.
(advice-add 'bbdb-puthash :around #'bbdb-china-bbdb-puthash)
(advice-add 'bbdb-remhash :around #'bbdb-china-bbdb-remhash)

;; Add pinyin abbreviation for BBDB recordes
(defun bbdb-china-add-pinyin-abbreviation (record)
  "Add pinyin abbrev field for `record', which records
the pinyins of firstname and lastname."
  (when (featurep 'chinese-pyim)
    (let* ((first-name
            (bbdb-china-return-chinese-string
             (bbdb-record-firstname record)))
           (last-name
            (bbdb-china-return-chinese-string
             (bbdb-record-lastname record)))
           pinyin-list)
      (setq pinyin-list
            (delete-dups
             `(,@(when first-name (pyim-hanzi2pinyin first-name nil nil t))
               ,@(when last-name (pyim-hanzi2pinyin last-name nil nil t))
               ,@(when first-name (pyim-hanzi2pinyin first-name t nil t))
               ,@(when last-name (pyim-hanzi2pinyin last-name t nil t)))))
      (bbdb-record-set-xfield
       record 'pinyin-abbrev (mapconcat 'identity pinyin-list ", ")))))

(defun bbdb-china-return-chinese-string (str)
  "When `str' include chinese char, return `str',otherwise
return `nil'."
  (when (and str (string-match-p "\\cc" str))
    str))

;; Add pinyin abbreviation, which make search chinese easily.
(add-hook 'bbdb-change-hook 'bbdb-china-add-pinyin-abbreviation)
;; Don't export pinyin-abbrev to vcard
(push "^pinyin-abbrev" bbdb-vcard-skip-on-export)

(provide 'bbdb-china)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; bbdb-china.el ends here
