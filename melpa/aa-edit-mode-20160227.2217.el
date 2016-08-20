;;; aa-edit-mode.el --- Major mode for editing AA -*- lexical-binding: t -*-

;; Copyright (C) 2016 USAMI Kenta

;; Author: USAMI Kenta <tadsan@zonu.me>
;; Created: 28 Feb 2016
;; Version: 0.0.1
;; Package-Version: 20160227.2217
;; Keywords: text shiftjis
;; Package-Requires: ((emacs "24") (navi2ch "2.0.0"))

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

;;; Commentary:

;; aa-edit-mode is Major mode for AA (アスキーアート, known as Shift_JIS art).
;;
;; https://en.wikipedia.org/wiki/Shift_JIS_art
;; https://ja.wikipedia.org/wiki/%E3%82%A2%E3%82%B9%E3%82%AD%E3%83%BC%E3%82%A2%E3%83%BC%E3%83%88

;;; Code:
(require 'navi2ch)
(require 'navi2ch-mona)

(defun aa-edit-mode--face ()
  "Return face for display AA."
  (if (eq navi2ch-mona-face-variable t)
      'navi2ch-mona16-face
    navi2ch-mona-face-variable))

;;;###autoload
(define-derived-mode aa-edit-mode text-mode "（´д｀）"
  "Major mode for editing AA"
  (navi2ch-mona-setup)
  (buffer-face-set (aa-edit-mode--face)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mlt\\'" . aa-edit-mode))

(provide 'aa-edit-mode)
;;; aa-edit-mode.el ends here
