;;; hgrc-mode.el --- major mode for editing hgrc files -*- lexical-binding: t -*-

;;; Copyright (C) 2015 Omair Majid

;; Author: Omair Majid <omair.majid@gmail.com>
;; URL: http://github.com/omajid/hgrc-mode
;; Package-Version: 20150409.2043
;; Keywords: convenience vc hg
;; Version: 0.1.20150403

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A major mode for editing hgrc files.

;;; Code:

;;;###autoload
(define-derived-mode hgrc-mode conf-unix-mode "hgrc"
  "A major mode for editing hgrc files."
  (let ((table hgrc-mode-syntax-table))
    (modify-syntax-entry ?\; "<" table)))

;;;###autoload
(dolist (pattern '("/\\.hgrc\\'"
                   "/\\.hg/hgrc\\'"
                   "/Mercurial\\.ini\\'"))
  (add-to-list 'auto-mode-alist (cons pattern 'hgrc-mode)))

(provide 'hgrc-mode)

;;; hgrc-mode.el ends here
