;;; strace-mode.el --- strace output syntax highlighting

;; Copyright © 2016, by Preston Moore

;; Author: Preston Moore (prestonkmoore@gmail.com)
;; Version: 0.0.2
;; Package-Version: 20160710.2134
;; Keywords: languages

;; This file is not part of GNU Emacs.

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

;; This package provides a major mode with highlighting for output
;; from `strace'.

;;; Code:

;; create the list for font-lock.
;; each category of keyword is given a particular face
(defvar strace-font-lock-keywords)
(setq strace-font-lock-keywords `(
                                  ("^\\([0-9]+\\) " . (1 font-lock-warning-face))
                                  ("^[0-9]+ \\([a-zA-Z0-9_]*\\)(" . (1 font-lock-constant-face))
                                  (" = \\(0x[[:xdigit:]]+\\).*$" . (1 font-lock-type-face))
                                  (" = \\(-?[[:digit:]?]+\\).*$" . (1 font-lock-type-face))
                                  (" = 0x[[:xdigit:]]+ \\([[:upper:]]+\\).*$" . (1 font-lock-negation-char-face))
                                  (" = -?[[:digit:]?]+ \\([[:upper:]]+\\).*$" . (1 font-lock-negation-char-face))
                                  (" \\((.*)\\)$" . (1 font-lock-comment-face))
                                  ("0x[[:xdigit:]]+" . font-lock-type-face)
                                  ("-?[[:digit:]]+" . font-lock-type-face)
                                  )
)

;;;###autoload
(define-derived-mode strace-mode fundamental-mode
  "strace"
  "Major mode for strace output."
  (setq font-lock-defaults '((strace-font-lock-keywords)))
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.strace\\'" . strace-mode))

;; add the mode to the `features' list
(provide 'strace-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;;; strace-mode.el ends here
