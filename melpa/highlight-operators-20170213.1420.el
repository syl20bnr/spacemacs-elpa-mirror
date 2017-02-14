;;; highlight-operators.el --- a face for operators in programming modes -*- lexical-binding: t -*-

;; Copyright (C) 2016 Jonathan Kotta

;; Author: Jonathan Kotta <jpkotta@gmail.com>
;; Maintainer: Jonathan Kotta <jpkotta@gmail.com>
;; Version: 0.1
;; Package-Version: 20170213.1420

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library defines a face named `highlight-operators-face' used
;; for operators (e.g. '+' and '&') in programming modes.
;; `highlight-operators-mode' is a minor mode that enables this extra
;; highlighting.  Add `highlight-operators-mode' to your favorite
;; programming major mode hooks (e.g. (add-hook 'python-mode
;; 'highlight-operator-mode)) or add it to `prog-mode-hook'.

;; This mode doesn't work well with lisp major modes.

;;; Code:

(defgroup highlight-operators nil
  "Extra highlighting for operators (e.g. '+' or '&') in programming modes."
  :group 'font-lock-extra-types
  :group 'faces)

(defface highlight-operators-face '((t (:inherit font-lock-builtin-face)))
  "Face for operators (e.g. '+' or '&') in programming modes.
This face is only used if `highlight-operators-mode' is turned on."
  :group 'highlight-operators)

(defcustom highlight-operators-regexp
  (regexp-opt '("+" "-" "*" "/" "%" "!"
                "&" "^" "~" "|"
                "=" "<" ">"
                "." "," ";" ":" "?"))
  "Regex to match operators."
  :group 'highlight-operators)
;;(make-variable-buffer-local 'highlight-operators-regexp)

;;;###autoload
(define-minor-mode highlight-operators-mode
  "Extra highlighting for operators in programming modes."
  :lighter ""
  :group 'highlight-operators
  (let ((font-lock-spec `((,highlight-operators-regexp
                           0 'highlight-operators-face keep))))
    (if highlight-operators-mode
        (font-lock-add-keywords nil font-lock-spec)
      (font-lock-remove-keywords nil font-lock-spec))
    (when (called-interactively-p 'any)
      (font-lock-flush))))

;;;###autoload
(define-globalized-minor-mode global-highlight-operators-mode
  highlight-operators-mode (lambda () (highlight-operators-mode 1))
  :group 'highlight-operators)

(provide 'highlight-operators)

;;; highlight-operators.el ends here
