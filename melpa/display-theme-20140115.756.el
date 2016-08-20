;;; display-theme.el --- display current theme(s) at mode-line -*- lexical-binding: t -*-

;; Copyright (C) 2013 KAWABATA, Taichi

;; Filename: display-theme.el
;; Description: display current theme(s) at mode-line.
;; Author: KAWABATA, Taichi <kawabata.taichi_at_gmail.com>
;; Created: 2013-09-29
;; Modified: 2013-09-30
;; Version: 1.0
;; Package-Version: 20140115.756
;; Keywords: tools
;; Package-Requires: ((emacs "24"))
;; URL: https://github.com/kawabata/emacs-display-theme/

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

;; This utility displays current theme(s) at mode-line.

;;; Code:

(require 'custom)

(defgroup display-theme nil
  "Display current theme(s) at mode-line."
  :group 'mode-line)

(defcustom display-theme-format " [%s]"
  "*Format to display current theme(s) at mode-line."
  :type 'string
  :group 'display-theme)

(defun display-theme (&rest _args)
  (format display-theme-format
          (if custom-enabled-themes
              ;; remove parenthesis surrounding S-expression.
              (substring (format "%s" custom-enabled-themes) 1 -1)
            "none")))

;;;###autoload
(define-minor-mode display-theme-mode
  "Minor mode to display current theme(s) at mode-line."
  :init-value nil
  :lighter (:eval (display-theme)))

;;;###autoload
(define-global-minor-mode global-display-theme-mode
  display-theme-mode display-theme-mode)

(provide 'display-theme)

;; Local Variables:
;; time-stamp-pattern: "10/Modified:\\\\?[ \t]+%:y-%02m-%02d\\\\?\n"
;; End:

;;; display-theme.el ends here
