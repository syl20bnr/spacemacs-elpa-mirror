;;; all-the-icons-gnus.el --- Shows icons for in Gnus  -*- lexical-binding: t; -*-

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; Version: 0.1.0
;; Package-Version: 20170718.2354
;; Keywords: mail tools
;; Package-Requires: ((emacs "24.4") (dash "2.12.0") (all-the-icons "3.1.0"))

;; Copyright (C) 2017 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software: you can redistribute it and/or modify
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

;; To use this package, do
;;
;; (require 'all-the-icons-gnus)
;; (all-the-icons-gnus-setup)
;;

;;; Code:

(require 'gnus)
(require 'all-the-icons)

(setq pretty-gnus-article-alist nil)

(defmacro all-the-icons-gnus--pretty-gnus (word icon props)
  "Replace sanitized word with icon, props."
  `(add-to-list 'pretty-gnus-article-alist
               (list (rx bow (group ,word " : "))
                     ,icon ',props)))

(all-the-icons-gnus--pretty-gnus "From: "              ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "Subject: "           ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "To: "                ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "CC: "                ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "Reply-To: "          ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "Date: "              ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "Organization: "      ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "Content-Type: "      ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "User-Agent: "        ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "X-mailer: "          ? (:foreground "#375E97" :height 1.2))
(all-the-icons-gnus--pretty-gnus "X-PGP-Fingerprint: " ? (:foreground "#375E97" :height 1.2))

(defun all-the-icons-gnus--add-faces ()
  "Add face properties and compose symbols for buffer from pretty-gnus-article."
  (interactive)
  (with-silent-modifications
    (--each pretty-gnus-article-alist
      (-let (((rgx icon props) it))
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp rgx nil t)
            (compose-region
             (match-beginning 1) (match-end 1) icon)
            (when props
              (add-face-text-property
               (match-beginning 1) (match-end 1) props))))))))


(defun all-the-icons-gnus--set-format ()
  (setq gnus-topic-line-format "%i[  %(%{%n -- %A%}%) ]%v\n"

        gnus-group-line-format "%1M%1S%5y  : %(%-50,50G%)\n"

        gnus-summary-line-format "%1{%U%R%z: %}%[%2{%&user-date;%}%]  %4{%-34,34n%} %3{ %}%(%1{%B%}%s%)\n"

        gnus-user-date-format-alist '((t . " %Y-%m-%d %H:%M"))

        gnus-sum-thread-tree-root " "
        gnus-sum-thread-tree-false-root " "
        gnus-sum-thread-tree-single-indent " "
        gnus-sum-thread-tree-leaf-with-other " "
        gnus-sum-thread-tree-vertical " "
        gnus-sum-thread-tree-single-leaf " "))

;;;###autoload
(defun all-the-icons-gnus-setup ()
  "Add icons for Gnus."
  ;; (advice-add 'gnus-summary-next-article :after 'all-the-icons-gnus--add-faces)
  (all-the-icons-gnus--set-format))


(provide 'all-the-icons-gnus)
;;; all-the-icons-gnus.el ends here
