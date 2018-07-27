;;; flycheck-dialyxir.el --- flycheck checker for elixir dialyxir

;; Copyright (C) 2016 by Aaron Jensen

;; Author: Aaron Jensen <aaronjensen@gmail.com>
;; URL: https://github.com/aaronjensen/flycheck-dialyxir
;; Package-Version: 20170515.1525
;; Version: 0.1.0
;; Package-Requires: ((flycheck "29"))

;;; Commentary:

;; This package adds support for dialyxir to flycheck.

;; To use it, require it and ensure you have elixir-mode set up for flycheck:

;;   (eval-after-load 'flycheck
;;     '(flycheck-dialyxir-setup))
;;   (add-hook 'elixir-mode-hook 'flycheck-mode)

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'flycheck)

(defun flycheck-dialyxir--project-root (&rest _ignored)
  "Find directory with mix.exs."
  (and buffer-file-name
       (locate-dominating-file buffer-file-name "mix.exs")))

(flycheck-define-checker elixir-dialyxir
  "Elixir dialyxir checker."
  :command ("mix" "dialyzer" "--no-check")
  :predicate
  (lambda ()
    (and
     (flycheck-buffer-saved-p)
     (file-exists-p "deps/dialyxir")))
  :error-patterns
  ((warning line-start
            (file-name)
            ":"
            line
            ": warning: "
            (message)
            line-end)
   (error line-start
          (file-name)
          ":"
          line
          ":"
          (message)
          line-end))
  :working-directory flycheck-dialyxir--project-root
  :modes elixir-mode)

;;;###autoload
(defun flycheck-dialyxir-setup ()
  "Setup flycheck-dialyxir.
Add `elixir-dialyxir' to `flycheck-checkers'."
  (interactive)
  (add-to-list 'flycheck-checkers 'elixir-dialyxir t))

(provide 'flycheck-dialyxir)
;;; flycheck-dialyxir.el ends here
