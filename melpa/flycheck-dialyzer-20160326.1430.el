;;; flycheck-dialyzer.el --- Support dialyzer in flycheck

;; Copyright (C) 2015 Lorenzo Bolla <lbolla@gmail.com>
;;
;; Author: Lorenzo Bolla <lbolla@gmail.com>
;; Created: 23 October 2015
;; Version: 1.0
;; Package-Version: 20160326.1430
;; Package-Requires: ((flycheck "0.18"))

;;; Commentary:

;; This package adds support for dialyzer to flycheck.  To use it, add
;; to your init.el:

;; (require 'flycheck-dialyzer)
;; (add-hook 'erlang-mode-hook 'flycheck-mode)

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

(flycheck-define-checker erlang-dialyzer
  "Erlang syntax checker based on dialyzer."
  :command ("dialyzer" source-original)
  :error-patterns
  ((error line-start
	  (file-name)
	  ":"
	  line
	  ":"
	  (message)
	  line-end))
  :modes erlang-mode)

(add-to-list 'flycheck-checkers 'erlang-dialyzer t)

(provide 'flycheck-dialyzer)
;;; flycheck-dialyzer.el ends here
