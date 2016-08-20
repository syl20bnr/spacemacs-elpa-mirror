;;; flycheck-protobuf.el --- protobuf checker for flycheck

;; Copyright (C) 2015 Edward Knyshov

;; Author: Edward Knyshov <edvorg@gmail.com>
;; Created: 31 July 2015
;; Version: 0.1
;; Package-Version: 20160211.700
;; Package-Requires: ((protobuf-mode "0"))
;; Keywords: flycheck
;; X-URL: https://github.com/edvorg/flycheck-protobuf

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Usage

;;; (eval-after-load 'flycheck
;;;   '(require 'flycheck-protobuf))
;;; (add-to-list 'flycheck-checkers 'protobuf-protoc-reporter t)

;;; Code:

(require 'flycheck)

(flycheck-define-checker protobuf-protoc-reporter
  "A protobuf syntax checker based on protoc compiler"
  :command ("protoc" "--error_format" "gcc" (eval (concat "--java_out=" temporary-file-directory)) "--proto_path" (eval (file-name-directory (buffer-file-name))) source-inplace)
  :error-patterns
  ((error line-start
          (message "In file included from") " " (file-name) ":" line ":"
          column ":"
          line-end)
   (info line-start (file-name) ":" line ":" column
         ": note: " (message) line-end)
   (error line-start (file-name) ":" line ":" column
          ": " (message) line-end))
  :modes (protobuf-mode))

(provide 'flycheck-protobuf)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; flycheck-protobuf.el ends here
