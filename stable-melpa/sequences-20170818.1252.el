;;; sequences.el --- Ports of some Clojure sequence functions.

;; Copyright (C) 2013 Tim Visher <tim.visher@gmail.com>

;; Author: Tim Visher <tim.visher@gmail.com>
;; Keywords: convenience
;; Package-Version: 20170818.1252
;; Package-Requires: ((emacs "24"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;; Ports of Clojure sequence functions that I find useful to have
;; around. Maybe others will too?

;;; Code:

(require 'cl-lib)

(defun sequences-tree-seq (branch? children root)
  (cl-labels ((walk (node)
                    (cons node
                          (if (funcall branch? node)
                              (cl-mapcan #'walk (cddr (funcall children node)))))))
    (walk root)))

(defun sequences-file-seq (root)
  (sequences-tree-seq 'file-directory-p
                      (lambda (directory)
                        (directory-files directory t))
                      (expand-file-name root)))

(provide 'sequences)

;;; Local Variables:
;;; tab-width:2
;;; indent-tabs-mode:nil
;;; lexical-binding:t
;;; End:
;;; sequences.el ends here
