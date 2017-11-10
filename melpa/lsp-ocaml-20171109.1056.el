;;; lsp-ocaml.el --- OCaml support for lsp-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Antonio N. Monteiro <anmonteiro@gmail.com>

;; Author: Antonio N. Monteiro <anmonteiro@gmail.com>
;; Version: 1.0
;; Package-Version: 20171109.1056
;; Package-Requires: ((emacs "25.1") (lsp-mode "3.0"))
;; Keywords: languages, ocaml, reason, lsp
;; URL: https://github.com/anmonteiro/lsp-ocaml

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

;; OCaml support for lsp-mode using freebroccolo's
;; ocaml-language-server (https://github.com/freebroccolo/ocaml-language-server).

;;; Code:

(require 'lsp-mode)

(defun lsp-ocaml--get-root ()
  "Retrieves the root directory of the OCaml project root if available.
The current directory is assumed to be the OCaml project’s root otherwise."
  (cond
   ((and (bound-and-true-p projectile-mode) (projectile-project-p)) (projectile-project-root))
   ((vc-backend default-directory) (expand-file-name (vc-root-dir)))
   (t (let ((project-types '("jbuild-workspace" "bsconfig.json" "package.json")))
	      (or (locate-dominating-file default-directory
                                    (lambda (dir)
                                      (directory-files dir nil "\.opam")))
            (seq-some (lambda (file) (locate-dominating-file default-directory file)) project-types)
	          default-directory)))))

(lsp-define-stdio-client lsp-ocaml "ocaml" #'lsp-ocaml--get-root
			 '("ocaml-language-server" "--stdio")
       :language-id-fn (lambda (buffer)
                         (let ((x (buffer-file-name buffer)))
                           (cond
                            ((or (string-suffix-p ".re" x)
                                 (string-suffix-p ".rei" x))
                             "reason")

                            ((or (string-suffix-p ".ml" x)
                                 (string-suffix-p ".mli" x)
                                 (string-suffix-p ".mll" x)
                                 (string-suffix-p ".mly" x))
                             "ocaml")))))

(provide 'lsp-ocaml)
;;; lsp-ocaml.el ends here
