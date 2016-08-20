;;; helm-idris.el --- A Helm datasource for Idris documentation, queried from the compiler

;; Copyright (C) 2014  David Raymond Christiansen

;; Author: David Raymond Christiansen <david@davidchristiansen.dk>
;; Maintainer: David Raymond Christiansen <david@davidchristiansen.dk>
;; Keywords: languages, helm
;; Package-Version: 20141202.957
;; Package-Requires: ((helm "0.0.0") (idris-mode "0.9.14"))
;; Version: 0.0.1

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

;; An interface to search the Idris documentation, live from a connection to
;; the compiler. Actions are available to insert an identifier, either
;; qualified or bare, and to see full documentation.

;;; Code:

(require 'helm)
(require 'idris-commands)
(require 'idris-common-utils)
(require 'idris-info)
(require 'inferior-idris)

;; see http://www.emacswiki.org/emacs/ElispCookbook
(defun helm-idris-chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(defun helm-idris-first-word (str)
  "Split STR into words and return the first word or the empty string."
  (let ((split (split-string str nil t)))
    (if split
        (car split)
      "")))

(defvar helm-idris-query-cache nil "The results of the last compiler query as (QUERY TIMESTAMP RES), or nil.")

(defun helm-idris-apropos-search (query)
  "Execute an idris-mode apropos search using QUERY and return the results."
  (if (and helm-idris-query-cache
           (string= (car helm-idris-query-cache) query)
           (< (- (cadr helm-idris-query-cache) 10) (float-time)))
      (nth 2 helm-idris-query-cache)
    (let ((res
           (progn
             ;; Complete pending Idris operations
             (dotimes (_ 5) (accept-process-output nil 0.01))
             (let ((info (idris-eval `(:apropos ,query) t))
                   (result-list ())
                   (result-beginning-marker (make-marker))
                   (result-end-marker (make-marker))
                   current-result)
               (with-temp-buffer
                 (idris-propertize-spans (idris-repl-semantic-text-props (cdr info))
                   (insert (car info)))
                 (goto-char (point-min))
                 (set-marker result-beginning-marker (point))
                 (while (and (not (eobp))
                             (re-search-forward "\n\n+" nil t))
                   (set-marker result-end-marker (match-beginning 0))
                   (setq current-result (buffer-substring result-beginning-marker result-end-marker))
                   (setq result-beginning-marker (match-end 0))
                   ;; Destructure the string to get the parts - NB clobbers match data
                   (push
                    (with-temp-buffer
                      (let (name type type-beginning docs)
                        (insert current-result)
                        (goto-char (point-min))
                        (re-search-forward "[^ ]+" nil t)
                        (setq name (match-string 0))
                        (re-search-forward "\\s-+:\\s-+" nil t)
                        ;; now we're at the beginning of the type - grab lines while they're indented
                        (setq type-beginning (point))
                        (beginning-of-line)
                        (forward-line)
                        (while (and (not (eobp)) (looking-at-p "    ")) (forward-line)) ; assuming type continuation is intented this much
                        (setq type (buffer-substring type-beginning (point)))
                        (setq docs (if (looking-at-p "[a-zA-Z]") (buffer-substring (point) (point-max)) nil))
                        `((:name ,name) (:type ,type) (:docs ,docs))))
                    result-list))
                 result-list)))))
          (setq helm-idris-query-cache (list query (float-time) res))
          res)))

(defun helm-idris-format-candidate (candidate)
  "Return a formatted string for CANDIDATE."
  (let ((name (assoc :name candidate))
        (type (assoc :type candidate))
        (docs (assoc :docs candidate)))
    (concat (if name (cadr name) "*unknown*")
            (if (and type (cadr type)) (concat " : " (helm-idris-chomp (cadr type))) "")
            (if (and docs (cadr docs)) (concat "\n" (helm-idris-chomp (cadr docs))) ""))))

(defun helm-idris-search-formatted (query)
  "Execute an idris-mode apropos search using QUERY and return formatted results."
  (let ((apropos-output (helm-idris-apropos-search (helm-idris-first-word query))))
    (mapcar #'(lambda (res)
                (cons (helm-idris-format-candidate res) res))
            apropos-output)))

(defun helm-idris-search ()
  "Execute a formatted idris search using `helm-pattern'."
  (helm-idris-search-formatted helm-pattern))

(defun helm-idris-insert-qualified (candidate)
  "Insert CANDIDATE as a qualified name."
  (let ((name (assoc :name candidate)))
    (if name
        (insert (substring-no-properties (cadr name)))
      (error "No name to insert"))))

(defun helm-idris-insert-unqualified (candidate)
  "Insert CANDIDATE as an unqualified name."
  (let ((name (assoc :name candidate)))
    (if name
        (insert (replace-regexp-in-string ".*\\." "" (substring-no-properties (cadr name))))
      (error "No name to insert"))))

(defun helm-idris-get-docs (candidate)
  "Get the internal documentation for CANDIDATE."
  (let ((name (assoc :name candidate)))
    (if name
        (idris-info-for-name :docs-for (substring-no-properties (cadr name)))
      (error "No name to get docs for"))))



(defvar helm-idris-apropos-source
  '((name . "Idris")
    (volatile)
    (delayed)
    (multiline)
    (requires-pattern)
    (candidates-process . helm-idris-search)
    (action . (("Insert" . helm-idris-insert-unqualified)
               ("Insert qualified" . helm-idris-insert-qualified)
               ("Full documentation" . helm-idris-get-docs))))
  "A source description for Idris :apropos.")

;;;###autoload
(defun helm-idris ()
  "Search the Idris documentation with Helm."
  (interactive)
  (helm :sources '(helm-idris-apropos-source)
        :buffer "*helm-idris*"))

(provide 'helm-idris)
;;; helm-idris.el ends here
