;;; ob-cypher.el --- query neo4j using cypher in org-mode blocks

;; Copyright (C) 2015 ZHOU Feng

;; Author: ZHOU Feng <zf.pascal@gmail.com>
;; URL: http://github.com/zweifisch/ob-cypher
;; Package-Version: 20170725.720
;; Keywords: org babel cypher neo4j
;; Version: 0.0.1
;; Created: 8th Feb 2015
;; Package-Requires: ((s "1.9.0") (cypher-mode "0.0.6") (dash "2.10.0") (dash-functional "1.2.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; query neo4j using cypher in org-mode blocks
;;

;;; Code:
(require 'org)
(require 'ob)
(require 's)
(require 'dash)
(require 'json)

(defvar org-babel-default-header-args:cypher
  '((:results . "output")))

(defun ob-cypher/parse-result (output)
  (->> (s-lines output)
    (-filter (-partial 's-starts-with? "|"))
    (-map (-partial 's-chop-suffix "|"))
    (-map (-partial 's-chop-prefix "|"))
    (-map (-partial 's-split " | "))))

(defun ob-cypher/table (output)
  (org-babel-script-escape (ob-cypher/parse-result output)))

(defun ob-cypher/property (property)
  (format "%s: %s" (car property) (cdr property)))

(defun ob-cypher/node-to-dot (node)
  (let ((labels (cdr (assoc 'labels node))))
    (s-format "n${id} [label=\"{${head}<body> ${body}}\"]" 'aget
              `(("id" . ,(cdr (assoc 'id node)))
                ("head" . ,(if (> (length labels) 0) (concat "<head>" (s-join ":" labels) "|") ""))
                ("body" . ,(s-join "\\n" (-map 'ob-cypher/property (cdr (assoc 'properties node)))))))))

(defun ob-cypher/rel-to-dot (rel)
  (s-format "n${start} -> n${end} [label = ${label}]" 'aget
            `(("start" . ,(cdr (assoc 'startNode rel)))
              ("end" . ,(cdr (assoc 'endNode rel)))
              ("label" . ,(cdr (assoc 'type rel))))))

(defun ob-cypher/json-to-dot (output)
  (let* ((parsed (json-read-from-string output))
         (results (cdr (assoc 'results parsed)))
         (data
          (if (> (length results) 0)
              (cdr (assoc 'data (elt results 0)))))
         (graphs (-map (lambda (graph) (cdr (assoc 'graph graph)))
                       data))
         (rels (-mapcat
                 (lambda (graph)
                   (append (cdr (assoc 'relationships graph)) nil))
                 graphs))
         (nodes (-mapcat
                 (lambda (graph)
                   (append (cdr (assoc 'nodes graph)) nil))
                 graphs)))
    (s-format "digraph {\nnode[shape=Mrecord]\n${nodes}\n${rels}\n} " 'aget
              `(("nodes" . ,(s-join "\n" (-map 'ob-cypher/node-to-dot nodes)))
                ("rels" . ,(s-join "\n" (-map 'ob-cypher/rel-to-dot rels)))))))

(defun ob-cypher/query (statement host port)
  (let* ((statement (s-replace "\"" "\\\"" statement))
         (body (format "{\"statements\":[{\"statement\":\"%s\",\"resultDataContents\":[\"graph\"]}]}"
                       (s-join " " (s-lines statement))))
         (url (format "http://%s:%d/db/data/transaction/commit" host port))
         (tmp (org-babel-temp-file "curl-"))
         (cmd (format "curl -sH 'Accept: application/json; charset=UTF-8' -H 'Content-Type: application/json' -d@'%s' '%s'" tmp url)))
    (message cmd)
    (with-temp-file tmp
      (insert body))
    (shell-command-to-string cmd)))

(defun ob-cypher/dot (statement host port output)
  (let* ((tmp (org-babel-temp-file "dot-"))
         (result (ob-cypher/query statement host port))
         (dot (ob-cypher/json-to-dot result))
         (cmd (format "dot -T%s -o %s %s" (file-name-extension output) output tmp)))
    (message result)
    (message dot)
    (message cmd)
    (with-temp-file tmp
      (insert dot))
    (org-babel-eval cmd "")
    nil))

(defun ob-cypher/shell (statement host port result-type)
  (let* ((tmp (org-babel-temp-file "cypher-"))
         (cmd (s-format "neo4j-shell -host ${host} -port ${port} -file ${file}" 'aget
                        `(("host" . ,host)
                          ("port" . ,(int-to-string port))
                          ("file" . ,tmp))))
         (result (progn (with-temp-file tmp (insert statement))
                        (shell-command-to-string cmd))))
    (message cmd)
    (if (string= "output" result-type) result (ob-cypher/table result))))

(defun org-babel-execute:cypher (body params)
  (let* ((host (or (assoc :host params) "127.0.0.1"))
         (port (or (assoc :host params) 1337))
         (http-port (or (assoc :host params) 7474))
         (result-type (cdr (assoc :result-type params)))
         (output (cdr (assoc :file params)))
         (body (if (s-ends-with? ";" body) body (s-append ";" body))))
    (if output (ob-cypher/dot body host http-port output) (ob-cypher/shell body host port result-type))))

(provide 'ob-cypher)
;;; ob-cypher.el ends here
