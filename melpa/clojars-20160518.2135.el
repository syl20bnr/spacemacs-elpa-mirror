;;; clojars.el --- clojars.org search interface

;; Filename: clojars.el
;; Description: clojars.org search interface
;; Author: Joshua Miller <josh@joshmiller.io>
;; License: GPLv3
;; Created: 2014-12-15 17:21:05
;; Version: 1.1.0
;; Package-Version: 20160518.2135
;; Package-X-Original-Version: 20160519.110
;; URL: https://github.com/joshuamiller/clojars.el
;; Keywords: docs, help, tools
;; Package-Requires: ((request-deferred "0.2.0"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; M-x clojars - to search clojars.org for a Clojure library

;;; Code:

(require 'request-deferred)
(require 'json)
(require 'deferred)

(defconst clojars-search-endpoint "https://clojars.org/search"
  "Clojars search endpoint")

(defun clojars-jar-name (result)
  (let ((group-name (cdr (assoc 'group_name result)))
        (jar-name (cdr (assoc 'jar_name result))))
    (if (string= group-name jar-name)
        jar-name
      (format "%s/%s" group-name jar-name))))

(defun clojars-format-dependency (result)
  (let ((version (cdr (assoc 'version result)))
        (name (clojars-jar-name result)))
    (format "[%s %S]" name version)))

(defun clojars-jar-result (result)
  (cons (clojars-format-dependency result) (clojars-jar-name result)))

;;;###autoload
(defun clojars (query)
  "Finds a Clojure library from clojars.org, and copies selected
   result to kill ring"
  (interactive "sSearch Clojars: ")
  (message "Loading...")
  (deferred:$
    (request-deferred
     clojars-search-endpoint
     :params `(("q" . ,query) ("format" . "json"))
     :parser 'json-read
     :sync   t)
    (deferred:nextc it
      (lambda (response)
        (let ((results (cdr (assoc 'results (request-response-data response)))))
          (kill-new (completing-read "Results: "
                                     (mapcar 'clojars-jar-result results))))))))

(provide 'clojars)

;;; clojars.el ends here
