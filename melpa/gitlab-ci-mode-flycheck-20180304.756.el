;;; gitlab-ci-mode-flycheck.el --- Flycheck support for ‘gitlab-ci-mode’ -*- lexical-binding: t; -*-
;;
;; Copyright 2018 Joe Wreschnig
;;
;; Author: Joe Wreschnig
;; Keywords: tools, vc, convenience
;; Package-Requires: ((emacs "25") (flycheck "31") (gitlab-ci-mode "1"))
;; Package-Version: 20180304.756
;; Package-X-Original-Version: 20180304.1
;; URL: https://gitlab.com/joewreschnig/gitlab-ci-mode-flycheck/
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Flycheck integration for the linter included with ‘gitlab-ci-mode’.
;; For security reasons, this checker is not enabled by default.  To
;; enable it, call ‘gitlab-ci-mode-flycheck-enable’.


;;; Code:

(require 'flycheck)
(require 'gitlab-ci-mode)

(flycheck-define-generic-checker 'gitlab-ci
  "Lint GitLab CI configuration files.

This checker will send your file to a remote service, as GitLab
offers no local linting tool. The service URL is configurable via
‘gitlab-ci-url’."
  :start
  (lambda (checker callback)
    (gitlab-ci-request-lint
     (lambda (status data)
       (if (eq status 'errored)
           (funcall callback status data)
         (funcall
          callback status
          (mapcar
           (lambda (message)
             (flycheck-error-new-at
              ;; Most errors from GitLab CI’s lint tool don’t give a
              ;; proper line number, but if they do, try to parse it
              ;; out. (Even then it’s often wrong…)
              (if (string-match "\\<line \\([0-9]+\\)\\>" message)
                  (string-to-number (match-string 1 message)) 1)
              (if (string-match "\\<column \\([0-9]+\\)\\>" message)
                  (string-to-number (match-string 1 message)) nil)
              'error message :checker checker))
           (alist-get 'errors data)))))
     :silent))

  :modes '(gitlab-ci-mode))

;;;###autoload
(defun gitlab-ci-mode-flycheck-enable ()
  "Enable Flycheck support for ‘gitlab-ci-mode’.

Enabling this checker will upload your buffer to the site
specified in ‘gitlab-ci-url’.  If your buffer contains sensitive
data, this is not recommended.  (Storing sensitive data in your
CI configuration file is also not recommended.)

If your GitLab API requires a private token, set
‘gitlab-ci-api-token’."
  (add-to-list 'flycheck-checkers 'gitlab-ci))


(provide 'gitlab-ci-mode-flycheck)
;;; gitlab-ci-mode-flycheck.el ends here
