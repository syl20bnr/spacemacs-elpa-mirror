;;; flycheck-rebar3.el --- Rebar3 flycheck integration for Erlang projects

;; Copyright (C) 2016 Joe DeVivo

;; Author: Joe DeVivo
;; Version: 1.1.0
;; Package-Version: 20161030.615
;; Package-Requires: ((flycheck "27"))
;; Keywords: erlang flycheck rebar3
;; URL: https://github/joedevivo/flycheck-rebar3

;;; Commentary:

;; This package adds support for erlang projects using rebar3 to
;; flycheck.  For more on rebar3: https://rebar3.org

;; To use it, add the following to wherever you configure Emacs.
;;
;; (require 'flycheck-rebar3)
;; (flycheck-rebar3-setup)

;; There's currently an open PR to include this checker with flycheck,
;; https://github.com/flycheck/flycheck/pull/1144
;; Once it's merged, this package will be sunset.

;;; Code:
(require 'flycheck)

;; rebar3 outputs ANSI color sometimes, and we need to strip it our
;; for the error checker to match filenames. The worst part is that
;; the ANSI color code it's outputing is actually the code for
;; clearing all style!
(require 'ansi-color)

(flycheck-define-checker erlang-rebar3
  "An Erlang syntax checker using the rebar3 build tool."
  :command ("rebar3" "compile")
  :error-parser
  (lambda (output checker buffer)
    (flycheck-parse-with-patterns (ansi-color-filter-apply output)
                                  checker
                                  buffer))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ": Warning:" (message) line-end)
   (error line-start
          (file-name) ":" line ": " (message) line-end))
  :modes erlang-mode
  :enabled (lambda () (flycheck-rebar3-project-root))
  :predicate (lambda () (flycheck-buffer-saved-p))
  :working-directory flycheck-rebar3-project-root)

(defun flycheck-rebar3-project-root (&optional _checker)
  "Return directory where =rebar.config= is located."
  (let* ((rebarconfig (locate-dominating-file buffer-file-name "rebar.config"))
         (project-dir (if rebarconfig
                          (file-name-as-directory
                           (substring rebarconfig
                                      0
                                      (string-match
                                       "\\(/_build/[^/]+/lib\\|/_checkouts\\)"
                                       rebarconfig)))
                        rebarconfig)))
    project-dir))


;;;###autoload
(defun flycheck-rebar3-setup ()
  "Setup Flycheck for Rebar3."
  (add-to-list 'flycheck-checkers 'erlang-rebar3))

(provide 'flycheck-rebar3)
;;; flycheck-rebar3.el ends here
