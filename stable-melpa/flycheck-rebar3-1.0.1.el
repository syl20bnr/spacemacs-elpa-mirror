;;; flycheck-rebar3.el --- Rebar3 flycheck integration for Erlang projects

;; Copyright (C) 2016 Joe DeVivo

;; Author: Joe DeVivo
;; Version: 1.0.1
;; Package-Version: 1.0.1
;; Package-Requires: ((flycheck "27"))
;; Keywords: erlang flycheck rebar3
;; URL: https://github/joedevivo/flymake-rebar3

;;; Commentary:

;; This package adds support for erlang projects using rebar3 to
;; flycheck.  For more on rebar3: https://rebar3.org

;; To use it, add the following to wherever you configure Emacs.
;;
;; (require 'flycheck-rebar3)
;; (flycheck-rebar3-setup)

;;; Code:
(require 'flycheck)

(flycheck-define-checker
  erlang-rebar3
  "Defines a checker for erlang with rebar3 compile."
  :command ("rebar3"
            "compile"
            )
  :error-patterns
  ((warning line-start
            (file-name) ":" line ": Warning:" (message) line-end)
   (error line-start
          (file-name) ":" line ": " (message) line-end))
  :modes erlang-mode
  :working-directory flycheck-rebar3-project-root
  )

(defun flycheck-rebar3-project-root (_checker)
  "Return directory where =rebar.config= is located."
  (locate-dominating-file buffer-file-name "rebar.config"))


;;;###autoload
(defun flycheck-rebar3-setup ()
  "Setup Flycheck for Rebar3."
  (add-to-list 'flycheck-checkers 'erlang-rebar3))

(provide 'flycheck-rebar3)
;;; flycheck-rebar3.el ends here
