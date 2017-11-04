;;; lsp-hack.el --- lsp-mode client for hacklang

;; Copyright (C) 2017  John Allen <oss@porcnick.com>

;; Author: John Allen <oss@porcnick.com>
;; Version: 1.0
;; Package-Version: 20171030.1159
;; Package-Requires: ((lsp-mode "3.1"))
;; URL: https://github.com/jra3/lsp-hack

;;; Commentary:
;; A simple LSP client for hooking up hack's hh_client/hh_server to lsp-mode
;; hacklang.org

;;; Code:
(require 'lsp-mode)
(require 'lsp-common)

(defgroup lsp_hack nil
  "Major mode `hack-mode' for editing Hack code."
  :prefix "lsp-hack-"
  :group 'languages
  )

(defcustom lsp-hack--binary
  "hh_client"
  "How to call hh_client.  Provide a path if necessary."
  :group 'hack-mode
  :type 'string)

(defconst lsp-hack--handlers
  '(("telemetry/event" . (lambda (_w _p)))
    )
  "Handlers for custom messages from hh."
  )

(defun lsp-hack--initialize (client)
  "Initialization callback for hack.
CLIENT will be passed into this function by `lsp-define-stdio-client`"
  (mapcar #'(lambda (p) (lsp-client-on-notification client (car p) (cdr p)))
          lsp-hack--handlers))

;;;###autoload
(lsp-define-stdio-client
 lsp-hack "hack"
 (lsp-make-traverser #'(lambda (dir)
                         (directory-files
                          dir
                          nil
                          "\\.hhconfig")))
 '(lsp-hack--binary "lsp" "--from=emacs")
 :initialize #'lsp-hack--initialize)

(provide 'lsp-hack)
;;; lsp-hack.el ends here
