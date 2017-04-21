;;; lsp-rust.el --- Rust support for lsp-mode

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20170421.326
;; Package-Requires: ((lsp-mode "2.0") (rust-mode "0.3.0"))
;; Keywords: rust
;; URL: https://github.com/emacs-lsp/lsp-rust

(require 'rust-mode)
(require 'lsp-mode)
(require 'json)

(defun lsp-rust--rls-command ()
  (let ((rls-root (getenv "RLS_ROOT"))
	(rls-path (executable-find "rls")))
    (if rls-path
	rls-path
      (when rls-root
	`("cargo" "+nightly" "run" "--quiet"
	  ,(concat "--manifest-path="
		   (concat
		    (file-name-as-directory (expand-file-name rls-root))
		    "Cargo.toml"))
	  "--release")))))

(defun lsp-rust--get-root ()
  (let (dir)
    (unless
	(ignore-errors
	  (let* ((output (shell-command-to-string "cargo locate-project"))
		 (js (json-read-from-string output)))
	    (setq dir (cdr (assq 'root js)))))
      (error "Couldn't find root for project at %s" default-directory))
    (file-name-directory dir)))

;;;###autoload
(lsp-define-stdio-client 'rust-mode "rust" 'stdio
		   #'lsp-rust--get-root
		    "Rust Language Server"
		   (lsp-rust--rls-command))

(lsp-client-on-notification 'rust-mode "rustDocument/diagnosticsBegin"
			    #'(lambda (_w _p)))
(lsp-client-on-notification 'rust-mode "rustDocument/diagnosticsEnd"
			    #'(lambda (_w _p)))
(provide 'lsp-rust)

;;; lsp-rust.el ends here
