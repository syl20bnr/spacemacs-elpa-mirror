;;; lsp-rust.el --- Rust support for lsp-mode

;; Copyright (C) 2017 Vibhav Pant <vibhavp@gmail.com>

;; Author: Vibhav Pant <vibhavp@gmail.com>
;; Version: 1.0
;; Package-Version: 20171127.729
;; Package-Requires: ((lsp-mode "3.0") (rust-mode "0.3.0"))
;; Keywords: rust
;; URL: https://github.com/emacs-lsp/lsp-rust

;;; Commentary:

;; lsp-mode client for the Rust Language Server (RLS).
;; See https://github.com/rust-lang-nursery/rls
;;
;; # Setup
;;
;; You can load lsp-rust after lsp-mode by adding the following to your init
;; file:
;;
;;    (with-eval-after-load 'lsp-mode
;;      (require 'lsp-rust)
;;      (add-hook 'rust-mode-hook #'lsp-rust-enable))
;;
;; You may want to customize the command that lsp-rust uses to launch the RLS.
;; See `lsp-rust-rust-command'.

;;; Code:

(require 'lsp-mode)
(require 'cl-lib)
(require 'json)
(require 'font-lock)

(defvar lsp-rust--config-options (make-hash-table))
(defvar lsp-rust--diag-counters (make-hash-table))

(defcustom lsp-rust-rls-command '("rls")
  "The command used to launch the RLS.

This should be a list of strings, the first string being the
executable, and the remaining strings being the arguments to this
executable.

If this variable is nil, lsp-rust will try to use the RLS located
at the environment variable RLS_ROOT, if set."
  :type '(repeat (string)))

(defun lsp-rust--rls-command ()
  "Return the command used to start the RLS for defining the LSP Rust client."
  (or lsp-rust-rls-command
      (when-let ((rls-root (getenv "RLS_ROOT")))
        `("cargo" "+nightly" "run" "--quiet"
          ,(concat "--manifest-path="
                   (concat
                    (file-name-as-directory (expand-file-name rls-root))
                    "Cargo.toml"))
          "--release"))))

(defun lsp-rust--get-root ()
  (let (dir)
    (unless
	(ignore-errors
	  (let* ((output (shell-command-to-string "cargo locate-project"))
		 (js (json-read-from-string output)))
	    (setq dir (cdr (assq 'root js)))))
      (error "Couldn't find root for project at %s" default-directory))
    (file-name-directory dir)))

(defconst lsp-rust--handlers
  '(("rustDocument/diagnosticsBegin" . (lambda (_w _p)))
    ("rustDocument/diagnosticsEnd" .
     (lambda (w _p)
       (when (< (cl-decf (gethash w lsp-rust--diag-counters 0)) 0)
	 (message "RLS: done"))))
    ("rustDocument/beginBuild" .
     (lambda (w _p)
       (cl-incf (gethash w lsp-rust--diag-counters 0))
       (message "RLS: working")))))

(defun lsp-rust--render-string (str)
  (condition-case nil
      (with-temp-buffer
	(delay-mode-hooks (rust-mode))
	(insert str)
	(font-lock-ensure)
	(buffer-string))
    (error str)))

(defun lsp-rust--initialize-client (client)
  (mapcar #'(lambda (p) (lsp-client-on-notification client (car p) (cdr p)))
	  lsp-rust--handlers)
  (lsp-provide-marked-string-renderer client "rust" #'lsp-rust--render-string))

(lsp-define-stdio-client lsp-rust "rust" #'lsp-rust--get-root nil
			 :command-fn #'lsp-rust--rls-command
			 :initialize #'lsp-rust--initialize-client)

(defun lsp-rust--set-configuration ()
  (lsp--set-configuration `(:rust ,lsp-rust--config-options)))

(add-hook 'lsp-after-initialize-hook 'lsp-rust--set-configuration)

(defun lsp-rust-set-config (name option)
  "Set a config option in the rust lsp server."
  (puthash name option lsp-rust--config-options))

(defun lsp-rust-set-build-lib (build)
  "Enable(t)/Disable(nil) building the lib target."
  (lsp-rust-set-config "build_lib" build))

(defun lsp-rust-set-build-bin (build)
  "The bin target to build."
  (lsp-rust-set-config "build_bin" build))

(defun lsp-rust-set-cfg-test (val)
  "Enable(t)/Disable(nil) #[cfg(test)]."
  (lsp-rust-set-config "cfg_test" val))

(defun lsp-rust-set-goto-def-racer-fallback (val)
  "Enable(t)/Disable(nil) goto-definition should use racer as fallback."
  (lsp-rust-set-config "goto_def_racer_fallback" val))

(provide 'lsp-rust)
;;; lsp-rust ends here

;;; lsp-rust.el ends here
