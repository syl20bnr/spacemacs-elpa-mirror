;;; vcl-mode.el --- Syntax highlighting for Varnish Command Language
;;;
;;; Copyright (c) 2008-2011 Redpill Linpro AS
;;; All rights reserved.
;;;
;;; Author: Stig Sandbeck Mathisen <ssm@redpill-linpro.com>
;;; Version: 0.1
;; Package-Version: 20170119.1251
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above
;;;    copyright notice, this list of conditions and the following
;;;    disclaimer in the documentation and/or other materials provided
;;;    with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;; PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR
;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
;;; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;; POSSIBILITY OF SUCH DAMAGE.
;;;

(defgroup vcl nil
  "Customizations for vcl-mode"
  :group 'data)

(defcustom vcl-indent-level 8
  "*The level of indentation (number of space characters) in VCL-mode."
  :type 'integer  :group 'vcl)

(defcustom vcl-indent-tabs-mode nil
  "*Allow tabs when indentation in vcl-mode if non-nil"
  :type 'boolean :group 'vcl)

;; I just love standards, there are so many to choose from
(if (string-match "XEmacs\\|Lucid" emacs-version)
    (require 'generic-mode)
  (require 'generic))

;; Add a VCL major mode called "vcl-mode", based on generic-mode

;;;###autoload
(define-generic-mode 'vcl-mode
  ;; comments (defined in "vcl-mode-setup-function"
  '("#")
  ;; keywords (defined under "others" instead)
  nil
  ;; others
  (list
   ;; Logic
   (generic-make-keywords-list
    (list
     "else"
     "elsif"
     "elseif"
     "if"
     "remove"
     "return"
     "error"
     "set"
     "unset"
     )
    'font-lock-keyword-face)

   ;; Types
   (generic-make-keywords-list
    (list
     "purge_url"
     "regsub"
     "regsuball"
     "hash_data"
     "synthetic"
     "ban"
     )
    'font-lock-builtin-face)

   ;; VCL Functions
   (generic-make-keywords-list
    (list
     "acl"
     "backend"
     "sub"
     "vcl_deliver"
     "vcl_discard"
     "vcl_fetch"
     "vcl_hash"
     "vcl_hit"
     "vcl_miss"
     "vcl_pass"
     "vcl_pipe"
     "vcl_recv"
     "vcl_timeout"
     "vcl_error"
     ;; new as of varnish 4/5
     "vcl_purge"
     "vcl_synth"
     "vcl_backend_fetch"
     "vcl_backend_response"
     "vcl_backend_error"
     "vcl_init"
     "vcl_fini"
     )
    'font-lock-function-name-face)

   ;; Actions
   (generic-make-keywords-list
    (list
     "deliver"
     "discard"
     "error"
     "miss"
     "fetch"
     "hash"
     "keep"
     "lookup"
     "pass"
     "pipe"
     "hit_for_pass"
     ;; new as of varnish 4/5
     "purge"
     "synth"
     "restart"
     "abandon"
     "retry"
     "ok"
     "fail"
     )
    'font-lock-function-name-face)

   ;; Variables
   (generic-make-keywords-list
    (list
     "backend.host"
     "backend.port"
     "bereq.proto"
     "bereq.request"
     "bereq.url"
     "client.ip"
     "now"
     "obj.cacheable"
     "obj.lastuse"
     "obj.proto"
     "obj.response"
     "obj.status"
     "obj.ttl"
     "obj.valid"
     "req.backend"
     "req.hash"
     "req.grace"
     "req.proto"
     "req.request"
     "req.url"
     "resp.proto"
     "resp.response"
     "resp.status"
     "beresp.ttl"
     "beresp.saintmode"
     "server.ip"
     ;; new as of varnish 4/5
     ;; hint: egrep ^[a-z] ./doc/sphinx/include/vcl_var.rs
     "bereq"
     "bereq.backend"
     "bereq.between_bytes_timeout"
     "bereq.connect_timeout"
     "bereq.first_byte_timeout"
     "bereq.method"
     ;; "bereq.proto"
     "bereq.retries"
     "bereq.uncacheable"
     ;; "bereq.url"
     "bereq.xid"
     "beresp"
     "beresp.age"
     "beresp.backend"
     "beresp.backend.ip"
     "beresp.backend.name"
     "beresp.do_esi"
     "beresp.do_gunzip"
     "beresp.do_gzip"
     "beresp.do_stream"
     "beresp.grace"
     ;; "beresp.http."
     "beresp.keep"
     "beresp.proto"
     "beresp.reason"
     "beresp.status"
     "beresp.storage_hint"
     ;; "beresp.ttl"
     "beresp.uncacheable"
     "beresp.was_304"
     "client"
     "client.identity"
     ;; "client.ip"
     "local"
     "local.ip"
     ;; "now"
     "obj"
     "obj.age"
     "obj.grace"
     "obj.hits"
     ;; "obj.http."
     "obj.keep"
     ;; "obj.proto"
     "obj.reason"
     ;; "obj.status"
     "obj.ttl"
     "obj.uncacheable"
     "remote"
     "remote.ip"
     "req"
     "req.backend_hint"
     "req.can_gzip"
     "req.esi"
     "req.esi_level"
     "req.hash_always_miss"
     "req.hash_ignore_busy"
     ;; "req.http."
     "req.method"
     ;; "req.proto"
     "req.restarts"
     "req.ttl"
     ;; "req.url"
     "req.xid"
     "req_top"
     ;; "req_top.http."
     "req_top.method"
     "req_top.proto"
     "req_top.url"
     "resp"
     ;; "resp.http."
     "resp.is_streaming"
     ;; "resp.proto"
     "resp.reason"
     ;; "resp.status"
     "server"
     "server.hostname"
     "server.identity"
     ;; "server.ip"
     )
    'font-lock-variable-name-face)

   ;; More variables
   '("\\(bereq\\|beresp\\|req\\|req_top\\|resp\\|obj\\)\.http\.[A-Za-z-]+\\|storage\.[A-Za-z0-9-]+\.\\(free_space\\|used_space\\|happy\\)" .
     font-lock-variable-name-face))

  ;; Filenames to highlight
  '("\\.vcl\\'")
  (list 'vcl-mode-setup-function)
  "Mode for Varnish Command Language")


;; A function to modify syntax, add a hook if needed, and setup
;; indentation.

(defun vcl-mode-setup-function ()
  ;; These are "part of words"
  (modify-syntax-entry ?_ "w")
  (modify-syntax-entry ?. "w")

  ;; C++-style comments
  (modify-syntax-entry ?/ ". 124")
  (modify-syntax-entry ?* ". 23b")

  ;; Perl-style comments
  (modify-syntax-entry ?# "<")
  (modify-syntax-entry ?\n ">")

  ;; long strings - handling as comments for now
  (modify-syntax-entry ?{ ". 1")
  (modify-syntax-entry ?\" "\". 23b")
  (modify-syntax-entry ?} ". 4")

  (run-hooks 'vcl-mode-hook)
  (set (make-local-variable 'indent-line-function) 'vcl-indent-line)
  (setq indent-tabs-mode vcl-indent-tabs-mode)
  )

(defvar vcl-mode-hook nil)

(defun vcl-indent-line ()
  "Indent the current VCL line according to syntax."
  (interactive)
  (indent-line-to
   (max (vcl-calculate-indentation) 0)))


;; The function to calculate indentation level.  This is a really
;; simple and naive function, and does not perform anything like a
;; syntax check.
(defun vcl-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (interactive)
  (save-excursion
    (cond
     ;; Do not indent the first line.
     ((vcl-first-line-p) 0)
     ;; neither empty lines
     ((vcl-empty-line-p) 0)
     ;; Reduce indent level if we close a block on this line
     ((vcl-closing-tag-on-this-line-p)
      (- (vcl-previous-line-indentation)
	 vcl-indent-level))
     ;; Increase indent level if a block opened on the previous line
     ((vcl-opening-tag-on-previous-line-p)
      (+ (vcl-previous-line-indentation)
	 vcl-indent-level))
     ;; for comments, check for c-scroll-style block
     ((or (vcl-comment-p (point))
	  (progn
	    (save-excursion
	      (back-to-indentation)
	      (vcl-comment-p (point)))))
      (+ (vcl-previous-line-indentation)
	 (if (vcl-c-block-comment-p) 1 0)))
     ;; indent to the level of the previous non-empty line
     ((or (vcl-previous-line-statement-end-p)
	  (vcl-empty-previous-line-p))
      (vcl-previous-line-indentation))
     ;; line continuation
     (t
      (+ (vcl-previous-line-indentation)
	 (floor (/ vcl-indent-level 2)))))))

(defun vcl-opening-tag-on-previous-line-p ()
  "Checks if we have an opening tag on the previous line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\n")
    (beginning-of-line)
    (if (and (looking-at ".*{[ \t]*$")
             (not (vcl-comment-p (point))))
        t)))

(defun vcl-previous-line-statement-end-p ()
  "Checks if last line ended a statement"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\n")
    (beginning-of-line)
    (looking-at "^[ \t]*\\(#\\|//\\)\\|.*\\(;\\|}C?\\|\*/\\)[ \t]*$")))

(defun vcl-closing-tag-on-this-line-p ()
  "Checks if we have a closing tag on this line."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (looking-at "}")))

(defun vcl-previous-line-indentation ()
  "Return the indent level of the previous line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward " \t\n")
    (back-to-indentation)
    (- (current-column)
       (mod (current-column)
	    vcl-indent-level))))

;; shamelessly copied from
;; http://emacs.stackexchange.com/questions/14269/how-to-detect-if-the-point-is-within-a-comment-area
(defun vcl-comment-p (pos)
  (interactive)
  (let ((fontfaces (get-text-property pos 'face)))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (delq nil
	  (mapcar #'(lambda (f)
		      ;; learn this trick from flyspell
		      (or (eq f 'font-lock-comment-face)
			  (eq f 'font-lock-comment-delimiter-face)))
		  fontfaces))))

(defun vcl-c-block-comment-p ()
  "Checks if we have a c-block line starting with *"
  (interactive)
  (save-excursion
    (back-to-indentation)
    (looking-at "\*")))

(defun vcl-empty-line-p ()
  "Checks for empty line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun vcl-empty-previous-line-p ()
  "Checks for empty line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-backward "\n")
    (beginning-of-line)
    (looking-at "^[ \t]*$")))

(defun vcl-first-line-p ()
  "Checks if we are on the first line."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (eq (point) 1)))

(provide 'vcl-mode)
;;; vcl-mode.el ends here
