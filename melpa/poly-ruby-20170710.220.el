;;; poly-ruby.el --- Provides poly-ruby-mode

;; Copyright (c) 2017 Akinori MUSHA
;;
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;; OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
;; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
;; SUCH DAMAGE.

;; Author: Akinori MUSHA <knu@iDaemons.org>
;; URL: https://github.com/knu/poly-ruby.el
;; Package-Version: 20170710.220
;; Created: 12 May 2017
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (polymode "1.0"))
;; Keywords: languages

;;; Commentary:
;;
;; This package defines poly-ruby-mode, which introduces polymode for
;; here-documents in a ruby script.
;;
;; Currently editing actions against sexps does not work properly in
;; polymode, so it is advised you turn this mode on only when
;; necessary.
;;
;;   (define-key ruby-mode-map (kbd "C-c m") 'toggle-poly-ruby-mode)

;;; Code:

(require 'polymode)

(defcustom pm-host/ruby
  (pm-bchunkmode "ruby" :mode 'ruby-mode)
  "Ruby host chunkmode."
  :group 'hostmodes
  :type 'object)

(defvar poly-ruby--heredoc-head-regexp
  "<<\\([-~]?\\)\\(['\"]?\\)\\([_[:word:]]+\\)\\2.*\n?")

(defvar poly-ruby--heredoc-eval-regexp
  "\\_<\\(?:class_\\|module_\\|instance_\\)?eval[ \t]*[ \t(][ \t]*")

(defun poly-ruby--get-faces-at-point ()
  (let* ((point (point))
         (value (or
                 (get-text-property point 'read-face-name)
                 (get-text-property point 'face))))
    (if (listp value) value (list value))))

(defun poly-ruby--faces-at-point-include-p (&rest faces)
  (loop for face in faces
        with pfaces = (poly-ruby--get-faces-at-point)
        thereis (memq face pfaces)))

(defun poly-ruby--comment-at-point-p ()
  (poly-ruby--faces-at-point-include-p
   'font-lock-comment-face))

(defun poly-ruby--heredoc-head-matcher (ahead)
  (save-excursion
    (if (re-search-forward poly-ruby--heredoc-head-regexp nil t ahead)
        (let ((head (cons (match-beginning 0) (match-end 0))))
          (save-match-data
            (goto-char (car head))
            (and (not (looking-at "[[:digit:]]"))
                 (not (poly-ruby--comment-at-point-p))
                 (not (looking-back "[_[:word:]]" nil))
                 head))))))

(defun poly-ruby--heredoc-tail-matcher (ahead)
  (save-excursion
    (save-match-data
      (if (poly-ruby--heredoc-head-matcher 1)
          (let* ((noindent (string= "" (match-string 1)))
                 (word (match-string 3))
                 (tail-reg (concat (if noindent "^" "^[ \t]*")
                                   (regexp-quote word)
                                   "\\(?:\n\\|\\'\\)")))
            (goto-char (match-end 0))
            (if (re-search-forward tail-reg nil t 1)
                (cons (match-beginning 0) (match-end 0))
              (cons (point-max) (point-max))))))))

(defun poly-ruby--heredoc-mode-retriever ()
  (save-match-data
    (poly-ruby--heredoc-head-matcher 1)
    (let ((word (match-string 3))) ;; no need to downcase
      (if (looking-back poly-ruby--heredoc-eval-regexp nil)
          'ruby
        (intern word)))))

(defcustom pm-inner/ruby-heredoc
  (pm-hbtchunkmode-auto "ruby here-document"
                        :head-mode 'host
                        :tail-mode 'host
                        :head-reg 'poly-ruby--heredoc-head-matcher
                        :tail-reg 'poly-ruby--heredoc-tail-matcher
                        :retriever-function 'poly-ruby--heredoc-mode-retriever)
  "Ruby here-document chunk."
  :group 'innermodes
  :type 'object)

(defcustom pm-poly/ruby
  (pm-polymode-multi-auto "ruby"
                          :hostmode 'pm-host/ruby
                          :auto-innermode 'pm-inner/ruby-heredoc)
  "Ruby polymode."
  :group 'polymodes
  :type 'object)

;;;###autoload (autoload 'poly-ruby-mode "poly-ruby" "Ruby polymode." t)
(define-polymode poly-ruby-mode pm-poly/ruby)

(defun poly-ruby-mode-fix-indent-function ()
  ;; smie-indent-line does not work properly in polymode
  (setq-local indent-line-function 'ruby-indent-line))

(defcustom poly-ruby-mode-hook '(poly-ruby-mode-fix-indent-function)
  "Hook run when entering poly-ruby-mode."
  :type 'hook
  :group 'polymodes)

(add-hook 'polymode-init-host-hook
          (lambda ()
            (when (eq major-mode 'ruby-mode)
              (run-hooks 'poly-ruby-mode-hook))))

;;;###autoload
(defun toggle-poly-ruby-mode ()
  "Toggle poly-ruby-mode."
  (interactive)
  (if (bound-and-true-p polymode-mode)
      (ruby-mode)
    (poly-ruby-mode)))

(provide 'poly-ruby)
;;; poly-ruby.el ends here
