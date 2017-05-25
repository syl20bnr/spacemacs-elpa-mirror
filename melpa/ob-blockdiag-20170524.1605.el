;;; ob-blockdiag.el --- org-babel functions for blockdiag evaluation

;; Copyright (C) 2017 Dmitry Moskowski

;; Author: Dmitry Moskowski
;; Keywords: tools, convenience
;; Package-Version: 20170524.1605
;; Package-X-Original-Version: 20170501.112
;; Homepage: https://github.com/corpix/ob-blockdiag.el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Org-Babel support for evaluating blockdiag source code.

;;; Requirements:

;; - blockdiag :: http://blockdiag.com/en/

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:blockdiag
  '(
    (:results . "file")
    (:exports . "results")
    (:tool    . "blockdiag")
    (:font    . nil)
    (:size    . nil))
  "Default arguments for drawing a blockdiag image.")

(add-to-list 'org-src-lang-modes '("blockdiag" . blockdiag))

(defun org-babel-execute:blockdiag (body params)
  (let ((file (cdr (assoc :file params)))
        (tool (cdr (assoc :tool params)))
        (font (cdr (assoc :font params)))
        (size (cdr (assoc :size params)))

        (buffer-name "*ob-blockdiag*")
        (error-template "Subprocess '%s' exited with code '%d', see output in '%s' buffer"))
    (save-window-excursion
      (let ((buffer (get-buffer buffer-name)))(if buffer (kill-buffer buffer-name) nil))
      (let ((data-file (org-babel-temp-file "blockdiag-input"))
            (args (append (list "-o" file)
                          (if size (list "--size" size) (list))
                          (if font (list "--font" font) (list))))
            (buffer (get-buffer-create buffer-name)))
        (with-temp-file data-file (insert body))
        (let
            ((exit-code (apply 'call-process tool nil buffer nil (append args (list data-file)))))
          (if (= 0 exit-code) nil (error (format error-template tool exit-code buffer-name))))))))

(provide 'ob-blockdiag)
;;; ob-blockdiag.el ends here
