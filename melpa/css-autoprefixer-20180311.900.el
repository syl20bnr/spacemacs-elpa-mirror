;;; css-autoprefixer.el --- Adds autoprefix to CSS -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Kyung Mo Kweon
;;
;; Author: Kyung Mo Kweon<kkweon@gmail.com> and contributors
;; URL: https://github.com/kkweon/emacs-css-autoprefixer
;; Package-Version: 20180311.900
;; Package-X-Original-Version: 20180310.839
;; Package-Requires: ((emacs "24"))
;; Version: 1.0
;; Keywords: convenience, usability, css

;; This file is not part of GNU Emacs.
;; Licensed under the same terms as Emacs.
;;
;;; Commentary:
;;
;; Quick start:
;; (require 'css-autoprefixer)
;; css-autoprefixer
;;
;; For a detailed introduction see:
;; https://github.com/kkweon/emacs-css-autoprefixer/README.org
;;
;;; Code:

;;;###autoload
(defun css-autoprefixer ()
  "Run autoprefix in the current buffer. If error, display error messages"
  (interactive)
  (if buffer-file-name (let* ((result (css-autoprefixer--execute-npx buffer-file-name))
                              (success-p (= (car result) 0))
                              (content (car (cdr result))))
                         (if success-p
                             (progn
                               (css-autoprefixer-clean-buffer)
                               (insert content))
                           (display-message-or-buffer content)))))


(defun css-autoprefixer-clean-buffer ()
  "Clear current selection or whole buffer"
  (if (region-active-p)
      (delete-region (region-beginning)
                     (region-end))
    (erase-buffer)))

(defun css-autoprefixer--execute-npx (filename)
  "Run autoprefix shell command for the given FILENAME. Return a list (EXITCODE, OUTPUT)"
  (with-temp-buffer
    (list (call-process "npx"
                        nil
                        (list (current-buffer)
                              t)
                        0
                        "postcss"
                        (shell-quote-argument (expand-file-name filename))
                        "--use"
                        "autoprefixer"
                        "--no-map")
          (buffer-string))))

(provide 'css-autoprefixer)
;;; css-autoprefixer.el ends here
