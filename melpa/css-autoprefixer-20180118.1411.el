;;; css-autoprefixer.el --- Adds autoprefix to CSS -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Kyung Mo Kweon
;;
;; Author: Kyung Mo Kweon<kkweon@gmail.com> and contributors
;; URL: https://github.com/kkweon/emacs-css-autoprefixer
;; Package-Version: 20180118.1411
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
  (save-excursion
    (let* ((temp-name (make-temp-file "css-prefixer" nil ".css"))
           (temp-css (if (region-active-p)
                         (buffer-substring-no-properties (region-beginning)
                                                         (region-end))
                       (buffer-string))))
      (with-temp-file temp-name
        (insert temp-css))
      (let* ((result (css-autoprefixer--execute-npx temp-name))
             (success-p (= (car result) 0))
             (content (car (cdr result))))
        (if success-p
            (progn
              (css-autoprefixer-clean-buffer)
              (insert (css-autoprefixer--trim-first-and-last content)))
          (display-message-or-buffer content))))))


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
                        "autoprefixer")
          (buffer-string))))

(defun css-autoprefixer--trim-first-and-last (message)
  "Delete first line and last line of MESSAGE because the first line is success message and the last message is useless message"
  (mapconcat 'identity
             (nbutlast (cdr (split-string message "\n")))
             "\n"))

(provide 'css-autoprefixer)
;;; css-autoprefixer.el ends here
