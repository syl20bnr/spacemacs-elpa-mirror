;;; banner-comment.el --- For producing banner comments. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 James Ferguson
;;
;; Author: James Ferguson <james@faff.org>
;; URL: https://github.com/WJCFerguson/banner-comment
;; Package-Version: 20180403.1438
;; Package-Requires: ((emacs "24.4"))
;; Version: 2.5
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; License:

;; Licensed under the same terms as Emacs.

;;; Commentary:

;; Quick start:
;;
;; Bind the following commands:
;; banner-comment
;;
;; Code to format a line into a banner comment
;;
;;  ;; ================================ like so ================================
;;
;; or in a language with comment-end, like C:
;;
;;  /* =============================== like so ============================== */
;;
;;; Code:

(defgroup banner-comment nil
  "Turning comments into banners."
  :group 'convenience)

(defcustom banner-comment-char ?=
  "Character to be used for comment banners."
  :type 'character)

(defcustom banner-comment-char-match "[[:space:];#/*~=-]*"
  "Regexp to match old comment-banner prefix/suffix text to be destroyed."
  :type 'regexp)

(defcustom banner-comment-width nil
  "Default final column for banner comment if not specified by prefix arg.

If nil, use (or `comment-fill-column' `fill-column')."
  :type '(choice (const :tag "(or comment-fill-column fill-column)" nil)
                 integer))

(require 'subr-x) ;; for string-trim

;;;###autoload
(defun banner-comment (&optional end-column)
  "Turn line at point into a banner comment.

Called on an existing banner comment, will reformat it.

Final column will be (or END-COLUMN comment-fill-column fill-column)."
  (interactive "P")
  (save-excursion
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (beginning-of-line)
      (if (re-search-forward
           (format
            "^\\(?98:[[:space:]]*\\)\\(%s\\|\\)%s\\(?99:.*?\\)%s\\(%s\\|%s\\|\\)$"
            (or comment-start-skip (regexp-quote (string-trim comment-start)))
            banner-comment-char-match
            banner-comment-char-match
            (regexp-quote (string-trim comment-start))
            (or comment-end-skip (regexp-quote (string-trim comment-start)))))
          (let* ((central-text (if (string-empty-p (match-string 99))
                                   (make-string 2 banner-comment-char)
                                 (format " %s " (match-string 99))))
                 (banner-char-width (- (or end-column
                                           banner-comment-width
                                           comment-fill-column
                                           fill-column)
                                       (length (match-string 98)) ;; initial ws
                                       (length comment-start)
                                       (length central-text) ;; actual text
                                       (length comment-end))))
            (replace-match
             (concat
              (match-string 98) ;; initial ws
              comment-start
              (make-string (+ (/ banner-char-width 2) (% banner-char-width 2)) ?=)
              central-text
              (make-string (/ banner-char-width 2) ?=)
              comment-end)))))))


(provide 'banner-comment)
;;; banner-comment.el ends here
