;;; ido-occasional.el --- Use ido where you choose. -*- lexical-binding: t -*-

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/ido-occasional
;; Package-Version: 20150214.448
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: completion

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package aims to replace `completing-read' with
;; `ido-completing-read', similarly to `ido-ubiquitous'.
;;
;; However, instead of doing it all at once, it only provides a convenience macro
;; for enabling `ido-completing-read' on a per-function basis:
;;
;; (global-set-key (kbd "<f1> f") (with-ido-completion describe-function))
;; (global-set-key (kbd "<f1> v") (with-ido-completion describe-variable))
;; (global-set-key (kbd "<f2> i") (with-ido-completion info-lookup-symbol))
;;
;; The advantage is that you don't get `ido' in places where you don't
;; want `ido', and you get better control over your completion
;; methods.  For instance, even if you have `helm-mode' on, with the
;; setup above, "<f1> f" will still use `ido' for completion.

;;; Code:

(defun ido-occasional-completing-read (prompt collection
                                       &optional predicate require-match initial-input
                                         hist def inherit-input-method)
  "Use `ido-completing-read' if the collection isn't too large.
Fall back to `completing-read' otherwise."
  (let ((filtered-collection
         (all-completions "" collection predicate)))
    (if (<= (length filtered-collection) 40000)
        (ido-completing-read
         prompt filtered-collection nil
         require-match initial-input hist
         def nil)
      (let ((completing-read-function 'completing-read-default))
        (completing-read
         prompt collection predicate
         require-match initial-input hist
         def inherit-input-method)))))

;;;###autoload
(defmacro with-ido-completion (fun)
  "Wrap FUN in another interactive function with ido completion."
  `(defun ,(intern (concat (symbol-name fun) "/with-ido")) ()
     ,(format "Forward to `%S' with ido completion." fun)
     (interactive)
     (let ((completing-read-function 'ido-occasional-completing-read))
       (call-interactively #',fun))))

(provide 'ido-occasional)

;;; ido-occasional.el ends here
