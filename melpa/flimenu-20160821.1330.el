;;; flimenu.el --- Flatten imenu automatically -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: imenu browse structure hook mode
;; Package-Version: 20160821.1330
;; URL: https://github.com/IvanMalison/flimenu
;; Version: 0.0.0
;; Package-Requires: ((dash "2.10.0") (emacs "24.4"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions to flatten an imenu index.

;;; Code:

(require 'cl-lib)
(require 'dash)

(defgroup flimenu ()
  "Flimenu minor mode."
  :group 'imenu
  :prefix "flimenu-")

(defcustom flimenu-imenu-separator "/"
  "The string to use to join the titles of nested entries."
  :type '(string)
  :group 'flimenu)

(defcustom flimenu-imenu-get-markers-from-entry-strings t
  "Whether or not to check the string properties of internal nodes for a marker.

`org-mode' is known to put such a property on its strings, which
enables the addition of entries for its internal nodes."
  :type '(boolean)
  :group 'flimenu)

(defcustom flimenu-ignore-modes-list nil
  "Modes where smartparens mode is inactive if allowed globally."
  :type '(repeat symbol)
  :group 'flimenu)

;;;###autoload
(define-minor-mode flimenu-mode
  "Toggle the automatic flattening of imenu indexes."
  :lighter nil
  :keymap nil
  :group 'flimenu
  :require 'flimenu)

;;;###autoload
(define-globalized-minor-mode flimenu-global-mode
  flimenu-mode
  flimenu-mode-turn-on)

(defun flimenu-mode-turn-on ()
  (unless (member major-mode flimenu-ignore-modes-list)
   (flimenu-mode t)))

(defun flimenu-get-marker-from-string (string)
  (--first (markerp it) (text-properties-at 0 string)))

(cl-defun flimenu-flatten-index-entry (index-entry &optional (prefix ""))
  (cl-destructuring-bind (entry-name . rest) index-entry
    (let ((new-entry-name (concat prefix entry-name))
          (entry-marker
           (when flimenu-imenu-get-markers-from-entry-strings
               (flimenu-get-marker-from-string entry-name))))
      (if (listp rest)
          ;; Internal Node
          (let* ((new-prefix (concat new-entry-name flimenu-imenu-separator))
                 (flattened-subentries
                  (cl-mapcan (lambda (entry)
                               (flimenu-flatten-index-entry entry new-prefix))
                             rest)))
            (if entry-marker
                (cons (cons new-entry-name entry-marker) flattened-subentries)
              flattened-subentries))
        ;; Leaf Node
        (list (cons new-entry-name rest))))))

(defun flimenu-flatten-imenu-index (index)
  (cl-mapcan 'flimenu-flatten-index-entry index))

(defun flimenu-make-current-imenu-index-flat ()
  (let ((original-imenu-function imenu-create-index-function))
    (setq imenu-create-index-function
          (lambda ()
            (flimenu-flatten-imenu-index
             (funcall original-imenu-function))))))

(defun flimenu-maybe-flatten-imenu-index (index)
  (if flimenu-mode
      (flimenu-flatten-imenu-index index)
    index))

(defun flimenu-flatten-imenu-index-with-function (fn &rest args)
  (flimenu-maybe-flatten-imenu-index (apply fn args)))

(advice-add 'imenu--make-index-alist :around
            'flimenu-flatten-imenu-index-with-function)

(provide 'flimenu)
;;; flimenu.el ends here
