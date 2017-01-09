;;; erc-colorize.el --- Per user colorization of whole message

;; Copyright (C) 2014-2017 Sylvain Rousseau

;; Author: Sylvain Rousseau <thisirs at gmail dot com>
;; Maintainer: Sylvain Rousseau <thisirs at gmail dot com>
;; Keywords: erc convenience
;; Package-Version: 20170107.539
;; URL: https://github.com/thisirs/erc-colorize.git

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

;;; Code:

(require 'erc)
(require 'ring)
(require 'erc-button)                   ; For erc-button-add-face

;;;###autoload (autoload 'erc-colorize-mode "erc-colorize")
(define-erc-module colorize nil
  "This module highlights messages of a user with the same face."
  ((add-hook 'erc-insert-modify-hook 'erc-colorize-message 'append)
   (add-hook 'erc-mode-hook 'erc-colorize-setup)
   (erc-buffer-list #'erc-colorize-setup))
  ((remove-hook 'erc-insert-modify-hook 'erc-colorize-message)
   (remove-hook 'erc-mode-hook 'erc-colorize-setup)))

(defun erc-colorize-setup ()
  "Initialize nickname vs face ring."
  (setq erc-colorize-ring (make-ring (length erc-colorize-faces))))

(defface erc-distinct-1-face '((t :foreground "#00538A"))
  "ERC face for distinguishing messages."
  :group 'erc-faces)

(defface erc-distinct-2-face '((t :foreground "#FF7A5C"))
  "ERC face for distinguishing messages."
  :group 'erc-faces)

(defface erc-distinct-3-face '((t :foreground "#007D34"))
  "ERC face for distinguishing messages."
  :group 'erc-faces)

(defface erc-distinct-4-face '((t :foreground "#FF8E00"))
  "ERC face for distinguishing messages."
  :group 'erc-faces)

(defface erc-distinct-5-face '((t :foreground "#F4C800"))
  "ERC face for distinguishing messages."
  :group 'erc-faces)

(defface erc-distinct-6-face '((t :foreground "#93AA00"))
  "ERC face for distinguishing messages."
  :group 'erc-faces)

(defface erc-distinct-7-face '((t :foreground "#F13A13"))
  "ERC face for distinguishing messages."
  :group 'erc-faces)

(defvar erc-colorize-faces
  '(
    erc-distinct-1-face
    erc-distinct-2-face
    erc-distinct-3-face
    erc-distinct-4-face
    erc-distinct-5-face
    erc-distinct-6-face
    erc-distinct-7-face
    )
  "List of faces to apply to users' messages.")

(defvar erc-colorize-ring nil "Ring of conses of the form (NICK . FACE).")
(make-variable-buffer-local 'erc-colorize-ring)

(defun ring-assoc (ring nickname)
  "Return index of conses in RING whose car is NICKNAME, else nil."
  (catch 'found
    (dotimes (ind (ring-length ring) nil)
      (when (equal nickname (car (ring-ref ring ind)))
        (throw 'found ind)))))

(defun erc-colorize-color (nickname)
  "Return the face used for NICKNAME.

Return nil if NICKNAME is not a string. Otherwise, first look up
`erc-colorize-ring' if there is already an association. If not,
pick the first face in `erc-colorize-faces' that is not already
used. If none, take the face of the least active user."
  (if (stringp nickname)
      (let ((ind (ring-assoc erc-colorize-ring nickname)))
        (if ind
            (progn
              (let ((last (ring-remove erc-colorize-ring ind)))
                (ring-insert erc-colorize-ring last))
              (cdr (ring-ref erc-colorize-ring 0)))
          (let* ((used (mapcar #'cdr (ring-elements erc-colorize-ring)))
                 (face (catch 'found
                         (dolist (f erc-colorize-faces)
                           (unless (member f used)
                             (throw 'found f))))))
            (if face
                (progn
                  (ring-insert erc-colorize-ring (cons nickname face))
                  face)
              (let ((older (ring-remove erc-colorize-ring)))
                (ring-insert erc-colorize-ring (cons nickname (cdr older)))
                (cdr older))))))))

(defun erc-colorize-message ()
  "Function used in `erc-insert-modify-hook' to apply the same face to a
message coming from a user."
  (erc-find-parsed-property)
  (let* ((vector (erc-get-parsed-vector (point)))
         (nickuserhost (erc-get-parsed-vector-nick vector))
         (nickname (and nickuserhost
                        (nth 0 (erc-parse-user nickuserhost))))
         (match-face (erc-colorize-color nickname)))
    (when match-face
      (erc-button-add-face (point-min) (point-max) match-face))))


(provide 'erc-colorize)

;;; erc-colorize.el ends here
