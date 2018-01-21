;;; magit-tbdiff.el --- Magit extension for git-tbdiff  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018  Kyle Meyer

;; Author: Kyle Meyer <kyle@kyleam.com>
;; URL: https://github.com/magit/magit-tbdiff
;; Package-Version: 20180120.1553
;; Keywords: vc, tools
;; Version: 0.2.0
;; Package-Requires: ((emacs "24.4") (magit "2.10.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Magit-tbdiff provides a Magit interface to git-tbdiff [1,2], a Git
;; extension for comparing two versions of a topic branch.
;;
;; There are three commands for calling git-tbdiff:
;;
;;   * `magit-tbdiff-ranges' is the most generic of the three
;;     commands.  It reads two ranges that represent the two series to
;;     be compared.
;;
;;   * `magit-tbdiff-revs' reads two revisions.  From these (say, "A"
;;     and "B"), it constructs the two series as B..A and A..B.
;;
;;   * `magit-tbdiff-revs-with-base' is like the previous command, but
;;     it also reads a base revision, constructing the range as
;;     <base>..A and <base>..B.
;;
;; These commands are available in the popup `magit-tbdiff-popup',
;; which in turn is available in the Magit diff popup, bound by
;; default to "i" (for "interdiff").  So, with the default
;; keybindings, you can invoke the tbdiff popup with "di".
;;
;; When Magit-tbdiff is installed from MELPA, no additional setup is
;; needed beyond installing git-tbdiff [1].  The tbdiff popup will be
;; added under the Magit diff popup, and Magit-tbdiff will be loaded
;; the first time that the tbdiff popup is invoked.
;;
;; [1] https://github.com/trast/tbdiff
;; [2] https://public-inbox.org/git/87ip2pfs19.fsf@linux-k42r.v.cablecom.net/

;;; Code:

(require 'magit)


;;; Options

(defgroup magit-tbdiff nil
  "Magit extension for git-tbdiff"
  :prefix "magit-tbdiff"
  :group 'magit-extensions)

(defface magit-tbdiff-marker-equivalent
  '((t (:inherit magit-cherry-equivalent)))
  "Face for '=' marker in tbdiff output."
  :group 'magit-tbdiff)

(defface magit-tbdiff-marker-different
  '((t (:inherit magit-cherry-unmatched)))
  "Face for '!' marker in tbdiff output."
  :group 'magit-tbdiff)

(defface magit-tbdiff-marker-unmatched
  '((t (:inherit magit-cherry-unmatched)))
  "Face for '<' and '>' markers in tbdiff output."
  :group 'magit-tbdiff)


;;; Internals

(defvar magit-tbdiff-assignment-re
  (eval-when-compile
    (let ((digit-re '(and (zero-or-more " ")  ; Retain left padding.
                          (or (one-or-more digit)
                              (one-or-more "-"))))
          (hash-re '(or (repeat 4 40 (char digit (?a . ?f)))
                        (repeat 7 "-"))))
      (rx-to-string `(and line-start
                          (group ,digit-re)
                          ":" (zero-or-more " ")
                          (group ,hash-re) " "
                          (group (any "<>!=")) " "
                          (group ,digit-re)
                          ":" (zero-or-more " ")
                          (group ,hash-re) " "
                          (group (zero-or-more not-newline)))
                    t))))

(defun magit-tbdiff-wash (_args)
  (while (not (eobp))
    (if (looking-at magit-tbdiff-assignment-re)
        (magit-bind-match-strings
            (num-a hash-a marker num-b hash-b subject) nil
          (magit-delete-line)
          (when (string-match-p "-" hash-a) (setq hash-a nil))
          (when (string-match-p "-" hash-b) (setq hash-b nil))
          (magit-insert-section (commit (or hash-b hash-a))
            (insert
             (mapconcat
              #'identity
              (list num-a
                    (if hash-a
                        (propertize hash-a 'face 'magit-hash)
                      (make-string (length hash-b) ?-))
                    (propertize marker
                                'face
                                (pcase marker
                                  ("=" 'magit-tbdiff-marker-equivalent)
                                  ("!" 'magit-tbdiff-marker-different)
                                  ((or "<" ">") 'magit-tbdiff-marker-different)
                                  (_
                                   (error "Unrecognized marker"))))
                    num-b
                    (if hash-b
                        (propertize hash-b 'face 'magit-hash)
                      (make-string (length hash-a) ?-))
                    subject)
              " ")
             ?\n)
            (magit-insert-heading)
            (when (not (looking-at-p magit-tbdiff-assignment-re))
              (let ((beg (point))
                    end)
                (while (looking-at "^    ")
                  (magit-delete-match)
                  (forward-line 1))
                (setq end (point))
                (goto-char beg)
                (save-restriction
                  (narrow-to-region beg end)
                  (magit-wash-sequence #'magit-diff-wash-hunk))))))
      (error "Unexpected tbdiff output"))))

(defun magit-tbdiff-insert ()
  "Insert tbdiff output into a `magit-tbdiff-mode' buffer."
  (apply #'magit-git-wash
         #'magit-tbdiff-wash
         "tbdiff" "--no-color" magit-refresh-args))

(defun magit-tbdiff-refresh-buffer (rev-a rev-b _args)
  (setq header-line-format
        (propertize (format " Interdiffs: %s vs %s" rev-a rev-b)
                    'face 'magit-header-line))
  (magit-insert-section (tbdiff-buf)
    (magit-tbdiff-insert)))

(define-derived-mode magit-tbdiff-mode magit-mode "Magit-tbdiff"
  "Mode for viewing git tbdiff output.

\\{magit-tbdiff-mode-map}"
  :group 'magit-tbdiff
  (setq-local magit-diff-highlight-trailing nil)
  (hack-dir-local-variables-non-file-buffer))

(defun magit-tbdiff-apply-error (&rest _args)
  (when (derived-mode-p 'magit-tbdiff-mode)
    (user-error "Cannot apply changes from interdiff hunk")))
(advice-add 'magit-apply :before #'magit-tbdiff-apply-error)
(advice-add 'magit-reverse :before #'magit-tbdiff-apply-error)


;;; Commands

;;;###autoload
(defun magit-tbdiff-ranges (range-a range-b &optional args)
  "Compare commits in RANGE-A with those in RANGE-B.
$ git tbdiff [ARGS...] RANGE-A RANGE-B"
  (interactive (list (magit-read-range "Range A")
                     (magit-read-range "Range B")
                     (magit-tbdiff-arguments)))
  (magit-mode-setup #'magit-tbdiff-mode range-a range-b args))

;;;###autoload
(defun magit-tbdiff-revs (rev-a rev-b &optional args)
  "Compare commits in REV-B..REV-A with those in REV-A..REV-B.
$ git tbdiff [ARGS...] REV-B..REV-A REV-A..REV-B"
  (interactive
   (let ((rev-a (magit-read-branch-or-commit "Revision A")))
     (list rev-a
           (magit-read-other-branch-or-commit "Revision B" rev-a)
           (magit-tbdiff-arguments))))
  (magit-tbdiff-ranges (concat rev-b ".." rev-a)
                       (concat rev-a ".." rev-b)
                       args))

;;;###autoload
(defun magit-tbdiff-revs-with-base (rev-a rev-b base &optional args)
  "Compare commits in BASE..REV-A with those in BASE..REV-B.
$ git tbdiff [ARGS...] BASE..REV-A BASE..REV-B"
  (interactive
   (let* ((rev-a (magit-read-branch-or-commit "Revision A"))
          (rev-b (magit-read-other-branch-or-commit "Revision B" rev-a)))
     (list rev-a rev-b
           (magit-read-branch-or-commit "Base"
                                        (or (magit-get-upstream-branch rev-b)
                                            (magit-get-upstream-branch rev-a)))
           (magit-tbdiff-arguments))))
  (magit-tbdiff-ranges (concat base ".." rev-a)
                       (concat base ".." rev-b)
                       args))

;;;###autoload (autoload 'magit-tbdiff-popup "magit-tbdiff" nil t)
(magit-define-popup magit-tbdiff-popup
  "Popup console for git tbdiff."
  'magit-popups
  :switches '((?s "Suppress diffs" "--no-patches"))
  :options '((?w "Creation weight [default: 0.6]" "--creation-weight="))
  :actions '((?b "Compare revs using common base" magit-tbdiff-revs-with-base)
             (?i "Compare revs" magit-tbdiff-revs)
             (?r "Compare ranges" magit-tbdiff-ranges))
  :max-action-columns 1)

;;;###autoload
(eval-after-load 'magit
  '(magit-define-popup-action 'magit-diff-popup
     ?i "Interdiffs" 'magit-tbdiff-popup))

(provide 'magit-tbdiff)
;;; magit-tbdiff.el ends here
