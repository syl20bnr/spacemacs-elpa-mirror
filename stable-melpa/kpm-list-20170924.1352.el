;;; kpm-list.el --- An emacs buffer list that tries to intelligently group together buffers.

;; Copyright 2011 Kevin Mahoney. All rights reserved.

;; Author:   Kevin Mahoney
;; URL:      https://github.com/KMahoney/kpm-list/
;; Package-Version: 20170924.1352
;; Version:  1.0

;; This file is NOT part of GNU Emacs.

;; Redistribution and use in source and binary forms, with or without modification, are
;; permitted provided that the following conditions are met:
;;
;;    1. Redistributions of source code must retain the above copyright notice, this list of
;;       conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright notice, this list
;;       of conditions and the following disclaimer in the documentation and/or other materials
;;       provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
;; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
;; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
;; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(require 'cl)
(require 'dired)

;;; ------------------------------------------------------------------
;;; Functions responsible for sorting buffers into their groups.

(defun kpml/buffer-info (buffer)
  "Collect list info for a buffer"
  (with-current-buffer buffer
    (list (buffer-name)
          (and (kpml/kpm-buffer-directory-name))
          (or (and (buffer-file-name) (file-name-nondirectory (buffer-file-name)))
              (buffer-name))
          (symbol-name major-mode)
          (buffer-modified-p)
          (member buffer (kpml/latest-buffers)))))

(defun kpml/all-buffers ()
  "All buffers suitable for listing"
  (remove-if
   (lambda (b)
     (or
      (string= (substring (buffer-name b) 0 1) " ")
      (string= (buffer-name b) kpm-list-buffer-name)))
   (buffer-list)))

(defun kpml/latest-buffers ()
  "Customisable list of last used buffers"
  (kpml/take kpm-list-highlight-most-recent (kpml/all-buffers)))

(defun kpml/kpm-buffer-directory-name (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (or dired-directory
        (and (buffer-file-name)
             (file-name-directory (buffer-file-name))))))

(defun kpml/file-buffers ()
  (remove-if-not 'kpml/kpm-buffer-directory-name (kpml/all-buffers)))

(defun kpml/non-file-buffers ()
  (remove-if 'kpml/kpm-buffer-directory-name (kpml/all-buffers)))

(defun kpml/filter-by-mode (buffers mode)
  (remove-if-not '(lambda (b) (string= mode (nth 3 b))) buffers))

;; buffer sorting
(defun kpml/sort-by-nth (buffers n)
  (sort buffers (lambda (a b) (string< (nth n a) (nth n b)))))
(defun kpml/sort-by-name      (buffers) (kpml/sort-by-nth buffers 0))
(defun kpml/sort-by-dir       (buffers) (kpml/sort-by-nth buffers 1))
(defun kpml/sort-by-file-name (buffers) (kpml/sort-by-nth buffers 2))
(defun kpml/sort-by-mode      (buffers) (kpml/sort-by-nth buffers 3))

(defun kpml/unique-modes (buffers)
  "A list of unique modes in buffer list"
  (sort (delete-dups (mapcar '(lambda (b) (nth 3 b)) buffers)) 'string<))

(defun kpml/is-prefix (prefix string)
  "is `prefix` the prefix of `string`"
  (and
   (<= (length prefix) (length string))
   (string= prefix (substring string 0 (length prefix)))))

(defun kpml/is-buffer-subdir (parent subdir)
  "True if subdir is a subdirectory of parent"
  (kpml/is-prefix (nth 1 parent) (nth 1 subdir)))

(defun kpml/buffer-path-difference (parent subdir)
  "Return the difference in buffer's paths as (same-part . new-part)"
  (let ((split (length (nth 1 parent)))
        (path (nth 1 subdir)))
    (cons
     (substring path 0 split)
     (substring path split))))

(defun kpml/merge-singles (groups)
  "Collect all the groups with a length of 1 into their own group"
  (remove-if-not 'identity
                 (append
                  (remove-if-not '(lambda (g) (> (length g) 1)) groups)
                  (list (apply 'append (remove-if '(lambda (g) (> (length g) 1)) groups))))))

(defun kpml/group-by-prefix (buffers &optional groups)
  "Group buffers if they are a subdirectory of the parent & add relative path to info."
  (if buffers
      (if (and groups (caar groups) (kpml/is-buffer-subdir (caar groups) (car buffers)))

          ;; append to current group
          (let* ((head-buffer (caar groups))
                 (tail-buffer (car (last (car groups))))
                 (buffer (car buffers))
                 (relative-path (kpml/buffer-path-difference
                                 (if (kpml/is-buffer-subdir tail-buffer buffer) tail-buffer head-buffer)
                                 buffer))
                 (new-buffer (append (car buffers) (list relative-path))))
            (kpml/group-by-prefix (cdr buffers)
                             (cons (append (car groups) (list new-buffer))
                                   (cdr groups))))

        ;; create new group
        (let ((new-buffer (append (car buffers) (list (cons "" (nth 1 (car buffers)))))))
          (kpml/group-by-prefix (cdr buffers)
                           (cons (list new-buffer) groups))))

    groups))

(defun kpml/get-kpm-list-buffers ()
  "Return a list of file buffers as a list of buffer groups for each mode."
  (let ((buffers (mapcar 'kpml/buffer-info (kpml/file-buffers))))
    (mapcar '(lambda (mode)
               (cons mode
                     (kpml/merge-singles
                      (kpml/group-by-prefix
                       (kpml/sort-by-dir
                        (kpml/sort-by-file-name
                         (kpml/filter-by-mode buffers mode)))))))
            (kpml/unique-modes buffers))))

(defun kpml/get-non-file-kpm-list-buffers ()
  "Return a list of non-file buffers as a list of buffers grouped by mode."
  (let ((buffers (mapcar 'kpml/buffer-info (kpml/non-file-buffers))))
    (kpml/merge-singles
     (mapcar '(lambda (mode) (kpml/sort-by-name (kpml/filter-by-mode buffers mode)))
             (kpml/unique-modes buffers)))))


;;; ------------------------------------------------------------------
;;; Functions responsible for presenting the list in a buffer.

(defun kpml/add-line-properties (properties)
  (add-text-properties (point-at-bol) (point-at-eol) properties))

(defun kpml/insert-dir (dir &optional face)
  (insert (propertize dir
                      'face (or face 'kpm-list-directory-face)
                      'mouse-face 'highlight
                      'dir-link t)))

(defun kpml/insert-buffer-line (buffer)
  (destructuring-bind (name dir filename mode modified highlight relative) buffer
    (insert (propertize (if modified "* " "  ") 'face 'kpm-list-modified-face))
    (insert (propertize filename 'face (if highlight 'kpm-list-buffer-highlight-face 'kpm-list-buffer-face)))
    (insert (propertize " "  'display '(space . (:align-to 40))))
    (kpml/add-line-properties (list 'mouse-face 'highlight))

    (insert " ")
    (if kpm-list-highlight-relative
        (progn
          (kpml/insert-dir (car relative) 'kpm-list-old-path-face)
          (kpml/insert-dir (cdr relative)))
      (kpml/insert-dir dir))

    (kpml/add-line-properties (list 'buffer-name name 'dir-name dir))
    (insert "\n")))

(defun kpml/insert-non-file-buffer-line (buffer)
  (destructuring-bind (name dir filename mode modified highlight) buffer
    (insert "  ")
    (insert (propertize name 'face (if highlight 'kpm-list-buffer-highlight-face 'kpm-list-buffer-face)))
    (insert (propertize " "  'display '(space . (:align-to 40))))
    (insert " ")
    (insert (propertize mode 'face 'kpm-list-mode-face))
    (kpml/add-line-properties (list 'buffer-name name 'mouse-face 'highlight))
    (insert "\n")))

(defun kpml/header (title)
  (insert "\n")
  (insert "  " title (propertize " "  'display '(space . (:align-to right))))
  (kpml/add-line-properties '(face kpm-list-header-face))
  (insert "\n\n"))

(defun kpml/make-kpm-list-buffer ()
  (with-current-buffer (get-buffer-create kpm-list-buffer-name)
    (let ((buffer-read-only nil))
      (erase-buffer)

      ;; modes
      (dolist (mode (kpml/get-kpm-list-buffers))
        (kpml/header (car mode))
        ;; groups
        (dolist (group (cdr mode))
          ;; buffers
          (dolist (buffer group)
            (kpml/insert-buffer-line buffer))
          (when (not kpm-list-compact) (insert "\n"))))

      ;; non-file
      (kpml/header "Other Buffers")
      (dolist (mode-group (kpml/get-non-file-kpm-list-buffers))
        (dolist (buffer mode-group)
          (kpml/insert-non-file-buffer-line buffer))
          (when (not kpm-list-compact) (insert "\n"))))

    (kpm-list-mode)))


;;; Util -------------------------------------------------------------

(defun kpml/take (n list)
  (if (and list (> n 0)) (cons (car list) (kpml/take (- n 1) (cdr list)))))

(defun kpml/buffer-at-point ()
  (get-text-property (point) 'buffer-name))

(defun kpml/buffer-point ()
  (+ 2 (point-at-bol)))

(defun kpml/dir-at-point ()
  (get-text-property (point) 'dir-name))

(defun kpml/first-line-p ()
  (= (point-at-bol) (point-min)))

(defun kpml/last-line-p ()
  (= (point-at-eol) (point-max)))

(defun kpml/first-buffer ()
  (beginning-of-buffer)
  (kpm-list-next-buffer))

(defun kpml/goto-buffer (buffer-name)
  (if buffer-name
      (progn
        (end-of-buffer)
        (while (and (not (string= buffer-name (kpml/buffer-at-point))) (not (kpml/first-line-p)))
          (forward-line -1))
        (if (kpml/buffer-at-point)
            (setf (point) (kpml/buffer-point))
          (kpml/first-buffer)))
    (kpml/first-buffer)))

(defun kpml/next-buffer-point ()
  (save-excursion
    (forward-line)
    (while (and (not (kpml/buffer-at-point)) (not (kpml/last-line-p)))
      (forward-line))
    (and (kpml/buffer-at-point) (kpml/buffer-point))))

(defun kpml/prev-buffer-point ()
  (save-excursion
    (forward-line -1)
    (while (and (not (kpml/buffer-at-point)) (not (kpml/first-line-p)))
      (forward-line -1))
    (and (kpml/buffer-at-point) (kpml/buffer-point))))

(defun kpml/is-directory-link ()
  (get-text-property (point) 'dir-link))

;;; Commands ---------------------------------------------------------

;;;###autoload
(defun kpm-list-select-buffer ()
  (interactive)
  (if (and (kpml/dir-at-point) (kpml/is-directory-link))
      (dired (kpml/dir-at-point))
    (when (kpml/buffer-at-point) (switch-to-buffer (kpml/buffer-at-point)))))

;;;###autoload
(defun kpm-list-select-other-window ()
  (interactive)
  (if (and (kpml/dir-at-point) (kpml/is-directory-link))
      (dired-other-window (kpml/dir-at-point))
    (when (kpml/buffer-at-point) (switch-to-buffer-other-window (kpml/buffer-at-point)))))

;;;###autoload
(defun kpm-list-select-dir ()
  (interactive)
  (when (kpml/dir-at-point) (dired (kpml/dir-at-point))))

;;;###autoload
(defun kpm-list-refresh ()
  (interactive)
  (let ((buffer (or (kpml/buffer-at-point) (buffer-name (car (kpml/all-buffers))))))
    (kpml/make-kpm-list-buffer)
    (kpml/goto-buffer buffer)))

;;;###autoload
(defun kpm-list-kill-buffer ()
  (interactive)
  (when (and (kpml/buffer-at-point) (kill-buffer (kpml/buffer-at-point)))
    (let ((buffer-read-only nil)) (kill-whole-line))
    (if (kpml/buffer-at-point)
        (setf (point) (kpml/buffer-point))
      (kpm-list-next-buffer))))

;;;###autoload
(defun kpm-list-prev-buffer ()
  (interactive)
  (let ((p (kpml/prev-buffer-point)))
    (when p (setf (point) p))))

;;;###autoload
(defun kpm-list-next-buffer ()
  (interactive)
  (let ((p (kpml/next-buffer-point)))
    (when p (setf (point) p))))

;;;###autoload
(defun kpm-list ()
  (interactive)
  (kpml/make-kpm-list-buffer)
  (switch-to-buffer kpm-list-buffer-name)
  (kpml/goto-buffer (buffer-name (car (kpml/all-buffers)))))

;;; Options ----------------------------------------------------------

(defgroup kpm-list ()
  "A list of open buffers."
  :group 'tools
  :group 'convenience)

(defcustom kpm-list-highlight-relative t
  "Non-nil means to highlight changing subdirectories."
  :type 'boolean
  :group 'kpm-list)

(defcustom kpm-list-compact nil
  "Non-nil means to use a more compact display."
  :type 'boolean
  :group 'kpm-list)

(defcustom kpm-list-buffer-name "*Grouped Buffer List*"
  "Buffer name to use."
  :type 'string
  :group 'kpm-list)

(defcustom kpm-list-highlight-most-recent 1
  "Highlight N most recently used buffers."
  :type 'number
  :group 'kpm-list)

;;; Faces ------------------------------------------------------------

(defface kpm-list-directory-face
  '((t (:foreground "LightSkyBlue" :inherit dired-directory)))
  "*Face used for directories in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-old-path-face
  '((t (:foreground "#28A" :inherit kpm-list-directory-face)))
  "*Face used for directories in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-buffer-face
  '((t (:inherit default)))
  "*Face used for buffers in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-buffer-highlight-face
  '((t (:foreground "#9E9" :inherit default)))
  "*Face used for buffers in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-header-face
  '((t (:foreground "White" :background "#420" :inherit default)))
  "*Face used for headers in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-mode-face
  '((t (:foreground "Orange" :inherit default)))
  "*Face used for modes in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

(defface kpm-list-modified-face
  '((t (:foreground "Red" :inherit default)))
  "*Face used for modified indicator in *Grouped Buffer List* buffer."
  :group 'kpm-list
  :group 'font-lock-highlighting-faces)

;;; Keymap -----------------------------------------------------------

(defvar kpm-list-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "<RET>") 'kpm-list-select-buffer)
    (define-key map (kbd "<mouse-1>") 'kpm-list-select-buffer)
    (define-key map "o" 'kpm-list-select-other-window)
    (define-key map "d" 'kpm-list-select-dir)
    (define-key map "g" 'kpm-list-refresh)
    (define-key map "k" 'kpm-list-kill-buffer)
    (define-key map "p" 'kpm-list-prev-buffer)
    (define-key map "n" 'kpm-list-next-buffer)
    map)
  "Keymap for buffer list.")

;;; Mode -------------------------------------------------------------

(define-derived-mode kpm-list-mode special-mode "Grouped Buffer List"
  "Major mode for editing a list of open buffers.")

(provide 'kpm-list)

;;; kpm-list.el ends here
