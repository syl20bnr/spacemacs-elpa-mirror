;;; thread-dump.el --- Java thread dump viewer
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;; Author: Dmitry Neverov
;; URL: http://github.com/nd/thread-dump.el
;; Package-Version: 20170816.1150
;; Version: 1.0
;;
;; Code goes here

(defconst thread-dump-overview-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "n") 'thread-dump-overview-show-next-thread)
    (define-key map (kbd "j") 'thread-dump-overview-show-next-thread)
    (define-key map (kbd "p") 'thread-dump-overview-show-prev-thread)
    (define-key map (kbd "k") 'thread-dump-overview-show-prev-thread)
    (define-key map (kbd "RET") 'thread-dump-overview-show-thread)
    (define-key map (kbd "o") 'thread-dump-overview-show-thread)
    (define-key map (kbd "v") 'thread-dump-overview-visit-thread)
    (define-key map (kbd "h") 'thread-dump-overview-hide)
    (define-key map (kbd "H") 'thread-dump-overview-hide-with-same-stack)
    (define-key map (kbd "q") 'thread-dump-overview-quit)
    (define-key map (kbd "/") 'thread-dump-overview-filter)
    (define-key map (kbd "N") 'thread-dump-overview-open-next-dump)
    (define-key map (kbd "P") 'thread-dump-overview-open-prev-dump)
    map))

(defun thread-dump-overview-mode ()
  (buffer-disable-undo)
  (setq major-mode 'thread-dump-overview-mode
        mode-name "Thread-Dump-Overview")
  (use-local-map thread-dump-overview-mode-map)
  (run-hooks 'thread-dump-overview-mode-hook))


(defun thread-dump-open-dir (dir)
  (interactive "DThread dump directory: ")
  (let ((files (directory-files dir t directory-files-no-dot-files-regexp)))
    (thread-dump-open-files files)))


(defun thread-dump-open-file (file)
  (interactive "FThread dump: ")
  (thread-dump-open-files (list file)))


(defun thread-dump-open-files (files &optional file-index use-old-buffer)
  (interactive)
  (let* ((findex (or file-index 0))
         (file (nth findex files))
         (threads (with-temp-buffer
                    (insert-file-contents file)
                    (thread-dump-parse-current-buffer))))
    (when (not use-old-buffer)
      (let ((old (get-buffer "*thread-dump-overview*")))
        (when old (kill-buffer old))))
    (with-current-buffer (thread-dump-get-overview-buffer)
      (setq thread-dump-file file)
      (setq thread-dump-files files)
      (setq thread-dump-file-index findex)
      (setq header-line-format (list file)))
    (thread-dump-show-overview threads)
    (thread-dump-overview-mode)))


(defun thread-dump-show-overview (threads)
  (let* ((buf (thread-dump-get-overview-buffer)))
    (set-buffer buf)
    (let ((inhibit-read-only t))
      (setq thread-dump-threads threads)
      (erase-buffer)
      (dolist (thread threads nil)
        (unless (or (thread-dump-hidden-thread? thread)
                    (thread-dump-filtered-thread? thread))
          (thread-dump-show-thread-header thread)))
      (backward-delete-char 1))
    (goto-char (point-min))
    (switch-to-buffer buf)
    (setq buffer-read-only t)
    (goto-line 1)
    (thread-dump-overview-visit-thread)))

(defun thread-dump-get-overview-buffer ()
  (let ((existing (get-buffer "*thread-dump-overview*")))
    (or existing
        (let ((new (get-buffer-create "*thread-dump-overview*")))
          (with-current-buffer new
            (make-variable-buffer-local 'thread-dump-ow-cur-thread-line)
            (make-variable-buffer-local 'thread-dump-hidden-threads)
            (make-variable-buffer-local 'thread-dump-filter)
            (make-variable-buffer-local 'thread-dump-threads)
            (make-variable-buffer-local 'thread-dump-file)
            (make-variable-buffer-local 'thread-dump-files)
            (make-variable-buffer-local 'thread-dump-file-index)
            (make-variable-buffer-local 'truncate-lines)

            (setq thread-dump-ow-cur-thread-line nil)
            (setq thread-dump-hidden-threads nil)
            (setq thread-dump-filter nil)
            (setq thread-dump-threads nil)
            (setq thread-dump-file nil)
            (setq thread-dump-files nil)
            (setq thread-dump-file-index nil)
            (setq truncate-lines t)
            new)
          ))))

(defun thread-dump-hidden-thread? (thread)
  (when thread-dump-hidden-threads
    (let ((s (thread-dump-get-thread-stack thread))
          (name (thread-dump-get-thread-name thread)))
      (delq nil
            (mapcar (lambda (hidden-thread)
                      (if (eq (thread-dump-get-hidden-thread-selection hidden-thread) 'same)
                          (and (string= (thread-dump-get-hidden-thread-stack hidden-thread) s)
                               (string= (thread-dump-get-hidden-thread-name hidden-thread) name))
                        (string= (thread-dump-get-hidden-thread-stack hidden-thread) s)))
                    thread-dump-hidden-threads)))))

(defun thread-dump-filtered-thread? (thread)
  (when thread-dump-filter
    (not (thread-dump-match thread-dump-filter thread))))

(defun thread-dump-overview-hide ()
  (interactive)
  (setq thread-dump-hidden-threads
          (cons
           (list
            (cons 'thread (thread-dump-get-thread-at-point))
            (cons 'selection 'same))
           thread-dump-hidden-threads))
  (let ((line (line-number-at-pos)))
    (thread-dump-show-overview thread-dump-threads)))

(defun thread-dump-overview-hide-with-same-stack (&optional arg)
  (interactive "P")
  (if arg
      (setq thread-dump-hidden-threads nil)
    (setq thread-dump-hidden-threads
          (cons
           (list
            (cons 'thread (thread-dump-get-thread-at-point))
            (cons 'selection 'with-same-stack))
           thread-dump-hidden-threads)))
  (let ((line (line-number-at-pos)))
    (thread-dump-show-overview thread-dump-threads)))

(defun thread-dump-overview-quit ()
  (interactive)
  (delete-other-windows)
  (bury-buffer))

(defun thread-dump-show-thread-header (thread)
  (insert (propertize (concat (thread-dump-get-thread-name thread) "\n")
                      'id (thread-dump-get-thread-id thread))))

(defun thread-dump-overview-next-thread ()
  (interactive)
  (unless (eq (point-max) (line-end-position))
    (next-line)))

(defun thread-dump-overview-prev-thread ()
  (interactive)
  (unless (eq (point-min) (line-beginning-position))
    (next-line -1)))

(defun thread-dump-overview-show-next-thread ()
  (interactive)
  (thread-dump-overview-next-thread)
  (thread-dump-overview-visit-thread))

(defun thread-dump-overview-show-prev-thread ()
  (interactive)
  (thread-dump-overview-prev-thread)
  (thread-dump-overview-visit-thread))

(defun thread-dump-overview-show-thread ()
  (interactive)
  (thread-dump-overview-visit-thread t))

(defun thread-dump-overview-visit-thread (&optional switch-to-details)
  (interactive)
  (thread-dump-highlight-cur-thread)
  (let* ((thread (thread-dump-get-thread-at-point))
         (file thread-dump-file)
         (buf (get-buffer-create "*thread-dump-details*"))
         (filter thread-dump-filter)
         (inhibit-read-only t))
    (set-buffer buf)
    (erase-buffer)
    (set (make-local-variable 'truncate-lines) t)
    (insert (thread-dump-get-thread-contents thread))
    (goto-char (point-min))
    (when filter
      (while (search-forward filter nil 't)
        (put-text-property (match-beginning 0) (match-end 0) 'face 'highlight)))
    (and file (setq header-line-format (list file)))

    (let* ((w (get-buffer-window buf))
           (cur-win (selected-window)))
      (if (and w switch-to-details)
          (select-window w)
        (unless w
          (delete-other-windows cur-win)
          (let ((w (split-window-right 60)))
            (select-window w)
            (switch-to-buffer buf)
            (unless switch-to-details
              (select-window cur-win))))))))

(defun thread-dump-get-thread-at-point ()
  (let ((id (get-text-property (point) 'id)))
    (thread-dump-find-thread-by-id id)))

(defun thread-dump-highlight-cur-thread ()
  (let ((inhibit-read-only t))
    (when thread-dump-ow-cur-thread-line
      (save-excursion
        (goto-line thread-dump-ow-cur-thread-line)
        (put-text-property (point-at-bol) (point-at-eol) 'face 'default)))
    (setq thread-dump-ow-cur-thread-line (line-number-at-pos))
    (put-text-property (point-at-bol) (point-at-eol) 'face 'thread-dump-current-thread)))

(defun thread-dump-overview-open-next-dump ()
  (interactive)
  (with-current-buffer (thread-dump-get-overview-buffer)
  (when (and thread-dump-files
             thread-dump-file-index
             (< thread-dump-file-index (- (length thread-dump-files) 1)))
    (thread-dump-open-files thread-dump-files (+ 1 thread-dump-file-index) 't))))

(defun thread-dump-overview-open-prev-dump ()
  (interactive)
  (with-current-buffer (thread-dump-get-overview-buffer)
    (when (and thread-dump-files
             thread-dump-file-index
             (> thread-dump-file-index 0))
    (thread-dump-open-files thread-dump-files (- thread-dump-file-index 1) 't))))


(defun thread-dump-find-thread-by-id (id)
  (find id
        thread-dump-threads
        :test '(lambda (x y) (= x (cdr (assoc 'id y))))))

(defun thread-dump-overview-filter (term)
  (interactive "MFilter: ")
  (setq thread-dump-filter (if (equal term "") nil term))
  (thread-dump-show-overview thread-dump-threads))

(defun thread-dump-match (term thread)
  (string-match term (thread-dump-get-thread-contents thread)))

(defun thread-dump-parse-current-buffer ()
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let ((threads (list))
            (thread-id 0))
        (while (re-search-forward "^\"" nil t)
          (move-beginning-of-line 1)
          (setq threads
                (cons (thread-dump-parse-thread-at-point thread-id) threads))
          (setq thread-id (+ thread-id 1)))

        (sort threads '(lambda (t1 t2)
                         (string< (downcase (thread-dump-get-thread-name t1))
                                  (downcase (thread-dump-get-thread-name t2)))))))))

(defun thread-dump-parse-thread-at-point (thread-id)
  (let* ((thread-start (point))
         (name-start (or (search-forward "\"" (line-end-position) t) thread-start))
         (name-end (or (- (search-forward "\"" (line-end-position) t) 1) (line-end-position)))
         (state (thread-dump-parse-thread-state-at-point))
         (stack-start (thread-dump-get-stack-start-at-point))
         (thread-end (if (re-search-forward "^\n" nil t) (line-beginning-position 1) (point-max))))
    (list
       (cons 'id thread-id)
       (cons 'name (buffer-substring-no-properties name-start name-end))
       (cons 'start thread-start)
       (cons 'end thread-end)
       (cons 'contents (buffer-substring-no-properties thread-start thread-end))
       (cons 'state state)
       (cons 'stack (if stack-start (buffer-substring-no-properties stack-start thread-end) nil)))))

(defun thread-dump-parse-thread-state-at-point ()
  (if (re-search-forward "java.lang.Thread.State: \\b\\([a-zA-Z_]+\\)\\b" (line-end-position 2) t)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1))
    nil))

(defun thread-dump-get-stack-start-at-point ()
  (if (re-search-forward "^\\( \\|\t\\)*at" (line-end-position 2) t)
      (line-beginning-position 1)
    nil))

(defun thread-dump-get-thread-name (thread)
  (cdr (assoc 'name thread)))

(defun thread-dump-get-thread-id (thread)
  (cdr (assoc 'id thread)))

(defun thread-dump-get-thread-contents (thread)
  (cdr (assoc 'contents thread)))

(defun thread-dump-get-thread-state (thread)
  (cdr (assoc 'state thread)))

(defun thread-dump-get-thread-stack (thread)
  (cdr (assoc 'stack thread)))

(defun thread-dump-get-hidden-thread-stack (thread)
  (thread-dump-get-thread-stack (cdr (assoc 'thread thread))))

(defun thread-dump-get-hidden-thread-selection (thread)
  (cdr (assoc 'selection thread)))

(defun thread-dump-get-hidden-thread-name (thread)
  (thread-dump-get-thread-name (cdr (assoc 'thread thread))))

(defface thread-dump-current-thread
  '((t :underline t
       :weight bold))
  "Current thread face."
  :group 'thread-dump-faces)



(provide 'thread-dump)

;;; thread-dump.el ends here
