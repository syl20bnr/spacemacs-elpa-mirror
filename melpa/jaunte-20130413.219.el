;;; jaunte.el --- Emacs Hit a Hint
;; Package-Version: 20130413.219

;; 参考
;; yafastnav.el
;; vimperator

;; 設定
;; (require 'jaunte)
;; (global-set-key (kbd "C-c C-j") 'jaunte)

(defvar jaunte-keys (mapcar #'identity "jklasdfghyuiopqwertnmzxcvb"))

(defface jaunte-hint-face
  '((t
     (:foreground "white"
      :background "blue"
      :italic nil
      :bold nil)))
  nil)

(defface jaunte-hint-face2
  '((t
     (:foreground "white"
      :background "royalblue"
      :italic nil
      :bold nil)))
  nil)

(defvar jaunte-hint-faces '(jaunte-hint-face jaunte-hint-face2))

(defun jaunte-cycle-reset (symbol)
  (put symbol 'jaunte-cycle-index 0))

(defun jaunte-cycle (symbol)
  (let ((index (or (get symbol 'jaunte-cycle-index)
                   (jaunte-cycle-reset symbol)))
        (list (symbol-value symbol)))
    (put symbol 'jaunte-cycle-index (1+ index))
    (nth (% index (length list)) list)))

(defvar jaunte--hints nil)

(defvar jaunte-hint-unit 'word
  "Hint unit. You can set this parameter same as `thing-at-point'.")
(make-variable-buffer-local 'jaunte-hint-unit)

(defun jaunte-forward-word ()
  "Move to beginning of a forward word, and return point."
  (interactive)
  (if (looking-at "\\w")
      (forward-thing jaunte-hint-unit))
  (if (re-search-forward "\\w" nil 'eob)
      (backward-char))
  (point))

(defun jaunte-make-hint (key overlay window point)
  (let ((hint (make-hash-table :test 'equal)))
    (puthash 'key key hint)
    (puthash 'overlay overlay hint)
    (puthash 'window window hint)
    (puthash 'point point hint)
    hint))

(defun jaunte-show-hints ()
  (condition-case err
      (let ((index 0))
        (jaunte-cycle-reset 'jaunte-hint-faces)
        (mapc
         (lambda (window)
           (save-excursion
             (save-window-excursion
               (select-window window)
               (move-to-window-line 0)
               (let ((point (if (looking-at "\\w")
                                (point)
                              (jaunte-forward-word)))
                     (window-end (window-end window))
                     (key (jaunte-make-key index)))
                 (while (< point window-end)
                   (add-to-list 'jaunte--hints
                                (jaunte-make-hint (jaunte-make-key index)
                                                  (jaunte-make-overlay point key)
                                                  window
                                                  point))
                   (jaunte-forward-word)
                   (setq index (1+ index)
                         point (point)
                         key (jaunte-make-key index)))))))
         (window-list)))
    (error
     (jaunte-remove-hints)
     (error (error-message-string err)))))

(defun jaunte-hint-match (key hint &optional perfect-match)
  (let ((hint-key (gethash 'key hint)))
    (if perfect-match
        (string= key hint-key)
      (equal 0 (string-match key hint-key)))))

(defun jaunte-search (key &optional perfect-match)
  (let (result)
    (mapc
     (lambda (hint)
       (if (jaunte-hint-match key hint perfect-match)
           (add-to-list 'result hint)
         (delete-overlay (gethash 'overlay hint))))
     jaunte--hints)
    (setq jaunte--hints result)))

(defun jaunte-remove-hints ()
  (jaunte-delete-overlays)
  (setq jaunte--hints nil))

(defun jaunte-delete-overlays ()
  (mapc
   (lambda (hint)
     (delete-overlay (gethash 'overlay hint)))
   jaunte--hints))

(defun jaunte-make-overlay (point key)
  (save-excursion
    (goto-char point)
    (let* ((width (length key))
           (rest width)
           (begin point)
           overlay)
      (while (and (> rest 0) (not (eolp)))
        (setq rest (- rest (char-width (char-after))))
        (forward-char))

      (if (and (eq (logand width 1) 1)
               (= 2 (char-width (char-before))))
          (setq key (concat key " ")))

      (setq overlay (make-overlay begin (point)))
      (overlay-put overlay 'display (propertize key 'face (jaunte-cycle 'jaunte-hint-faces)))
      (overlay-put overlay 'window (selected-window))
      (overlay-put overlay 'priority 100)
      overlay)))

(defun jaunte-make-key (index)
  (let* ((key-length (length jaunte-keys))
         (excess (/ index key-length))
         prefix
         (n (% index key-length)))
    (setq prefix (if (zerop excess)
                          "" (jaunte-make-key (1- excess))))
    (concat prefix (char-to-string (nth n jaunte-keys)))))

(defun jaunte-to (hint)
  (select-window (gethash 'window hint))
  (goto-char (gethash 'point hint)))

(defun jaunte ()
  (interactive)
  (jaunte-show-hints)
  (unwind-protect
      (let (k key)
        (while (not (null jaunte--hints))
          (setq k (read-event (concat "Jaunte to " key)))
          (if (and (not (null key))
                   (or (equal k 13) ;; C-m
                       (equal k 10) ;; C-j
                       (equal k 'return))) ;; RET
              (jaunte-search key t)
            (setq key (concat key (char-to-string k)))
            (jaunte-search key))
          (if (= 1 (length jaunte--hints))
              (progn (jaunte-to (car jaunte--hints)) (jaunte-remove-hints)))))
    (jaunte-remove-hints)))

(provide 'jaunte)
;;; jaunte.el ends here
