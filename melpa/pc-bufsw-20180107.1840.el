;;; pc-bufsw.el --- PC style quick buffer switcher

;; This is free and unencumbered software released into the public domain.
;;`
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; Author: Igor Bukanov <igor@mir2.org>
;; Version: 3.0
;; Package-Version: 20180107.1840
;; Keywords: buffer
;; URL: https://github.com/ibukanov/pc-bufsw

;;; Commentary:

;; This switches Emacs buffers according to
;; most-recently-used/least-recently-used order using `C-tab` and
;; `C-S-tab` keys. It is similar to window or tab switchers that are
;; available in PC desktop environments or applications.

;;; ChangeLog:

;; 2015-09-18 (3.0 release)
;; Support for the customization.
;; Support for autoloading.
;; Using pc-bufsw- for public and pc-bufsw-- for private functions and
;; variables, not non-standard pc-bufsw:: prefix for function names.

;; 2007-06-27 (2.0 release)
;; Removal of window switching facility making pc-bufsw to switch only between
;; buffers. Emacs and window managers provides enough key bindings to switch
;; between windows and frames.

;; 2005-08-25
;; Introduction of pc-bufsw--keep-focus-window mode. This is not the
;; start of the feature creep as the old mode is kept for compatibility
;; as users may not appreciate the new behavior.

;; 2005-08-17 (1.3 release)
;; * Use buffer-display-time to construct buffer list in proper least
;;   recently used order to defeat bury-buffer abuse by various tools.
;; * When switching from initial window, restore the original buffer
;;   there.
;; * Fix frame switching using select-frame-set-input-focus. It does
;;   not resolve all the issue, but at least it works.

;;; Code:

;;;###autoload
(defun pc-bufsw-mru ()
  "Switch to the most recently used buffer."
  (interactive)
  (pc-bufsw--walk 1))

;;;###autoload
(defun pc-bufsw-lru ()
  "Switch to the least recently used buffer."
  (interactive)
  (pc-bufsw--walk -1))

;;;###autoload
(defun pc-bufsw-clear-default-keybindings ()
  "Clear default keybindings set by `pc-bufsw-default-keybindings'."
  (let ((gm (current-global-map)))
    (mapc (lambda (key)
	    (when (eq (lookup-key gm key) 'pc-bufsw-mru)
		(define-key gm key nil)))
	  (car pc-bufsw-keys))
    (mapc (lambda (key)
	    (when (eq (lookup-key gm key) 'pc-bufsw-lru)
	      (define-key gm key nil)))
	  (cadr pc-bufsw-keys))))

;; See
;; https://stackoverflow.com/questions/32693757/emacs-package-customization-and-autoloads
;; why I have to copy all defcustom definition literally into the
;; autoload file. I also copied pc-bufsw-default-keybindings there so
;; the function can be called without loading the whole file.

;;;###autoload
(unless (fboundp 'pc-bufsw-default-keybindings)

  (defun pc-bufsw-default-keybindings ()
    "Enable keybindings according to `pc-bufsw-keys'."
    (mapc (lambda (key) (global-set-key key 'pc-bufsw-mru)) (car pc-bufsw-keys))
    (mapc (lambda (key) (global-set-key key 'pc-bufsw-lru)) (cadr pc-bufsw-keys)))

  (defgroup pc-bufsw nil
    "Settings for PC style quick buffer switcher."
    :group 'convenience)

  (defcustom pc-bufsw-keys
    '(([C-tab] "\e[1;5I") ([C-S-tab] [C-S-iso-lefttab] "\e[1;6I"))
    "Two-element list with key sets to cycle from most to least recently
used buffers and in reverse.  The default sets contain <C-tab> and <C-S-tab> plus sequence
reported by some terminals when pressing those keys that Emacs does not recognize as such."
    :group 'pc-bufsw
    :type '(list (repeat
		  :tag "Cycle from most to least recently used buffers using any of"
		  key-sequence)
		 (repeat
		  :tag "Cycle from least to most recently used buffers using any of"
		  key-sequence)))

  (defcustom pc-bufsw-keys-enable nil
    "If true, enable keybindings from `pc-bufsw-keys'."
    :group 'pc-bufsw
    :type 'boolean
    :set-after '(pc-bufsw-keys)
    :set (lambda (symbol value)
	   ;; During the package initialization boundp gives false
	   ;; preventing call to the clear which triggers otherwise
	   ;; loading of the whole file.
	   (when (boundp `pc-bufsw-keys-enable)
	     (pc-bufsw-clear-default-keybindings))
	   (set-default symbol value)
	   (when value
	     (pc-bufsw-default-keybindings))))

  (defcustom pc-bufsw-quit-time 3
    "Quit buffer switching after the given time in seconds.  If
there is no input during this interval the last choosen buffer
becomes current."
    :group 'pc-bufsw
    :type 'number)

  (defcustom pc-bufsw-wrap-index t
    "Wrap to the other end of the buffer list when attempting to navigate past its edge."
    :group 'pc-bufsw
    :type 'boolean))

(defvar pc-bufsw--walk-vector nil
  "Vector of buffers to navigate during buffer switch.
Buffers are odered from most to least recently used.")

(defun pc-bufsw--get-buf (index)
  (aref pc-bufsw--walk-vector index))

(defvar pc-bufsw--cur-index 0
  "Index of currently selected buffer in `pc-bufsw--walk-vector'.")

(defvar pc-bufsw--start-buf-list nil
  "The buffer list at the start of the buffer switch.
When the user stops the selection, the new order of buffers
matches the list except the selected buffer that is moved on the
top.")

(defun pc-bufsw--walk (direction)
  ;; Main loop. It does 4 things. First, select new buffer and/or
  ;; windows according to user input. Second, it selects the newly
  ;; choosen buffer/windows/frame. Third, it draw in the echo area
  ;; line with buffer names. Forth, it waits for a timeout to
  ;; terminate the switching.
  (when (and (null pc-bufsw--walk-vector) (pc-bufsw--can-start))
    (setq pc-bufsw--start-buf-list (buffer-list))
    (setq pc-bufsw--cur-index 0)
    (setq pc-bufsw--walk-vector (pc-bufsw--get-walk-vector))
    (add-hook 'pre-command-hook 'pc-bufsw--switch-hook))
  (when pc-bufsw--walk-vector
    (let ((prev-index pc-bufsw--cur-index))
      (pc-bufsw--choose-next-index direction)
      (when (not (= pc-bufsw--cur-index prev-index))
	(switch-to-buffer (pc-bufsw--get-buf pc-bufsw--cur-index) t))
      (pc-bufsw--show-buffers-names)
      (when (sit-for pc-bufsw-quit-time)
	(pc-bufsw--finish)))))

(defun pc-bufsw--can-start ()
  (not (window-minibuffer-p (selected-window))))

(defun pc-bufsw--get-buffer-display-time (buffer)
  (with-current-buffer buffer
    buffer-display-time))

(defun pc-bufsw--set-buffer-display-time (buffer time)
  (with-current-buffer buffer
    (setq buffer-display-time time)))

(defun pc-bufsw--switch-hook ()
  ;; Hook to access next input from user.
  (when (or (null pc-bufsw--walk-vector)
	    (not (or (eq 'pc-bufsw-lru this-command)
		     (eq 'pc-bufsw-mru this-command)
		     (eq 'handle-switch-frame this-command))))
    (pc-bufsw--finish)))

(defun pc-bufsw--get-walk-vector ()
  ;; Construct main buffer vector.
  (let* ((cur-buf (current-buffer))
	 (assembled (list cur-buf)))
    (mapc (lambda (buf)
	    (when (and (pc-bufsw--can-work-buffer buf)
		       (not (eq buf cur-buf)))
	      (setq assembled (cons buf assembled))))
	  pc-bufsw--start-buf-list)
    (setq assembled (nreverse assembled))
    (apply 'vector assembled)))

(defun pc-bufsw--can-work-buffer (buffer)
  ;; Return nill if buffer is not sutable for switch.
  (let ((name (buffer-name buffer)))
    (not (char-equal ?\  (aref name 0)))))

(defun pc-bufsw--show-buffers-names ()
  ;; Echo buffer list. Current buffer marked by <>.
  (let* ((width (frame-width))
	 (n (pc-bufsw--find-first-visible width))
	 (str (pc-bufsw--make-show-str n width)))
    (message "%s" str)))

(defun pc-bufsw--find-first-visible (width)
  (let ((first-visible 0)
	(i 1)
	(visible-length (pc-bufsw--show-name-len 0 t)))
    (while (<= i pc-bufsw--cur-index)
      (let ((cur-length (pc-bufsw--show-name-len i (= first-visible i))))
	(setq visible-length (+ visible-length cur-length))
	(when (> visible-length width)
	  (setq first-visible i)
	  (setq visible-length cur-length)))
      (setq i (1+ i)))
    first-visible))

(defun pc-bufsw--show-name-len (i at-left-edge)
  (+ (if at-left-edge 2 3)
     (length (buffer-name (pc-bufsw--get-buf i)))))

(defun pc-bufsw--make-show-str (first-visible width)
  (let* ((i (1+ first-visible))
	 (count (length pc-bufsw--walk-vector))
	 (str (pc-bufsw--show-name first-visible t))
	 (visible-length (length str))
	 (continue-loop (not (= i count))))
    (while continue-loop
      (let* ((name (pc-bufsw--show-name i nil))
	     (name-len (length name)))
	(setq visible-length (+ visible-length name-len))
	(if (> visible-length width)
	    (setq continue-loop nil)
	  (setq str (concat str name))
	  (setq i (1+ i))
	  (when (= i count)
	    (setq continue-loop nil)))))
    str))

(defun pc-bufsw--show-name (i at-left-edge)
  (let ((name (buffer-name (pc-bufsw--get-buf i))))
    (cond
     ((= i pc-bufsw--cur-index) (concat (if at-left-edge "<" " <") name ">"))
     (at-left-edge (concat " " name " "))
     (t (concat "  " name " ")))))

(defun pc-bufsw--choose-next-index (direction)
  (setq pc-bufsw--cur-index
	(if pc-bufsw-wrap-index
	    (mod (+ pc-bufsw--cur-index direction)
		 (length pc-bufsw--walk-vector))
	  (max 0 (min (1- (length pc-bufsw--walk-vector))
		      (+ pc-bufsw--cur-index direction))))))

(defun pc-bufsw--finish ()
  ;; Called on switch mode close.
  (pc-bufsw--restore-order (pc-bufsw--get-buf pc-bufsw--cur-index)
			   pc-bufsw--start-buf-list)
  (remove-hook 'pre-command-hook 'pc-bufsw--switch-hook)
  (setq pc-bufsw--walk-vector nil)
  (setq pc-bufsw--cur-index 0)
  (setq pc-bufsw--start-buf-list nil)
  (message nil))

(defun pc-bufsw--restore-order (chosen-buffer list)
  ;; Put buffers in Emacs buffer list according to oder indicated by
  ;; list except put chosen-buffer to the first place.
  (mapc (lambda (buf)
	  (when (not (eq buf chosen-buffer))
	    (bury-buffer buf)))
	list))

(provide 'pc-bufsw)

;;; pc-bufsw.el ends here
