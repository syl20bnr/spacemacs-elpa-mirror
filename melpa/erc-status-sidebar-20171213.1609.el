;;; erc-status-sidebar.el --- a hexchat-like activity overview for ERC channels

;; Author: Andrew Barbarello
;; Version: 0.1
;; Package-Version: 20171213.1609
;; Package-Requires: ((emacs "24.5") (seq "2.3"))
;; URL: https://github.com/drewbarbs/erc-status-sidebar

;; This file is not part of GNU Emacs.

;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:
;;
;; This package is provides a hexchat-like status bar for joined
;; channels in ERC.  It relies on the `erc-track' module, and displays
;; all the same information erc-track does in the mode line, but in an
;; alternative format.
;;
;; Credit to sidebar.el (https://github.com/sebastiencs/sidebar.el)
;; and outline-toc.el (https://github.com/abingham/outline-toc.el),
;; from which all the sidebar window management ideas were lifted.
;;
;; # Setup
;;
;; To open the ERC status sidebar in the current frame:
;;
;; M-x erc-status-sidebar-open
;;
;; Ensure the `erc-track' module is active (a member of
;; `erc-modules'). This is the default.
;;
;; To close the sidebar on the current frame:
;;
;; M-x erc-status-sidebar-close
;;
;; Use a prefix argument to close the sidebar on all frames.
;;
;; To kill the sidebar buffer and close the sidebar on all frames:
;;
;; M-x erc-status-sidebar-kill

;;; Code:

(require 'erc)
(require 'erc-track)
(require 'fringe)
(require 'seq)

(defgroup erc-status-sidebar nil
  "A sidebar for ERC channel status."
  :group 'convenience)

(defcustom erc-status-sidebar-buffer-name "*ERC Status*"
  "Name of the sidebar buffer."
  :type 'string
  :group 'erc-status-sidebar)

(defcustom erc-status-sidebar-mode-line-format "ERC Status"
  "Mode line format for the status sidebar."
  :type 'string
  :group 'erc-status-sidebar)

(defcustom erc-status-sidebar-header-line-format nil
  "Header line format for the status sidebar."
  :type 'string
  :group 'erc-status-sidebar)

(defcustom erc-status-sidebar-width 15
  "Default width of the sidebar (in columns)."
  :type 'number
  :group 'erc-status-sidebar)

(defcustom erc-status-sidebar-channel-sort 'erc-status-sidebar-default-chansort
  "Sorting function used to determine order of channels in the sidebar."
  :type 'function
  :group 'erc-status-sidebar)

(defcustom erc-status-sidebar-channel-format 'erc-status-sidebar-default-chan-format
  "Function used to format channel names for display in the sidebar."
  :type 'function
  :group 'erc-status-sidebar)

(defun erc-status-sidebar-display-window ()
  "Display the status buffer in a side window.  Return the new window."
  (display-buffer (erc-status-sidebar-get-buffer)
                  `(display-buffer-in-side-window . ((side . left) (window-width . ,erc-status-sidebar-width)))))

(defun erc-status-sidebar-get-window (&optional no-creation)
  "Return the created/existing window displaying the status buffer.

If NO-CREATION is non-nil, the window is not created."
  (let ((sidebar-window (get-buffer-window erc-status-sidebar-buffer-name)))
    (unless (or sidebar-window no-creation)
      (with-current-buffer (erc-status-sidebar-get-buffer)
        (setq-local vertical-scroll-bar nil))
      (setq sidebar-window (erc-status-sidebar-display-window))
      (set-window-dedicated-p sidebar-window t)
      (set-window-parameter sidebar-window 'no-delete-other-windows t)
      ;; don't cycle to this window with `other-window'
      (set-window-parameter sidebar-window 'no-other-window t)
      (internal-show-cursor sidebar-window nil)
      (set-window-fringes sidebar-window 0 0)
      ;; set a custom display table so the window doesn't show a
      ;; truncation symbol when a channel name is too big
      (let ((dt (make-display-table)))
        (set-window-display-table sidebar-window dt)
        (set-display-table-slot dt 'truncation ?\ )))
    sidebar-window))

(defun erc-status-sidebar-buffer-exists-p ()
  "Check if the sidebar buffer exists."
  (get-buffer erc-status-sidebar-buffer-name))

(defun erc-status-sidebar-get-buffer ()
  "Return the sidebar buffer, creating it if it doesn't exist."
  (get-buffer-create erc-status-sidebar-buffer-name))

(defun erc-status-sidebar-close (&optional all-frames)
  "Close the sidebar.

If called with prefix argument (ALL-FRAMES non-nil), the sidebar
will be closed on all frames.

The erc-status-sidebar buffer is left alone, but the window
containing it on the current frame is closed.  See
`erc-status-sidebar-kill'"
  (interactive "P")
  (mapcar #'delete-window
          (get-buffer-window-list (erc-status-sidebar-get-buffer)
                                  nil (if all-frames t))))

(defmacro erc-status-sidebar-writable (&rest body)
  "Make the status buffer writable while executing BODY."
  `(let ((buffer-read-only nil))
     ,@body))

;;;###autoload
(defun erc-status-sidebar-open ()
  "Open or create a sidebar."
  (interactive)
  (save-excursion
    (let ((sidebar-exists (erc-status-sidebar-buffer-exists-p))
          (sidebar-buffer (erc-status-sidebar-get-buffer))
          (sidebar-window (erc-status-sidebar-get-window)))
      (unless sidebar-exists
        (with-current-buffer sidebar-buffer
          (erc-status-sidebar-mode)
          (erc-status-sidebar-refresh))))))

;;;###autoload
(defun erc-status-sidebar-toggle ()
  "Toggle the sidebar open/closed on the current frame."
  (interactive)
  (if (get-buffer-window erc-status-sidebar-buffer-name nil)
      (erc-status-sidebar-close)
    (erc-status-sidebar-open)))

(defun erc-status-sidebar-get-channame (buffer)
  "Return name of BUFFER with all leading \"#\" characters removed."
  (let ((s (buffer-name buffer)))
    (if (string-match "^#\\{1,2\\}" s)
        (setq s (replace-match "" t t s)))
    (downcase s)))

(defun erc-status-sidebar-default-chansort (chanlist)
  "Sort CHANLIST case-insensitively for display in the sidebar."
  (sort chanlist (lambda (x y)
                   (string< (erc-status-sidebar-get-channame x)
                            (erc-status-sidebar-get-channame y)))))

(defun erc-status-sidebar-default-chan-format (channame
                                               &optional num-messages erc-face)
  "Format CHANNAME for display in the sidebar.

If NUM-MESSAGES is non-nil, append it to the channel name.  If
ERC-FACE is non-nil, apply it to channel name.  If it is equal to
`erc-default-face', also apply bold property to make the channel
name stand out."
  (when num-messages
    (setq channame (format "%s [%d]" channame num-messages)))
  (when erc-face
    (put-text-property 0 (length channame) 'face erc-face channame)
    (when (eq erc-face 'erc-default-face)
      (add-face-text-property 0 (length channame) 'bold t channame)))
  channame)

(defun erc-status-sidebar-refresh ()
  "Update the content of the sidebar."
  (interactive)
  (let ((chanlist (apply erc-status-sidebar-channel-sort
                         (erc-channel-list nil) nil)))
    (with-current-buffer (erc-status-sidebar-get-buffer)
      (erc-status-sidebar-writable
       (delete-region (point-min) (point-max))
       (goto-char (point-min))
       (dolist (chanbuf chanlist)
         (let* ((tup (seq-find (lambda (tup) (eq (car tup) chanbuf))
                               erc-modified-channels-alist))
                (count (if tup (cadr tup)))
                (face (if tup (cddr tup)))
                (channame (apply erc-status-sidebar-channel-format
                                 (buffer-name chanbuf) count face nil))
                (cnlen (length channame)))
           (put-text-property 0 cnlen 'erc-buf chanbuf channame)
           (put-text-property 0 cnlen 'mouse-face 'highlight channame)
           (put-text-property 0 cnlen 'help-echo
                              "mouse-1: switch to buffer in other window" channame)
           (insert channame "\n")))))))

(defun erc-status-sidebar-kill ()
  "Close the ERC status sidebar and its buffer."
  (interactive)
  (ignore-errors (kill-buffer erc-status-sidebar-buffer-name)))

(defun erc-status-sidebar-click (event)
  "Handle click EVENT in `erc-status-sidebar-mode-map'."
  (interactive "e")
  (save-excursion
    (let ((window (posn-window (event-end event)))
          (pos (posn-point (event-end event))))
      (set-buffer (window-buffer window))
      (let ((buf (get-text-property pos 'erc-buf)))
        (when buf
          (select-window window)
          (switch-to-buffer-other-window buf))))))

(defvar erc-status-sidebar-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map [mouse-1] #'erc-status-sidebar-click)
    map))

(defvar erc-status-sidebar-refresh-triggers
  '(erc-track-list-changed-hook
    erc-join-hook
    erc-part-hook
    erc-kill-buffer-hook
    erc-kill-channel-hook
    erc-kill-server-hook
    erc-kick-hook
    erc-disconnected-hook
    erc-quit-hook))

(defun erc-status-sidebar--post-refresh (&rest ignore)
  "Schedule sidebar refresh for execution after command stack is cleared.

Ignore arguments in IGNORE, allowing this function to be added to
hooks that invoke it with arguments."
  (run-at-time 0 nil #'erc-status-sidebar-refresh))

(defun erc-status-sidebar-mode--unhook ()
  "Remove hooks installed by `erc-status-sidebar-mode'."
  (dolist (hk erc-status-sidebar-refresh-triggers)
    (remove-hook hk #'erc-status-sidebar--post-refresh))
  (remove-hook 'window-configuration-change-hook
               #'erc-status-sidebar-set-window-preserve-size))

(defun erc-status-sidebar-set-window-preserve-size ()
  "Tell Emacs to preserve the current height/width of the ERC statusbar window.

Note that preserve status needs to be reset when the window is
manually resized, so `erc-status-sidebar-mode' adds this function
to the `window-configuration-change-hook'"
  (when (and (eq (selected-window) (erc-status-sidebar-get-window))
             (fboundp 'window-preserve-size))
    (unless (eq (window-total-width) (window-min-size nil t))
      (apply 'window-preserve-size (selected-window) t t nil))))

(define-derived-mode erc-status-sidebar-mode special-mode "ERC Sidebar"
  "Major mode for ERC status sidebar"
  (use-local-map erc-status-sidebar-mode-map)
  ;; Don't scroll the buffer horizontally, if a channel name is
  ;; obscured then the window can be resized
  (setq-local auto-hscroll-mode nil)
  ;; Don't want Malabarba's beacon mode blinking in the sidebar buffer
  (if (boundp 'beacon-mode)
      (setq-local beacon-mode nil))
  (setq cursor-type nil
	buffer-read-only t
	mode-line-format erc-status-sidebar-mode-line-format
	header-line-format erc-status-sidebar-header-line-format)
  (erc-status-sidebar-set-window-preserve-size)

  (add-hook 'window-configuration-change-hook
            #'erc-status-sidebar-set-window-preserve-size nil t)
  (dolist (hk erc-status-sidebar-refresh-triggers)
    (add-hook hk #'erc-status-sidebar--post-refresh))

  ;; `change-major-mode-hook' is run *before* the
  ;; erc-status-sidebar-mode initialization code, so it won't undo the
  ;; add-hook's we did in the previous expressions
  (add-hook 'change-major-mode-hook #'erc-status-sidebar-mode--unhook nil t)
  (add-hook 'kill-buffer-hook #'erc-status-sidebar-mode--unhook nil t)
  :group 'erc-status-sidebar)

(provide 'erc-status-sidebar)
;;; erc-status-sidebar.el ends here
