;;; mu4e-conversation.el --- Show a complete thread in a single buffer -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt <ambrevar@gmail.com>

;; Author: Pierre Neidhardt <ambrevar@gmail.com>
;; Maintainer: Pierre Neidhardt <ambrevar@gmail.com>
;; URL: https://gitlab.com/Ambrevar/mu4e-conversation
;; Package-Version: 20180613.325
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: mail, convenience, mu4e

;; This file is not part of GNU Emacs.

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
;; In this file we define `mu4e-conversation' (+ helper functions), which is
;; used for viewing all e-mail messages of a thread in a single buffer.
;;
;; From the headers view, run the command `mu4e-conversation'.
;;
;; To fully replace `mu4e-view' with `mu4e-conversation' from any other command
;; (e.g. `mu4e-headers-next', `helm-mu'), call
;;
;;   (setq mu4e-view-func 'mu4e-conversation)

;;; Code:

;; TODO: Overrides are not commended.  Use unwind-protect to set handlers?  I don't think it would work.
;; TODO: Only mark visible messages as read.
;; TODO: Indent user messages?
;; TODO: Detect subject changes.
;; TODO: Check out mu4e gnus view.
;; TODO: Should we reply to the selected message or to the last?  Make it an option: 'current, 'last, 'ask.
;; TODO: Does toggle-display HTML work?

(require 'mu4e)
(require 'rx)
(require 'outline)

(defvar mu4e-conversation--thread-headers nil)
(defvar mu4e-conversation--thread nil)
(defvar mu4e-conversation--current-message nil)

(defvar mu4e-conversation-print-message-function 'mu4e-conversation-print-message-linear
  "Function that insert the formatted content of a message in the current buffer.
The argument is the message index in `mu4e-conversation--thread',
counting from 0.")

(defgroup mu4e-conversation nil
  "Settings for the mu4e conversation view."
  :group 'mu4e)

(defcustom mu4e-conversation-own-name "Me"
  "Name to display instead of your own name.
This applies to addresses matching `mu4e-user-mail-address-list'.
If nil, the name value is not substituted."
  :type 'string
  :group 'mu4e-conversation)

(defface mu4e-conversation-unread
  '((t :weight bold))
  "Face for unread messages."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-me
  '((t :inherit default))
  "Face for conversation message sent by yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-1
  `((t :foreground ,(face-foreground 'outline-1)))
  "Face for conversation message from the 1st sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-2
  `((t :foreground ,(face-foreground 'outline-2)))
  "Face for conversation message from the 2rd sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-3
  `((t :foreground ,(face-foreground 'outline-3)))
  "Face for conversation message from the 3rd sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-4
  `((t :foreground ,(face-foreground 'outline-4)))
  "Face for conversation message from the 4th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-5
  `((t :foreground ,(face-foreground 'outline-5)))
  "Face for conversation message from the 5th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-6
  `((t :foreground ,(face-foreground 'outline-6)))
  "Face for conversation message from the 6th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-7
  `((t :foreground ,(face-foreground 'outline-7)))
  "Face for conversation message from the 7th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-sender-8
  `((t :foreground ,(face-foreground 'outline-8)))
  "Face for conversation message from the 8th sender who is not yourself."
  :group 'mu4e-conversation)

(defface mu4e-conversation-header
  '((t :foreground "grey70" :background "grey25"))
  "Face for conversation message sent by someone else."
  :group 'mu4e-conversation)

(defcustom mu4e-conversation-max-colors -1
  "Max number of colors to use to colorize sender messages.
If 0, don't use colors.
If less than 0, don't limit the number of colors."
  :type 'integer
  :group 'mu4e-conversation)

(defcustom mu4e-conversation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "V") 'mu4e-conversation-toggle-view)
    (define-key map (kbd "#") 'mu4e-conversation-toggle-hide-cited)
    map)
  "Map for `mu4e-conversation'."
  :type 'key-sequence
  :group 'mu4e-conversation)

(defcustom mu4e-conversation-linear-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-p") 'mu4e-conversation-previous-message)
    (define-key map (kbd "C-c C-n") 'mu4e-conversation-next-message)
    (define-key map (kbd "q") 'mu4e~view-quit-buffer)
    map)
  "Map for `mu4e-conversation' in linear view."
  :type 'key-sequence
  :group 'mu4e-conversation)

(defcustom mu4e-conversation-tree-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'mu4e-conversation-quit)
    (define-key map (kbd "C") 'mu4e-compose-new)
    (define-key map (kbd "R") 'mu4e-compose-reply)
    (define-key map (kbd "E") 'mu4e-compose-edit)
    (define-key map (kbd "F") 'mu4e-compose-forward)
    (define-key map (kbd ".") 'mu4e-view-raw-message)
    (define-key map (kbd "A") 'mu4e-view-attachment-action)
    (define-key map (kbd "a") 'mu4e-view-action)
    (define-key map (kbd "|") 'mu4e-view-pipe)
    (define-key map (kbd "M-q") 'mu4e-view-fill-long-lines)
    map)
  "Map for `mu4e-conversation' in tree view."
  :type 'key-sequence
  :group 'mu4e-conversation)

(defun mu4e-conversation-previous-message (&optional count)
  "Go to previous message in linear view.
With numeric prefix argument or if COUNT is given, move that many
messages.  A negative COUNT goes forwards."
  (interactive "p")
  (mu4e-conversation-next-message (if count (- count) -1)))

(defun mu4e-conversation-next-message (&optional count)
  "Go to next message in linear view.
With numeric prefix argument or if COUNT is given, move that many
messages.  A negative COUNT goes backwards."
  (interactive "p")
  (when (eq major-mode 'org-mode)
    (user-error "Not in linear view."))
  (let ((move-function (if (< count 0)
                           'previous-char-property-change
                         'next-char-property-change)))
    (setq count (abs count))
    (dotimes (_ count)
      (while (and (goto-char (funcall move-function (point)))
                  (not (eq (get-text-property (point) 'face) 'mu4e-conversation-header))
                  (not (eobp)))))))

(defun mu4e-conversation-toggle-hide-cited ()
  "Toggle hiding of cited lines in the message body."
  (interactive)
  (if (and (listp buffer-invisibility-spec)
           (member '(mu4e-conversation-quote . t) buffer-invisibility-spec))
      (remove-from-invisibility-spec '(mu4e-conversation-quote . t))
    (add-to-invisibility-spec '(mu4e-conversation-quote . t)))
  (force-window-update))

(defun mu4e-conversation-quit ()
  "Quit conversation window."
  (interactive)
  (mu4e-view-mode)
  (mu4e~view-quit-buffer))

(defun mu4e-conversation-toggle-view ()
  "Switch between tree and linear view."
  (interactive)
  (mu4e-conversation-show-thread
   (if (eq major-mode 'org-mode)
       'mu4e-conversation-print-message-linear
     'mu4e-conversation-print-message-tree)))

(defun mu4e-conversation-show-thread (&optional print-function)
  "Display the thread in the `mu4e-conversation--buffer-name' buffer."
  ;; See the docstring of `mu4e-message-field-raw'.
  (switch-to-buffer (get-buffer-create mu4e~view-buffer-name))
  (let* ((current-message-pos 0)
         (index 0)
         (filter (lambda (seq) (if (eq mu4e-conversation-print-message-function 'mu4e-conversation-print-message-linear)
                                   ;; In linear view, it makes more sense to sort messages chronologically.
                                   (sort seq
                                         (lambda (msg1 msg2)
                                           (time-less-p (mu4e-message-field msg1 :date)
                                                        (mu4e-message-field msg2 :date))))
                                 seq)))
         (mu4e-conversation--thread (funcall filter mu4e-conversation--thread))
         (mu4e-conversation--thread-headers (funcall filter mu4e-conversation--thread-headers))
         (inhibit-read-only t))
    (erase-buffer)
    (delete-all-overlays)
    (dolist (msg mu4e-conversation--thread)
      (when (= (mu4e-message-field msg :docid)
               (mu4e-message-field mu4e-conversation--current-message :docid))
        (setq current-message-pos (point)))
      (funcall (or print-function
                   mu4e-conversation-print-message-function)
               index)
      (mu4e~view-show-images-maybe msg)
      (goto-char (point-max))
      (insert (propertize "\n" 'msg msg)) ; Insert a final newline after potential images.
      (mu4e~view-mark-as-read-maybe msg)
      (setq index (1+ index))
      (goto-char (point-max)))
    (goto-char current-message-pos)
    (recenter)
    (unless (eq major-mode 'org-mode)
      (mu4e~view-make-urls-clickable))     ; TODO: Don't discard sender face.
    (setq header-line-format (propertize
                              (mu4e-message-field (car mu4e-conversation--thread) :subject)
                              'face 'bold))
    (add-to-invisibility-spec '(mu4e-conversation-quote . t)))
  (read-only-mode 1))

(defun mu4e-conversation--get-message-face (index)
  "Map 'from' addresses to 'sender-N' faces in chronological
order and return corresponding face for e-mail at INDEX in
`mu4e-conversation--thread'.
E-mails whose sender is in `mu4e-user-mail-address-list' are skipped."
  (let* ((message (nth index mu4e-conversation--thread))
         (from (car (mu4e-message-field message :from)))
         ;; The e-mail address is not enough as key since automated messaging
         ;; system such as the one from github have the same address with
         ;; different names.
         (sender-key (concat (car from) (cdr from)))
         (sender-faces (make-hash-table :test 'equal))
         (face-index 1))
    (dotimes (i (1+ index))
      (let* ((msg (nth i mu4e-conversation--thread))
             (from (car (mu4e-message-field msg :from)))
             (sender-key (concat (car from) (cdr from)))
             (from-me-p (member (cdr from) mu4e-user-mail-address-list)))
        (unless (or from-me-p
                    (gethash sender-key sender-faces))
          (when (or (not (facep (intern (format "mu4e-conversation-sender-%s" face-index))))
                    (< 0 mu4e-conversation-max-colors face-index))
            (setq face-index 1))
          (puthash sender-key
                   (intern (format "mu4e-conversation-sender-%s" face-index))
                   sender-faces)
          (setq face-index (1+ face-index)))))
    (gethash sender-key sender-faces)))

(defun mu4e-conversation--from-name (message)
  "Return a string describing the sender (the 'from' field) of MESSAGE."
  (let* ((from (car (mu4e-message-field message :from)))
         (from-me-p (member (cdr from) mu4e-user-mail-address-list)))
    (if (and mu4e-conversation-own-name from-me-p)
        mu4e-conversation-own-name
      (concat (car from)
              (when (car from) " ")
              (format "<%s>" (cdr from))))))

(defun mu4e-conversation--propertize-quote (message)
  "Trim the replied-to emails quoted at the end of message."
  (with-temp-buffer
    (insert message)
    (goto-char (point-min))
    ;; Regexp seemed to be doomed to kill performance here, so we do it manually
    ;; instead.  It's not much longer anyways.
    (let (start)
      (while (not (eobp))
        (while (and (not (eobp)) (not (= (following-char) ?>)))
          (forward-line))
        (unless (eobp)
          (setq start (point))
          (while (and (not (eobp)) (= (following-char) ?>))
            (forward-line))
          (unless (eobp)
            ;; Optional gap.
            (while (and (not (eobp))
                        (string-match (rx line-start (* (any space)) line-end)
                                      (buffer-substring-no-properties
                                                  (line-beginning-position)
                                                  (line-end-position))))
              (forward-line))
            (if (or (eobp)
                    (string-match (rx line-start "--" (* (any space)) line-end)
                                  (buffer-substring-no-properties
                                                (line-beginning-position)
                                                (line-end-position))))
                ;; Found signature or end of buffer, no need to continue.
                (goto-char (point-max))
              ;; Restart the loop.
              (setq start nil)))))
      (when start
        ;; Buffer functions like (point) return 1-based indices while string
        ;; functions use 0-based indices.
        (add-text-properties (1- start) (length message)
                             '(invisible mu4e-conversation-quote) message)))))

(defun mu4e-conversation-print-message-linear (index)
  "Insert formatted message found at INDEX in `mu4e-conversation--thread'."
  (unless (eq major-mode 'mu4e-view-mode)
    (mu4e-view-mode)
    (use-local-map (make-composed-keymap (list mu4e-conversation-linear-map mu4e-conversation-map)
                                         mu4e-view-mode-map)))
  (let* ((msg (nth index mu4e-conversation--thread))
         (from (car (mu4e-message-field msg :from)))
         (from-me-p (member (cdr from) mu4e-user-mail-address-list))
         (sender-face (or (get-text-property (point) 'face)
                          (and from-me-p 'mu4e-conversation-sender-me)
                          (and (/= 0 mu4e-conversation-max-colors) (mu4e-conversation--get-message-face index))
                          'default)))
    (insert (propertize (format "%s, %s %s\n"
                                (mu4e-conversation--from-name msg)
                                (current-time-string (mu4e-message-field msg :date))
                                (mu4e-message-field msg :flags))
                        'face 'mu4e-conversation-header
                        'msg msg)
            (or (mu4e~view-construct-attachments-header msg) "") ; TODO: Append newline?
            ;; TODO: Add button to display trimmed quote of current message only.
            ;; TODO: `mu4e-compose-reply' does not work when point is at end-of-buffer.
            (let ((s (propertize (mu4e-message-body-text msg) 'msg msg)))
              (add-face-text-property 0 (length s) sender-face nil s)
              (mu4e-conversation--propertize-quote s)
              (when (memq 'unread (mu4e-message-field msg :flags))
                (add-face-text-property 0 (length s) 'mu4e-conversation-unread nil s))
              s))))

(defun mu4e-conversation-print-message-tree (index)
  "Insert formatted message found at INDEX in `mu4e-conversation--thread'."
  (unless (eq major-mode 'org-mode)
    (insert "#+SEQ_TODO: UNREAD READ\n\n") ; TODO: Is it possible to set `org-todo-keywords' locally?
    (org-mode)
    (use-local-map (make-composed-keymap (list mu4e-conversation-tree-map mu4e-conversation-map)
                                         org-mode-map)))
  (let* ((msg (nth index mu4e-conversation--thread))
         (msg-header (nth index mu4e-conversation--thread-headers))
         (from (car (mu4e-message-field msg :from)))
         (from-me-p (member (cdr from) mu4e-user-mail-address-list))
         (level (plist-get (mu4e-message-field msg-header :thread) :level))
         (org-level (make-string (1+ level) ?*)))
    (let ((header (format "%s %s%s, %s %s\n"
                          org-level
                          (if (memq 'unread (mu4e-message-field msg :flags))
                              "UNREAD "
                            "")
                          (mu4e-conversation--from-name msg)
                          (current-time-string (mu4e-message-field msg :date))
                          (mu4e-message-field msg :flags)))
          (body
           (concat
            ;; Use Org syntax for quoting.  TODO: How to nest quotes?
            (replace-regexp-in-string
             (rx line-start "--8<---------------cut here---------------start------------->8---" line-end) "#+begin_src"
             (replace-regexp-in-string
              (rx line-start "--8<---------------cut here---------------end--------------->8---" line-end) "#+end_src"
              (replace-regexp-in-string
               (rx line-start ">" (* space)) ": "
               ;; Prefix "*" at the beginning of lines with a space to prevent them
               ;; from being interpreted as Org sections.
               (replace-regexp-in-string
                (rx line-start "*") " *"
                ;; TODO: Propertize HTML links.
                (mu4e-message-body-text msg)))))
            "\n"
            (let ((attachments (mu4e~view-construct-attachments-header msg)))
              ;; TODO: Propertize attachments.
              (if attachments
                  (format "
:PROPERTIES:
:ATTACHMENTS: %s
:END:
"
                          attachments)
                "")))))
      ;; TODO: Put cited text and attachments in subsection / property?
      ;; To use mu4e-conversation-unread on body, we would need to override Org faces.
      (let ((s (concat header body)))
        (add-text-properties 0 (length s) (list 'msg msg) s)
        (insert s)))))

(defun mu4e-conversation-view-handler (msg)
  "Handler function for displaying a message."
  (push msg mu4e-conversation--thread)
  (when (= (length mu4e-conversation--thread)
           (length mu4e-conversation--thread-headers))
    (advice-remove mu4e-view-func 'mu4e-conversation-view-handler)
    ;; Headers are collected in reverse order, let's order them.
    (setq mu4e-conversation--thread-headers (nreverse mu4e-conversation--thread-headers))
    (let ((viewwin (mu4e~headers-redraw-get-view-window)))
      (unless (window-live-p viewwin)
        (mu4e-error "Cannot get a conversation window"))
      (select-window viewwin))
    (mu4e-conversation-show-thread)))

(defun mu4e-conversation-header-handler (msg)
  "Store thread messages.
The header handler is run for all messages before the found-handler.
See `mu4e~proc-filter'"
  (push msg mu4e-conversation--thread-headers))

(defun mu4e-conversation-erase-handler (&optional _msg)
  "Don't clear the header buffer when viewing.")

(defun mu4e-conversation-found-handler (_count)
  (advice-remove mu4e-header-func 'mu4e-conversation-header-handler)
  (advice-remove mu4e-erase-func 'mu4e-conversation-erase-handler)
  (advice-remove mu4e-found-func 'mu4e-conversation-found-handler)
  (setq mu4e-conversation--thread nil)
  (advice-add mu4e-view-func :override 'mu4e-conversation-view-handler)
  (dolist (msg mu4e-conversation--thread-headers)
    (let ((docid (mu4e-message-field msg :docid))
          ;; decrypt (or not), based on `mu4e-decryption-policy'.
          (decrypt
           (and (member 'encrypted (mu4e-message-field msg :flags))
                (if (eq mu4e-decryption-policy 'ask)
                    (yes-or-no-p (mu4e-format "Decrypt message?")) ; TODO: Never ask?
                  mu4e-decryption-policy))))
      (mu4e~proc-view docid mu4e-view-show-images decrypt))))

;;;###autoload
(defun mu4e-conversation (&optional msg)
  (interactive)
  (setq mu4e-conversation--current-message (or msg (mu4e-message-at-point)))
  (unless mu4e-conversation--current-message
    (mu4e-warn "No message at point"))
  (setq mu4e-conversation--thread-headers nil)
  (advice-add mu4e-header-func :override 'mu4e-conversation-header-handler)
  (advice-add mu4e-erase-func :override 'mu4e-conversation-erase-handler)
  (advice-add mu4e-found-func :override 'mu4e-conversation-found-handler)
  (mu4e~proc-find
   (funcall mu4e-query-rewrite-function
            (format "msgid:%s" (mu4e-message-field
                                mu4e-conversation--current-message
                                :message-id)))
   'show-threads
   :date
   'ascending
   (not 'limited)
   'skip-duplicates
   'include-related))

(provide 'mu4e-conversation)
;;; mu4e-conversation.el ends here
