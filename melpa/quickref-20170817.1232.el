;;; quickref.el --- Display relevant notes-to-self in the echo area

;; Copyright (C) 2017  Kyle Hargraves

;; Author: Kyle Hargraves
;; URL: https://github.com/pd/quickref.el
;; Package-Version: 20170817.1232
;; Version: 0.2
;; Package-Requires: ((dash "1.0.3") (s "1.0.0"))

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; Easily create and retrieve notes-to-self, defaulting to notes regarding
;; the active modes in your current buffer.

;;; Code:

(require 'dash)
(require 's)

(defgroup quickref nil
  "Display notes-to-self in the echo area."
  :group 'help)

(defcustom quickref-command-prefix (kbd "C-c q")
  "The prefix for all quickref key commands."
  :type 'string
  :group 'quickref)

(defcustom quickref-save-file (expand-file-name "quickrefs" user-emacs-directory)
  "File in which to save your quickref definitions."
  :type 'file
  :group 'quickref)

(defcustom quickref-guess-topics-functions '(quickref-guess-topic-by-major-mode
                                             quickref-guess-topics-by-derived-mode
                                             quickref-guess-topics-by-minor-modes)
  "List of functions used to guess relevant quickref topics.

The functions are called in order, with no arguments, and should return the
name of a topic to display, a list of topic names, or nil if none."
  :type 'list
  :group 'quickref)

(defcustom quickref-show-guesses 'all
  "Controls which guesses to show when called without an explicit topic."
  :type '(choice (const :tag "Never guess" nil)
                 (symbol :tag "First guess" 'first)
                 (symbol :tag "All guesses" 'all))
  :group 'quickref)

(defcustom quickref-separator " | "
  "The separator to be placed between notes displayed in the echo area."
  :type 'string
  :group 'quickref)

(defcustom quickref-format-label-function
  (lambda (label) (propertize label 'face 'quickref-label-face))
  "Function used to format the label."
  :type 'function
  :group 'quickref)

(defcustom quickref-format-note-function
  (lambda (note) (propertize note 'face 'quickref-note-face))
  "Function used to format the note."
  :type 'function
  :group 'quickref)

(defcustom quickref-message-function 'message
  "Function used to display the quickref message."
  :type 'function
  :group 'quickref)

(defface quickref-label-face
  '((t :inherit font-lock-function-name-face))
  "Face for label name."
  :group 'quickref)

(defface quickref-note-face
  '((t :inherit default-face))
  "Face for note."
  :group 'quickref)

(defface quickref-separator-face
  '((t :inherit font-lock-comment-face))
  "Face for separator between notes."
  :group 'quickref)

(define-prefix-command 'quickref-mode-keymap)
(define-key quickref-mode-keymap (kbd "e") 'quickref-in-echo-area)
(define-key quickref-mode-keymap (kbd "w") 'quickref-in-window)
(define-key quickref-mode-keymap (kbd "0") 'quickref-dismiss-window)
(define-key quickref-mode-keymap (kbd "a") 'quickref-add-note)
(define-key quickref-mode-keymap (kbd "d") 'quickref-delete-note)
(define-key quickref-mode-keymap (kbd "v") 'quickref-describe-refs)
(define-key quickref-mode-keymap (kbd "C-s") 'quickref-write-save-file)
(define-key quickref-mode-keymap (kbd "C-l") 'quickref-load-save-file)

(defvar quickref-refs nil
  "The list of quickref topics mapped to their notes.")

(defun quickref-interactive-topics ()
  "Guess or read a topic, returning relevant topics as a list."
  (let ((guessed (quickref-guess-topics)))
    (if (or current-prefix-arg (null guessed))
        (list (quickref-read-topic))
      guessed)))

(defun quickref-guess-topics ()
  "Successively call `quickref-guess-topic-functions` and return the guessed topics as a list."
  (let ((guesses (-reject 'null (mapcar 'funcall quickref-guess-topics-functions))))
    (-distinct
     (-flatten (cond
                ((null quickref-show-guesses) nil)
                ((equal quickref-show-guesses 'first) (car guesses))
                (t guesses))))))

(defun quickref-guess-topic-by-major-mode ()
  "If the current `major-mode' is an available topic, return it."
  (and (assoc (symbol-name major-mode) quickref-refs)
       (symbol-name major-mode)))

(defun quickref-guess-topics-by-derived-mode ()
  "If the current `major-mode' is derived from any topic, return those topics."
  (let ((topics (mapcar 'car quickref-refs)))
    (--select (derived-mode-p (intern it)) topics)))

(defun quickref-guess-topics-by-minor-modes ()
  "Return the list of active minor modes which are available topics."
  (let ((active-modes (--filter (and (boundp it) (symbolp it) (symbol-value it))
                                minor-mode-list)))
    (--filter (assoc it quickref-refs)
              (mapcar 'symbol-name active-modes))))

(defun quickref-read-topic ()
  "Read a topic name."
  (let ((topic-names (mapcar 'car quickref-refs))
        (guessed (car (quickref-guess-topics))))
    (if (fboundp 'ido-completing-read)
        (ido-completing-read "Topic: " topic-names nil nil nil nil guessed)
      (completing-read "Topic: " topic-names nil nil  nil nil guessed))))

(defun quickref-read-label (&optional topic)
  "Read a label name, optionally those relating to TOPIC."
  (let ((default-labels (when topic (mapcar 'car (cdr (assoc topic quickref-refs))))))
    (if default-labels
        (if (fboundp 'ido-completing-read)
            (ido-completing-read "Label: " default-labels)
          (completing-read "Label: " default-labels))
      (read-from-minibuffer "Label: "))))

(defun quickref-read-note ()
  "Read a note."
  (read-from-minibuffer "Note: "))

(defun quickref-format (label &optional note)
  "Format a single quickref LABEL and NOTE value for display."
  (let ((label (if (consp label) (car label) label))
        (note  (if (consp label) (cdr label) note)))
    (format "%s %s"
            (funcall quickref-format-label-function label)
            (funcall quickref-format-note-function note))))

(defun quickref-notes (topic)
  "Return the notes for the given TOPIC."
  (cdr (assoc topic quickref-refs)))

(defun quickref-join-into-lines (msgs sep)
  "Joins series of strings MSGS with SEP.

Inserts a newline before any string that would cause the length of the
current line to exceed the width of the echo area."
  (let ((ea-width (1- (window-width (minibuffer-window))))
        (reduction (lambda (lines msg)
                     (let ((curline (car lines))
                           (addlen  (+ (length sep) (length msg))))
                       (cond
                        ((null lines)
                         (list msg))

                        ((> (+ (length curline) addlen) ea-width)
                         (cons msg lines))

                        (t (cons (concat curline sep msg) (cdr lines))))))))
    (s-join "\n" (nreverse (-reduce-from reduction nil msgs)))))

(defun quickref-build-message (notes)
  "Generate the full message to be displayed for NOTES."
  (quickref-join-into-lines (mapcar 'quickref-format notes)
                            (propertize quickref-separator 'face 'quickref-separator-face)))

(defun quickref-find-buffer ()
  "Find or create the *QuickRef* buffer."
  (get-buffer-create "*QuickRef*"))

(defun quickref-display-topics-in-buffer (buf topics)
  "Use buffer BUF to display the quickref notes for TOPICS."
  (with-current-buffer buf
    (delete-region (point-min) (point-max))
    (dolist (topic topics)
      (insert topic ": " (quickref-build-message (quickref-notes topic)) "\n"))))

(defun quickref-find-window ()
  "Return the window currently displaying the quickref buffer, if any."
  (let ((buf (quickref-find-buffer)))
    (--first (eq buf (window-buffer it)) (window-list))))

(defun quickref-split-window ()
  "Create a window for displaying a quickref by splitting the selected window."
  (save-excursion (split-window-below -10)))

;; Interactive
;;;###autoload
(defun quickref-in-echo-area (topics)
  "Display quickref about TOPICS in the echo area."
  (interactive (list (quickref-interactive-topics)))
  (let ((notes (-reject 'null (mapcar 'quickref-notes topics))))
    (funcall quickref-message-function "%s" (quickref-build-message (apply 'append notes)))))

;;;###autoload
(defun quickref-in-window (topics)
  "Display quickref about TOPICS in a window.

Use `quickref-dismiss-window' to hide it again."
  (interactive (list (quickref-interactive-topics)))
  (let ((buf (quickref-find-buffer))
        (win (or (quickref-find-window) (quickref-split-window))))
    (quickref-display-topics-in-buffer buf topics)
    (set-window-buffer win buf)))

(defun quickref-dismiss-window ()
  "Close the current quickref window, if any."
  (interactive)
  (let ((win (quickref-find-window)))
    (when (window-live-p win) (delete-window win))))

;;;###autoload
(defun quickref-add-note (topic label note)
  "Add quickref about TOPIC labeled LABEL with value NOTE."
  (interactive (list (quickref-read-topic)
                     (quickref-read-label)
                     (quickref-read-note)))
  (let ((entry (cons label note))
        (ref   (assoc topic quickref-refs)))
    (if ref
        (nconc (cdr ref) (list entry))
      (nconc quickref-refs (list (cons topic (list entry)))))))

;;;###autoload
(defun quickref-delete-note (topic label)
  "Delete the note about TOPIC with label LABEL."
  (interactive
   (let ((topic (quickref-read-topic)))
     (list topic (quickref-read-label topic))))
  (let ((ref (assoc topic quickref-refs)))
    (when ref (assq-delete-all label ref))))

;;;###autoload
(defun quickref-load-save-file ()
  "If `quickref-save-file' exists, set `quickref-refs' to its contents."
  (interactive)
  (when (file-exists-p quickref-save-file)
    (with-temp-buffer
      (insert-file-contents quickref-save-file)
      (setq quickref-refs (read (buffer-string))))))

;;;###autoload
(defun quickref-write-save-file ()
  "Write the pretty printed contents of `quickref-refs' to `quickref-save-file'."
  (interactive)
  (save-excursion
    (find-file quickref-save-file)
    (delete-region (point-min) (point-max))
    (insert ";; -*- mode: emacs-lisp -*-\n")
    (insert (pp-to-string quickref-refs))
    (save-buffer)
    (kill-buffer)))

;;;###autoload
(defun quickref-describe-refs ()
  "`describe-variable' `quickref-refs'."
  (interactive)
  (describe-variable 'quickref-refs))

;;;###autoload
(defun turn-on-quickref-mode ()
  "Turn on `quickref-mode'."
  (interactive)
  (quickref-mode +1))

;;;###autoload
(defun turn-off-quickref-mode ()
  "Turn off `quickref-mode'."
  (interactive)
  (quickref-mode -1))

;;;###autoload
(define-minor-mode quickref-mode
  "Quickly display notes you've made to yourself."
  :init-value nil
  :lighter " qr"
  :keymap `((,quickref-command-prefix . quickref-mode-keymap))
  (when (and quickref-mode (null quickref-refs)) (quickref-load-save-file)))

;;;###autoload
(define-globalized-minor-mode quickref-global-mode
  quickref-mode
  turn-on-quickref-mode)

(provide 'quickref)

;;; quickref.el ends here
