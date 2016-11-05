;;; tile.el --- Tile windows with layouts -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Ivan Malison

;; Author: Ivan Malison <IvanMalison@gmail.com>
;; Keywords: tile tiling window manager dynamic
;; Package-Version: 20161104.1737
;; URL: https://github.com/IvanMalison/tile
;; Package-Requires: ((emacs "25.1") (s "1.9.0") (dash "2.12.0") (stream "2.2.3"))
;; Version: 0.0.0

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

;; tile provides a way to interactively cycle through pre-defined custom layout
;; definitions à la XMonad.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'eieio)
(require 's)
(require 'stream)


;; General functions

(defun tile-buffer-filter (buffer)
  (not (s-matches? (buffer-name buffer) "\*Minibuf-?[0-9]*\*")))

(defvar tile-buffer-filter 'tile-buffer-filter)

(cl-defun tile-get-buffers
    (target-num-buffers &optional (buffer-filter tile-buffer-filter))
  (setq target-num-buffers (or target-num-buffers 1))
  (let* ((visible-buffers (mapcar 'window-buffer (window-list nil -1 nil)))
         (the-stream (stream-append
                      (stream visible-buffers)
                      (seq-filter (lambda (x)
                                    (not (memq x visible-buffers)))
                                  (stream (buffer-list)))
                      (stream-iterate-function 'identity (current-buffer)))))
    (seq-into-sequence
     (seq-take (seq-filter buffer-filter the-stream) target-num-buffers))))

(defun tile-split-evenly (split-function buffers)
  (when buffers
    (set-window-buffer nil (car buffers))
    (cl-loop for buffer in (cdr buffers)
             do
             (progn
               (funcall split-function)
               (other-window 1)
               (set-window-buffer nil buffer)))
    (balance-windows)
    (other-window 1)))


;; Buffer fetchers

(defclass tile-buffer-fetcher nil
  ((layout :initarg :layout)
   (name :initarg :name :initform nil)))

(cl-defmethod tile-execute ((strategy tile-buffer-fetcher) target-num-buffers)
  (let ((layout (oref strategy layout))
        (buffers (tile-strategy-get-buffers strategy target-num-buffers)))
    ;; This isn't the greatest place for this... but it needs to happen after
    ;; the buffers are obtained.
    (delete-other-windows)
    (if (functionp layout)
        (funcall layout buffers)
      (tile-do-layout layout buffers))))

(defun tile-has-class (obj)
  (condition-case _
      (progn (eieio-object-class obj)
             t)
    ('error nil)))

(cl-defmethod tile-get-name ((strategy tile-buffer-fetcher))
  (with-slots (name layout) strategy
      (or name
          (replace-regexp-in-string
           "tile-" ""
           (format "%s-%s"
                   (if (tile-has-class layout)
                       (eieio-object-class layout)
                     layout)
                   (tile-fetcher-name strategy))))))

(cl-defmethod tile-fetcher-name ((strategy tile-buffer-fetcher))
  (symbol-name (eieio-object-class strategy)))

(defclass tile-argument-buffer-fetcher (tile-buffer-fetcher) nil)

(cl-defmethod tile-strategy-get-buffers
  ((_strategy tile-argument-buffer-fetcher) target-num-buffers)
  (tile-get-buffers target-num-buffers))

(cl-defmethod tile-fetcher-name ((_ tile-argument-buffer-fetcher))
  "args")

(defclass tile-n-buffer-fetcher (tile-buffer-fetcher)
  ((n :initarg :n)))

(cl-defmethod tile-fetcher-name ((strategy tile-n-buffer-fetcher))
  (format "%s" (oref strategy n)))

(cl-defmethod tile-strategy-get-buffers ((strategy tile-n-buffer-fetcher) _)
  (tile-get-buffers (oref strategy n)))


;; Layout classes

(defalias 'tile-wide (-partial 'tile-split-evenly 'split-window-vertically))
(defalias 'tile-tall (-partial 'tile-split-evenly 'split-window-horizontally))

(defclass tile-master-layout nil
  ((master-fn :initarg :master-fn)
   (other-fn :initarg :other-fn)))

(cl-defmethod tile-do-layout ((strategy tile-master-layout) buffers)
  (set-window-buffer nil (car buffers))
  (with-slots (master-fn other-fn) strategy
    ;; NOTE: Master function should execute a split, and put focus in the window
    ;; that should be used for NON-MASTER tiling.
    (funcall master-fn)
    (tile-split-evenly other-fn (cdr buffers))))


;; Default layouts and convenience functions

(defvar tile-master-left
  (tile-master-layout
   :master-fn (lambda () (split-window-horizontally) (other-window 1))
   :other-fn 'split-window-vertically))

(defvar tile-master-right
  (tile-master-layout
   :master-fn 'split-window-horizontally
   :other-fn 'split-window-vertically))

(defvar tile-master-top
  (tile-master-layout
   :master-fn (lambda () (split-window-vertically) (other-window 1))
   :other-fn 'split-window-horizontally))

(defvar tile-master-bottom
  (tile-master-layout
   :master-fn 'split-window-vertically
   :other-fn 'split-window-horizontally))

(defun tile-split-n-tall (n)
  (tile-n-buffer-fetcher :n n :layout 'tile-tall))

(defun tile-split-n-wide (n)
  (tile-n-buffer-fetcher :n n :layout 'tile-wide))


;; Strategy cycler

(defclass tile-strategy-cycler ()
  ((current-strategy :initform nil)))

(cl-defmethod tile-get-next-strategy
    ((cycler tile-strategy-cycler)
     &optional (current-strategy (or (oref cycler current-strategy)
                                     (car (last (tile-get-strategies cycler))))))
  (let* ((strategies (tile-get-strategies cycler))
         (current-index (--find-index (equal current-strategy it) strategies)))
    (if current-index
        (nth (mod (1+ current-index) (length strategies)) strategies)
      (car strategies))))

(cl-defmethod tile-get-candidates ((cycler tile-strategy-cycler))
  (cl-loop for strategy in (tile-get-strategies cycler)
           collect (cons (tile-get-name strategy) strategy)))

(defclass tile-strategies (tile-strategy-cycler)
  ((strategies :initarg :strategies)))

(cl-defmethod tile-get-strategies ((cycler tile-strategies))
  (oref cycler strategies))


;; Customize support

(defgroup tile ()
  "Tile windows à la XMonad."
  :group 'layout
  :prefix "tile-")

(eval-and-compile
  (defun tile-maybe-symbol-name (arg)
    (if (symbolp arg)
        (symbol-name arg)
      arg))

  (defun tile-concat-symbols (&rest args)
    (intern (mapconcat 'tile-maybe-symbol-name args ""))))

(defvar tile-customize-strategies nil)

(cl-defmacro tile-defstrategy (name initform &key (enabled t) docstring inhibit-naming)
  (let* ((strategy-name (tile-concat-symbols 'tile- name))
         (custom-name (tile-concat-symbols 'tile-enable- name))
         (docstring (or docstring (format "Enable the `%s' strategy." name)))
         (initform (if (and (not inhibit-naming) (listp initform))
                       (append initform (list :name (symbol-name name)))
                     initform)))
    `(progn
       (defvar ,strategy-name ,initform)
       (defcustom ,custom-name ,enabled
         ,docstring
         :type '(boolean)
         :group 'tile)
       (push (quote ,name) tile-customize-strategies))))

(put 'tile-defstrategy 'lisp-indent-function 1)

(tile-defstrategy wide
  (tile-argument-buffer-fetcher :layout 'tile-wide))

(tile-defstrategy tall
  (tile-argument-buffer-fetcher :layout 'tile-tall))

(tile-defstrategy wide-3
  (tile-n-buffer-fetcher :n 3 :layout 'tile-wide)
  :enabled nil)

(tile-defstrategy tall-3
  (tile-n-buffer-fetcher :n 3 :layout 'tile-tall)
  :enabled nil)

(tile-defstrategy master-left-3
  (tile-n-buffer-fetcher :n 3 :layout tile-master-left))

(tile-defstrategy master-right-3
  (tile-n-buffer-fetcher :n 3 :layout tile-master-right)
  :enabled nil)

(tile-defstrategy master-top-3
  (tile-n-buffer-fetcher :n 3 :layout tile-master-top))

(tile-defstrategy master-bottom-3
  (tile-n-buffer-fetcher :n 3 :layout tile-master-bottom)
  :enabled nil)

(tile-defstrategy one
  (tile-argument-buffer-fetcher :layout 'identity))

(defclass tile-customize-strategy-cycler (tile-strategy-cycler) nil)

(cl-defmethod tile-get-strategies ((_ tile-customize-strategy-cycler))
  (cl-loop for name in tile-customize-strategies
           for enabled = (symbol-value (tile-concat-symbols 'tile-enable- name))
           for strategy-var = (tile-concat-symbols 'tile- name)
           when enabled collect (symbol-value strategy-var)))

(defcustom tile-cycler (tile-customize-strategy-cycler)
  "The object that will be used to cycle through strategies.
The customize variables that enabled and disable certain
strategies will only work if this is set to and instance of
`tile-customize-stragety-cycler'."
  :options '((tile-customize-strategy-cycler))
  :group 'tile)


;; Interactive functions

;;;###autoload
(defun tile-select ()
  "Select a tile strategy by name."
  (interactive)
  (let ((candidates (tile-get-candidates tile-cycler)))
    (tile :strategy (cdr (assoc (completing-read "Select a layout strategy: "
                                                 candidates) candidates)))))

;;;###autoload
(cl-defun tile (&key (window-count (length (window-list nil -1 nil)))
                     (cycler tile-cycler)
                     (strategy (tile-get-next-strategy cycler)))
  "Tile WINDOW-COUNT windows using STRATEGY.

STRATEGY defaults to the return value
of `(tile-get-next-strategy)' and WINDOW-COUNT defaults to the
current window count."
  (interactive)
  (tile-execute strategy window-count)
  (oset cycler current-strategy strategy))

(provide 'tile)
;;; tile.el ends here
