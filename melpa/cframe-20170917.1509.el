;;; cframe.el --- customize a frame and fast switch size and positions

;; Copyright (C) 2015 - 2017 Paul Landes

;; Version: 0.1
;; Package-Version: 20170917.1509
;; Author: Paul Landes
;; Maintainer: Paul Landes
;; Keywords: frame customize
;; URL: https://github.com/plandes/cframe
;; Package-Requires: ((emacs "25") (buffer-manage "0.6") (dash "2.13.0"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Allows for customization of Emacs frames, which include height and width of
;; new Emacs frames.  Options for new frames are those given to `make-frame`.
;; This is handy for those that rather resize your Emacs frames with a key
;; binding than using your mouse.

;; The library "learns" frame configurations, then restores them later on:

;; * Record frame positions with `M-x cframe-add-or-advance-setting`.
;; * Restore previous settings on start up with `cframe-restore`.
;; * Cycles through configuratinos with `cframe-add-or-advance-setting`.
;; * Pull up the [entries buffer] with `cframe-list`.

;; I use the following in my `~/.emacs` configuration file:

;; (require 'cframe)
;; ;; frame size settings based on screen dimentions
;; (global-set-key "\C-x9" 'cframe-restore)
;; ;; doesn't clobber anything in shell, Emacs Lisp buffers (maybe others?)
;; (global-set-key "\C-\\" 'cframe-add-or-advance-setting)

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'dash)
(require 'config-manage)

(defvar cframe-settings-restore-hooks nil
  "Functions to call with `cframae-settings-restore' is called.
When hook functions are called `setting' is bound to an instance
of `cframe-settings'.")

(defclass cframe-setting (config-entry)
  ((width :initarg :width
	  :initform 80
	  :type integer
	  :documentation "Width of the frame.")
   (height :initarg :height
	   :initform 120
	   :type integer
	   :documentation "Height of the frame.")
   (position :initarg :position
	     :initform (0 . 0)
	     :type cons
	     :documentation "Top/left position of the frame."))
  :documentation "A frame settings: size and location.")

(cl-defmethod cframe-setting-frame ((this cframe-setting))
  "Return the setting's frame."
  (selected-frame))

(cl-defmethod config-entry-description ((this cframe-setting))
  "Get the description of the configuration entry."
  (with-slots (width height) this
    (format "w: %d, h: %d" width height)))

(cl-defmethod config-entry-save ((this cframe-setting))
  "Save the current frame configuration."
  (let ((frame (cframe-setting-frame this)))
    (with-slots (width height position) this
      (setq width (frame-width frame)
	    height (frame-height frame)
	    position (frame-position)))))

(cl-defmethod config-entry-restore ((this cframe-setting))
  "Restore the frame to the current state of the setting."
  (let ((frame (cframe-setting-frame this)))
    (with-slots (name width height position) this
      (set-frame-width frame width)
      (set-frame-height frame height)
      (set-frame-position frame (car position) (cdr position)))
    (let ((setting this))
      (run-hooks 'cframe-settings-restore-hooks))))

(cl-defmethod cframe-setting-set-name ((this cframe-setting)
				       &optional new-name)
  "Set the name of the `cframe-setting'."
  (with-slots (name width) this
    (let ((new-name (or new-name
			(cond ((<= width 80) "narrow")
			      ((<= width 140) "wide")
			      (t "huge")))))
      (setq name new-name))))

(cl-defmethod object-format ((this cframe-setting))
  (with-slots (name width height position) this
    (format "%s: [top: %d, left: %d, width: %d, height: %d]"
	    name (car position) (cdr position) width height)))

(cl-defmethod initialize-instance ((this cframe-setting) &optional args)
  (config-entry-save this)
  (cframe-setting-set-name this)
  (with-slots (pslots description width height) this
    (setq pslots '(name width height position)))
  (cl-call-next-method this args))



(defun cframe-display-id ()
  "Create a key for a display."
  (cons (display-pixel-height)
	(display-pixel-width)))

(defclass cframe-display (config-manager)
  ((id :initarg :id
       :initform (cframe-display-id)
       :type cons
       :documentation "Identifies this display."))
  :documentation "Represents a monitor display.")

(cl-defmethod config-manager-entry-default-name ((this cframe-display))
  (with-slots (id) this
    (format "Display (%d X %d)" (car id) (cdr id))))

(cl-defmethod config-manager-new-entry ((this cframe-display) &optional args)
  (cframe-setting))

(cl-defmethod object-format ((this cframe-display))
  (with-slots (name id entries) this
    (format "%s [%s]: %d entries"
	    name id (length entries))))

(cl-defmethod config-manager--update-entries ((this cframe-display) entries)
  (cframe-save))

(cl-defmethod initialize-instance ((this cframe-display) &optional args)
  (with-slots (pslots list-header-fields cycle-method) this
    (setq pslots (append pslots '(id))
	  cycle-method 'next
	  list-header-fields '("C" "Name" "Dimensions")))
  (cl-call-next-method this args))



(defclass cframe-manager (config-persistent config-persistable)
  ((displays :initarg :displays
	     :initform nil
	     :type list
	     :documentation "Displays that have settings."))
  :documentation "Manages displays.")

(cl-defmethod cframe-manager-display ((this cframe-manager)
				      &optional no-create-p id)
  "Get display with index ID.
If the dipslay doesn't exist create a new display if NO-CREATE-P is non-nil."
  (with-slots (displays) this
    (let* ((find-id (or id (cframe-display-id)))
	   (display (->> displays
			 (cl-remove-if (lambda (display)
					 (with-slots (id) display
					   (not (equal find-id id)))))
			 car)))
      (when (and (null display) (not no-create-p))
	(setq display (cframe-display)
	      displays (append displays (list display))))
      display)))

(cl-defmethod cframe-manager-advance-display ((this cframe-manager)
					      &optional criteria)
  "Iterate the settings for the current display and restore it.

See `config-manager-entry' for the CRITERIA parameter."
  (let ((criteria (or criteria 'cycle)))
    (-> (cframe-manager-display this)
	(config-manager-activate criteria))))

(cl-defmethod cframe-manager-reset ((this cframe-manager))
  (with-slots (displays) this
    (dolist (display displays)
      (config-manager-list-clear display))))

(cl-defmethod initialize-instance ((this cframe-manager) &optional args)
  (with-slots (pslots) this
    (setq pslots '(displays)))
  (cl-call-next-method this args))


;; funcs

(defgroup cframe nil
  "Customize a frame and fast switch size and positions."
  :group 'cframe
  :prefix "cframe-")

(defcustom cframe-persistency-file-name
  (expand-file-name "cframe" user-emacs-directory)
  "File containing the Flex compile configuration data."
  :type 'file
  :group 'cframe
  :set (lambda (sym val)
	 (set-default sym val)
	 (if (and (boundp 'the-cframe-manager)
		  the-cframe-manager)
	     (oset the-cframe-manager :file val))))

(defvar the-cframe-manager nil
  "The singleton manager instance.")

(defun the-cframe-manager ()
  "Return the configuration manager singleton."
  (or the-cframe-manager (cframe-restore)))

;;;###autoload
(defun cframe-current-setting (&optional include-display-p)
  "Get the current frame setting.

If INCLUDE-DISPLAY-P is non-nil, or provided interactively with
\\[universal-argument]]."
  (interactive "P")
  (let* ((display (-> (the-cframe-manager)
		      cframe-manager-display))
	 (setting (config-manager-current-instance display)))
    (-> (if include-display-p
	    (concat (object-format display) ", "))
	(concat (object-format setting))
	message)))

;;;###autoload
(defun cframe-add-or-advance-setting (addp)
  "Either add with ADDP the current frame setting advance the next."
  (interactive (list current-prefix-arg))
  (if addp
      (progn
	(-> (the-cframe-manager)
	    cframe-manager-display
	    config-manager-add-entry)
	(message "Added setting and saved"))
    (-> (the-cframe-manager)
	cframe-manager-advance-display))
  (cframe-current-setting))

;;;###autoload
(defun cframe-save ()
  "Save the state of all custom frame settings."
  (interactive)
  (-> (the-cframe-manager)
      config-persistable-save))

;;;###autoload
(defun cframe-restore ()
  "Restore the state of all custom frame settings."
  (interactive)
  (let* ((file (expand-file-name "cframe" user-emacs-directory))
	 (mng (if (file-exists-p file)
		  (with-temp-buffer
		    (insert-file-contents file)
		    (->> (read (buffer-string))
			 config-persistent-unpersist))
		(cframe-reset))))
    (oset mng :file file)
    (setq the-cframe-manager mng)
    (cframe-manager-advance-display mng 'first)
    mng))

;;;###autoload
(defun cframe-reset (&optional hardp)
  "Reset the state of the custom frame manager.

HARDP means to recreate the manager instance, all data is reset
\\(across all displays).  This is very destructive.

This blows away all frame settings configuration in memory.  To
wipe the state on the storage call `cframe-restore' or
`cframe-add-or-advance-setting' after calling this."
  (interactive "P")
  (when (or hardp (not the-cframe-manager))
    (setq the-cframe-manager
	  (cframe-manager :file cframe-persistency-file-name)))
  (cframe-manager-reset the-cframe-manager)
  the-cframe-manager)

;;;###autoload
(defun cframe-list ()
  "List settings for current display."
  (interactive)
  (let ((display (-> (the-cframe-manager)
		     (cframe-manager-display t))))
    (if display
	(config-manager-list-entries-buffer display)
      (error "No display entries--use `cframe-add-or-advance-setting'"))))

(provide 'cframe)

;;; cframe.el ends here
