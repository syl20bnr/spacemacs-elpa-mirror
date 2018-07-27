;;; quick-preview.el --- quick preview using GNOME sushi, gloobus or quick look

;; Author: myuhe <yuhei.maeda_at_gmail.com>
;; URL: https://github.com/myuhe/quick-preview.el
;; Package-Version: 20150829.439
;; Version: 0.1
;; Maintainer: myuhe
;; Copyright (C) :2015 myuhe all rights reserved.
;; Created: :15-08-22
;; Keywords: files,hypermedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published byn
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 0:110-1301, USA.

;;; Commentary:
;;
;; Put the quick-preview.el to your
;; load-path.
;; Add to .emacs:
;; (require 'quick-preview)
;;
;;#Setting for key bindings
;; (global-set-key (kbd "C-c q") 'quick-preview-at-point)
;; (define-key dired-mode-map (kbd "Q") 'quick-preview-at-point)
;;
;;; Changelog:
;; 2015-08-22 Initial release.

;;; Code:

(require 'ffap)
(require 'dbus)

;; Customization
(defgroup quick-preview nil
  "quick preview using GNOME sushi or gloobus"
  :tag "Quick Preview"
  :group 'files)

(defcustom quick-preview-method (if (eq system-type 'darwin) 'quick-look 'sushi)
  "quick preview tool. select sushi  gloobus or quick-look"
  :group 'quick-preview
  :type `(choice  ,@(mapcar (lambda (c)
                              `(const :tag ,c ,c))
                            '('sushi 'gloobus 'quick-look))))

(defun quick-preview--sushi (filename)
  (if filename
      (if (executable-find (symbol-name quick-preview-method))
          (dbus-call-method 
           :session                         
           "org.gnome.NautilusPreviewer"    
           "/org/gnome/NautilusPreviewer"   
           "org.gnome.NautilusPreviewer"    
           "ShowFile"
           :string (concat (when (file-exists-p filename) "file://") filename)
           :int32 (string-to-number (frame-parameter nil 'window-id))
           :boolean t)
        (error (concat "command not found :"
                       (symbol-name quick-preview-method))))
    (error "file not found")))

(defun quick-preview--process (filename comm)
  (let ((process (get-process "quick-preview")))
    (if (file-exists-p filename)
        (if process
            (kill-process process)
          (eval (apply 'append
                       `((start-process "quick-preview" nil)
                         ,comm (,filename)))))
      (when process (kill-process process))
      (error "file not found"))))

(defun quick-preview--get-filename ()
  (if (eq major-mode 'dired-mode)
      (dired-get-filename)
    (or (ffap-url-at-point)
        (ffap-file-at-point))))

;;;###autoload
(defun quick-preview-at-point ()
  (interactive)
  "Preview a file at point with quick preview tool."
  (let ((filename (quick-preview--get-filename)))
    (cond
     ((eq quick-preview-method 'sushi)
      (quick-preview--sushi filename))
     ((eq quick-preview-method 'gloobus)
      (quick-preview--process filename '("gloobus-preview")))
     ((eq quick-preview-method 'quick-look)
      (quick-preview--process filename '("qlmanage" "-p"))))))

(provide 'quick-preview)
;;; quick-preview.el ends here
