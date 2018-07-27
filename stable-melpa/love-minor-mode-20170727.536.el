;;; love-minor-mode.el --- Minor mode for working on LÖVE projects
;;
;; Copyright 2012--2017 Eric James Michael Ritz
;;
;; Author: Eric James Michael Ritz
;; URL: https://github.com/ejmr/love-minor-mode
;; Package-Version: 20170727.536
;; Version: 1.2
;; Package-Requires: ((lua-mode "20130419"))
;;
;;
;;
;;; License:
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;;; Commentary:
;;
;; This project adds a minor mode for GNU Emacs that adds tools to help
;; developing games using the `LÖVE' engine. This minor mode works in
;; conjunction with and requires `lua-mode'.
;;
;; Usage:
;;
;; Put this file in your Emacs lisp path (i.e. site-lisp) and add
;; this to your `.emacs' file:
;;
;;     (require 'love-minor-mode)
;;
;; If you are working on a LÖVE project then you can enable the minor
;; mode with the command (love-minor-mode t).  Emacs will activate
;; the minor mode automatically if you visit a Lua buffer that
;; contains any built-in LÖVE names.
;;
;; See the file 'README.markdown' for a description of the commands
;; that LÖVE minor mode provides.  If you do not have the file
;; available then you can see the key-bindings and their commands by
;; entering 'C-h f love-minor-mode'.
;;
;;
;;; Code:

(require 'lua-mode)

(defconst love-minor-mode-version-number "1.2"
  "The version number of the LÖVE minor mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create the keymap.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar love-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o p") 'love/create-project-configuration)
    (define-key map (kbd "C-c C-o f") 'love/search-forums)
    (define-key map (kbd "C-c C-o d") 'love/browse-documentation)
    (define-key map (kbd "M-p") 'love/play)
    (define-key map [menu-bar] (make-sparse-keymap))
    (define-key map [menu-bar love]
      (cons "LÖVE" (make-sparse-keymap "LÖVE")))
    (define-key map [menu-bar love play]
      '("Playtest" . love/play))
    (define-key map [menu-bar love --] '("--" . nil))
    (define-key map [menu-bar love browse-documentation]
      '("Browse Documentation" . love/browse-documentation))
    (define-key map [menu-bar love create-project]
      '("Create Project" . love/create-project-configuration))
    (define-key map [menu-bar love search-forums]
      '("Search Forums" . love/search-forums))
    map)
  "A keymap for LÖVE minor mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Define a customize group for LÖVE and the minor mode itself.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup love nil
  "The customization group for LÖVE minor mode."
  :prefix "love-"
  :group 'lua)

;;;###autoload
(define-minor-mode love-minor-mode
  "Toggles LÖVE minor mode.

\\{love-minor-mode-map}"
  :init-value nil
  :lighter " LÖVE"
  :group 'love
  :keymap love-minor-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Automatically enable LÖVE minor mode if the current buffer
;;; contains any of the built-in LÖVE callback functions or modules.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst love/built-in-names
  (regexp-opt
   ;; Built-in Callbacks
   '("love.draw"
     "love.focus"
     "love.joystickpressed"
     "love.joystickreleased"
     "love.keypressed"
     "love.keyreleased"
     "love.load"
     "love.mousepressed"
     "love.mousereleased"
     "love.quit"
     "love.run"
     "love.update"

     ;; Standard Modules
     "love.audio"
     "love.event"
     "love.filesystem"
     "love.font"
     "love.graphics"
     "love.image"
     "love.joystick"
     "love.keyboard"
     "love.mouse"
     "love.physics"
     "love.sound"
     "love.thread"
     "love.timer"))
  "A regular expression matching built-in LÖVE callback functions
and standard modules.")

;;;###autoload
(defun love/possibly-enable-mode ()
  "This function determines whether or not to automatically
enable `love-minor-mode'.  If the current buffer contains any
LÖVE-specific functions then we enable the minor mode."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward love/built-in-names nil t)
        (love-minor-mode t))))

;;;###autoload
(progn
  (add-hook 'lua-mode-hook 'love/possibly-enable-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This functionality helps to create a new LÖVE project.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun love/create-project-configuration (directory name identity)
  "This function creates a `conf.lua' file in a given directory.
It automatically fills the file with the love.conf() function and
sets the name and identity of the game."
  (interactive "DDirectory: \nsName: \nsIdentity: ")
  (let* ((directory (concat directory "/"))
         (conf-buffer
          (find-file-noselect
           (concat (file-name-directory directory) "conf.lua"))))
    (with-current-buffer conf-buffer
      ;; This format call is a little messy for the sake of
      ;; indentation, even though after the insertion we indent
      ;; everything according to the user's preferences.
      (insert (format "function love.conf(settings)
    settings.title = \"%s\"
    settings.identity = \"%s\"
end\n" name identity))
      (indent-region (point-min) (point-max))
      (save-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Provide commands for browsing documentation like the official wiki
;;; and any copy of local documentation the user may have.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom love-wiki-url "http://love2d.org/wiki/Main_Page"
  "URL for the official LÖVE wiki."
  :type 'string
  :group 'love)

(defcustom love-local-documentation-path ""
  "A path to a local copy of the LÖVE documentation, which is
available for download from the official LÖVE wiki.  This path
should point to the index.html file inside that official
documentation package."
  :type 'string
  :group 'love)

(defun love/browse-documentation ()
  "This function opens up the browser with LÖVE documentation.
If a path to local documentation is available then we use that.
Otherwise we open the browser to the online wiki."
  (interactive)
  (if (string= love-local-documentation-path "")
      (browse-url love-wiki-url)
    (browse-url love-local-documentation-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Provides a command for searching the official forums.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom love-forum-url "https://love2d.org/forums/"
  "URL for the official LÖVE forums."
  :type 'string
  :group 'love)

(defun love/search-forums (terms)
  "Searchs the official LÖVE forums for the given `terms' and
opens the results in the user's web browser."
  (interactive "sSearch For: ")
  (let* ((search-terms (replace-regexp-in-string "\\s-+" "+" terms))
         (search-url (format
                      (concat "%ssearch.php?keywords=%s"
                              "&terms=all&author=&sc=1&sf=all&sr=posts&sk=t&"
                              "sd=d&st=0&ch=300&t=0&submit=Search")
                      love-forum-url
                      search-terms)))
    (browse-url search-url)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Playtesting.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom love-exe (or (executable-find "love")
			"c:/programs/love-0.10.2-win32/love.exe")
  "Path to LÖVE executable for playtesting."
  :type 'string
  :group 'love)

(defun love/play ()
  "Run LÖVE externally for the sake of playtesting."
  (interactive)
  (message love-exe)
  (call-process love-exe nil "*love-output*" t 
		(file-name-directory (buffer-file-name))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; This section contains any wrap-up or clean-up code in the package
;;; before providing it for use.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'love-minor-mode)

;;; love-minor-mode.el ends here
