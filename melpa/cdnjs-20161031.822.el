;;; cdnjs.el --- A front end for http://cdnjs.com

;; Copyright (C)  2014 Yasuyuki Oka <yasuyk@gmail.com>

;; Author: Yasuyuki Oka <yasuyk@gmail.com>
;; Version: 0.2.1
;; Package-Version: 20161031.822
;; URL: https://github.com/yasuyk/cdnjs.el
;; Package-Requires: ((dash "2.13.0") (deferred "0.4") (f "0.17.2") (pkg-info "0.5"))
;; Keywords: tools

;; This file is not part of GNU Emacs.

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

;; Usage:
;;
;; - M-x `cdnjs-install-gocdnjs'
;;
;;   Install `gocdnjs` command.
;;
;;     `wget` and `unzip` commands are required to use this function.
;;
;; - M-x `cdnjs-list-packages'
;;
;;   List packages that are retrieved from cdnjs.
;;
;; - M-x `cdnjs-describe-package'
;;
;;   Describe the package information.
;;
;; - M-x `cdnjs-insert-url'
;;
;;   Insert URL of a JavaScript or CSS package.
;;
;; - M-x `cdnjs-select-and-insert-url'
;;
;;   Select version and file of a JavaScript or CSS package, then insert URL.
;;
;; - M-x `cdnjs-update-package-cache'
;;
;;   Update the package cache file.
;;
;;
;; Customization:
;;
;; - `cdnjs-completing-read-function' (default `ido-completing-read')
;;
;;   Function to be called when requesting input from the user.
;;
;; - `cdnjs-gocdnjs-program' (default `~/.gocdnjs/bin/gocdnjs')
;;
;;   Name of `gocdnjs' command.
;;

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'deferred)
(require 'f)
(require 'pkg-info) ; For `pkg-info-version-info'


(defconst cdnjs--gocdnjs-name "gocdnjs")

(defconst cdnjs--gocdnjs-dir (f-expand (f-join "~/" (concat "." cdnjs--gocdnjs-name))))

(defconst cdnjs--gocdnjs-bin-dir (f-join cdnjs--gocdnjs-dir "bin"))

(defconst cdnjs--gocdnjs-default-path (f-join cdnjs--gocdnjs-bin-dir cdnjs--gocdnjs-name))

;;; Customization
(defgroup cdnjs nil
  "Interface for cdnjs.com"
  :group 'tools
  :prefix "cdnjs-")

(defcustom cdnjs-completing-read-function 'ido-completing-read
  "Function to be called when requesting input from the user."
  :group 'cdnjs
  :type '(choice (const :tag "Ido" ido-completing-read)
                 (const :tag "Plain" completing-read)
                 (function :tag "Other function")))

(defcustom cdnjs-gocdnjs-program cdnjs--gocdnjs-default-path
  "Name of `gocdnjs' command.")


;;; Version information
(defun cdnjs-version (&optional show-version)
  "Get the Cdnjs version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list t))
  (let ((version (pkg-info-version-info 'cdnjs)))
    (when show-version
      (message "cdnjs version: %s" version))
    version))

;;; Global variable
(cl-defstruct cdnjs-info
  "Structure representing cdnjs information.

Slots:
`gocdnjs-version'
  The version number of `gocdnjs'.
  See URL `https://github.com/yasuyk/gocdnjs'.

`name-max-column'
  Maximum column name of all packages.

`packages'
  The property list has `name', `version' and `description' properties.
  Thease properties ares retrieved from  `http://cdnjs.com/packages.json'."
  gocdnjs-version
  name-max-column
  packages)

(defun cdnjs-info-package-names (cdnjs-info)
  "Get all pacakge names from CDNJS-INFO."
  (-map 'cadr (cdnjs-info-packages cdnjs-info)))

(defvar cdnjs--info nil)

;; `gocdnjs' executable

(defconst cdnjs--gocdnjs-required-version "0.2.0")

(defconst cdnjs--gocdnjs-notfound-msg
  "gocdnjs not found. Install gocdnjs by M-x `cdnjs-install-gocdnjs' \
and configure `cdnjs-gocdnjs-program'.")

(defconst cdnjs--gocdnjs-update-msg-format
  "your gocdnjs is old. Update gocdnjs to latest gocdnjs by M-x `cdnjs-install-gocdnjs'.")

(defvar cdnjs--gocdnjs-version-checked nil)

(defconst cdnjs--gocdnjs-release-url
  "https://github.com/yasuyk/gocdnjs/releases/")

(defconst cdnjs--gocdnjs-download-root-url
  (format "%sdownload/v%s/"
          cdnjs--gocdnjs-release-url cdnjs--gocdnjs-required-version))

(defconst cdnjs--gocdnjs-zip-name-format
  (concat cdnjs--gocdnjs-name "_"
          cdnjs--gocdnjs-required-version "_" "%s_%s.zip"))

(defun cdnjs--executable-zip-name ()
  "Get zip file name of gocdnjs executable."
  (let ((arch (if (string-match "x86_64" system-configuration) "amd64" "386")))
    (cond ((string-match "linux" system-configuration)
           (format cdnjs--gocdnjs-zip-name-format "linux" arch))
          ((string-match "darwin" system-configuration)
           (format cdnjs--gocdnjs-zip-name-format "darwin" arch))
          ((string-match "windows" system-configuration)
           (format cdnjs--gocdnjs-zip-name-format "window" arch))
          (t (user-error "Sorry, your operating system is not supported")))))

(defun cdnjs--gocdnjs-download-url ()
  "Get download url."
  (concat cdnjs--gocdnjs-download-root-url (cdnjs--executable-zip-name)))

(defun cdnjs--check-executable (exe)
  "Check EXE exists."
  (if (executable-find exe) exe
    (user-error
     (format
      "%s is not found. Download manually gocdnjs from %s"
      exe cdnjs--gocdnjs-release-url))))

(defun cdnjs--gocdnjs-update-message (version)
  "Construct an indication to update VERSION."
  (format cdnjs--gocdnjs-update-msg-format version))

(defun cdnjs--gocdnjs-version (gocdnjs)
  "Get version of GOCDNJS."
  (cl-third
   (split-string
    (car (process-lines gocdnjs "-v")))))

(defun cdnjs--verify-gocdnjs-version ()
  "Verify version of `gocdnjs' or signal a pilot error."
  (if (executable-find cdnjs-gocdnjs-program)
      (unless cdnjs--gocdnjs-version-checked
        (let ((ver (cdnjs--gocdnjs-version cdnjs-gocdnjs-program)))
          (if (version< ver cdnjs--gocdnjs-required-version)
              (user-error (cdnjs--gocdnjs-update-message ver))
            (setq cdnjs--gocdnjs-version-checked t))))
    (user-error cdnjs--gocdnjs-notfound-msg)))


;;; cdnjs-list-mode

(defun cdnjs--cache-file-last-modified (file)
  "Get last modification time of cache FILE."
  (format-time-string "%Y-%m-%d %T" (nth 5 (file-attributes file))))

(defun cdnjs--list-column-format (name-max-column)
  "Get list format with NAME-MAX-COLUMN."
  (vector `("Package" ,name-max-column t)
          '("Version"  8 t)
          '("Description"  15 t)))

(defconst cdnjs--list-sort-key
  '("Package" . nil)
  "Sort table on this key.")

(defun cdnjs--list-describe-package-info ()
  "Return package info for the current package."
  (interactive)
  (when (eq major-mode 'cdnjs-list-mode)
    (cdnjs--describe-package (tabulated-list-get-id))))

(defun cdnjs--plist-to-tabulated-list-entry (plist)
  "Convert PLIST to tabulated-list."
  (let ((name (plist-get plist 'name))
        (version (plist-get plist 'version))
        (description (plist-get plist 'description)))
    (list name
          (apply #'vector
                 `(,(list
                     name
                     'name name
                     'action
                     'cdnjs--list-describe-package-button-action
                     'face 'link
                     'follow-link t)
                   ,version ,description)))))

(defun cdnjs--plists-to-tabulated-list-entries (plists)
  "Convert PLISTS to list of values suitable for `tabulated-list-entries'.
Use parameters from `cdnjs--list-column-format'."
  (-map 'cdnjs--plist-to-tabulated-list-entry plists))

(defun cdnjs--list-describe-package-button-action (button)
  "Trigger the package BUTTON."
  (cdnjs--describe-package (button-get button 'name)))

(defvar cdnjs-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'cdnjs--list-describe-package-info)
    map)
  "Keymap for `cdnjs-list-mode'.")

(define-derived-mode cdnjs-list-mode tabulated-list-mode "Cdnjs-List"
  "Major mode for listing packages hosted in http://cdnjs.com.

\\{cdnjs-list-mode-map}."
  (setq truncate-lines t)
  (setq tabulated-list-sort-key cdnjs--list-sort-key)
  (cdnjs--tabulated-list-entries-refresh)
  (add-hook 'tabulated-list-revert-hook 'cdnjs--list-refresh nil t)
  (tabulated-list-init-header))

(defun cdnjs--tabulated-list-entries-refresh ()
  "Refresh `tabuated-list-entries'."
  (rename-buffer (cdnjs--list-mode-buffer-name))
  (setq tabulated-list-format
        (cdnjs--list-column-format
         (cdnjs-info-name-max-column cdnjs--info)))
  (setq tabulated-list-entries
        (cdnjs--plists-to-tabulated-list-entries
         (cdnjs-info-packages cdnjs--info))))

(defun cdnjs--list-refresh ()
  "Update pacakge cache file and re-populate the `tabulated-list-entries'."
  (when (y-or-n-p "Update the package cache? ")
  (deferred:$
    (deferred:next 'cdnjs-update-package-cache)
    (deferred:nextc it 'cdnjs--tabulated-list-entries-refresh))))

(defun cdnjs--package-cache-path ()
  "Get the package cache file path."
  (car (process-lines cdnjs-gocdnjs-program "c")))

(defun cdnjs--list-mode-buffer-name ()
  "GET `cdnjs-list-mode' buffer name."
  (format
   "*Cdnjs %s*"
   (cdnjs--cache-file-last-modified (cdnjs--package-cache-path))))

(defun cdnjs--packages-alist-sort (alist)
  "Sort ALIST."
  (sort alist (lambda (a b) (string< (car a) (car b)))))

(defun cdnjs--construct-cdnjs-info (buffer)
  "Construct `cdnjs--info' with data got from BUFFER."
  (with-current-buffer buffer
    (let ((plist (read (buffer-string))))
      (setq cdnjs--info
            (make-cdnjs-info
             :gocdnjs-version
             (plist-get plist 'gocdnjs-version)
             :name-max-column
             (plist-get plist 'name-max-length)
             :packages
             (cdnjs--packages-alist-sort
              (plist-get plist 'packages))))))
  cdnjs--info)

(defun cdnjs--show-cdnjs-list (output-buffer)
  "Show `cdnjs-list-mode' with data got from OUTPUT-BUFFER."
  (let ((list-buffer (get-buffer-create (cdnjs--list-mode-buffer-name))))
    (with-current-buffer list-buffer
      (cdnjs-list-mode)
      (tabulated-list-print)
      (pop-to-buffer-same-window list-buffer))))

(defun cdnjs--fill-regioned-string (string)
  "Get fill regioned STRING."
  (with-temp-buffer
    (insert string)
    (setq fill-column 70)
    (fill-region (point-min) (point-max) 'left)
    (buffer-string)))


;; Buttons

(defun cdnjs--button-label (label)
  "Get button's LABEL."
  (if (display-graphic-p)
      label (concat "[" label "]")))

(defun cdnjs--make-button (text &rest props)
  "Make Button with the TEXT and PROPS."
  (let ((button-text (cdnjs--button-label text))
        (button-face
         (if (display-graphic-p)
             '(:box (:line-width 2 :color "dark grey")
                    :background "light grey"
                    :foreground "black")
           'link)))
    (apply 'insert-text-button
           button-text
           'face button-face
           'follow-link t
           props)))

(defun cdnjs-info-insert-url (url)
  "Make button from URL and insert it at point."
  (insert-button
   url
   'action (lambda (btn) (browse-url (button-label btn)))
   'follow-link t
   'help-echo "mouse-2, RET: Browse URL"))

(defun cdnjs--button-set-label (button label)
  "Change BUTTON's text label to LABEL.

copied from http://lists.gnu.org/archive/html/bug-gnu-emacs/2011-06/msg00474.html."
  (save-excursion
    (let ((old-start (button-start button))
          (old-end (button-end button)))
      (goto-char old-end)
      (insert label)
      (if (overlayp button)
          (move-overlay button old-end (point))
        (add-text-properties old-end (point) (text-properties-at button)))
      (delete-region old-start old-end))))

(defun cdnjs--toggle-show-hide-button (button)
  "Action of Show/hide BUTTON."
  (let ((hide (button-get button 'hide))
        (start (button-get button 'start))
        (end (button-get button 'end))
        (button (button-at (point))))
    (let ((buffer-read-only nil)
          (pos (point)))
      (button-put button 'hide (not hide))
      (cdnjs--button-set-label
       button
       (cdnjs--button-label (if hide "Show" "Hide")))
      (put-text-property start end 'invisible hide)
      (goto-char pos))))

(defun cdnjs--download-button-action (button)
  "Action of Download BUTTON."
  (let ((name (button-get button 'name))
        (version (button-get button 'version)))
    (when (y-or-n-p (format "Download %s(%s)? " name version))
      (let ((dir (f-expand (read-directory-name "Directory: "))))
        (message "Downloading...")
        (deferred:$
          (deferred:process cdnjs-gocdnjs-program "d" name "-v" version "-d" dir)
          (deferred:error it 'message)
          (deferred:nextc it 'message))))))

(defun cdnjs--info-insert-buttons (name version)
  "Insert buttons for `cdnjs-info-mode'.  NAME and VERSION are required for Installation."
  (let (pos)
    (setq pos(1+ (point)))
    (cdnjs--make-button
     "Show"
     'action 'cdnjs--toggle-show-hide-button)
    (insert " ")
    (cdnjs--make-button
     "Download"
     'action 'cdnjs--download-button-action
     'name name 'version version)
    (insert "\n") pos))

(defun cdnjs--info-buttons-set-assets-pos (button start end)
  "Set BUTTON's properties for Showing/hiding Text from START to END."
  (button-put button 'start start)
  (button-put button 'end end))

;;; cdnjs-info-mode

(defvar cdnjs-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map (kbd "TAB") 'forward-button)
    (define-key map (kbd "DEL") 'backward-button)
    map)
  "Keymap for `cdnjs-info-mode'.")

(define-derived-mode cdnjs-info-mode fundamental-mode "Cdnjs-info"
  "Major mode for displaying information about an cdnjs package.

\\{cdnjs-info-mode-map}"
  (setq buffer-read-only t))

(defun cdnjs--info-show (output-buffer)
  "Display package information INFO got from OUTPUT-BUFFER."
  (unwind-protect
      (with-current-buffer output-buffer
        (let ((string (buffer-string))
              (buf (get-buffer-create "*Cdnjs Info*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (cdnjs-info-mode)
              (cdnjs--info-write (read string))
              (pop-to-buffer-same-window buf)))))
    (kill-buffer output-buffer)))

(defun cdnjs--insert-assets (assets max)
  "Write ASSETS with MAX length left padding."
  (dolist (file assets)
    (let ((text (concat (make-string (1+ max) ? ) file "\n")))
      (put-text-property 0 (length text) 'invisible t text)
      (insert text))))

(defun cdnjs--assets-max-version-length (assets-alist)
  "GET maximum version length from ASSETS-ALIST."
  (cl-flet ((ver-len (l) (length (car l))))
    (let ((max 0))
      (dolist (assets assets-alist max)
        (when (> (ver-len assets) max)
          (setq max (ver-len assets)))))))

;; (cdnjs--assets-max-version-length '(("0" ("" ""))("1.2.2" ("" ""))))

(defun cdnjs--info-write (info)
  "Write INFO to buffer of `cdnjs-info-mode'."
  (let* ((name (plist-get info 'package))
         (version (plist-get info 'version))
         (homepage (plist-get info 'homepage))
         (description (plist-get info 'description))
         (assets-list (plist-get info 'assets)))
    (save-excursion
      (insert (format "%s\n\n" name))
      (when homepage
        (insert (propertize "HOMEPAGE\n" 'font-lock-face 'bold))
        (cdnjs-info-insert-url homepage)
        (insert "\n\n"))
      (when description
        (insert (propertize "DESCRIPTON\n" 'font-lock-face 'bold))
        (insert (format "%s\n\n"
                        (cdnjs--fill-regioned-string
                         description))))
      (insert (propertize "ASSETS\n" 'font-lock-face 'bold))
      (let ((ver-max-length (cdnjs--assets-max-version-length assets-list)))
        (cl-flet ((pad (v) (make-string (- ver-max-length (length v)) ? )))
          (dolist (alist assets-list)
            (let ((version (car alist))
                  (assets (cadr alist)))
              (insert (format "%s%s " version (pad version)))
              (let ((button (button-at (cdnjs--info-insert-buttons name version)))
                    (start (point)) end)
                (cdnjs--insert-assets assets ver-max-length)
                (cdnjs--info-buttons-set-assets-pos button start (point))))))))))

;;; Private functions

(defun cdnjs--completing-read (&rest args)
  "Call `cdnjs-completing-read-function' with ARGS."
  (apply cdnjs-completing-read-function args))

(defun cdnjs--completing-read-maybe (prompt collection)
  "Execute `cdnjs--completing-read' with PROMPT, unless size of COLLECTION is one."
  (if (eq (length collection) 1)
      (car collection)
    (cdnjs--completing-read prompt collection)))

(defun cdnjs--get-cdnjs--info ()
  "Get cdnjs infomation."
  (deferred:$
    (deferred:process-buffer cdnjs-gocdnjs-program "l" "-d" "-p")
    (deferred:error it 'message)
    (deferred:nextc it 'cdnjs--construct-cdnjs-info)))

(defun cdnjs--select-and-get-package-info (packages)
  "Select package from PACKAGES, and get the package information."
  (deferred:process-buffer cdnjs-gocdnjs-program "i" "-p"
    (cdnjs--completing-read-maybe
     "Package: "
     (cdnjs-info-package-names packages))))

(defun cdnjs--insert-url (buffer)
  "Insert URL got from BUFFER."
  (let* ((info (read (with-current-buffer buffer (buffer-string))))
         (assets (plist-get info 'assets))
         (url (cl-caadar assets)))
    (insert url)))

(defun cdnjs--select-and-insert-url (buffer)
  "Select version and URL got from BUFFER, then insert URL."
  (let* ((info (read (with-current-buffer buffer (buffer-string))))
         (assets (plist-get info 'assets))
         (versions (-map 'car assets))
         (version (cdnjs--completing-read-maybe "Version: " versions))
         (urls (cadr (assoc version assets)))
         (url (cdnjs--completing-read-maybe "Url: " urls)))
    (insert url)))

(defun cdnjs--describe-package (package)
  "Describe the PACKAGE information."
  (deferred:$
    (deferred:process-buffer cdnjs-gocdnjs-program "i" "-p" package)
    (deferred:error it 'message)
    (deferred:nextc it 'cdnjs--info-show)))

(defun cdnjs--update-package-cache ()
  "Update the package cache file."
  (deferred:$
    (deferred:process cdnjs-gocdnjs-program "update")
    (deferred:error it 'message)
    (deferred:nextc it
      (lambda ()
        (message
         (format "%s is updated."
                 (cdnjs--package-cache-path)))))))

(defun cdnjs--delete-executable-zip ()
  "Delete zip file."
  (let ((zip (f-join cdnjs--gocdnjs-bin-dir
                     (cdnjs--executable-zip-name))))
    (when (f-exists? zip) (f-delete zip t))))

;;; Public API

;;;###autoload
(defun cdnjs-install-gocdnjs ()
  "Install `gocdnjs' command to `cdnjs--gocdnjs-bin-dir'.

wget and unzip commands are required to use this function."
  (interactive)
  (cdnjs--check-executable "wget")
  (cdnjs--check-executable "unzip")
  (message "Downloading gocdnjs")
  (deferred:$
    (deferred:$
      (deferred:process "wget"
        (cdnjs--gocdnjs-download-url)
        "-nc" "-P" cdnjs--gocdnjs-bin-dir)
      (deferred:nextc it
        (lambda ()
          (message "Extracting gocdnjs zip file")
          (deferred:process "unzip"
            "-o" (f-join cdnjs--gocdnjs-bin-dir (cdnjs--executable-zip-name))
            "-d" cdnjs--gocdnjs-bin-dir)))
      (deferred:nextc it
        (lambda ()
          (message "gocdnjs has been installed!"))))
    (deferred:error it 'message)
    (deferred:nextc it 'cdnjs--delete-executable-zip)))

;;;###autoload
(defun cdnjs-list-packages ()
  "List packages that are retrieved from cdnjs.com."
  (interactive)
  (cdnjs--verify-gocdnjs-version)
  (deferred:$
    (deferred:next 'cdnjs--get-cdnjs--info)
    (deferred:nextc it 'cdnjs--show-cdnjs-list)))

;;;###autoload
(defun cdnjs-describe-package ()
  "Describe the PACKAGE information."
  (interactive)
  (cdnjs--verify-gocdnjs-version)
  (deferred:$
    (deferred:next 'cdnjs--get-cdnjs--info)
    (deferred:nextc it 'cdnjs--select-and-get-package-info)
    (deferred:error it 'message)
    (deferred:nextc it 'cdnjs--info-show)))

;;;###autoload
(defun cdnjs-insert-url ()
  "Insert URL of a JavaScript or CSS package."
  (interactive)
  (cdnjs--verify-gocdnjs-version)
  (deferred:$
    (deferred:next 'cdnjs--get-cdnjs--info)
    (deferred:nextc it 'cdnjs--select-and-get-package-info)
    (deferred:error it 'message)
    (deferred:nextc it 'cdnjs--insert-url)))

;;;###autoload
(defun cdnjs-select-and-insert-url ()
  "Select version and file of a JavaScript or CSS package, then insert URL."
  (interactive)
  (cdnjs--verify-gocdnjs-version)
  (deferred:$
    (deferred:next 'cdnjs--get-cdnjs--info)
    (deferred:nextc it 'cdnjs--select-and-get-package-info)
    (deferred:error it 'message)
    (deferred:nextc it 'cdnjs--select-and-insert-url)))

;;;###autoload
(defun cdnjs-update-package-cache ()
  "Update the package cache file."
  (interactive)
  (cdnjs--verify-gocdnjs-version)
  (cdnjs--update-package-cache))

(provide 'cdnjs)

;; Local Variables:
;; coding: utf-8
;; End:

;;; cdnjs.el ends here
