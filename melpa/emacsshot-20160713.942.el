;;; emacsshot.el --- Snapshot a frame or window from within Emacs
;; #+STARTUP: oddeven

;;; Header:

;; Copyright 2014-2016 Marco Wahl

;; Author: Marco Wahl <marcowahlsoft@gmail.com>
;; Maintainer: Marco Wahl
;; Version: 0.3
;; Package-Version: 20160713.942
;; Created: 2014-01-26
;; Keywords: convenience
;; URL: https://github.com/marcowahl/emacsshot

;; This file is not part of Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Meta:

;; Create documentation by switching to the lentic Org view followed by
;; export of subtree [[id:7351e8d6-758c-4561-a938-1f9912f19f69][Documentation]].

;;; Documentation:
;;   :PROPERTIES:
;;   :ID:       7351e8d6-758c-4561-a938-1f9912f19f69
;;   :END:

;; ** What

;; =emacsshot= provides a few commands to take a screenshot of
;; Emacs from within Emacs.

;; [[./emacsshot.png]]

;; ** Usage

;; *** Quick

;; With =emacsshot= there are

;; - =M-x emacsshot-snap-frame=
;; - =M-x emacsshot-snap-window=
;; - =M-x emacsshot-snap-window-include-modeline=

;; for creating a shot of Emacs.

;; *** Hints and Detais

;; With the default settings =M-x emacsshot-snap-frame= creates file
;; '~/emacsshot.png' which is a snapshot of the current Emacs-frame
;; with all its windows.

;; There is also =M-x emacsshot-snap-window= which is for creating a
;; snapshot of the current Emacs-window (i.e. the window which contains
;; the active cursor.)

;; Further there is function =emacsshot-snap-window-include-modeline=
;; which does as =emacsshot-snap-window= but includes the modeline when
;; taking the shot.

;; The filenames are configurable.  Hint: =M-x customize-group emacsshot=.

;; It's even possible to add a timestamp to the filename as postfix.  See
;; =M-x customize-variable emacsshot-with-timestamp=.

;; It might be a good idea to bind the functions to a key.  This can
;; make the usage more convenient.  Further the binding is a way to
;; avoid images which contain the command that has been used to create
;; the image e.g. "M-x emacsshot-snap-frame" in the minibuffer.
;; Beware of the heisenshot!

;; Concretely the print-key could trigger the shot.  Evaluation of

;; #+BEGIN_EXAMPLE
;; (global-set-key [print] 'emacsshot-snap-frame)
;; #+END_EXAMPLE

;; yields this behavior.

;; Or evaluate

;; #+BEGIN_EXAMPLE
;; (global-set-key [print]
;;  (lambda (&optional current-window)
;;   (interactive "P")
;;   (if current-window (emacsshot-snap-window)
;;     (emacsshot-snap-frame))))
;; #+END_EXAMPLE

;; to be able to snap the frame by pressing the print-key and to snap the
;; current window by prefixing the keypress with C-u.

;; Note that emacsshot currently trys to overwrite any existing file with
;; the target name without asking.

;; ** Install
;; *** Emacs Package

;; When emacsshot has been installed as elpa-package
;; [[http://melpa.org/#/emacsshot][file:http://melpa.org/packages/emacsshot-badge.svg]] then the functions
;; are available without need of further action.

;; *** Direct Install

;; Activate this program by loading it into Emacs and evaluate it with
;; =M-x eval-buffer=.

;; Automatically activate this program at Emacs start by adding the lines

;; #+BEGIN_EXAMPLE
;; (add-to-list 'load-path "/...path to this program...")
;; (require 'emacsshot)
;; #+END_EXAMPLE

;; to your .emacs or whatever you use for Emacs intitialization.

;; ** Dependencies

;; - Emacs is running under X.
;; - The programm =convert= of the ImageMagick-suite is available.

;; =convert= actually creates the snapshots.

;; ** Development
;; *** Lentic Literate Style

;; This program is written in Emacs Lisp in lentic style based on the
;; 'lentic' package [[http://melpa.org/#/lentic][file:http://melpa.org/packages/lentic-badge.svg]].

;; This means the that this file can be regarded just as an Emacs Lisp
;; file.  But actually this file contains extra comments which allow the
;; interpretation of the file as Org file.  Lentic-mode makes it easy to
;; write this style.

;; A possible initialization of lentic is this:

;; #+BEGIN_EXAMPLE
;; (global-lentic-start-mode)
;; #+END_EXAMPLE

;; Find more about lentic at
;; [[http://melpa.org/#/lentic][file:http://melpa.org/packages/lentic-badge.svg]].

;; *** Ideas, Contributions, Bugs

;; Contributions, ideas and bug-reports are welcome.

;; Please use the infrastructure of github for communication.  See
;; https://github.com/marcowahl/emacsshot/issues.

;; ** Related

;; There is elpa-package 'screenshot' which allows to pick windows
;; with the mouse, even windows from non-Emacs (!) programs.  See
;; http://melpa.org/#/screenshot.  BTW 'screenshot' has even more!

;; emacsshot only takes images of Emacs.

;; ** History

;; | 201501071941 | New function to take snapshot of a window |
;; | 201505162319 | Optionally add timestamp to save-filename |

;;; Code:

;; ** Configuration

;; #+BEGIN_SRC emacs-lisp
(defcustom emacsshot-snap-frame-filename "~/emacsshot.png"
  "Filename under which to store the next frame-snap.
A timestamp may be added."
  :group 'emacsshot)

(defcustom emacsshot-snap-window-filename "~/emacsshot.png"
  "Filename under which to store the next window-snap.
A timestamp may be added."
  :group 'emacsshot)

(defcustom emacsshot-with-timestamp nil
  "When t add current timestamp to the filename."
  :group 'emacsshot)
;; #+END_SRC

;; ** Auxilliary Function

;; #+BEGIN_SRC emacs-lisp
(defun emacsshot-enhance-filename-with-timestamp (filename)
  "Append a timestamp to the given FILENAME."
  (concat
   (or (file-name-sans-extension filename) "")
   "-"
   (format-time-string "%Y%m%d%H%M%S")
   (if (file-name-extension filename)
       (concat  "." (file-name-extension filename))
     "")))
;; #+END_SRC

;; ** Snapshot Functions

;; #+BEGIN_SRC emacs-lisp
(defun emacsshot--snap-window (include-modeline)
  "Save an image of the current window.

  The image is stored with the name defined in
  `emacsshot-snap-window-filename'.  There is no check against
  overriding.
Argument INCLUDE-MODELINE t means to include, else exclude the modeline."
  (let ((filename
         (expand-file-name
          (if emacsshot-with-timestamp
              (emacsshot-enhance-filename-with-timestamp
               emacsshot-snap-window-filename)
            emacsshot-snap-window-filename))))
    (if (= 0 (call-process
              "convert"
              nil (get-buffer-create "*convert-output*") nil
              (format
               "x:%s[%dx%d+%d+%d]"
               (frame-parameter nil 'window-id)
               (window-pixel-width)
               (+ (window-body-height nil t)
                  (or (when include-modeline
                        (window-mode-line-height))
                      0))

               (nth 0 (window-pixel-edges))
               (nth 1 (window-pixel-edges)))
              filename))
        (message (concat "Written file " filename))
      (error (concat "Could not write file " filename)))))
;; #+END_SRC

;; #+BEGIN_SRC emacs-lisp
;;;###autoload
(defun emacsshot-snap-frame ()
  "Save an image of the current Emacs-frame.

  The image is stored with the name defined in
  `emacsshot-snap-frame-filename'.  There is no check against
  overriding."
  (interactive)
  (let ((filename
         (expand-file-name
          (if emacsshot-with-timestamp
              (emacsshot-enhance-filename-with-timestamp
               emacsshot-snap-frame-filename)
            emacsshot-snap-frame-filename))))
    (if (= 0 (call-process
              "convert"
              nil (get-buffer-create "*convert-output*") nil
              (format
               "x:%s"
               (frame-parameter nil 'outer-window-id))
              filename))
        (message (concat "Written file " filename))
      (error (concat "Could not write file " filename)))))

;;;###autoload
(defun emacsshot-snap-window ()
  "Save an image of the current window (without modeline.)

  The image is stored with the name defined in
  `emacsshot-snap-window-filename'.  There is no check against
  overriding."
  (interactive)
  (emacsshot--snap-window nil))

;;;###autoload
(defun emacsshot-snap-window-include-modeline ()
    "Save an image of the current window including the modeline.

  The image is stored with the name defined in
  `emacsshot-snap-window-filename'.  There is no check against
  overriding."
  (interactive)
  (emacsshot--snap-window t))

(provide 'emacsshot)

;; #+END_SRC

;;; Rest:

;; You can export section
;; [[id:7351e8d6-758c-4561-a938-1f9912f19f69][Documentation]] to get
;; documentation.  E.g. a README.md suitable for github.

;; # Local Variables:
;; # lentic-init: lentic-orgel-org-init
;; # End:

;;; emacsshot.el ends here
