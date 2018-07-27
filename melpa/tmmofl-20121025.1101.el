;;; tmmofl.el --- Calls functions dependant on font lock highlighting at point
;; $Revision: 1.9 $
;; $Date: 2000/06/19 22:05:07 $

;; This file is not part of Emacs

;; Author: Phillip Lord <p.lord@hgmp.mrc.ac.uk>
;; Maintainer: Phillip Lord <p.lord@hgmp.mrc.ac.uk>
;; Keywords: minor mode, font lock, toggling.
;; Package-Version: 20121025.1101

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;; Status:
;;
;; This seems to work at the moment. It is an extension of the
;; jde-auto-abbrev.el which I wrote a while back. I wanted to use this
;; in many other modes as well, so I have written it more
;; generically. The name by the way stands for "toggle minor mode (based)
;; on font lock" or "tuh-mof-l". It has recently been re-written
;; totally. All the macros have gone west, which should make life a
;; little bit easier. Effectively its entire core has been re-written
;; and only the peripheral functions really remain the same. It seems
;; to work for me but it needs more testing...
;;

;;; Limitations:
;; 
;; 1) The code looks right, but Im not convinced that the make mode
;; local hook thing is actually working.
;; 2) At the moment using tmmofl as a minor mode will conflict with it
;; being installed as part of the normal function of a mode.
;; 3) Only works on GnuEmacs at the moment, due to the use of
;; easy-mmode. It shouldnt take to much effort to remove this requirement

;;; Commentary:
;; 
;; This code is used to run to functions depending on whether the
;; current font-lock font is at point.  As font-lock is usually
;; syntactically meaningful this means that you can for instance
;; toggle minor modes on and off depending on the current syntax.
;;
;; To install this software place this file and the tmmofl-x.el file
;; into your load path and place
;; 
;; (require 'tmmofl)
;;
;; if your .emacs.
;;
;; To switch on this minor mode use the command tmmofl-mode.  The mode
;; line will indicate that the mode is switched on.  What this actually
;; does will depend on what the main mode of the current buffer
;; is.  The default behaviour is to switch `auto-fill' mode on when
;; point is within comments, and off when its in anything else.
;;

;;; Notes for developers:
;;~/src/ht/home_website/
;; There are actually two ways to use this mode, firstly as a minor
;; mode. Default behaviour is to toggle auto-fill on and off, but you
;; might want additional behaviour. To do this you define a variable called
;; `tmmofl-MODENAME-actions' where mode name is the name for mode as
;; returned by the `major-mode' variable. This variable is as
;; follows...
;;
;;(defvar tmmofl-jde-mode-actions
;;  '(
;;    (font-lock-comment-face
;;     (lambda()
;;       (progn
;;         (abbrev-mode 0)
;;         (auto-fill-mode 1)))
;;     (lambda()
;;       (progn
;;         (abbrev-mode 1)
;;         (auto-fill-mode 0))))
;;    
;;    (font-lock-string-face
;;     (lambda()
;;       (abbrev-mode 0))
;;     (lambda()
;;       (abbrev-mode 1)))))
;;
;; This is a list each element of which is a list defining the
;; font-lock-symbol to be acted on, the on function, and the off
;; function. If tmmofl can not find this variable the default of...
;;
;;(defvar tmmofl-default-actions
;;      '(
;;        (font-lock-comment-face
;;         (lambda()
;;           (auto-fill-mode 1))
;;         (lambda()
;;           (auto-fill-mode 0)))))
;;
;; can be used instead, which toggles auto fill on and off when on of
;; off comments. There are some sample action variables defined in
;; tmmofl-x.el which you may load if you wish.
;;
;; The second way to use this mode is outside of the tmmofl minor
;; mode. For instance say you wanted emacs to display the fully
;; referenced name of a class every time you moved point on top of a
;; Type declaration in Java code. If you had a function called
;; `java-show-full-class-name' (which I dont before you ask) you might
;; want to use tmmofl to call this function. To do this you would use
;; the `tmmofl-install-for-mode' function like so...
;;
;;(tmmofl-install-for-mode
;; java-mode-hook
;; font-lock-type-face
;; (lambda()
;;   (java-show-full-class-name))
;; (lambda()
;;   ()))
;;
;; where the first argument is the install hook. This would work
;; without showing the tmmofl mode information in the mode line. I am
;; fairly sure that this should work independantely of `tmmofl-mode'.

;; The software was designed, written and tested on win 95, using
;; NTEmacs. It has since been rewritten on a Gnu/Linux system. Please
;; let me know if it works elsewhere. The current version should be
;; available at http://www.bioinf.man.ac.uk/~lord
;;

;;; Acknowledgements:
;;
;; This code has grown up over about a year. It originally started off
;; as jde-auto-abbrev. I would like to thank Joakim Verona
;; (joakim@verona.se) who sent me the code which did part of what
;; tmmofl does (toggled abbrev mode!). He used `defadvice' on
;; `put-text-property'. I got the idea for using `post-command-hook'
;; from Gerd Neugebauer's multi-mode.el.
;; Finally Stefan Monnier who gave me lots of good advice about both
;; the overall structure of the file, and some specific problems I
;; had. Thanks a lot. Much appreciated.

;; TODO
;;
;; More stuff in tmmofl-x.el, but at the moment its working quite
;; nicely.
;;

;;; History:
;;
;; $Log: tmmofl.el,v $
;; Revision 1.9  2000/06/19 22:05:07  phil
;; Total rewrite
;;
;; Revision 1.8  2000/04/11 19:15:16  phil
;; Updated documentation
;;
;; Revision 1.7  2000/01/25 14:24:18  lord
;; Now requires easy-mmode, which it needs.
;; Documentation changes
;;
;; Revision 1.6  1999-12-21 17:09:03+00  phillip2
;; Applied Eric  Ludlam's checkdoc to buffer
;;
;;

;;; Code:
(eval-when-compile (require 'cl))
(require 'cl)
(require 'font-lock)
(require 'easy-mmode)

(defvar tmmofl-default-actions
  '((font-lock-comment-face
     (lambda()
       (auto-fill-mode 1))
     (lambda()
       (auto-fill-mode 0))))
  "Standard actions when mode specific actions are not provided.")


(defvar tmmofl-actions nil
  "An list which stores the functions to be run for a given face.
Each element of the list is off form (face on-function off-function)")

(make-variable-buffer-local 'tmmofl-actions)

(defvar tmmofl-font-lock-symbols-cache nil
  "Internal cache so we know where we were.")

(make-variable-buffer-local 'tmmofl-font-lock-symbols-cache)

(defun tmmofl-post-command-hook-function()
  "Run on the post command hook"
  (interactive)
  (condition-case err
      (let ((faces-at-point (get-text-property (point) 'face)))
        ;;run the on and off functions
        (tmmofl-run-off-functions faces-at-point)
        (tmmofl-run-on-functions faces-at-point)
        ;;and remember these for next time
        (setq tmmofl-font-lock-symbols-cache faces-at-point))
    (error
     ;;if there is a problem in the called functions show the error message
     ;;and then bomb out. If we dont do this emacs will empty post-command-hook for
     ;;us silently which makes things difficult to debug, and may also
     ;;other packages which use post-command-hook.
     (progn (message "Error caught by tmmofl: %s" (error-message-string err))
            (remove-hook 'post-command-hook 'tmmofl-post-command-hook-function)))))

(defun tmmofl-run-on-functions-for-face ( current-face )
  "Run the on functions defined for CURRENT-FACE."
  (interactive)
  (dolist (face-and-action tmmofl-actions)
    (if (eq (car face-and-action) current-face)
        (funcall (nth 1 face-and-action)))))

(defun tmmofl-run-on-functions (faces-at-point)
  "Run the on functions.
Those faces in FACES-AT-POINT that are not also in
`tmmofl-font-lock-symbols-cache' have just been moved onto,
so we should run the on-functions"
  (tmmofl-iterate-and-run-functions 'tmmofl-run-on-functions-for-face
                                    faces-at-point
                                    tmmofl-font-lock-symbols-cache))

(defun tmmofl-run-off-functions-for-face( face )
  "Runs the off functions for this face"
  (interactive)
  (dolist (face-and-action tmmofl-actions)
    (if (eq (car face-and-action) face)
        (funcall (nth 2 face-and-action)))))


(defun tmmofl-run-off-functions( faces-at-point )
  "Runs the off functions.
Those faces in `tmmofl-font-lock-symbols-cache' that are not also in `faces-at-point'
have just been moved off, so we should run the off-functions"
  (tmmofl-iterate-and-run-functions 'tmmofl-run-off-functions-for-face
                                    tmmofl-font-lock-symbols-cache
                                    faces-at-point))

(defun tmmofl-iterate-and-run-functions( function-to-call faces-to-interate faces-cache )
  "Calls functions depending on changes in faces.
If a face is in `faces-to-iterate' but not in `faces-cache', then call
`function-to-call' with that face as an argument"
  (let ((remaining-faces faces-to-interate))
    ;;iterate through all of faces in the cache
    (while remaining-faces
      (let ((current-face
             (if (listp remaining-faces)
                 (car remaining-faces)
               remaining-faces)))
        ;;is the current face also at point.If not run the off-function
        ;;if its theres a nil cache do it
        (if (not faces-cache)
            (funcall function-to-call current-face)
          ;;else if its a list and it DOES not contain it
          (if (and (listp faces-cache)
                   (not (memq current-face faces-cache)))
              (funcall function-to-call current-face)
            ;;else if it is not equal. There must be a better way of doing this
            (if (not (eq current-face faces-cache))
                (funcall function-to-call current-face))))
        (setq remaining-faces (cdr-safe remaining-faces))))))
  
(defun tmmofl-ensure-buffer-tmmofl-ready()
  "Ensure that the `tmmofl-post-command-hook-function' is on the
post-command-hook and that this hook is local"
  (add-hook 'post-command-hook 'tmmofl-post-command-hook-function nil t))

(defun tmmofl-possibly-remove-tmmofl-readiness()
  "Remove the `tmmofl-post-command-hook-function' from the
post-command-hook, if `tmmofl-actions' is empty. "
  ;;if both of these two are empty
  (if (not tmmofl-actions)
      ;;then we can make this non-local. Cant really make it no longer
      ;;local as we dont know that something else hasnt already make
      ;;it so
      (remove-hook 'post-command-hook
                   'tmmofl-post-command-hook-function t)))

(defun tmmofl-install-in-buffer
  ( face on-function off-function )
  "Install the functions to be run for a given face.
On moving onto a part of the buffer fontified by FACE run
ON-FUNCTION.  When moving of this run OFF-FUNCTION."
  ;;make sure all the hooks are in the right place
  (tmmofl-ensure-buffer-tmmofl-ready)
  ;;now add to the on-actions. We should really check here that we have not already got
  ;;an identical component on the list but at the moment I can be bothered
  (push (list face on-function off-function) tmmofl-actions))

(defun tmmofl-deinstall-from-buffer
  ( face on-function off-function )
  "Deinstall the following functions from tmmofl.
This works by removing them from `tmmofl-actions'.  Should also remove
`tmmofl-post-command-hook-function' from `post-command-hook' if
appropriate.
Argument FACE the face affected.
Argument ON-FUNCTION the function to run when moving on.
Argument OFF-FUNCTION the function to run on moving off."
  ;;on actions first
  (setq tmmofl-actions
        (delete
         (list face on-function off-function)
         tmmofl-actions))
  ;;and if there is nothing left remove the hooks
  (tmmofl-possibly-remove-tmmofl-readiness))

(defun tmmofl-blitz-from-buffer()
  "Remove all tmmofl-actions from current buffer, under all circumstances.
This is an emergency function to be used in case of tmmofl related disasters. It
may leave tmmofl-minor-mode in an inconsistant state"
  (interactive)
  ;;kill the hook
  (remove-hook 'post-command-hook
               'tmmofl-post-command-hook-function
               t)
  ;;kill the action variables
  (setq tmmofl-actions nil))

(defun tmmofl-blitz-from-buffer-for-symbol (symbol)
  "Remove all the `tmmofl-actions' from the current buffer for `SYMBOL'.
This is an emergency function to be used in case of tmmofl related disasters.
It removes all actions for a given font-lock-symbol regardless of the function.
Like `tmmofl-blitz-from-buffer' it may leave tmmofl-minor-mode in a
inconsistant state."
  (interactive "MSymbol to untmmofl: ")
  (setq tmmofl-actions
        (remove* (intern symbol) tmmofl-actions :key 'car)))

(defun tmmofl-install-for-mode (install-hook face on-function off-function)
  "Install tmmofl on any buffer running the hook INSTALL-HOOK.
Functions added in this way operate independantly of tmmofl-minor-mode
Argument FACE the face affected.
Argument ON-FUNCTION function to run moving onto FACE.
Argument OFF-FUNCTION function to run moving off FACE."
  (interactive)
  (add-hook install-hook
            (lambda()
               "This function was auto-coded by `tmmofl-install-for-mode'"
               (tmmofl-install-in-buffer
                face
                on-function
                off-function))))

(defun tmmofl-install-action(command)
  "Install tmmofl `action' in the current buffer.
The action should consist of a font-lock-symbol, and function to run on moving
onto this face, and another to run on moving off this symbol. Actually it does
need to be a font-lock-face, but if it isnt the functions will never be called."
  (let ((face (nth 0 command))
        (on-function (nth 1 command))
        (off-function (nth 2 command)))
    (tmmofl-install-in-buffer face
                              on-function
                              off-function)))

(defun tmmofl-deinstall-action(command)
  "Deinstall `action' in the current buffer.
See `tmmofl-install-action' which is the complementary function."
  (let ((face (nth 0 command))
        (on-function (nth 1 command))
        (off-function (nth 2 command)))
    (tmmofl-deinstall-from-buffer face
                                on-function
                                off-function)))

(defun tmmofl-install-mode-actions()
  "Add all of the actions defined as appropriate for this mode"
  (let ((list (tmmofl-get-actions-for-mode)))
    (while list
      (let ((cur (car list)))
        (progn
          (tmmofl-install-action cur)
          (setq list (cdr list)))))))

(defun tmmofl-deinstall-mode-actions()
  "Remove all of the actions defined as appropriate for this mode"
  (interactive)
  (let ((list (tmmofl-get-actions-for-mode)))
    (while list
      (let ((cur (car list)))
        (progn
          (tmmofl-deinstall-action cur)
          (setq list (cdr list)))))))

(defun tmmofl-get-actions-for-mode()
  "Get the actions defined as appropriate for this mode"
  (let((mode-variable (intern (concat "tmmofl-" (symbol-name major-mode) "-actions"))))
    (if (boundp mode-variable)
        (eval mode-variable)
      tmmofl-default-actions)))

;;we also want to define a minor mode which allows others to define
;;tmmofl additions to their own major odes.
(easy-mmode-define-minor-mode tmmofl-mode
"Toggle tmmofl minor mode.
With no arguments, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When tmmofl mode is enabled various other minor
modes are turned on or off, depending on the fontification
scheme at point. This is useful for instance for turning on
 whilst in comments, and turning it off whilst
in code.

tmmofl minor mode provides a default minor mode toggle
(which is auto-filling as described above). This may be altered in a
mode specific way by defining function foo-tmmofl-install for
foo-mode."
;;the initial value
nil
;;the mode line indicator
" Tmmofl" nil)

(add-hook 'tmmofl-mode-on-hook 'tmmofl-install-mode-actions)
(add-hook 'tmmofl-mode-off-hook 'tmmofl-deinstall-mode-actions)

(provide 'tmmofl)

;;; tmmofl.el ends here

