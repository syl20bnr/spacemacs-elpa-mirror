;;; wide-column.el --- Calls functions dependant on column position.
;; $Revision: 1.4 $
;; $Date: 2002/04/05 09:28:09 $

;; This file is not part of Emacs

;; Copyright (c) 2002 Phillip Lord

;; Author: Phillip Lord <p.lord@russet.org.uk>
;; Maintainer: Phillip Lord <p.lord@russet.org.uk>
;; Keywords: minor mode, cursor colour, column width
;; Package-Version: 20170925.913

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Status:
;;
;; This has been released for quite a while now, and works well for
;; me. There are few issues with it, which are mentioned in issues.
;; It's pretty much ready, but I haven't put custom support in yet
;; which would be nice.


;;; Commentary:
;;
;; This package is designed to run functions depending on the column
;; that the cursor is in.  My initial idea with it, is just to have it
;; change the cursor colour, lightening it as you go over the fill
;; column length.
;;
;; The point of this is that monitor sizes have in recent years got
;; plain silly, and its now relatively easy to buy one the size of a
;; small wardrobe.  Combined with the other wise wonderful
;; `dabbrev-expand' which makes it feasible to use very explantory,
;; and very long variable, and function names, source code has a habit
;; of becoming stupidly wide.  Now of course this wouldn't matter very
;; much, if we all had wide screens.  However in recent years, flat
;; screen monitors have become widely prevelant, and these generally
;; have lower resolutions, and smaller screen sizes, unless you are
;; very rich.  This raises the nasty possibility of a split therefore
;; in behaviour between those using LCD, and CRT based monitors.
;; Coming, as I do, from the left of the political spectrum, naturally
;; I find such divisiveness worrying.  This, therefore, is my
;; contribution to preventing it.
;;
;; This package functions as a normal minor mode, so
;; `wide-column-mode' toggles it on and off. There is also a global
;; minor mode which you can access with `global-wide-column-mode'
;; (Emacs 21 only). There is a problem with the getting the default
;; cursor colour; this happens when wide-column is loaded, and I can't
;; get around it without a hook in `set-cursor-color'. Set the
;; variable `wide-column-default-cursor-colour' which will solve this
;; problem.
;;

;;; Similar Packages:
;;
;; Sandip Chitale (sandip.chitale@blazesoft.com) highlight-beyond-fill

;;; Installation
;;
;; Place this file in your Emacs load path. Put (require 'wide-column)
;; into your .emacs or equivalent file. This operates as a normal
;; minor mode, so `wide-column-mode' will toggle it on and off.
;;
;; The code was developed on Gnu Emacs 21. Emacs 20 support has now
;; been removed because it required code duplication horribleness. 
;;
;; It may work on XEmacs, but I don't have one around to try. You will
;; certainly need the fsf compatibility packages if you do. 

;;; Issues;
;;
;; 1) I'm not sure about the error handling. I think things are
;; working quite well. However if the affector function crashes out,
;; it will appear to the user that wide-column mode is on, but
;; actually, it will be disabled. I can solve this easily, by
;; switching the mode off on errors, but easy-mmode produces
;; mini-buffer messages, which hide my own attempts to provide error
;; reporting. I think this way is better. If a crash happens the
;; system will be inconsistent, but the alternative will be to have
;; the minor-mode switch itself off.
;;
;; 2) The colour list is poor. I would like to improve things here,
;; but I am not sure how. See the comments near the definition of
;; `wide-column-colour-list'
;;
;; 3) Custom support would be good, and no doubt will be added at some
;; time. 
;;
;; 4) It's not going to work if people use lots of different default
;; cursor colours. Seems like a daft thing to do to me! Something to
;; work on anyway. Maybe I could solve this by advicing
;; `set-cursor-colour', but this would fail if someone uses
;; `modify-frame-parameters' directly, and I really don't want to
;; advice this function anyway.

(require 'easy-mmode)

;;; Code:

;; Basic variables. Defcustom these later.
(defvar wide-column-start-width nil
  "The column beyond which the `wide-column-affector-function' is called.
If this variable is set to nil then the value of `fill-column' is
used instead.")

(make-variable-buffer-local 'wide-column-start-width)

(defvar wide-column-affector-function 'wide-column-warning-colour
  "This defines the main affector function.
This function is called when the cursor is at a position greater than
`wide-column-start-width'.  If this affector function fails for some
reason then errors are reported to the mini-buffer.  The system will
try to do its best to return things to normal, but obviously this is a
programming error somewhere, so there are no guarentees.

The affector function must have the following properties:-

It should take a single parameter.

If this parameter is positive then it is the amount that the cursor
position is in excess of the maximum.  The function will be called
after every command while the cursor is beyond the maximum allowable
value, so don't make it too heavy weight, or it will make editing
slow.

If the parameter is negative, or zero then its still the amount that
the cursor is in excess of the maximum (i.e. the cursor is lower than
or equal to the maximum).  The function will be called with these
values however only when moving from over the maximum to below it
once, as an optimisation.

If the parameter is the symbol `on', then it mean that the function is
being called for the first time in this buffer, and it should do what
ever is necessary.

If the parameter is the symbol `off', then it means that the mode is
being switched off, in the current buffer.

If the parameter is the symbol `reset' then it means that the cursor
has moved out of the old buffer and into a new one, and a reset should
happen.  Its important to realise here that when this reset happens
the `current-buffer' may or may not be using the option
`wide-column-mode'.  The function only needs to do something
therefore, if it affects a global property, like for instance the
cursor colour.  If it affects a buffer local property, then IT WILL BE
IN THE WRONG BUFFER

And finally it shouldn't do anything daft, like leaving the current
buffer changed, probably it shouldn't move point.  Deleting all of the
text in excess of the wide column would be amusing, but still perhaps
not a good idea.")

(make-variable-buffer-local 'wide-column-affector-function)

;;; This section provides the basic functionality of the mode.
(defvar wide-column-last-command-over-width-p nil
  "The last command executed beyond the maximum width.")
(make-variable-buffer-local 'wide-column-last-command-over-width-p)

(defvar wide-column-buffer-affector-last-called-in nil
  "The last buffer an affector was called in.
This is the last buffer that any `wide-column-affector-function' was
called in.  This information is recorded so that things can be reset,
when the buffer is moved out of.")

(defvar wide-column-affector-function-last-called nil
  "This is the last affector function that was called.")

(defun wide-column-post-command-hook-function()
  "This calls the function specified by `wide-column-affector-function'
when the cursor is beyond the column `wide-column-start-width' if it
is set, or `fill-column' if it is not. See the documentation of
`wide-column-affector-function' for full details."
  (interactive)
  (condition-case err
      (progn
        (let ((buffer (current-buffer)))
          (if (not (eq wide-column-buffer-affector-last-called-in buffer))
              ;; we have moved out of the a wide column buffer,
              ;; therefore we need to reset the affector from the last
              ;; buffer
              (if wide-column-affector-function-last-called
                  (funcall wide-column-affector-function-last-called 'reset))))
        ;; now only actually do anything if wide-column-mode is on
        (if wide-column-mode
            (let ((column-position (current-column))
                  (start-width
                   (or wide-column-start-width
                       fill-column)))
              (if (> column-position start-width)
                  (progn
                    (wide-column-call-affector)
                    (setq wide-column-last-command-over-width-p t))
                (if wide-column-last-command-over-width-p
                    (progn (wide-column-call-affector)
                           (setq wide-column-last-command-over-width-p nil)))))))
    (error
     ;; this catches errors in this function, or in the affector
     ;; function. If I don't do this then emacs just empties
     ;; post-command-hook, which makes things a pain in the ass to
     ;; debug, and will affect other packages using this hook
     (progn
       ;; Switch the mode off. This will leave the system in an
       ;; inconsistent state, as the minor mode will still appear to
       ;; be on. I am not sure what to do with this. I've tried just
       ;; switching the mode off, but the informative message from
       ;; easy-mmode covers up the error report.
       (wide-column-mode-emergency-off)
       (backtrace)
       (message "Error from `wide-column-affector-function' caught: %s"
                (error-message-string err))))))

(defun wide-column-call-affector ()
  "Call the affector with the column position."
  ;; sing hey diddle dey, for dynamic scoping
  (funcall wide-column-affector-function (- column-position start-width))
  ;; record this stuff so that we can reset correctly.
  (setq wide-column-affector-function-last-called wide-column-affector-function)
  (setq wide-column-buffer-affector-last-called-in (current-buffer)))

(define-minor-mode wide-column-mode
  "Toggle wide-column mode.
With no argument, this command toggles this mode.
Non-null prefix arguments turns on the mode,
Null prefix argument turns it off.

When wide-column mode is enabled, the function defined in
`wide-column-affector-function' is called, when your cursor has gone
beyond `wide-column-start-width' if it's set, or `fill-column' it
its not.

By default the practical upshot of this is that cursor colour changes,
when your lines get too long."
  :group 'wide-column
  :lighter " Wc"
  (if wide-column-mode
      (progn 
        ;; add hook if we need to. 
        (wide-column-mode-reset)
        (funcall wide-column-affector-function 'on))
    (funcall wide-column-affector-function 'off)))


;; define global-minor-mode
(define-global-minor-mode global-wide-column-mode 
  wide-column-mode wide-column-turn-on)
      
;;       (add-hook
;;        'global-wide-column-mode-hook
;;        'global-wide-column-hook)))

;; (defun global-wide-column-hook()
;;   "Help to switch off global mode"
;;   (interactive)
;;   (if (not global-wide-column-mode)
;;       (funcall wide-column-affector-function 'off)))

(defun wide-column-turn-on()
  (wide-column-mode 1))

(defun wide-column-mode-emergency-off()
  "Get out of `wide-column-mode'. Calling this function disabled this mode
totally, and irrespective of whether its actually switched on or
not. It's an emergency function, in case of crashes, and should not
normally be called. `wide-column-mode-reset' turns it back on again. "
  (interactive)
  ;; switch mode off
  (remove-hook 'post-command-hook
               'wide-column-post-command-hook-function))

;; I can't find any leaving or entering buffer hooks. So I have to use
;; a global post-command hook. I don't really like this, but what can
;; you do?

(defun wide-column-mode-reset()
  "This function resets` wide-column mode' if its been switched off due to errors"
  (interactive)
  (add-hook 'post-command-hook
            'wide-column-post-command-hook-function))


;; This is the bit which provides the colour switching code, which is
;; the default behaviour of this package.


;; with faces you can set colours depending on whether the background
;; is dark or light. I don't know how to do this with colour
;; names. Also the colour list that I am using here is fine for me,
;; but its based on my usual cursor colour. What I would really like
;; to do is lighten the colour each time I move further from the
;; fill-column. So it would be nice to be able to get from one colour
;; to the next automatically, without just specifying a list. 
(defvar wide-column-colour-list
  '("orange" "yellow" "white"))

(setq wide-column-warning-colour-quotient 5)

(defvar wide-column-default-cursor-colour
  (frame-parameter (selected-frame) 'cursor-color)
  "Place to store the default cursor colour.")

(defun wide-column-warning-colour (current-excess-column)
  "Set the cursor colour depending on the column position"
  ;; first we need to test for a flag condition, mostly to
  ;; reset or store the current cursor colour.
  (cond
   ((or (eq 'reset current-excess-column)
        (eq 'off current-excess-column))
    (set-cursor-color wide-column-default-cursor-colour))
   ;; switched on the first time. 
   ((eq 'on current-excess-column)
    nil)
    ;;(setq wide-column-default-cursor-colour
    ;;     (frame-parameter (selected-frame) 'cursor-color)))
   ;; now we need to actually do the cursor colour change. Change it
   ;; back to default.
   ((>= 0 current-excess-column)
    (set-cursor-color wide-column-default-cursor-colour))
   ;; change it toggles something else.
   (t
    (let* ((max-colour-index
            (- (length wide-column-colour-list) 1))
           (suggested-colour-number
             (/ current-excess-column wide-column-warning-colour-quotient))
           (actual-colour-number
            (if (> suggested-colour-number max-colour-index)
                max-colour-index
              suggested-colour-number)))
      (set-cursor-color (nth actual-colour-number wide-column-colour-list))))))                                

;; Some test code
(defun wide-column-warning-colour-test()
  (interactive)
  (wide-column-warning-colour
   (- (current-column) 20)))

(defun wide-column-shout-about-affector()
  (interactive)
  (setq wide-column-affector-function
        (lambda(current-excess-column)
          (message "Wide Column Affector called with column %s" current-excess-column))))

(defun wide-column-restore-default-affector()
  (interactive)
  (setq wide-column-affector-function
        'wide-column-warning-colour))
                      ;(default-value wide-column-affector-function)))

(provide 'wide-column)

;;; wide-column.el ends here
