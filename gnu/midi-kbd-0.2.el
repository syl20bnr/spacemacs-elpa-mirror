;;; midi-kbd.el --- Create keyboard events from Midi input  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Free Software Foundation, Inc.

;; Author: David Kastrup <dak@gnu.org>
;; Keywords: convenience, hardware, multimedia
;; Version: 0.2
;; Maintainer: David Kastrup <dak@gnu.org>
;; Package-Requires: ((emacs "25"))

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

;; Entry point of this package is M-x midikbd-open RET
;;
;; It opens a raw ALSA midi device (see its documentation for how to
;; deal with non-raw devices) and feeds MIDI note-on and note-off
;; events into the Emacs input queue associated with the terminal from
;; which midikbd-open has been called.  Macro recording and replay is
;; possible.  The interpretation of such events is left to
;; applications establishing appropriate key bindings.
;;
;; Since macro recording and replay makes it very desirable to have
;; every generated event be interpretable standalone rather than split
;; into several Emacs events, every MIDI event is encoded into one
;; mouse-like event similar to <Ch1 C_4>.  Consequently, the following
;; functions are applicable to such events:
;;
;; (event-start EVENT) returns the down event part
;; (event-end EVENT) returns the up event part
;;
;; The up event is only available with bindings of <Ch1 up-C-4> and
;; similar, whereas the down event is available for all bindings.
;;
;; up/down event parts may be further split with
;;
;; (posn-area EV) returns a channel symbol Ch1..Ch16
;;
;; (posn-x-y EV) returns numeric values 0..127 for pitch and velocity
;;
;; (posn-timestamp EV) returns a millisecond time value that will wrap
;; around when reaching most-positive-fixnum, about every 12 days on a
;; 32bit system.
;;
;; Note events (omitting the channel modifier) are
;; <C_-1> <Csharp_-1> ... <G_9>
;;
;; Since Midi does not encode enharmonics, there are no *flat_* key
;; names: it is the job of the key bindings to give a higher level
;; interpretation to the basic pitch.

;;; Code:


(defconst midikbd-notenames
  (vconcat
   (cl-loop for i from 0 to 127
	    collect (intern
		     (format "%s_%d"
			     (aref ["C" "Csharp" "D" "Dsharp" "E" "F"
				    "Fsharp" "G" "Gsharp" "A" "Asharp" "B"]
				   (mod i 12))
			     (1- (/ i 12)))))))

;; Necessary to allow bindings to <Ch1 C_4> without splitting events
(cl-loop for key across midikbd-notenames do
	 (put key 'event-kind 'mouse-click))

;; We have `midikbd-notenames' for looking up the basic note name
;; events, `midikbd-upnames' for the keyrelease events, and
;; `midikbd-downnames' for the keypress events.  Those will, for now,
;; produce the likes of `C_-1', `up-C_-1', and `C_-1': we don't
;; actually use `down-C_-1' since the down-event is the principally
;; important one most likely to be bound to keys.

(defconst midikbd-downnames midikbd-notenames)

(defconst midikbd-upnames
  (vconcat
   (cl-loop for i across midikbd-notenames
	    collect
	    (intern (concat "up-" (symbol-name i))))))

;; Emacs can deal with up-events like with down-events since the patch
;; in <URL:http://debbugs.gnu.org/cgi/bugreport.cgi?bug=19746> has
;; been committed to Emacs.
;;
;; Older versions will erupt in violence when forced to deal with an
;; uncached "up-" event, so we need to put the full cache in place
;; ourselves.  We do this only if we find Emacs unable to identify
;; up-events.

;; Calling event-modifiers may poison the cache for up-C_-1 but since
;; we overwrite it first thing afterwards, this is not really an
;; issue.

(unless (event-modifiers 'up-C_-1)
  (cl-loop for key across midikbd-upnames for base across midikbd-notenames
	   do
	   (put key 'event-symbol-element-mask (list base 1))
	   (put key 'event-symbol-elements (list base 'up))
	   (let ((modc (get base 'modifier-cache)))
	     (unless (assq 1 modc)
	       (put base 'modifier-cache (cons (cons 1 key) modc))))))


(defconst midikbd-channelnames
  [Ch1 Ch2 Ch3 Ch4 Ch5 Ch6 Ch7 Ch8
       Ch9 Ch10 Ch11 Ch12 Ch13 Ch14 Ch15 Ch16])

;; CCL programs used in coding systems apparently don't save registers
;; across suspension so we don't use a coding system.  Instead our CCL
;; program is run using ccl-execute-on-string in the filter routine.
;; That allows us to interpret all _completed_ Midi commands without
;; getting confused, and it also gives us a well-defined internal
;; state (namely one for every call of midikbd-filter-create).

;; Decoding Midi is a positive nuisance because of "running status":
;; if a Midi command byte is the same as the last one, it can be
;; omitted and just the data sent.
;; So we keep the current command in r0, the currently read byte in r1,
;; the channel in r6.

(define-ccl-program midikbd-decoder
  '(2
    (loop
     (loop
      ;; central message receiver loop here.
      ;; When it exits, the command to deal with is in r0
      ;; Any arguments are in r1 and r2
      ;; r3 contains: 0 if no arguments are accepted
      ;;              1 if 1 argument can be accepted
      ;;              2 if 2 arguments can be accepted
      ;;              3 if the first of two arguments has been accepted
      ;; Arguments are read into r1 and r2.
      ;; r4 contains the current running status byte if any.
      (read-if (r0 < #x80)
	       (branch r3
		       (repeat)
		       ((r1 = r0) (r0 = r4) (break))
		       ((r1 = r0) (r3 = 3) (repeat))
		       ((r2 = r0) (r3 = 2) (r0 = r4) (break))))
      (if (r0 >= #xf8) ; real time message
	  (break))
      (if (r0 < #xf0) ; channel command
	  ((r4 = r0)
	   (if ((r0 & #xe0) == #xc0)
	       ;; program change and channel pressure take only 1 argument
	       (r3 = 1)
	     (r3 = 2))
	   (repeat)))
      ;; system common message, we swallow those for now
      (r3 = 0)
      (repeat))
     (if ((r0 & #xf0) == #x90)
	 (if (r2 == 0)		    ; Some Midi devices use velocity 0
					; for switching notes off,
					; so translate into note-off
					; and fall through
	     (r0 -= #x10)
	   ((r0 &= #xf)
	    (write 0)
	    (write r0 r1 r2)
	    (repeat))))
     (if ((r0 & #xf0) == #x80)
	 ((r0 &= #xf)
	  (write 1)
	  (write r0 r1 r2)
	  (repeat)))
     (repeat))))

(defun midikbd-get-ts-lessp (pivot)
  "Return a comparison operator for timestamps close to PIVOT.

Timestamps are just a millisecond count that wraps around
eventually.  To compare two timestamps TS1 and TS2, one can
generally just look at the sign of their difference.  However,
this relation is not really transitive when given input spanning
more than half of the given number range (should only happen in
degenerate cases since the overall range spans several days).

Sort algorithms may require transitivity in order to complete, so
this routine creates a transitive comparison operator when given
a \"pivot\" from within the sorted range."
  (lambda (ts1 ts2)
    (< (- ts1 pivot) (- ts2 pivot))))

(defun midikbd-filter-create ()
  "Create one Midi process filter keeping state across calls."
  (let* ((state (make-vector 9 nil))
	 (keypress (make-vector 2048 nil))
	 (param-len [3 3])
	 (hooks (vector
		 (lambda (ts ch pitch velocity)
		   (let ((res
			  (list (aref midikbd-downnames pitch)
				(list nil
				      (aref midikbd-channelnames ch)
				      (cons pitch velocity)
				      ts))))
		     (aset keypress (+ (* ch 128) pitch) res)
		     (list res)))
		 (lambda (ts ch pitch velocity)
		   (let* ((idx (+ (* ch 128) pitch))
			  (oldpress (prog1 (aref keypress idx)
				      (aset keypress idx nil))))
		     (and oldpress
			  (list
			   (list (aref midikbd-upnames pitch)
				 (cadr oldpress)
				 (list nil
				       (aref midikbd-channelnames ch)
				       (cons pitch velocity)
				       ts)))))))))
    (lambda (_process string)
      (let* ((ct (current-time))
	     (ts (+ (* (nth 0 ct) 65536000)
		    (* (nth 1 ct) 1000)
		    (/ (nth 2 ct) 1000)))
	     (str (ccl-execute-on-string 'midikbd-decoder
					 state string t t)))
	(setq unread-command-events
	      (append unread-command-events
		      (cl-loop with i = 0 while (< i (length str))
			       nconc
			       (let* ((code (aref str i))
				      (beg (1+ i)))
				 (setq i (+ beg (aref param-len	code)))
				 (apply (aref hooks code)
					ts
					(append (substring str beg i)
						nil))))))))))

(defcustom midikbd-default-device
  "/dev/snd/midiC1D0"
  "Default MIDI raw device for midikbd."
  :type '(file)
  :group 'midi-kbd
  :package-version '(midi-kbd . "0.2"))

;;;###autoload
(defun midikbd-open (file)
  "Open the raw Midi device FILE as a source for Midi input.
This should be an ALSA device like \"/dev/snd/midiC1D0\".  If your
Midi producing device is a software Midi device, you might need to
call

    sudo modprobe snd-virmidi

in order to have some virtual ALSA ports available as such raw Midi
devices."
  (interactive (list (read-file-name "Midi device: "
				     (file-name-directory midikbd-default-device)
				     (file-name-nondirectory midikbd-default-device)
				     t nil
				     #'file-readable-p)))
  (let* ((file (expand-file-name file
				 (file-name-directory midikbd-default-device)))
	 (buffer (get-buffer-create (concat " *Midi process " file " *")))
	 (oldproc (get-buffer-process buffer)))
    (if (processp oldproc) (delete-process oldproc))
    (make-serial-process :port file
			 :speed nil
			 :buffer buffer
			 :coding 'raw-text
			 :filter (midikbd-filter-create)
			 :sentinel #'ignore
			 :noquery t)))

;;;; ChangeLog:

;; 2015-10-11  David Kastrup  <dak@gnu.org>
;; 
;; 	Add packages/midi-kbd/
;; 
;; 	This introduces the package midi-kbd version 0.2 for converting raw
;; 	MIDI input into Emacs events.
;; 


(provide 'midi-kbd)
;;; midi-kbd.el ends here
