;;; pulseaudio-control.el --- Use `pactl' to manage PulseAudio volumes.  -*- lexical-binding: t -*-

;; Copyright (C) 2017  Alexis <flexibeast@gmail.com>

;; Author: Alexis <flexibeast@gmail.com>
;; Maintainer: Alexis <flexibeast@gmail.com>
;; Created: 2017-08-23
;; URL: https://github.com/flexibeast/pulseaudio-control
;; Keywords: multimedia, hardware, sound, PulseAudio
;; Version: 0.1
;; Package-Version: 20171004.428
;; Package-X-Original-Version: 0.1

;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

;;; Commentary:

;; `pulseaudio-control' controls PulseAudio volumes from Emacs, via `pactl`.

;; ## Table of Contents

;; - [Installation](#installation)
;; - [Usage](#usage)
;; - [Issues](#issues)
;; - [License](#license)

;; ## Installation

;; Install [pulseaudio-control from MELPA](http://melpa.org/#/pulseaudio-control), or put `pulseaudio-control.el` in your load-path and do a `(require 'pulseaudio-control)'.

;; ## Usage

;; Initially, the `pulseaudio-control' keymap is not bound to any prefix. You can call the command `pulseaudio-control-default-keybindings' to use the prefix `C-x /' to access the `pulseaudio-control' keymap globally; if you wish to use this prefix by default, add the line:

;;     (pulseaudio-control-default-keybindings)

;; to your init file.

;; The default keybindings in the `pulseaudio-control' keymap are:

;; * + : Increase the volume of the currently-selected sink by `pulseaudio-control-volume-step' (`pulseaudio-control-increase-volume').

;; * - : Decrease the volume of the currently-selected sink by `pulseaudio-control-volume-step' (`pulseaudio-control-decrease-volume').

;; * v : Directly specify the volume of the currently-selected sink (`pulseaudio-control-set-volume').  The value can be:

;;   * a percentage, e.g. '10%';
;;   * in decibels, e.g. '2dB';
;;   * a linear factor, e.g. '0.9' or '1.1'.

;; * m : Toggle muting of the currently-selected sink (`pulseaudio-control-toggle-current-sink-mute').

;; * x : Toggle muting of a sink, specified by index (`pulseaudio-control-toggle-sink-mute-by-index').

;; * e : Toggle muting of a sink, specified by name (`pulseaudio-control-toggle-sink-mute-by-name').

;; * i : Select a sink to be the current sink, specified by index (`pulseaudio-control-select-sink-by-index').

;; * n : Select a sink to be the current sink, specified by name (`pulseaudio-control-select-sink-by-name').

;; Customisation options, including `pulseaudio-control-volume-step', are available via the `pulseaudio-control' customize-group.

;; ## Issues / bugs

;; If you discover an issue or bug in `pulseaudio-control' not already noted:

;; * as a TODO item, or

;; * in [the project's "Issues" section on GitHub](https://github.com/flexibeast/pulseaudio-control/issues),

;; please create a new issue with as much detail as possible, including:

;; * which version of Emacs you're running on which operating system, and

;; * how you installed `pulseaudio-control'.

;; ## License

;; [GNU General Public License version 3](http://www.gnu.org/licenses/gpl.html), or (at your option) any later version.

;;; Code:


;; Customisable variables.

(defgroup pulseaudio-control nil
  "Control PulseAudio volumes via `pactl'."
  :group 'external)

(defcustom pulseaudio-control-default-sink "0"
  "Default Pulse sink index to act on."
  :type 'string
  :group 'pulseaudio-control)

(defcustom pulseaudio-control-pactl-path "/usr/bin/pactl"
  "Absolute path of `pactl' executable."
  :type '(file :must-match t)
  :group 'pulseaudio-control)

(defcustom pulseaudio-control-volume-step "10%"
  "Step to use when increasing or decreasing volume.

The value can be:

* a percentage, e.g. '10%';
* in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'."
  :type 'string
  :group 'pulseaudio-control)


;; Internal variables.

(defvar pulseaudio-control--current-sink pulseaudio-control-default-sink
  "String containing index of currently-selected Pulse sink.")


;; Internal functions.

(defun pulseaudio-control--call-pactl (command)
  "Call `pactl' with COMMAND as its arguments.

COMMAND is a single string separated by spaces,
e.g. 'list short sinks'."
  (let ((args `("" nil
                ,pulseaudio-control-pactl-path
                nil t nil
                ,@(split-string command " "))))
    (apply #'call-process-region args)))

(defun pulseaudio-control--get-sinks ()
  "Internal function; get a list of Pulse sinks via `pactl'."
  (let ((fields-re "^\\(\\S-+\\)\\s-+\\(\\S-+\\)")
        (sinks '()))
    (with-temp-buffer
      (pulseaudio-control--call-pactl "list short sinks")
      (goto-char (point-min))
      (while (re-search-forward fields-re nil t)
        (let ((number (match-string 1))
              (name (match-string 2)))
          (setq sinks (append sinks (list `(,number . ,name)))))))
    sinks))


;; User-facing functions.

;;;###autoload
(defun pulseaudio-control-decrease-volume ()
  "Decrease volume of currently-selected Pulse sink.

Amount of decrease is specified by `pulseaudio-control-volume-step'."
  (interactive)
  (pulseaudio-control--call-pactl (concat "set-sink-volume "
                                          pulseaudio-control--current-sink
                                          " -"
                                          pulseaudio-control-volume-step)))

;;;###autoload
(defun pulseaudio-control-default-keybindings () 
  "Make `C-x /' the prefix for accessing pulseaudio-control bindings."
  (interactive)
  (global-set-key (kbd "C-x /") 'pulseaudio-control-map))

;;;###autoload
(defun pulseaudio-control-increase-volume ()
  "Increase volume of currently-selected Pulse sink.

Amount of increase is specified by `pulseaudio-control-volume-step'."
  (interactive)
  (pulseaudio-control--call-pactl (concat "set-sink-volume "
                                          pulseaudio-control--current-sink
                                          " +"
                                          pulseaudio-control-volume-step)))

;;;###autoload
(defun pulseaudio-control-select-sink-by-index (sink)
  "Select which Pulse sink to act on, by numeric index.

Accepts number as prefix argument.

Argument SINK is the number provided by the user."
  (interactive "NSink index: ")
  (let ((sink (number-to-string sink))
        (valid-sinks (mapcar 'car (pulseaudio-control--get-sinks))))
    (if (member sink valid-sinks)
        (progn
          ;;
          ;; NOTE:
          ;;
          ;; The documentation for pactl(1) version 10.0-1+deb9u1
          ;; states:
          ;;
          ;;     set-default-sink SINK
          ;;         Make the specified sink (identified by its symbolic name)
          ;;         the default sink.
          ;;
          ;; However, as at 20170828, it seems to work with
          ;; a numeric index also.
          ;;
          (pulseaudio-control--call-pactl (concat "set-default-sink "
                                                  sink))
          (setq pulseaudio-control--current-sink sink))
      (error "Invalid sink index"))))

;;;###autoload
(defun pulseaudio-control-select-sink-by-name ()
  "Select which Pulse sink to act on, by name."
  (interactive)
  (let* ((valid-sinks (mapcar 'cdr (pulseaudio-control--get-sinks)))
         (sink (completing-read "Sink name: " valid-sinks))) 
    (if (member sink valid-sinks)
        (progn
          (pulseaudio-control--call-pactl (concat "set-default-sink "
                                                  sink))
          (setq pulseaudio-control--current-sink sink))
      (error "Invalid sink name"))))

;;;###autoload
(defun pulseaudio-control-set-volume (volume)
  "Set volume of currently-selected Pulse sink.

The value can be:

* a percentage, e.g. '10%';
* in decibels, e.g. '2dB';
* a linear factor, e.g. '0.9' or '1.1'.

Argument VOLUME is the volume provided by the user." 
  (interactive "MVolume: ")
  (let ((valid-volumes-re (concat
                           "[[:digit:]]+%"
                           "\\|[[:digit:]]+dB"
                           "\\|[[:digit:]]+\.[[:digit:]]+")))
    (if (string-match valid-volumes-re volume)
        (pulseaudio-control--call-pactl (concat "set-sink-volume "
                                                pulseaudio-control--current-sink
                                                " "
                                                volume))
      (error "Invalid volume"))))

;;;###autoload
(defun pulseaudio-control-toggle-current-sink-mute ()
  "Toggle muting of currently-selected Pulse sink."
  (interactive)
  (pulseaudio-control--call-pactl (concat "set-sink-mute "
                                          pulseaudio-control--current-sink
                                          " toggle")))

;;;###autoload
(defun pulseaudio-control-toggle-sink-mute-by-index (sink)
  "Toggle muting of Pulse sink, specified by index.

Argument SINK is the number provided by the user."
  (interactive "NSink index: ")
  (let ((sink (number-to-string sink))
        (valid-sinks (mapcar 'car (pulseaudio-control--get-sinks))))
    (if (member sink valid-sinks)
        (progn
          (pulseaudio-control--call-pactl (concat "set-sink-mute "
                                                  sink
                                                  " toggle")))
      (error "Invalid sink index"))))

;;;###autoload
(defun pulseaudio-control-toggle-sink-mute-by-name ()
  "Toggle muting of Pulse sink, specified by name."
  (interactive)
  (let* ((valid-sinks (mapcar 'cdr (pulseaudio-control--get-sinks)))
         (sink (completing-read "Sink name: " valid-sinks))) 
    (if (member sink valid-sinks)
        (progn
          (pulseaudio-control--call-pactl (concat "set-sink-mute "
                                                  sink
                                                  " toggle")))
      (error "Invalid sink name"))))


;; Default keymap.

(define-prefix-command 'pulseaudio-control-map)
(define-key pulseaudio-control-map (kbd "-") 'pulseaudio-control-decrease-volume)
(define-key pulseaudio-control-map (kbd "+") 'pulseaudio-control-increase-volume)
(define-key pulseaudio-control-map (kbd "m") 'pulseaudio-control-toggle-current-sink-mute)
(define-key pulseaudio-control-map (kbd "x") 'pulseaudio-control-toggle-sink-mute-by-index)
(define-key pulseaudio-control-map (kbd "e") 'pulseaudio-control-toggle-sink-mute-by-name)
(define-key pulseaudio-control-map (kbd "i") 'pulseaudio-control-select-sink-by-index)
(define-key pulseaudio-control-map (kbd "n") 'pulseaudio-control-select-sink-by-name)
(define-key pulseaudio-control-map (kbd "v") 'pulseaudio-control-set-volume)


;; --

(provide 'pulseaudio-control)

;;; pulseaudio-control.el ends here
