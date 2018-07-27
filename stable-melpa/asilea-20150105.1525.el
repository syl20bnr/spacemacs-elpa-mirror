;;; asilea.el --- Find best compiler options using simulated annealing -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/asilea
;; Package-Version: 20150105.1525
;; Package-X-Original-Version: 0.2.1
;; Package-Requires: ((emacs "24") (cl-lib "0.5"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2015, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; Asilea is a library using simulated annealing to try to find best compiler
;; options, where "best" is typically either the fastest executable or the
;; smallest one, but may be any arbitrary metric.
;;
;; Typical usage (assuming lexical binding):
;;
;; (let* ((asilea-max-steps 1000)
;;        (solution nil)
;;        (asilea-solution-accepted-function
;;         (lambda (state energy)
;;           (setq solution (cons state energy))))
;;        (asilea-finished-function
;;         (lambda ()
;;           (message "Solution found: %s (score: %s)"
;;                    (car solution)
;;                    (cdr solution)))))
;;   (asilea-run
;;    ;; timing-script is a script that compiles the program, measures
;;    ;; the time and/or size, and prints the score ("energy") on the
;;    ;; stdout. By default, lower energy is better.
;;    "timing-script"
;;    [["-O2" "-O3" "-Ofast" "-Os"]
;;     [nil "-ffast-math"]
;;     [nil "-ffoo" ("-ffoo" "-fbar")]
;;     [nil "-fexample-optimization"]
;;     ;; and so on
;;     ]))
;;
;; For more details, see the function `asilea-run' and variables whose names
;; start with `asilea-'.

;;; Code:
(require 'cl-lib)


;;; Variables

(defvar asilea-random-generator-function #'cl-random
  "(Pseudo)-random number generator to use.

It should be a function that, when called with an argument LIM, an
integer or a float, returns a pseudo-random non-negative number less
than LIM.")

(defvar asilea-concurrent-jobs 1
  "The number of simulated annealing jobs to spawn concurrently.
The jobs are independent from each other.

It should be an integer and not less than 1.")

(defvar asilea-max-steps nil
  "The maximum number of steps performed by a single concurrent job.
When a simulated annealing job performs this many steps, the job
ends.

This variable may be nil, in which case both
`asilea-initial-temperature' and `asilea-final-temperature' must be
non-nil; otherwise, it should be an integer not less than 1.")

(defvar asilea-cooling-rate 0.005
  "The cooling rate.

It should be a floating-point number in the range (0.0, 1.0).")

(defvar asilea-initial-temperature nil
  "The initial temperature.

When `asilea-max-steps' is non-nil, this variable can be nil, in
which case the library tries to pick a reasonable value
automatically; otherwise, it should be a number.")

(defvar asilea-final-temperature nil
  "The final temperature.
When a simulated annealing job hits a temperature this low, the job
ends.

When `asilea-max-steps' is non-nil, this variable has no effect;
otherwise, it should be a number.")

(defvar asilea-acceptance-function #'asilea-default-acceptance-function
  "Function determining whether to accept a candidate solution.

It's called with four arguments (NEW-ENERGY OLD-ENERGY TERMPERATURE
RANDOM-FUNCTION), where:
 * NEW-ENERGY is the energy of the candidate being considered.
 * OLD-ENERGY is the energy of the currently accepted candidate.
 * TEMPERATURE is the current temperature.
 * RANDOM-FUNCTION is the value of
   `asilea-random-generator-function' that was in effect
   when the simulated annealing process begun.
It should return non-nil if the new candidate is to be accepted.")

(defvar asilea-parse-energy-function #'string-to-number
  "Function converting process output into energy proper.

It's called with one argument, the process output as a string.
It should return the parsed energy value, or nil if parsing
failed.")

(defvar asilea-report-candidate-function #'ignore
  "Function called for each and every candidate.

It's called with two arguments (STATE ENERGY), where:
 * STATE is the candidate state, i.e. the list of options passed to
   the external program.
 * ENERGY is the candidate energy, i.e. the result of a
   `asilea-parse-energy-function' call.
The return value is ignored.")

(defvar asilea-solution-accepted-function #'ignore
  "Function called when a solution is accepted.

It's called with two arguments (STATE ENERGY), where:
 * STATE is the accepted solution state, i.e. the list of options
   passed to the external program.
 * ENERGY is the candidate energy, i.e. the result of a
   `asilea-parse-energy-function' call.
The return value is ignored.")

(defvar asilea-finished-function #'ignore
  "Function called when the simulated annealing process is finished.

It's called with no arguments, its return value is ignored.")


;;; Public functions

(defun asilea-run (program options)
  "Try to find the best compiler options for a given program.
\"Best\" here typically means the fastest or the smallest
executable, but any metric may be used.

PROGRAM should be a program that runs the compiler, measures the
time, size or whatever metric desired and prints the score (called
\"energy\") on the standard output.

OPTIONS is a vector of possible options to pass to PROGRAM.
Each element of this vector (an \"option group\") should be a vector
of the form [OPTION1 OPTION2 ... OPTION-N].
Each element of an option group should be either a string or a list
of strings; in the latter case, all options from this list are
passed as-is, in-order.

Each time PROGRAM is called, it receives an argument list formed by
picking one element from each option group.

The search for the best set of compiler options is done by using
simulated annealing.

When the simulated annealing process is started, the
function returns: programs are started asynchronously; to
run synchronously, use `asilea-run-synchronously' instead.

When the simulated annealing process process ends,
`asilea-finished-function' is called.

The search ends when all concurrent jobs have performed
`asilea-max-steps' steps, or when all concurrent jobs hit a
temperature not above `asilea-final-temperature'.

For each candidate solution tried,
`asilea-report-candidate-function' is called.
For each accepted solution, `asilea-solution-accepted-function' is
called.

To set the initial temperature, see `asilea-initial-temperature'.
To set the cooling rate, see `asilea-cooling-rate'.
To change the acceptance function, see `asilea-acceptance-function'.
To change the pseudo-random number generator used, see
`asilea-random-generator-function'.

When the PROGRAM's output is more complicated than just a number,
set `asilea-parse-energy-function' to a function able to parse the
output correctly.

There is an option to have several independent simulated annealing
jobs for the same PROGRAM and OPTIONS running concurrently, see
`asilea-concurrent-jobs'.
Concurrent jobs will share all parameters and callback functions.

Note: the preferred way to bind the configuration variables is to
use `let' around an `asilea-run' call.
Note: changing the configuration variables will have no effect on
already running jobs.

This function can be used in an interactive Emacs session, but it's
probably a better idea to run it in a separate batch Emacs process."
  (asilea--sanitize-variables)
  (let* ((asilea-initial-temperature (asilea--initial-temperature))
         (number-of-jobs-still-running asilea-concurrent-jobs)
         ;; Capture the settings so that changes to global variables have no
         ;; effect on this run.
         (random-function asilea-random-generator-function)
         (final-temperature asilea-final-temperature)
         (cooling-rate-inv (- 1.0 asilea-cooling-rate))
         (acceptance-function asilea-acceptance-function)
         (parse-energy-function asilea-parse-energy-function)
         (report-candidate-function asilea-report-candidate-function)
         (solution-accepted-function asilea-solution-accepted-function)
         (finished-function asilea-finished-function)
         (starting-directory default-directory)
         (finish-job
          (lambda ()
            (when (<= 0 (cl-decf number-of-jobs-still-running))
              ;; The last job finished, we're done.
              (with-demoted-errors "Error in `asilea-finished-function': %S"
                (funcall finished-function))))))
    (dotimes (_ asilea-concurrent-jobs)
      (let* ((current-state (asilea--generate-random-state options random-function))
             (accepted-state current-state)
             (accepted-state-energy nil)
             (temperature asilea-initial-temperature)
             (consider-candidate
              (lambda (energy)
                "Consider the candidate `current-state', possibly accepting it."
                (let ((option-list
                       (asilea--state-to-option-list current-state options)))
                  (with-demoted-errors "Error in `asilea-report-candidate-function': %S"
                    (funcall report-candidate-function option-list energy))
                  (cond
                   ((null accepted-state-energy)
                    (setq accepted-state-energy energy))
                   ((with-demoted-errors "Error in `asilea-acceptance-function': %S"
                      (funcall acceptance-function
                               energy accepted-state-energy temperature random-function))
                    (setq accepted-state current-state)
                    (setq accepted-state-energy energy)
                    (with-demoted-errors "Error in `asilea-solution-accepted-function': %S"
                      (funcall solution-accepted-function option-list energy)))))))
             (sentinel nil)
             (steps-left asilea-max-steps))
        (setq
         sentinel
         (lambda (process process-state)
           (condition-case err
               (progn
                 ;; Consider the candidate only when the process returned zero.
                 (when (string-equal process-state "finished\n")
                   (let ((energy
                          (with-demoted-errors "Error in `asilea-parse-energy-function': %S"
                            (funcall parse-energy-function
                                     (with-current-buffer (process-buffer process)
                                       (buffer-string))))))
                     (when energy
                       (funcall consider-candidate energy))))
                 (if (if steps-left (<= (cl-decf steps-left) 0)
                       (<= temperature final-temperature))
                     (funcall finish-job)
                   ;; We're not done, spawn another process with a new state and
                   ;; continue.
                   (setq temperature (* temperature cooling-rate-inv))
                   (setq current-state (asilea--neighboring-state
                                        accepted-state options random-function))
                   (set-process-sentinel
                    (let ((default-directory starting-directory))
                      (asilea--start-process program current-state options))
                    sentinel)))
             (error
              ;; Something went very wrong, no point in trying to continue.
              (funcall finish-job)
              (signal (car err) (cdr err))))))
        (set-process-sentinel
         (asilea--start-process program current-state options)
         sentinel)))))

(defun asilea-run-synchronously (program options)
  "Synchronous version of `asilea-run'.
It won't return until the simulated annealing process is
finished.

For more details and the meaning of PROGRAM and OPTIONS,
see `asilea-run'."
  (let* ((finished-function asilea-finished-function)
         (should-wait-p t)
         (asilea-finished-function
          (lambda ()
            (setq should-wait-p nil)
            (funcall finished-function))))
    (asilea-run program options)
    (while should-wait-p
      (accept-process-output))))

(defun asilea-default-acceptance-function
    (new-energy old-energy temperature random-function)
  "The default acceptance function.
Smaller energies are favored.

NEW-ENERGY, OLD-ENERGY, TEMPERATURE and RANDOM-FUNCTION are as in
`asilea-acceptance-function'."
  (> (exp (/ (- old-energy new-energy)
             temperature))
     (funcall random-function 1.0)))


;;; Private functions

(defun asilea--sanitize-variables ()
  "Raise an error if there's something wrong with the configuration variables."
  (unless asilea-max-steps
    (unless asilea-initial-temperature
      (error "At least one of `asilea-max-steps' and `asilea-initial-temperature' must be non-nil"))
    (unless asilea-final-temperature
      (error "At least one of `asilea-max-steps' and `asilea-final-temperature' must be non-nil"))))

(defun asilea--initial-temperature ()
  "Get the initial temperature.
Use `asilea-initial-temperature' when it's non-nil, otherwise
guess."
  (or asilea-initial-temperature
      ;; Try to choose an initial temperature so that the last annealing step
      ;; has a temperature of just around 1.
      (fceiling (expt (/ 1.0 (- 1.0 asilea-cooling-rate)) asilea-max-steps))))

(defun asilea--state-to-option-list (state options)
  "Convert the internal state format to an option list.

STATE is the internal state to convert.
OPTIONS is a vector of options as passed to `asilea-run'."
  (let ((result '()))
    (dotimes (i (length state))
      (let ((option (aref (aref options i) (aref state i))))
        (if (listp option)
            (setq result (nconc (reverse option) result))
          (push option result))))
    (nreverse result)))

(defun asilea--neighboring-state (state options random-function)
  "Generate a state neighboring to STATE.

OPTIONS is a vector of options as passed to `asilea-run'.
RANDOM-FUNCTION is as in `asilea-random-generator-function'."
  (setq state (copy-sequence state))
  (let ((i (funcall random-function (length state))))
    (aset state i (funcall random-function (length (aref options i)))))
  state)

(defun asilea--generate-random-state (options random-function)
  "Generate an initial random state.

OPTIONS is a vector of options as passed to `asilea-run'.
RANDOM-FUNCTION is as in `asilea-random-generator-function'."
  (setq options (copy-sequence options))
  (dotimes (i (length options))
    (let ((option (aref options i)))
      (aset options i (funcall random-function (length option)))))
  options)

(defun asilea--start-process (program state options)
  "Start a child process and return it.

PROGRAM is the program to start.
STATE is the internal state representing options to pass.
OPTIONS is a vector of options as passed to `asilea-run'."
  (let ((buffer (generate-new-buffer " *asilea process output*"))
        (option-list (asilea--state-to-option-list state options))
        (process-connection-type nil))
    (apply #'start-process program buffer program option-list)))

(provide 'asilea)
;;; asilea.el ends here
