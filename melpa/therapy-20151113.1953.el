;;; therapy.el --- Hooks for managing multiple Python major versions
;;
;; Copyright (c) 2015 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.2
;; Package-Version: 20151113.1953
;; URL: https://github.com/abingham/therapy
;; Package-Requires: ((emacs "24"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; "This is supposed to be torture, not therapy!"  -- Minerva Mayflower
;;
;; Description:
;;
;; Therapy is fundamentally a set of hooks which get executed when the major
;; version of your configured Python interpreter changes. If you regularly need
;; to switch between Python major version in an Emacs session, this is for you!
;;
;; A typical situation where therapy can come in handy is if you've got Python2
;; and Python3 system installations. In this case, certain tools like flake8 and
;; ipython can have different names depending on the major version of Python
;; you're using: flake8 vs. flake8-3 and ipython vs. ipython3, respectively.
;; Since Emacs can use these tools, you need to be able to tell Emacs which
;; commands to use in which situations. Therapy gives you hooks for doing just
;; this.
;;
;; The basic approach is that you add hooks to the `therapy-python2-hooks' and
;; `therapy-python3-hooks' lists. Your hooks will be called when therapy detects
;; that the Python major version has changed, and your hook functions can do
;; things like set the flake8 command, update the `python-shell-interpreter'
;; variable, and so forth.
;;
;; But how does therapy know when the Python major version has changed? It
;; doesn't, really. You have to tell it by calling
;; `therapy-interpreter-changed'. This tells therapy to run the hooks; it will
;; detect the configured major version and run the appropriate hooks.
;; Alternatively, you can call `therapy-set-python-interpreter' which a) sets
;; `python-shell-interpreter' and b) calls the hooks.
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'python)

(defgroup therapy nil
  "Extension for switching between Python2 and Python3 configurations."
  :group 'tools
  :group 'programming)

(defcustom therapy-python2-hooks nil
  "Hook for when Python 2 is activated."
  :group 'therapy
  :type 'hook)

(defcustom therapy-python3-hooks nil
  "Hook for when Python 3 is activated."
  :group 'therapy
  :type 'hook)

;;;###autoload
(defun therapy-interpreter-changed ()
  "Call this when the Python interpreter is changed.

This will run the correct hooks for the new version."
  (interactive)
  (if (string-equal "3" (therapy-python-major-version python-shell-interpreter))
      (run-hooks 'therapy-python3-hooks)
    (run-hooks 'therapy-python2-hooks)))

;;;###autoload
(defun therapy-set-python-interpreter (cmd)
  "Set the `python-shell-interpreter' variable to CMD and run the hooks."
  (interactive
   (list
    (read-shell-command "Interpreter command: ")))
  (set-variable 'python-shell-interpreter cmd)
  (therapy-interpreter-changed))

(defun therapy-python-major-version (interpreter)
  "Find major version of INTERPRETER, a Python interpreter command."
  (let* ((program "\"import sys; print(sys.version_info.major)\"")
         (command (format "%s -c %s" interpreter program))
         (results (shell-command-to-string command)))
    ;; The output has the major version in the firt character.
    (substring results 0 1)))

(provide 'therapy)

;;; therapy.el ends here
