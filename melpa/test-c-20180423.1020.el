;;; test-c.el --- quickly test c code

;; Copyright (C) 2017 Aurélien Aptel <aurelien.aptel@gmail.com>

;; Author: Aurélien Aptel <aurelien.aptel@gmail.com>
;; URL: http://github.com/aaptel/test-c
;; Package-Version: 20180423.1020
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Ever wanted to write, compile and run a snippet of C code small
;; enough that making a new file and writing the boiler plates puts
;; you off? Tired of doing M-x compile with the same command?
;; Accumulating small useless variations of "test.c" in your home
;; directory? Here comes `test-c'!

;; Usage:

;; Call M-x `test-c' to open a temporary "*test-c*" buffer. It is
;; prefilled with a skeleton C program (customized through
;; `test-c-default-code') which is then compiled and run.

;; Every following call to `test-c' will compile and run the program
;; and show its ouput in the minibar.

;; You can customize the compilation and run commands from the source
;; itself using special definitions lines (very similar to Emacs file
;; local variables in concept). Those lines must be of the form:
;;
;;                      /*= var: value =*/
;;
;; The 'compile' and 'run' variable are the one used respectively for
;; compiling and running the file. You can refer to other variable
;; from these variables using the $var syntax, similar to the
;; shell. If you refer to a variable which has not been defined it
;; will be passed as is to the shell, who might expand them.

;; The default value of 'compile' and 'run' inserted with the initial
;; skeleton can be customized via the `test-c-default-compile-command'
;; and `test-c-default-run-command' variables.

;; $exe and $src are special variabled defined by test-c that expands
;; to respectively the temporary executable filename and the temporary
;; source file name.

;; You can save the file and keep using Test-C afterwards.

;;; License:

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

;;; Code:

(defgroup test-c nil
  "Customization of Test-C."
  :group 'tools
  :group 'processes
  :group 'c)

(define-minor-mode test-c-mode nil
  :lighter " test-c"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'test-c)
            map))

(defcustom test-c-default-compile-command "gcc -std=c11 -Wall $src -o $exe"
  "Default compile command to use"
  :group 'test-c)

(defcustom test-c-default-run-command "$exe"
  "Default run command to use"
  :group 'test-c)

(defcustom test-c-default-code "
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdint.h>
#include <unistd.h>

int main(int argc, char **argv)
{
    printf(\"%d\\n\", 42);
    return 0;
}
"
  "Default source code to use"
  :group 'test-c)

(defvar-local test-c-buffer nil
  "Is t when the buffer can be used by test-c")

(defun test-c-set-var (key val)
  (save-excursion
    (goto-char (point-min))
    (let (done)
      (while (search-forward-regexp
              (eval `(rx bol "/*=" (* space) ,key (* space) ":" (* space) (group (+? nonl)) (* space) "=*/")) nil t)
        (replace-match val nil nil nil 1)
        (setq done t))
      (when (not done)
        (goto-char (point-min))
        (insert "/*= " key ": " val " =*/\n")))))

(defun test-c-get-var (key val)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward-regexp (eval `(rx bol "/*=" (* space) ,key (* space) ":" (* space) (group (+? nonl)) (* space) "=*/")) nil t)
        (match-string 1)
      nil)))

(defun test-c-get-vars ()
  (save-excursion
    (goto-char (point-min))
    (let ((r (make-hash-table :test 'equal)))
      (while (search-forward-regexp
              (rx bol "/*=" (* space) (group (+? (not space))) (* space) ":"
                  (* space) (group (+? nonl)) (* space) "=*/") nil t)
        (puthash (match-string 1) (match-string 2) r))
      r)))

(defun test-c-interpolate (str env)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (search-forward-regexp (rx "$" (group (+ (not space)))) nil t)
      (let ((v (gethash (match-string 1) env)))
        (when v
          (replace-match v))))
    (buffer-string)))

;;;###autoload
(defun test-c (new-buffer)
  "Compile and run a new test-c buffer (or reuse existing one)."
  (interactive "P")
  (when (not (bound-and-true-p test-c-mode))
    (switch-to-buffer (get-buffer-create "*test-c*"))
    (when (string= (buffer-string) "")
      (erase-buffer)
      (setq test-c-buffer t)
      (insert test-c-default-code)
      (test-c-set-var "run" test-c-default-run-command)
      (test-c-set-var "compile" test-c-default-compile-command)
      (c-mode)
      (test-c-mode)
      (indent-region (point-min) (point-max))
      (goto-char (point-min))
      (search-forward "{" nil t)))
  (let* ((src (buffer-string))
         (env (test-c-get-vars))
         (fn (puthash "src" "/tmp/emacs-test.c" env))
         (exe (puthash "exe" "/tmp/emacs-test" env))
         (compile (test-c-interpolate (gethash "compile" env) env))
         (run (test-c-interpolate (gethash "run" env) env))
         (cmd (concat "( " compile " ) && ( " run " )")))
    (with-temp-file fn
      (insert src))
    (shell-command cmd)))

(provide 'test-c)

;;; test-c.el ends here
