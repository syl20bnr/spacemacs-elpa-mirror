;;; py-gnitset.el --- Run your Python tests any way you'd like
;;
;; Version: 0.1
;; Package-Version: 20170821.1732
;; Copyright (C) 2014 Brandon W Maister
;; Author: Brandon W Maister <quodlibetor@gmail.com>
;; URL: https://www.github.com/quodlibetor/py-gnitset
;;
;; License: GNU GPL version 3, or (at your option) any later version
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Description
;; -----------
;;
;; This minor mode adds some useful functionality for running Python unittest
;; programs from within Emacs.
;;
;; It allows you to run py.test, nose, or custom test runners within various
;; buffer types.  py-gnitset knows about the ``virtualenv-workon`` dir-local, in
;; addition to allowing you to specify explicit paths to any test runner so
;; it's easy to write custom shell scripts that set up your test environment
;; however you want -- see `py-gnitset-test-runner' and
;; `py-gnitset-runner-format' for details.
;;
;; - Compile buffer, via py-gnitset-compile-*: the shortest to type, because
;;   it's generally the most useful.  These buffers have the fancy "click an
;;   error message to go to that line in the file" functionality you expect.
;; - PDB buffer, via py-gnitset-pdb-*: Runs the tests in a `pdb'-buffer, with
;;   the "--pdb" flag appended.  This is particularly useful if you "import pdb;
;;   pdb.set_trace()" within your tests, as the pdb buffer tracks stepping
;;   through code within the associated Emacs code buffer.
;; - Ansi-term buffer, via py-gnitset-term-*: this is a generic escape hatch
;;   for when things aren't working in another buffer type.  I mostly use it for
;;   debugging py-gnitset-mode at this point, although it is also useful if you
;;   prefer ipdb to pdb+emacs pdb mode.
;;
;; In addition to the various buffer types, you can select which funcions to
;; test via the py-gnitset-*-all, py-gnitset-*-module, py-gnitset-*-class
;; commands which all do the obvious things.  And the py-gnitset-*-one
;; commands finds the nearest class *or* def statement and runs that.
;;
;; Additionally, prefixing any command with C-u will allow you to edit it in
;; the minibuffer before creating the buffer that you'll interact with your
;; tests in.  This also gets you M-p/M-n command history navigation for free,
;; which is nice.
;;
;; My goal is to make testing Python in Emacs *obviously* the best way to do it,
;; so if you have ideas for how to improve things please open an issue the github
;; page or send me an email.  This set of commands came from the fact that I use
;; both py.test and nose, and the pytest and nose Emacs runners have different but
;; complementary features, so I kinda hacked them all together, much is owed to
;; both of them.
;;
;; The home page is https://github.com/quodlibetor/py-gnitset
;;
;; Setting Up
;; ----------
;;
;; Adding `(py-gnitset-global-mode)' to your .emacs will attempt to turn on
;; py-gnitset-mode in every `python-mode' buffer.
;;
;; You can try py-gnitset without global installation by just calling "M-x
;; py-gnitset-mode" in any Python buffer.  `py-gnitset-mode' just adds
;; some keybindings to the C-c t map, so if you want to do something fancy you
;; could for example do:
;;
;;    (add-hook 'python-mode-hook
;;               (lambda ()
;;                  (local-set-key (kbd "C-c n") py-gnitset-map)))
;;
;; To bind the keys to the C-c n map instead, with no loss of functionality.
;;
;; Default Bindings
;; ----------------
;;
;; key             binding
;; ---             -------
;; C-c t a         py-gnitset-compile-all
;; C-c t c         py-gnitset-compile-class
;; C-c t m         py-gnitset-compile-module
;; C-c t o         py-gnitset-compile-one
;;
;; C-c t p a       py-gnitset-pdb-all
;; C-c t p c       py-gnitset-pdb-class
;; C-c t p m       py-gnitset-pdb-module
;; C-c t p o       py-gnitset-pdb-one
;;
;; C-c t r a       py-gnitset-term-all
;; C-c t r c       py-gnitset-term-class
;; C-c t r m       py-gnitset-term-module
;; C-c t r o       py-gnitset-term-one
;; C-c t r t       py-gnitset-term-again
;;
;; To Do
;; -----
;;
;; - macro-ize the py-gnitset-*-all/class/module/function duplication, so that
;;   it's just a matter of (def-py-gnitset-generic ...) instead of the current
;;   quadruple replication
;; - Add the ability to save custom command formulations (basically hack
;;   py-gnitset-run to look for the function to run based on an alist,
;;   rather than the static cond list)
;; - Add a history of test runs, instead of just clearing out test buffers.
;; - Add a way to run tests associated with the current *non-test* function
;; - Remove dependency on virtualenv.el-defined variables
;; - Create a new more versatile `py-gnitset-runners' alist into an alist of
;;   ("runner" . 'format) pairs that combines `py-gnitset-test-runner' and
;;   `py-gnitset-runner-format' in a way that doesn't require multiple
;;   dir-locals in the common case of a bunch of projects that use similar
;;   conventions.
;;
;;; Code:

(require 'term)
(require 'compile)

(defgroup py-gnitset nil
  "Customization options for py-gnitset"
  :group 'external
  :prefix 'py-gnitset)

(defcustom py-gnitset-project-root-files
  '("setup.py" "setup.cfg" ".git" ".hg")
  "Files that mark the root of a project for testing.

Used if `py-gnitset-project-root' is not bound."
  :group 'py-gnitset
  :type '(repeat string))

(defcustom py-gnitset-project-root
  nil
  "Directory-local explicit project root directory to use.

Don't set in custom interface, useful in .dir-locals.el to override
Automatic project searching."
  :group 'py-gnitset
  :type '(string))

(defcustom py-gnitset-test-runner
  "py.test"
  "Test runner to use to run tests.

nil means to try to use py.test from `virtualenv-workon', or if
that's not set to use a global py.test command.

To set this for a project you should use dir-locals, for example,
this .dir-locals.el:

    ((nil . ((virtualenv-workon . \"welltested\")
             (virtualenv-default-directory . \"~/projects/welltested/\"))))

is equivalent to this one:

    ((nil . ((virtualenv-workon . \"welltested\")
             (virtualenv-default-directory . \"~/projects/welltested/\")
             (py-gnitset-test-runner .
                \"~/.virtualenvs/welltested/bin/py.test\"))))

Which is equivalent to the following:

    ((nil . ((virtualenv-workon . \"welltested\")
             (virtualenv-default-directory . \"~/projects/welltested/\")
             (py-gnitset-test-runner . \"py.test\"))))

But if you wanted to use nose (which doesn't work for function or
class level things yet) you would probably do something like:

    ((nil . ((virtualenv-workon . \"welltested\")
             (virtualenv-default-directory . \"~/projects/welltested/\")
             (py-gnitset-test-runner . \"nosetests\"))))

And if you have a runner script called in
\"/home/user/projects/welltested/runtests\" you could do:

    ((nil . ((py-gnitset-test-runner . \"runtests\"))))"
  :group 'py-gnitset
  :type '(string))

(defcustom py-gnitset-runner-format
  'pytest
  "How to format args to `py-gnitset-test-runner'.

This value is used internally to figure out how to format
arguments to the test runner, and is only a separate thing
because you can define your own script as the test runner, and
then how do I know how to format the args?

Use it in directory-local variables like so:

    ((nil . ((py-gnitset-test-runner . \"runtests\")
             (py-gnitset-runner-format . 'nose))))

Valid choices are 'pytest and 'nose"
  :group 'py-gnitset
  :type '(symbol pytest nose)
  :options '(pytest nose))


(defvar py-gnitset--run-history nil)
(defvar py-gnitset--source nil)

(let ((anything "[A-Za-z0-9_]"))
  (defconst py-gnitset-def-re (format "\\(def\\|class\\) \\(%s*[tT]est%s*\\)" anything anything))
  (defconst py-gnitset-class-re (format "class \\(%s*[tT]est%s*\\)" anything anything)))

(defun py-gnitset-locate-dominating-file ()
  "Find the project root."
  (cond
   ((and (boundp 'py-gnitset-project-root) py-gnitset-project-root)
    py-gnitset-project-root)
   (t
    (expand-file-name (let ((py-gnitset-dom-re (mapconcat 'identity py-gnitset-project-root-files "\\|")))
                        (locate-dominating-file
                         (buffer-file-name)
                         (lambda (dir)
                           (when (file-directory-p dir)
                             (directory-files dir nil py-gnitset-dom-re)))))))))

(defun py-gnitset-virtualenv-bin-or-current-dir ()
  "Try to find the virtualenv for the local dir, else local dir."
  (cond
   ((and (boundp 'venv-current-name) venv-current-name (boundp 'venv-current-dir))
    (concat (expand-file-name venv-current-dir) "/bin"))
   ((and (boundp 'virtualenv-workon) virtualenv-workon (boundp 'virtualenv-root))
    (concat (expand-file-name virtualenv-root)
            "/" virtualenv-workon "/bin"))
   (t
    (message "Couldn't infer virtualenv, using current directory")
    ".")))

(defun py-gnitset-get-bin ()
  "Try to figure out what file is meant by `py-gnitset-test-runner'.

Look in the current dir, all the ancestors, and the virtualenv."
  (if (file-exists-p py-gnitset-test-runner)
      py-gnitset-test-runner
    (let* ((runner-dir (locate-dominating-file (buffer-file-name)
                                               py-gnitset-test-runner))
           (runner (file-truename (concat runner-dir "/" py-gnitset-test-runner)))
           (venv-runner (concat (py-gnitset-virtualenv-bin-or-current-dir)
                                "/" py-gnitset-test-runner)))
      (cond ((file-exists-p runner)
             runner)
            ((file-exists-p venv-runner)
             (file-truename venv-runner))
            (t
             (error "Couldn't find runner (%s) in virtualenv (%s) or project (%s)"
                    py-gnitset-test-runner venv-runner runner))))))

(defun py-gnitset-local-bufname ()
  "Get the compilation py-gnitset buffer for the currently executing buffer.

Tries to get py-gnitset for the local virtualenv, falling back to global.

Doesn't work in compilation buffers because dir-local variables aren't set"
  (cond
   ((boundp 'venv-current-name)
    (format "*py-gnitset-%s*" venv-current-name))
   ((boundp 'pyvenv-virtual-env)
    (format "*py-gnitset-%s*" pyvenv-virtual-env))
   ((boundp 'virtualenv-workon)
    (format "*py-gnitset-%s*" virtualenv-workon))
   (t "*py-gnitset*")))

(defun py-gnitset-term-sentinel (proc msg)
  "Function to monitor the py-gnitset-term.

PROC is the process created by `py-gnitset-run' with the 'ansi style.
MSG is the message passed by `set-process-sentinel' within
py-gnitset-run."
  (term-sentinel proc msg)
  (when (memq (process-status proc) '(signal exit))
    (with-current-buffer (process-buffer proc)
      (setq buffer-read-only t)
      (local-set-key "q" 'quit-window)
      (local-set-key "g" 'py-gnitset-term-again))))

(defun py-gnitset-run (cmdline show-prompt style)
  "The test dispatcher, given a command and a style execute it.

This is the underlying runner used by the py-gnitset-STYLE-SELECTOR
functions.

CMDLINE is the command line string generated by the command.
SHOW-PROMPT t or nil, describes whether to show the CMDLINE in
the minibuffer before executing.
STYLE should be one of 'compile, 'ansi, or 'pdb, and descibes the
kind of buffer to run the process in, as well as some extra args
needed by that style."
  (let* ((default-directory (py-gnitset-locate-dominating-file))
         (process-environment (list (format "PWD=%s" default-directory)))
         (cmdline (if show-prompt
                      (read-shell-command "Run: " cmdline
                                          'py-gnitset--run-history)
                    cmdline))
         (bufname (py-gnitset-local-bufname)))
    (cond
     ((equal style 'ansi)
      (let ((term-buffer (get-buffer-create bufname)))
        (if (not (equal (current-buffer) term-buffer))
            (switch-to-buffer-other-window term-buffer))
        (if (get-buffer-process (current-buffer))
            (term-kill-subjob))
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert cmdline)
        (newline)
        (term-ansi-make-term bufname "/bin/sh" nil "-c" (concat  cmdline " -s"))
        (term-char-mode)
        (let ((proc (get-buffer-process term-buffer)))
          ; override the default sentinel set by term-ansi-make-term
          (set-process-sentinel proc 'py-gnitset-term-sentinel))
        (set (make-local-variable 'show-trailing-whitespace) nil)))
     ((equal style 'compile)
      (let ((cmdline (concat "cd " default-directory " ; " cmdline)))
        (compilation-start cmdline nil
                           (lambda (mode)
                             (if (and (boundp 'py-gnitset--source) py-gnitset--source)
                                 (with-current-buffer py-gnitset--source
                                   (py-gnitset-local-bufname))
                               (py-gnitset-local-bufname)))))
      (let ((buf (current-buffer)))
        (with-current-buffer (py-gnitset-local-bufname)
          (set (make-local-variable 'py-gnitset--source) buf)
          (set (make-local-variable 'show-trailing-whitespace) nil)
          (py-gnitset-compilation-mode))))
     ((equal style 'pdb)
      (pdb (concat cmdline " -s --pdb"))))))

;;;###autoload
(defun py-gnitset-recompile ()
  "Basically just advice `recompile' to use the original source.

This is because recompile creates a new buffer, instead of using
the current one, meaning that buffer-local variables get reset on
every recompilation, meaning that it's hard to know where to go."
  (interactive)
  (let* ((source-buffer py-gnitset--source)
         (compile-buffer (with-current-buffer source-buffer
                    (py-gnitset-local-bufname))))
    (recompile)
    (with-current-buffer compile-buffer
      (set (make-local-variable 'py-gnitset--source) source-buffer)
      (set (make-local-variable 'show-trailing-whitespace) nil))))

;;; Locator functions -- find the thing to test
(defun py-gnitset-arg-from-path (path)
  "Run either the current test file, or the current directory.

If the current file name (PATH) ends or starts with \"test\" it
will be selected, otherwise we assume that you're in a directory
containing tests (e.g. editing conftest.py) and want all of them
to be run"
  (let ((filename (file-name-nondirectory path)))
    (file-truename
     (if (or (string-match "test_.*\\.py$" filename)
             (string-match ".*_test\\.py$" filename)
             (file-directory-p path))
         path
       (file-name-directory path)))))

(defun py-gnitset-current-function-name ()
  "Return the name of the function that point is within."
  (save-excursion
    (if (search-backward-regexp py-gnitset-def-re)
        (match-string 2)
      nil)))

(defun py-gnitset-current-class-name ()
  "Return the name of the class that point is within."
  (save-excursion
    (when (search-backward-regexp py-gnitset-class-re)
      (match-string 1))))

(defun py-gnitset-all-command ()
  "Create a full command line."
  (format "%s %s "
          (py-gnitset-get-bin)
          (file-truename (py-gnitset-locate-dominating-file))))

(defun py-gnitset-module-command ()
  "Get the command for the current file."
  (format "%s %s "
          (py-gnitset-get-bin)
          (py-gnitset-arg-from-path (buffer-file-name))))

(defun py-gnitset-test-specifier ()
  "Get the correct def/class specifier for different runners.

    * py.test -> 'module -k filter'
    * nose    -> 'module:filter'

This is based on the `py-gnitset-runner-format'"
  (cond ((eq py-gnitset-runner-format
             'pytest)
         "%s -k %s")
        ((eq py-gnitset-runner-format
             'nose)
         "%s:%s")
        (t
         (error "No matching py-gnitset-runner format for format '%s"
                py-gnitset-runner-format))))

(defun py-gnitset-class-command ()
  "Get the command for the current file."
  (format (py-gnitset-test-specifier)
          (py-gnitset-module-command)
          (py-gnitset-current-class-name)))

(defun py-gnitset-function-command ()
  "Get the command for the current function."
  (format (py-gnitset-test-specifier)
          (py-gnitset-module-command)
          (py-gnitset-current-function-name)))

;;; temp
;; (defmacro compile-group (tag)
;;   (let (fun)
;;     (dolist (group ("function" "file" "all"))
;;       `(defun ,(intern (concat "py-gnitset-" tag "-" group) (show-prompt))
;;          (interactive "P")
;;          (py-gnitset-run ,(intern (concat "py-gnitset-" group "-command")) show-prompt ,(intern tag))))))


;;; Actual test functions
;;;###autoload
(defun py-gnitset-compile-all (show-prompt)
  "Execute all the tests in a `compilation-mode' buffer.

Makes jumping to failures and original files very easy.

SHOW-PROMPT, when t, means to show the command for editing in the
minibuffer before it is executed."
  (interactive "P")
  (py-gnitset-run (py-gnitset-all-command) show-prompt 'compile))

;;;###autoload
(defun py-gnitset-compile-module (show-prompt)
  "Execute this module's tests in a compilation buffer.

See `py-gnitset-compile-all' for details about usage and
SHOW-PROMPT"
  (interactive "P")
  (py-gnitset-run (py-gnitset-module-command) show-prompt 'compile))

;;;###autoload
(defun py-gnitset-compile-class (show-prompt)
  "Execute the enclosing class's tests in a compilation buffer.

See `py-gnitset-compile-all' for details about usage and
SHOW-PROMPT"
  (interactive "P")
  (py-gnitset-run (py-gnitset-class-command) show-prompt 'compile))

;;;###autoload
(defun py-gnitset-compile-one (show-prompt)
  "Run this function as a test in a compilation buffer.

See `py-gnitset-compile-all' for details about usage and
SHOW-PROMPT"
  (interactive "P")
  (py-gnitset-run (py-gnitset-function-command) show-prompt 'compile))

;;;###autoload
(defun py-gnitset-term-all (show-prompt)
  "Run py.test on the current test.

'Run' in this context means 'execute in an ANSI term.  This should
work better with things like `import ipdb; ipdb.set_trace()', if
you just to use the --pdb flag use `py-gnitset-pdb-all' and related
functions.

SHOW-PROMPT, when t, means to show the command for editing in the
minibuffer before it is executed."
  (interactive "P")
  (py-gnitset-run (py-gnitset-all-command) show-prompt 'ansi))

;;;###autoload
(defun py-gnitset-term-module (show-prompt)
  "Run py.test on the current file in an interactive test buffer.

See `py-gnitset-term-all' for details about usage and
SHOW-PROMPT"
  (interactive "P")
  (py-gnitset-run (py-gnitset-module-command) show-prompt 'ansi))

;;;###autoload
(defun py-gnitset-term-class (show-prompt)
  "Run py.test on the current file in an interactive test buffer

See `py-gnitset-term-all' for details"
  (interactive "P")
  (py-gnitset-run (py-gnitset-class-command) show-prompt 'ansi))

;;;###autoload
(defun py-gnitset-term-one (show-prompt)
  "Run py.test on the current test.

See `py-gnitset-term-all' for details about usage and
SHOW-PROMPT"
  (interactive "P")
  (py-gnitset-run (py-gnitset-function-command) show-prompt 'ansi))

;;;###autoload
(defun py-gnitset-term-again (show-prompt)
  "Re-run the last py.test command.

If prefixed by C-u SHOW-PROMPT is true, and this function lets
you edit the command in the minibuffer before executing it."
  (interactive "P")
  (if (not py-gnitset--run-history)
      (message "No preceding py-gnitset commands in history")
    (let ((cmdline (car py-gnitset--run-history)))
      (py-gnitset-run cmdline show-prompt 'ansi))))

;;;###autoload
(defun py-gnitset-pdb-all (show-prompt)
  "Run all every test in a pdb buffer.

This will drop into pdb mode on error, and should give you all
the niceties associated with it (e.g. automatic buffer movement
when you step into new functions).

SHOW-PROMPT, set by C-u, means to show the command that will be
executed within the minibuffer for editing before running."
  (interactive "P")
  (py-gnitset-run (py-gnitset-all-command) show-prompt 'pdb))

;;;###autoload
(defun py-gnitset-pdb-module (show-prompt)
  "Run current module in a pdb buffer.

See `py-gnitset-pdb-all' for details about usage and
SHOW-PROMPT"
  (interactive "P")
  (py-gnitset-run (py-gnitset-module-command) show-prompt 'pdb))

;;;###autoload
(defun py-gnitset-pdb-class (show-prompt)
  "Run current module in a pdb buffer.

See `py-gnitset-pdb-all' for details about usage and
SHOW-PROMPT"
  (interactive "P")
  (py-gnitset-run (py-gnitset-class-command) show-prompt 'pdb))

;;;###autoload
(defun py-gnitset-pdb-one (show-prompt)
  "Run current function in a pdb buffer.

See `py-gnitset-pdb-all' for details about usage and
SHOW-PROMPT"
  (interactive "P")
  (py-gnitset-run (py-gnitset-function-command) show-prompt 'pdb))


;;; Mode-general things
(defvar py-gnitset-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") 'py-gnitset-compile-one)
    (define-key map (kbd "c") 'py-gnitset-compile-class)
    (define-key map (kbd "m") 'py-gnitset-compile-module)
    (define-key map (kbd "a") 'py-gnitset-compile-all)
    (define-key map (kbd "r o") 'py-gnitset-term-one)
    (define-key map (kbd "r c") 'py-gnitset-term-class)
    (define-key map (kbd "r m") 'py-gnitset-term-module)
    (define-key map (kbd "r a") 'py-gnitset-term-all)
    (define-key map (kbd "r t") 'py-gnitset-term-again)
    (define-key map (kbd "p o") 'py-gnitset-pdb-one)
    (define-key map (kbd "p c") 'py-gnitset-pdb-class)
    (define-key map (kbd "p m") 'py-gnitset-pdb-module)
    (define-key map (kbd "p a") 'py-gnitset-pdb-all)
    map)
  "All the specific Python testing commands.")

(define-prefix-command 'py-gnitset-mode-map)
(define-key py-gnitset-mode-map (kbd "C-c t") py-gnitset-map)

;;;###autoload
(define-minor-mode py-gnitset-mode
  "Add commands to run tests in Python buffers

The primary entry point is `py-gnitset-all', but see also
`py-gnitset-term-all' which runs tests in a an ansi-term buffer,
`py-gnitset-pdb-all' which runs tests in a pdb buffer. Additionally
there are 'py-gnitset-*-module', -class, and -function commands.

\\{py-gnitset-mode-map}"
  :keymap py-gnitset-mode-map
  :group 'py-gnitset)


;;;###autoload
(defvar py-gnitset-compilation-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'py-gnitset-recompile)
    (define-key map (kbd "q") 'quit-window)
    map))

(define-minor-mode py-gnitset-compilation-mode
  "Customize compilation mode"
  :keymap py-gnitset-compilation-mode-map
  :group 'py-gnitset)

;;;###autoload
(defun turn-on-py-gnitset-mode ()
  "Turn on py-gnitset-mode iff this is a `python-mode' buffer."
  (interactive)
  (when (eq major-mode 'python-mode)
    (py-gnitset-mode)))

(define-globalized-minor-mode py-gnitset-global-mode
  py-gnitset-mode
  turn-on-py-gnitset-mode
  :group 'py-gnitset)

(provide 'py-gnitset)

;;; py-gnitset.el ends here
