;;; requirejs.el --- Requirejs import manipulation and source traversal.

;; Author: Joe Heyming <joeheyming@gmail.com>
;; URL: https://github.com/joeheyming/requirejs-emacs
;; Package-Version: 20151203.2319
;; Version: 1.1
;; Keywords: javascript, requirejs
;; Package-Requires: ((js2-mode "20150713")(popup "0.5.3")(s "1.9.0")(cl-lib "0.5")(yasnippet "20151011.1823"))

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
;; This module allows you to:
;;  - Sort a define block
;;   - define rules about how to sort shims/aliases
;;  - Helps you add new define paths to a define block
;;   - which uses the above sorting functionality when any arbitrary line is added to the path array.
;;  - Allows you to jump to a module under your cursor as long as it exists in the same requirejs project.

;; installation: put this file under an Emacs Lisp directory, then include this file (require 'requirejs)
;;  Usage:
;; Here is a sample configuration that may be helpful to get going.
;; 
;; (setq requirejs-require-base "~/path/to/your/project")
;; (requirejs-add-alias "jquery" "$" "path/to/jquery-<version>.js")
;;
;; (add-hook 'js2-mode-hook
;;           '(lambda ()
;;              (local-set-key [(super a) ?s ?r ] 'requirejs-sort-require-paths)
;;              (local-set-key [(super a) ?a ?r ] 'requirejs-add-to-define)
;;              (local-set-key [(super a) ?r ?j ] 'requirejs-jump-to-module)
;;              ))
;;

;; (setq requirejs-define-header-hook
;;       '(lambda ()
;;          (insert
;;           (format "// (c) Copyright %s ACME, Inc.  All rights reserved.\n"
;;                   (format-time-string "%Y")))))

;;; Code:

(require 'js2-mode)
(require 'popup)
(require 'cl-lib)
(require 's)
(require 'yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; requirejs-mode customizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defgroup requirejs-mode nil
  "requirejs-mode customizations"
  :group 'development)

(defvar requirejs-var-formatter '(lambda (path basename) nil)
  "Override `requirejs-var-formatter' to add variable formatting rules.
If this returns nil, then we use the basename
 of the path as the default variable name.
If `requirejs-var-formatter' returns a string,
 then we will use that string for the variable name.")

(defvar requirejs-text-suffix "TextStr"
  "Requirejs text! paths are constructed with this string.
We take the basename of the path file and tack on `requirejs-text-suffix'.
Override this if TextStr doesn't work for you.")

(defvar requirejs-tail-path '(lambda (path) nil)
  "Specify if a path belongs at the of the requirejs define block.
Certain teams/companies have guidelines where they always
 put text! paths at the end of the function declaration.
In this case, you would (setq requirejs-tail-path 'your-team-requirements)
`requirejs-tail-path' takes a path and if it should be put at the end,
returns a non-nil value.")

(defvar requirejs-max-list-size 80
  "Specifies how long a list is until we put it on the next line.  Default 80.")

(defvar requirejs-require-base nil
  "`requirejs-require-base' is the base path for looking for javascript files.")

(defvar requirejs-path-formatter '(lambda (a) a)
  "`requirejs-path-formatter' takes a found javascript file and formats it so it can go in a define([...] block.")

(defvar requirejs-popup-keymap
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap popup-menu-keymap)
    (define-key keymap [return] 'popup-select )
    (define-key keymap [tab] 'popup-select )
    keymap)
  "*A keymap for `requirejs-jump-to-module' of `requirejs'.")

(defvar requirejs-mode-map
  (make-sparse-keymap)
  "Keymap for variable `requirejs-mode'.")

(defconst requirejs-snippets-root
  (file-name-directory (or load-file-name
                           (buffer-file-name))))

(defvar requirejs-define-header-hook nil "Function that inserts a header above a define block")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define a structure for storing an alias
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defvar requirejs-aliases (make-hash-table :test 'equal)
  "`requirejs-aliases' is a table that stores user defined aliases.
An alias is added with `requirejs-add-alias'")

(defvar requirejs-alias-var-lookup (make-hash-table :test 'equal)
  "`requirejs-alias-var-lookup' Defines a reverse lookup of variable name to requirejs alias.")

(cl-defstruct requirejs-alias
  "A rule for a special shim in your requirejs conf"
  label ;; The lookup value of an alias
  variableName ;; The desired variable name to place in a define function.
  path) ;; The path that we will sort by.

(defun requirejs-add-alias (label variableName path)
  "Add a requirejs config shim to `requirejs-aliases'.

Argument LABEL The label we are using to look up the alias.
Argument VARIABLENAME The name of the variable
 as it should appear in the define block.
Argument PATH The exact path that is found under `requirejs-require-base'.

Example 1:
 If you want to expose that lookup: knockout, but name the variable ko:
 (requirejs-add-alias \"knockout\" \"ko\" \"knockout\")
 This says that when you sort require PATHs,
  we will interpret knockout as the variable ko:
 define(['knockout'], function(ko) { ... }
 The third string is used to determine how to sort

Example 2: if you include lodash, but you want to sort by 'Lodash', not '_':
 (requirejs-add-alias \"_\", \"_\" \"lodash\")
 This will make sure the define block looks like this:
 define(['knockout', '_'], function(ko, _) { ... }  instead of this:
 define(['_', 'knockout'], function(_, ko) { ... }  because K comes before L"
  (interactive)
  (let ((alias (make-requirejs-alias :label label :variableName variableName :path path)))
    (puthash label alias requirejs-aliases)
    (puthash variableName alias requirejs-alias-var-lookup)
    ))

(defun requirejs-alias-compare(a b)
  "Compare paths ignore case"
  (let* ((alias-a (gethash a requirejs-aliases))
         (alias-b (gethash b requirejs-aliases))
         (shim-a (if alias-a (requirejs-alias-path alias-a) a))
         (shim-b (if alias-b (requirejs-alias-path alias-b) b)))
    (string< shim-a shim-b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode utilities
;;  These utilities use the js2-mode abstract syntax tree to do useful operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(defun requirejs-js2-goto-node (fn)
  "Go to a js2-node using the passed in function.
Returns that node, if any.
Argument FN A js2-node function that takes a js2-node and returns a js2-node."
  (let* ( (node (js2-node-at-point))
          (child-node (funcall fn node)) )
    (if child-node
        (goto-char (js2-node-abs-pos child-node)))
    child-node))


(defun requirejs-js2-goto-parent-node()
  "Goes to your parent node, returns that node."
  (interactive)
  (let ((curnode (js2-node-at-point))
        (node (requirejs-js2-goto-node 'js2-node-parent)))
    (if (eq (js2-node-abs-pos curnode) (js2-node-abs-pos node))
        (progn
          (setq node (js2-node-at-point))
          (goto-char (- (js2-node-abs-pos node) 1))
          (setq node (js2-node-at-point))
          (goto-char (js2-node-abs-pos node))
          node) node)))

(defun requirejs-js2-goto-next-node()
  "Goes to the next sibling, if any.  Returns the sibling"
  (interactive)
  (requirejs-js2-goto-node 'js2-node-next-sibling))


;; The following functions were shamelessly stolen from https://github.com/ScottyB/ac-js2
(defun requirejs-js2-root-or-node ()
  "Return the current node or js2-ast-root node."
  (let ((node (js2-node-at-point)))
    (if (js2-ast-root-p node)
        node
      (js2-node-get-enclosing-scope node))))

(defun requirejs-js2-get-function-name (fn-node)
  "Return the name of the function FN-NODE.
Value may be either function name or the variable name that holds
the function."
  (let ((parent (js2-node-parent fn-node)))
    (if (js2-function-node-p fn-node)
        (or (js2-function-name fn-node)
            (if (js2-var-init-node-p parent)
                (js2-name-node-name (js2-var-init-node-target parent)))))))

(defun requirejs-js2-get-function-node (name scope)
  "Return node of function named NAME in SCOPE."
  (catch 'function-found
    (js2-visit-ast
     scope
     (lambda (node end-p)
       (when (and (not end-p)
                  (string= name (requirejs-js2-get-function-name node)))
         (throw 'function-found node))
       t))
    nil))

(defun requirejs-js2-name-declaration (name)
  "Return the declaration node for node named NAME."
  (let* ((node (requirejs-js2-root-or-node))
         (scope-def (js2-get-defining-scope node name))
         (scope (if scope-def (js2-scope-get-symbol scope-def name) nil))
         (symbol (if scope (js2-symbol-ast-node scope) nil)))
    (if (not symbol)
        (requirejs-js2-get-function-node name scope-def)
      symbol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun requirejs-get-first-define-node()
  "Get the first define node, if any.  Returns the whole js2-EXPR_RESULT node."
  (interactive)
  (let* (
         (root-node (js2-node-root (js2-node-at-point)))
         )
    (cl-first
     (cl-remove-if-not
      (lambda (child)
        (progn
          ;; (message "child: %s" (js2-node-string child))
          (and
           ;; node is an expression
           (= (js2-node-type child) js2-EXPR_RESULT)
           ;; expression is a function call
           (= (js2-node-type (js2-expr-stmt-node-expr child)) js2-CALL)
           ;; function call is 'define'
           (string= (js2-node-string (js2-call-node-target (js2-expr-stmt-node-expr child))) "define")
           )
          ))
      (js2-node-child-list root-node)
      ))
    ))

(defun requirejs-goto-define()
  "Jump to the define at the beginning of the buffer"
  (interactive)
  (let* ((define-node (requirejs-get-first-define-node))
         (define-params)
         (path-array-node))
    (if define-node
        (progn
          (setq define-params (js2-call-node-args (js2-expr-stmt-node-expr define-node)))
          (setq path-array-node  (cl-first define-params))
          ;; (message "node %s" (js2-node-string define-node))
          (goto-char (js2-node-abs-pos path-array-node)))
      (message "define node not found")
      )
    )
  )

(defun requirejs-buffer-has-define ()
  "Determine if this buffer has a define block in the root node."
  (not (equal (requirejs-get-first-define-node) nil) )
  )

(defun requirejs-get-require-paths()
  "Gets all the require paths and returns them without quotes."
  (let* ((node (requirejs-get-first-define-node))
         (define-params (js2-call-node-args (js2-expr-stmt-node-expr node)))
         (path-array-node (cl-first define-params)))
    (or (mapcar 'js2-string-node-value (js2-node-child-list path-array-node)) '())
    ))

(defun requirejs-clear-list()
  "Look at your parent node and clear out all child nodes"
  (interactive)
  (backward-up-list)
  (mark-sexp)
  (kill-region (+ 1(region-beginning)) (- (region-end) 1) )
  (forward-char 1))

(defun requirejs-clear-function-params (fn-node)
  "Remove all function params under FN-NODE."
  (let* ((fn-pos (js2-node-abs-pos fn-node))
         (lp-pos (+ 1 (+ fn-pos (js2-function-node-lp fn-node)))))
    (kill-region
     lp-pos
     (+ fn-pos (js2-function-node-rp fn-node)))
    (goto-char lp-pos)))

(defun requirejs-clear-array-node (array-node)
  "Remove all items in ARRAY-NODE."
  (let* ((array-pos (js2-node-abs-pos array-node)))
    (kill-region
     (+ 1 array-pos)
     (- (js2-node-abs-end array-node) 1))
    (goto-char (+ 1 array-pos))))

(defun requirejs-path-compare(a b)
  "Compare paths ignore case"
  (string< (downcase a) (downcase b)))

(defun requirejs-get-variable-name(path)
  (interactive)
  (let* ((basename (car (last (split-string path "/" )) ))
         (shim (gethash path requirejs-aliases))
         ;; replace dashes with underscore
         (basename (replace-regexp-in-string "-" "_" basename))
         (formatted (funcall requirejs-var-formatter path basename))
         )
    (cond
     ;; if path is a alias
     (shim (requirejs-alias-variableName shim))
     ;; if path is a text! path
     ((string-match "^text!" path)
      (concat (file-name-sans-extension basename) requirejs-text-suffix))
     ;; if path was formatted by requirejs-var-formatter
     (formatted formatted)
     
     ;; default return the basename
     (t basename)
     )))

(defun requirejs-sort-require-paths(&optional other)
  "Sorts the paths inside define, then injects variable names into the corresponding function declaration."
  (interactive)
  (let ( (require-paths (requirejs-get-require-paths))
         (standard-paths '())
         (tail-paths '())
         (final-list '())
         (final-params)
         (cur-node) )

    (if other
        (push other require-paths))
    (setq require-paths (delete-dups require-paths))
    
    ;; Create two lists, standard-paths go at the beginning
    ;;  tail-paths go at the end
    (dolist (elt require-paths)
      (progn
        (if (or (gethash elt requirejs-aliases) (funcall requirejs-tail-path elt))
            (push elt tail-paths)
          (push elt standard-paths))))

    ;; final-list stores the list we will inject into the define block
    (setq final-list
          (append (sort standard-paths 'requirejs-path-compare)
                  (sort tail-paths 'requirejs-alias-compare)))
    (save-excursion
      (requirejs-goto-define)
      (setq cur-node (requirejs-js2-goto-next-node))
      (requirejs-clear-function-params cur-node)

      ;; inject the function variable params
      (setq final-params (mapcar 'requirejs-get-variable-name  final-list))
      (insert (mapconcat 'identity final-params ", "))
      
      (setq cur-node (cl-first (js2-call-node-args (js2-node-parent cur-node))))
      (requirejs-clear-array-node cur-node)
      
      ;; inject the newline separated paths
      (if (> (length final-list) 0)
          (progn
            (insert "\n")
            (insert (mapconcat 'identity (mapcar #'(lambda(a) (concat "'" a "'")) final-list) ",\n"))
            (insert "\n")
            ))
      
      ;; indent the array node
      (let ((end (+ 1 (point))))
        (goto-char (js2-node-abs-pos cur-node))
        (js2-indent-region (point) end))

      ;; eightify the list manually since the syntax tree spacing may be borked at this point.
      (save-excursion
        (search-forward "{")
        (save-restriction
          (narrow-to-region (point-min) (point))
          (js2-parse)
          (requirejs-js2-eightify-define-params)
          )
        )
      ) ;; save-excursion
    ))

(defun requirejs-js2-eightify-define-params()
  (requirejs-goto-define)
  (let* ((fn-node (requirejs-js2-goto-next-node))
         (children (js2-function-node-params fn-node)))
    (if children
        (goto-char (js2-node-abs-pos (car children))))
    (dolist (node children)
      (progn
        (forward-char (+ 2 (js2-node-len node)))
        (if (> (current-column) requirejs-max-list-size)
            (progn
              (delete-char -1)
              (insert "\n")
              (js2-indent-line)
              )
          )
        )
      )
    ) ;; let
  )
  
(defun requirejs-is-define-call (node)
  "Return true if the NODE is a CALL node and it equals 'define'."
  (and
   (= (js2-node-type node) js2-CALL)
   (equal "define" (buffer-substring (js2-node-abs-pos node) (+ 6 (js2-node-abs-pos node)) )) ))

(defun requirejs-jump-to()
  "Goes to the variable declaration of the node under the cursor.  If you are inside a define block's function parameters, `requirejs-jump-to' attempts to call `requirejs-jump-to-module' to go to the corresponding file."
  (interactive)
  (let* ( (node (js2-node-at-point))
          (name (js2-name-node-name node))
          (declaration (requirejs-js2-name-declaration name))
          (define) )
    (if declaration
        (progn
          (setq define (js2-node-parent (js2-node-parent node)))
          (if (requirejs-is-define-call define)
              ;; navigate to corresponding path
              (requirejs-jump-to-module))
          
          ;; jump to the declaration
          (goto-char (js2-node-abs-pos declaration)) )) ))

(defun requirejs-validate-project ()
  "Determines if your requireJS project is valid to be able to run filesystem actions."
  (if (not requirejs-require-base) (error "Please set requirejs-require-base to your requireJS project root")))

(defun requirejs-navigate (filename)
  "Navigates to a file, if it exists.  Errors out if FILENAME is a directory."
  (let ((filepath (concat requirejs-require-base filename)))
    (if (not (file-directory-p filepath))
        (find-file-existing filepath)
      (error (format "Won't navigate, %s is a directory." filepath)))))

(defun requirejs-find-filepath (variableName)
  "Invokes a find command under `requirejs-require-base' and look for VARIABLENAME."
  (message (format "Finding variableName: '%s' ..." variableName))
  (requirejs-validate-project)
  (let* ((command (format "cd %s; find . -name \"%s.js\"" requirejs-require-base variableName))
         (files (split-string (s-trim (shell-command-to-string command)) "\n")))
    ;; (message (format "command = %s" command))
    ;; (message (format "files = %s %s" files (length files)))
    (cond ((equal (car files) "") (error (format "Can't find module: %s" variableName)))
          ((= (length files) 1) (s-trim (cl-first files)))
          ((> (length files) 1) (popup-menu* files :keymap requirejs-popup-keymap))
          )))

(defun requirejs-jump-to-module ()
  "Jump to the file that is behind the variable name under your cursor.
If more than one file matches your variable name,
 requirejs provides a menu to select the appropriate file.
`requirejs-require-base' Must be set in order to execute this function."
  (interactive)
  (requirejs-validate-project)
  (let* (
         (node (js2-node-at-point))
         (name (buffer-substring (js2-node-abs-pos node) (js2-node-abs-end node)))
         (alias (gethash name requirejs-alias-var-lookup))
         (result)
         )
    (message (format "alias = %s" alias))
    (if alias
        (progn
          (setq result (requirejs-alias-path alias))
          (message (format "Jumping to requirejs alias = %s" result))
          (requirejs-navigate result))
      (progn
        (setq result (requirejs-find-filepath name))
        (message (format "Jumping to: %s" result))
        (requirejs-navigate result)
        ))))

(defun requirejs-add-to-define ()
  "Add the path behind the variable name under your cursor to the define block.
If more than one file matches your variable name,
 requirejs provides a menu to prompt for the appropriate file path.
`requirejs-require-base' Must be set in order to execute this function."
  (interactive)
  (requirejs-validate-project)
  (let* ( (node (js2-node-at-point))
          (name (buffer-substring (js2-node-abs-pos node) (js2-node-abs-end node)))
          (alias (gethash name requirejs-alias-var-lookup))
          (result) )
    (message (format "alias = %s" alias))
    (if alias (progn
                (setq result (requirejs-alias-label alias))
                (message (format "Adding requirejs alias = %s" result))
                (requirejs-sort-require-paths result))
      (progn
        (setq result
              (replace-regexp-in-string "^\.\/" ""
                                        (replace-regexp-in-string "\.js$" ""
                                                                  (requirejs-find-filepath name))))
        (message (format "Adding: %s" result))
        (requirejs-sort-require-paths (funcall requirejs-path-formatter result))
        ))))

(defun requirejs-reset-project ()
  "Reset all project specific variables."
  (interactive)
  (setq requirejs-require-base nil)
  (setq requirejs-path-formatter '(lambda (a) a))
  (clrhash requirejs-aliases)
  (clrhash requirejs-alias-var-lookup)
  (setq requirejs-var-formatter '(lambda (path basename) nil))
  (setq requirejs-tail-path '(lambda (path) nil))
  (setq requirejs-define-header-hook nil))

(defun requirejs-js2-eightify-list (&optional line-break)
  "Formats a list (parameters or an array) to be at most 80 characters wide.
Optional argument LINE-BREAK If true,
 we will add a line break around the list that is 80ified."
  (interactive)
  (save-excursion
    (let* ((parent (requirejs-js2-goto-parent-node))
           (orig-pos (point))
           (nodes (js2-node-child-list parent))
           (node-length (length nodes))
           (n (- node-length 1))
           (node (nth n nodes))
           (next-column-spot))
      (goto-char (js2-node-abs-end node))
      (kill-region (point) (progn
                             (search-forward-regexp ")\\|]")
                             (- (point) 1) ))
      ;; Remove whitespace around nodes
      (while (>= n 0)
        (goto-char (js2-node-abs-pos node))
        (setq next-column-spot (point))
        (kill-region (progn
                       (search-backward-regexp "[,(\[]")
                       (+ (point) 1)) next-column-spot)
        (setq n (- n 1))
        (setq node (nth n nodes)))
      (if (looking-at "(")
          (forward-char 1))
      (setq n (+ n 1))
      (if line-break
          (progn
            (insert "\n")
            (js2-indent-line) ))
      (while (< n node-length)
        (setq next-column-spot (current-column)) ;;(+ (current-column) (js2-node-len node)))
        (if (> next-column-spot requirejs-max-list-size)
            (progn
              (delete-char -1) ;; remove the added space
              (insert "\n")
              (js2-indent-line)))
        (goto-char (+ (+(point) (js2-node-len node)) 1))
        (if (looking-at ",")
            (forward-char 1))
        (insert " ")
        (setq n (+ n 1))
        (setq node (nth n nodes)) )
      (delete-char -1)
      (if line-break
          (progn
            (backward-char 1)
            (insert "\n"))) )))


(define-key requirejs-mode-map
  (kbd "C-c sr") 'requirejs-sort-require-paths)

(define-key requirejs-mode-map
  (kbd "C-c ar") 'requirejs-add-to-define)

(define-key requirejs-mode-map
  (kbd "C-c rj") 'requirejs-jump-to-module)

;;;###autoload
(define-minor-mode requirejs-mode
  "Minor mode for handling requirejs imports in a JavaScript file."
  :lighter " RequireJS"
  :keymap requirejs-mode-map)

;;;###autoload
(defun requirejs-snippets-initialize ()
  "Add this directory with '/snippets' to the `yas-snippets-dir' list.
Compile and load the new snippets directory."
  (let ((snip-dir (expand-file-name "snippets" requirejs-snippets-root)))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-compile-directory snip-dir)
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet
  '(requirejs-snippets-initialize))

(provide 'requirejs)

;;; requirejs.el ends here
