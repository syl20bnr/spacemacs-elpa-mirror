;;; call-graph.el --- Library to generate call graph for cpp functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph
;; Package-Version: 20180222.2322
;; Version: 0.0.5
;; Keywords: programming, convenience
;; Created: 2018-01-07
;; Package-Requires: ((emacs "25.1") (hierarchy "0.7.0") (tree-mode "1.0.0"))

;; This file is not part of GNU Emacs.

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

;; Library to generate call graph for cpp functions.

;;; Install:

;; Put this file into load-path directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'call-graph)
;;     (global-set-key (kbd "C-c g") 'call-graph)
;;
;;; Usage:

;; "C-c g" => (call-graph) => buffer <*call-graph*> will be generated

;;; Code:

(require 'hierarchy)
(require 'tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup call-graph nil
  "Customization support for the `call-graph'."
  :version "0.0.5"
  :group 'applications)

(defcustom call-graph-initial-max-depth 2
  "The maximum initial depth of call graph."
  :type 'integer
  :group 'call-graph)

(defcustom call-graph-filters '("grep -E \"\\.(cpp|cc):\"")
  "The filters used by `call-graph' when searching caller."
  :type 'list
  :group 'call-graph)

(defcustom call-graph-display-file t
  "Non-nil means display file in another window while moving from one field to another in `call-graph'."
  :type 'boolean
  :group 'call-graph)

(defcustom call-graph-path-to-global nil
  "If non-nil the directory to search global executables."
  :type '(choice (const :tag "Unset" nil) directory)
  :risky t
  :group 'call-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar call-graph--current-depth 0
  "The current depth of call graph.")

(defvar call-graph--default-instance nil
  "Default CALL-GRAPH instance.")

(defvar call-graph--default-hierarchy nil
  "Hierarchy to display call-graph.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (call-graph
               (:constructor nil)
               (:constructor call-graph--make)
               (:conc-name call-graph--))
  (callers (make-hash-table :test 'equal)) ; map func to its callers
  (locations (make-hash-table :test 'equal))) ; map func <- caller to its locations

(defun call-graph-new ()
  "Create a call-graph and return it."
  (call-graph--make))

(defun call-graph--extract-method-name (full-func)
  "Given FULL-FUNC, return a SHORT-FUNC.
e.g: class::method => method."
  (when-let ((full-func-str (symbol-name full-func))
             (temp-split (split-string full-func-str "::"))
             (short-func
              (intern (seq-elt temp-split (1- (seq-length temp-split))))))
    short-func))

(defun call-graph--get-func-caller-key (func caller)
  "Given FUNC, CALLER, return func-caller-key.
Which is used to retrieve location information."
  (when (and func caller)
    (intern (concat (symbol-name func) " <- " (symbol-name caller)))))

(defun call-graph--add-callers (call-graph func callers)
  "In CALL-GRAPH, given FUNC, add CALLERS."
  (when (and call-graph func callers)
    (let ((short-func (call-graph--extract-method-name func))) ; method only
      (unless (map-elt (call-graph--callers call-graph) short-func)
        (seq-doseq (caller callers)
          (let* ((full-caller (car caller)) ; class::method
                 (location (cdr caller)) ; location
                 (func-caller-key
                  (intern (concat (symbol-name func) " <- " (symbol-name full-caller))))) ; "class::callee <- class::caller" as key

            ;; populate caller data
            (push full-caller (map-elt (call-graph--callers call-graph) short-func (list)))

            ;; populate location data
            (map-put (call-graph--locations call-graph) func-caller-key location)))))))

(defun call-graph--get-buffer ()
  "Generate ‘*call-graph*’ buffer."
  (let ((buffer-name "*call-graph*"))
    (get-buffer-create buffer-name)))

(defun call-graph--get-path-to-global ()
  "Return path to program GNU GLOBAL."
  (if call-graph-path-to-global
      (expand-file-name "global" call-graph-path-to-global)
    "global"))

(defun call-graph--find-caller (reference)
  "Given a REFERENCE, return the caller as (caller . location)."
  (when-let ((tmp-val (split-string reference ":"))
             (file-name (seq-elt tmp-val 0))
             (line-nb-str (seq-elt tmp-val 1))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb)))
    (let ((location (concat file-name ":" line-nb-str))
          caller)
      (with-temp-buffer
        (insert-file-contents-literally file-name)
        ;; TODO: leave only hooks on which 'which-function-mode depends
        ;; (set (make-local-variable 'c++-mode-hook) nil)
        (c++-mode)
        (which-function-mode t)
        (forward-line line-nb)
        (setq caller (which-function)))
      (when caller
        (cons (intern caller) location)))))

(defun call-graph--find-references (func)
  "Given a FUNC, return all references as a list."
  (let ((command
         (format "%s -a --result=grep -r %s"
                 (call-graph--get-path-to-global)
                 (shell-quote-argument (symbol-name func))))
        (filter-separator " | ")
        command-filter command-out-put)
    (when (and (> (length call-graph-filters) 0)
               (setq command-filter
                     (mapconcat #'identity (delq nil call-graph-filters) filter-separator))
               (not (string= command-filter filter-separator)))
      (setq command (concat command filter-separator command-filter)))
    (when (setq command-out-put (shell-command-to-string command))
      (split-string command-out-put "\n" t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph--search-callers (call-graph func depth &optional calculate-depth)
  "In CALL-GRAPH, given FUNC, search callers deep to level DEPTH.
CALCULATE-DEPTH is used to calculate actual depth."
  (when-let ((next-depth (and (> depth 0) (1- depth)))
             (calculate-depth (or calculate-depth 1))
             (next-calculate-depth (1+ calculate-depth))
             (short-func (call-graph--extract-method-name func)))
    (let ((caller-pairs (list)))

      ;; callers not found.
      (unless (map-elt (call-graph--callers call-graph) short-func)
        (seq-doseq (reference (call-graph--find-references short-func))
          (when-let ((caller-pair
                      (and reference (call-graph--find-caller reference))))
            (message (format "Search returns: %s" (symbol-name (car caller-pair))))
            (push caller-pair caller-pairs)))
        (call-graph--add-callers call-graph func caller-pairs))

      ;; calculate depth.
      (when (> calculate-depth call-graph--current-depth)
        (setq call-graph--current-depth calculate-depth))

      ;; recursively search callers.
      (seq-doseq (caller (map-elt (call-graph--callers call-graph) short-func))
        (call-graph--search-callers call-graph caller next-depth next-calculate-depth)))))

(defun call-graph--build-hierarchy (call-graph func depth)
  "In CALL-GRAPH, given FUNC, build hierarchy deep to DEPTH level."
  (when-let ((next-depth (and (> depth 0) (1- depth)))
             (hierarchy call-graph--default-hierarchy)
             (short-func (call-graph--extract-method-name func))
             (callers (map-elt (call-graph--callers call-graph) short-func (list))))

    ;; populate hierarchy data.
    (seq-doseq (caller callers)
      (hierarchy-add-tree hierarchy caller (lambda (item) (when (eq item caller) func)))
      (message "insert child %s under parent %s" (symbol-name caller) (symbol-name func)))

    ;; recursively populate callers.
    (seq-doseq (caller callers)
      (call-graph--build-hierarchy call-graph caller next-depth))))

(defun call-graph--display-hierarchy (call-graph)
  "Display CALL-GRAPH in hierarchy."
  (let ((switch-buffer (not (eq major-mode 'call-graph-mode)))
        hierarchy-buffer)
    (setq hierarchy-buffer
          (hierarchy-tree-display
           call-graph--default-hierarchy
           (lambda (tree-item _)
             (let* ((parent (hierarchy-parent call-graph--default-hierarchy tree-item))
                    (func-caller-key (call-graph--get-func-caller-key parent tree-item))
                    (location (map-elt (call-graph--locations call-graph) func-caller-key))
                    (caller (symbol-name tree-item)))
               ;; use propertize to avoid this error => Attempt to modify read-only object
               ;; @see https://stackoverflow.com/questions/24565068/emacs-text-is-read-only
               (insert (propertize caller 'caller-location location))))
           (call-graph--get-buffer)))
    (when switch-buffer
      (switch-to-buffer-other-window hierarchy-buffer))
    (call-graph-mode)
    (call-graph-widget-expand-all)))

(defun call-graph--create (func depth)
  "Generate `call-graph' for FUNC.
DEPTH is the depth of caller-map."
  (when-let ((call-graph call-graph--default-instance))
    (setq call-graph--default-hierarchy (hierarchy-new)
          call-graph--current-depth 0)
    (call-graph--search-callers call-graph func depth)
    (call-graph--build-hierarchy call-graph func depth)
    (call-graph--display-hierarchy call-graph)))

;;;###autoload
(defun call-graph ()
  "Generate `call-graph' for function at point.
With prefix argument, regenerate reference data."
  (interactive)
  (save-excursion
    (when-let ((func (symbol-at-point)))
      (when (or current-prefix-arg (null call-graph--default-instance))
        (setq call-graph--default-instance (call-graph-new)))
      (call-graph--create func call-graph-initial-max-depth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-Graph operation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph-visit-file-at-point ()
  "Visit occurrence on the current line."
  (when-let ((location (get-text-property (point) 'caller-location))
             (tmp-val (split-string location ":"))
             (file-name (seq-elt tmp-val 0))
             (line-nb-str (seq-elt tmp-val 1))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb)))
    (find-file-read-only-other-window file-name)
    (with-no-warnings (goto-line line-nb))))

(defun call-graph-goto-file-at-point ()
  "Go to the occurrence on the current line."
  (interactive)
  (save-excursion
    (when (get-char-property (point) 'button)
      (forward-char 4))
    (call-graph-visit-file-at-point)))

(defun call-graph-display-file-at-point ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (save-selected-window
    (call-graph-goto-file-at-point)))

(defun call-graph-at-point ()
  "Within buffer <*call-graph*>, generate new `call-graph' for symbol at point."
  (interactive)
  (save-excursion
    (when (get-char-property (point) 'button)
      (forward-char 4))
    (call-graph)))

(defun call-graph-quit ()
  "Quit `call-graph'."
  (interactive)
  (kill-this-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget operation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-graph-widget-expand-all ()
  "Iterate all widgets in buffer and expand em."
  (interactive)
  (tree-mode-expand-level 0))

(defun call-graph-widget-collapse-all ()
  "Iterate all widgets in buffer and close em."
  (interactive)
  (goto-char (point-min))
  (tree-mode-expand-level 1))

(defun call-graph-expand (&optional level)
  "Expand `call-graph' by LEVEL."
  (interactive "p")
  (when-let ((call-graph call-graph--default-instance)
             (hierarchy call-graph--default-hierarchy)
             (depth (+ call-graph--current-depth level))
             (func (car (hierarchy-roots hierarchy))))
    (call-graph--create func depth)))

(defun call-graph-collapse (&optional level)
  "Collapse `call-graph' by LEVEL."
  (interactive "p")
  (let ((level (- call-graph--current-depth level)))
    (goto-char (point-min))
    (cond
     ((> level 0)
      (tree-mode-expand-level level)
      (setq call-graph--current-depth level))
     ((<= level 0)
      (tree-mode-expand-level 1)
      (setq call-graph--current-depth 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar call-graph-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "e") 'call-graph-widget-expand-all)
    (define-key map (kbd "c") 'call-graph-widget-collapse-all)
    (define-key map (kbd "p") 'widget-backward)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "q") 'call-graph-quit)
    (define-key map (kbd "d") 'call-graph-display-file-at-point)
    (define-key map (kbd "o") 'call-graph-goto-file-at-point)
    (define-key map (kbd "+") 'call-graph-expand)
    (define-key map (kbd "_") 'call-graph-collapse)
    (define-key map (kbd "g") 'call-graph-at-point)
    (define-key map (kbd "<RET>") 'call-graph-goto-file-at-point)
    map)
  "Keymap for `call-graph' major mode.")

;;;###autoload
(define-derived-mode call-graph-mode special-mode "call-graph"
  "Major mode for viewing function's `call graph'.
\\{call-graph-mode-map}"
  :group 'call-graph
  (buffer-disable-undo)
  (setq truncate-lines t
        buffer-read-only t
        show-trailing-whitespace nil)
  (setq-local line-move-visual t)
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (when call-graph-display-file
    (add-hook 'widget-move-hook (lambda () (call-graph-display-file-at-point))))
  (run-mode-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "C-c g") 'call-graph)


(provide 'call-graph)
;;; call-graph.el ends here
