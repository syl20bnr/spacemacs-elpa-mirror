;;; call-graph.el --- Library to generate call graph for c/c++ functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Huming Chen

;; Author: Huming Chen <chenhuming@gmail.com>
;; Maintainer: Huming Chen <chenhuming@gmail.com>
;; URL: https://github.com/beacoder/call-graph
;; Package-Version: 20180508.1924
;; Version: 0.1.0
;; Keywords: programming, convenience
;; Created: 2018-01-07
;; Package-Requires: ((emacs "25.1") (cl-lib "0.6.1") (hierarchy "0.7.0") (tree-mode "1.0.0") (ivy "0.10.0"))

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

;; Library to generate call graph for c/c++ functions.

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

(require 'cl-lib)
(require 'hierarchy)
(require 'tree-mode)
(require 'ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup call-graph nil
  "Customization support for the `call-graph'."
  :version "0.1.0"
  :group 'applications)

(defcustom cg-initial-max-depth 2
  "The maximum initial depth of call graph."
  :type 'integer
  :group 'call-graph)

(defcustom cg-search-filters '("grep -E \"\\.(cpp|cc|c):\"")
  "The filters used by `call-graph' when searching caller."
  :type 'list
  :group 'call-graph)

(defcustom cg-display-file t
  "Non-nil means display file in another window while moving from one field to another in `call-graph'."
  :type 'boolean
  :group 'call-graph)

(defcustom cg-path-to-global nil
  "If non-nil the directory to search global executables."
  :type '(choice (const :tag "Unset" nil) directory)
  :risky t
  :group 'call-graph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cg-persist-caller-filters nil
  "The alist form of `cg--caller-filters'.")

(defvar cg--caller-filters nil
  "The filters describing caller relations, used when building caller-map.")

(defvar cg--current-depth 0
  "The current depth of call graph.")

(defvar cg--default-instance nil
  "Default CALL-GRAPH instance.")

(defvar cg--default-hierarchy nil
  "Hierarchy to display call-graph.")

(defvar cg--window-configuration nil
  "The window configuration to be restored upon closing the buffer.")

(defvar cg--selected-window nil
  "The currently selected window.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defstruct (call-graph
               (:constructor nil)
               (:constructor call-graph--make)
               (:conc-name call-graph--))
  (callers (make-hash-table :test #'equal)) ; map func to its callers
  (locations (make-hash-table :test #'equal))) ; map func <- caller to its locations

(defun cg-new ()
  "Create a call-graph and return it."
  (call-graph--make))

(defun cg--add-callers (call-graph func callers)
  "In CALL-GRAPH, given FUNC, add CALLERS."
  (when (and call-graph func callers)
    (let* ((short-func (cg--extract-method-name func))) ; method only
      (unless (map-elt (call-graph--callers call-graph) short-func)
        (seq-doseq (caller callers)
          (let* ((full-caller (car caller)) ; class::method
                 (location (cdr caller)) ; location
                 (func-caller-key
                  (intern (concat (symbol-name short-func) " <- " (symbol-name full-caller))))) ; "callee <- class::caller" as key

            ;; populate caller data
            (cl-pushnew full-caller (map-elt (call-graph--callers call-graph) short-func (list)))

            ;; populate location data
            (cl-pushnew location (map-elt (call-graph--locations call-graph) func-caller-key (list))
                        :test #'equal)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Persitence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg/prepare-persistent-data ()
  "Prepare data for persistence."
  (when cg--caller-filters
    (setq cg-persist-caller-filters
          (map-into cg--caller-filters 'list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--extract-method-name (full-func)
  "Given FULL-FUNC, return a SHORT-FUNC.
e.g: class::method => method."
  (when-let ((full-func-str (symbol-name full-func))
             (temp-split (split-string full-func-str "::"))
             (short-func
              (intern (seq-elt temp-split (1- (seq-length temp-split))))))
    short-func))

(defun cg--get-func-caller-location (call-graph func caller)
  "In CALL-GRAPH, given FUNC and CALLER, return the caller postion."
  (when (and call-graph func caller)
    (let ((locations (call-graph--locations call-graph))
          (func-caller-key
           (if (eq 'root-function func) 'root-function ; special treatment for root-function
             (intern (concat (symbol-name (cg--extract-method-name func)) " <- " (symbol-name caller))))))
      (map-elt locations func-caller-key))))

(defun cg--get-buffer ()
  "Generate ‘*call-graph*’ buffer."
  (let ((buffer-name "*call-graph*"))
    (get-buffer-create buffer-name)))

(defun cg--get-path-to-global ()
  "Return path to program GNU GLOBAL."
  (let ((path (if cg-path-to-global (expand-file-name "global" cg-path-to-global) "global")))
    (if (file-exists-p path) path
      (error "Failed to find \"GNU GLOBAL\" in path: %s" path))))

(defun cg--dwim-at-point ()
  "If there's an active selection, return that.
Otherwise, get the symbol at point."
  (if (use-region-p)
      (prog1 (intern (buffer-substring-no-properties (region-beginning) (region-end)))
        (deactivate-mark))
    (symbol-at-point)))

(defun cg--visit-function (func-location)
  "Visit function location FUNC-LOCATION."
  (when-let ((temp-split (split-string func-location ":"))
             (file-name (car temp-split))
             (line-nb-str (cadr temp-split))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb)))
    (find-file-read-only-other-window file-name)
    (with-no-warnings (goto-line line-nb))))

(defun cg--find-caller (reference)
  "Given a REFERENCE, return the caller as (caller . location)."
  (when-let ((tmp-split (split-string reference ":"))
             (file-name (car tmp-split))
             (line-nb-str (cadr tmp-split))
             (line-nb (string-to-number line-nb-str))
             (is-valid-file (file-exists-p file-name))
             (is-valid-nb (integerp line-nb)))
    (let ((location (concat file-name ":" line-nb-str))
          (caller nil))
      (with-temp-buffer
        (insert-file-contents-literally file-name)
        ;; TODO: leave only hooks on which 'which-function-mode depends
        ;; (set (make-local-variable 'c++-mode-hook) nil)
        (c++-mode)
        (goto-char (point-min))
        (while (re-search-forward "__attribute__[ \t\n]*(([[:alpha:]]+))" nil t)
          (replace-match "__attribute__" t nil)) ; imenu failed to parse function with __attribute__ ((...)) as args
        (which-function-mode t)
        (goto-char (point-min))
        (forward-line (1- line-nb))
        (setq caller (which-function)))
      (when caller
        (cons (intern caller) location)))))

(defun cg--find-references (func)
  "Given a FUNC, return all references as a list."
  (let ((command
         (format "%s -a --result=grep -r %s"
                 (cg--get-path-to-global)
                 (shell-quote-argument (symbol-name func))))
        (filter-separator " | ")
        command-filter command-out-put)
    (when (and (> (length cg-search-filters) 0)
               (setq command-filter
                     (mapconcat #'identity (delq nil cg-search-filters) filter-separator))
               (not (string= command-filter filter-separator)))
      (setq command (concat command filter-separator command-filter)))
    (when (setq command-out-put (shell-command-to-string command))
      (split-string command-out-put "\n" t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg--search-callers (call-graph func depth)
  "In CALL-GRAPH, given FUNC, search callers deep to level DEPTH."
  (when-let ((next-depth (and (> depth 0) (1- depth)))
             (short-func (cg--extract-method-name func)))
    (let ((caller-pairs (list))
          (callers (map-elt (call-graph--callers call-graph) short-func (list))))

      ;; callers not found.
      (unless callers
        (seq-doseq (reference (cg--find-references short-func))
          (when-let ((caller-pair (and reference (cg--find-caller reference))))
            (message (format "Search returns: %s" (symbol-name (car caller-pair))))
            (push caller-pair caller-pairs)))
        (cg--add-callers call-graph func caller-pairs)
        (setq callers (map-elt (call-graph--callers call-graph) short-func (list))))

      ;; recursively search callers.
      (seq-doseq (caller callers)
        (cg--search-callers call-graph caller next-depth)))))

(defun cg--build-hierarchy (call-graph func depth &optional calculate-depth)
  "In CALL-GRAPH, given FUNC, build hierarchy deep to level DEPTH.
CALCULATE-DEPTH is used to calculate actual depth."
  (when-let ((next-depth (and (> depth 0) (1- depth)))
             (calculate-depth (or calculate-depth 1))
             (next-calculate-depth (1+ calculate-depth))
             (hierarchy cg--default-hierarchy)
             (short-func (cg--extract-method-name func))
             (callers
              (or (map-elt cg--caller-filters func (list))
                  (map-elt (call-graph--callers call-graph) short-func (list)))))

    ;; populate hierarchy data.
    (seq-doseq (caller callers)
      (hierarchy-add-tree
       hierarchy caller
       (lambda (item) (when (eq item caller) (put caller 'caller-depth calculate-depth) func)))
      (message "Insert child %s under parent %s" (symbol-name caller) (symbol-name func)))

    ;; recursively populate callers.
    (seq-doseq (caller callers)
      (cg--build-hierarchy call-graph caller next-depth next-calculate-depth))))

(defun cg--display-hierarchy ()
  "Display `call-graph' in hierarchy."
  (let ((switch-buffer (not (eq major-mode 'call-graph-mode)))
        hierarchy-buffer)
    (setq hierarchy-buffer
          (hierarchy-tree-display
           cg--default-hierarchy
           (lambda (tree-item _)
             (let ((depth (get tree-item 'caller-depth))
                   (caller (symbol-name tree-item))
                   (parent (or (hierarchy-parent cg--default-hierarchy tree-item) 'root-function)))

               ;; calculate depth.
               (and depth (> depth cg--current-depth) (setq cg--current-depth depth))

               ;; use propertize to avoid this error => Attempt to modify read-only object
               ;; @see https://stackoverflow.com/questions/24565068/emacs-text-is-read-only
               (insert (propertize caller 'caller-name tree-item 'callee-name parent))))
           (cg--get-buffer)))
    (when switch-buffer
      (switch-to-buffer-other-window hierarchy-buffer))
    (call-graph-mode)
    (cg/widget-expand-all)))

(defun cg--create (call-graph func depth)
  "Generate CALL-GRAPH for FUNC, DEPTH is the depth of caller-map."
  (when (and call-graph func depth)
    (setq cg--default-hierarchy (hierarchy-new)
          cg--current-depth 0)
    (cg--search-callers call-graph func depth)
    (cg--build-hierarchy call-graph func depth)
    (cg--display-hierarchy)))

(defun cg--initialize ()
  "Initialize data for `call-graph'."
  (when (or current-prefix-arg (null cg--default-instance))
    (setq cg--default-instance (cg-new))) ; clear cached reference

  (when (null cg--caller-filters)
    (if cg-persist-caller-filters ; load filters from saved session
        (setq cg--caller-filters (map-into cg-persist-caller-filters 'hash-table)
              cg-persist-caller-filters nil)
      (setq cg--caller-filters (make-hash-table :test #'equal)))))

;;;###autoload
(defun call-graph (&optional func)
  "Generate `call-graph' for FUNC / func-at-point / func-in-active-rigion.
With prefix argument, discard cached data and re-generate reference data."
  (interactive (list (cg--dwim-at-point)))
  (when func
    (cg--initialize)
    (let ((call-graph cg--default-instance)
          (window-configuration (current-window-configuration))
          (selected-window (frame-selected-window)))

      (when-let ((file-name (buffer-file-name))
                 (line-nb (line-number-at-pos))
                 (location (concat file-name ":" (number-to-string line-nb))))
        ;; save root function location
        (setf (map-elt (call-graph--locations call-graph) 'root-function) (list location)))

      (save-mark-and-excursion
       (cg--create call-graph func cg-initial-max-depth)
       (setq cg--window-configuration window-configuration
             cg--selected-window selected-window)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call-Graph Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg/visit-file-at-point ()
  "Visit occurrence on the current line."
  (when-let ((call-graph cg--default-instance)
             (callee (get-text-property (point) 'callee-name))
             (caller (get-text-property (point) 'caller-name))
             (locations (cg--get-func-caller-location call-graph callee caller))
             (location (car locations)))
    (cg--visit-function location)
    (setq cg--window-configuration (current-window-configuration)
          cg--selected-window (frame-selected-window)) ; update window configuration
    (when (> (seq-length locations) 1)
      (message "Multiple locations for this function, select with `cg/select-caller-location'"))))

(defun cg/goto-file-at-point ()
  "Go to the occurrence on the current line."
  (interactive)
  (save-mark-and-excursion
   (when (get-char-property (point) 'button)
     (forward-char 4))
   (cg/visit-file-at-point)))

(defun cg/display-file-at-point ()
  "Display in another window the occurrence the current line describes."
  (interactive)
  (save-selected-window
    (cg/goto-file-at-point)))

(defun cg/at-point ()
  "Within buffer <*call-graph*>, generate new `call-graph' for symbol at point."
  (interactive)
  (save-mark-and-excursion
   (when (get-char-property (point) 'button)
     (forward-char 4))
   (when-let ((caller (get-text-property (point) 'caller-name)))
     (call-graph caller))))

(defun cg/select-caller-location ()
  "Select caller location as default location for function at point."
  (interactive)
  (save-mark-and-excursion
   (when (get-char-property (point) 'button)
     (forward-char 4))
   (when-let ((call-graph cg--default-instance)
              (callee (get-text-property (point) 'callee-name))
              (caller (get-text-property (point) 'caller-name))
              (func-caller-key
               (intern (concat (symbol-name (cg--extract-method-name callee)) " <- " (symbol-name caller))))
              (locations (cg--get-func-caller-location call-graph callee caller))
              (has-many (> (seq-length locations) 1)))
     (ivy-read "Caller Locations:" locations
               :action (lambda (func-location)
                         (while (not (equal func-location (car locations)))
                           (setq locations
                                 (nconc (cdr locations) (cons (car locations) ())))) ; put selected location upfront
                         (setf (map-elt (call-graph--locations call-graph) func-caller-key) locations)
                         (cg--visit-function func-location))))))

(defun cg/remove-caller ()
  "Within buffer <*call-graph*>, remove caller at point."
  (interactive)
  (when (get-char-property (point) 'button)
    (forward-char 4))
  (when-let ((call-graph cg--default-instance)
             (callee (get-text-property (point) 'callee-name))
             (caller (get-text-property (point) 'caller-name))
             (short-func (cg--extract-method-name callee))
             (callers (map-elt (call-graph--callers call-graph) short-func (list)))
             (deep-copy-of-callers (seq-map #'identity callers))
             (filters
              (or (map-elt cg--caller-filters callee deep-copy-of-callers)
                  (setf (map-elt cg--caller-filters callee) deep-copy-of-callers))))
    (tree-mode-delete-match (symbol-name caller))
    (setf (map-elt cg--caller-filters callee)
          (remove caller filters))))

(defun cg/reset-caller-filter ()
  "Within buffer <*call-graph*>, reset caller filter for symbol at point.
With prefix argument, discard whole caller filter."
  (interactive)
  (if current-prefix-arg
      (when (yes-or-no-p "Reset whole caller filter ?")
        (setf cg--caller-filters nil)
        (message "Reset whole caller filter done"))
    (save-mark-and-excursion
     (when (get-char-property (point) 'button)
       (forward-char 4))
     (when-let ((caller (get-text-property (point) 'caller-name)))
       (setf (map-elt cg--caller-filters caller) nil)
       (message (format "Reset caller filter for %s done" caller))))))

(defun cg/quit ()
  "Quit `call-graph'."
  (interactive)
  (when (eq major-mode 'call-graph-mode)
    (setq major-mode nil)
    (let ((configuration cg--window-configuration)
          (selected-window cg--selected-window))
      (kill-this-buffer)
      (set-window-configuration configuration)
      (select-window selected-window))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cg/widget-expand-all ()
  "Iterate all widgets in buffer and expand em."
  (interactive)
  (tree-mode-expand-level 0))

(defun cg/widget-collapse-all ()
  "Iterate all widgets in buffer and close em."
  (interactive)
  (goto-char (point-min))
  (tree-mode-expand-level 1))

(defun cg/expand (&optional level)
  "Expand `call-graph' by LEVEL."
  (interactive "p")
  (when-let ((call-graph cg--default-instance)
             (hierarchy cg--default-hierarchy)
             (depth (+ cg--current-depth level))
             (func (car (hierarchy-roots hierarchy))))
    (cg--create call-graph func depth)))

(defun cg/collapse (&optional level)
  "Collapse `call-graph' by LEVEL."
  (interactive "p")
  (let ((level (- cg--current-depth level)))
    (goto-char (point-min))
    (cond
     ((> level 0)
      (tree-mode-expand-level level)
      (setq cg--current-depth level))
     ((<= level 0)
      (tree-mode-expand-level 1)
      (setq cg--current-depth 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar call-graph-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "e") 'cg/widget-expand-all)
    (define-key map (kbd "c") 'cg/widget-collapse-all)
    (define-key map (kbd "p") 'widget-backward)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "q") 'cg/quit)
    (define-key map (kbd "+") 'cg/expand)
    (define-key map (kbd "_") 'cg/collapse)
    (define-key map (kbd "o") 'cg/goto-file-at-point)
    (define-key map (kbd "g") 'cg/at-point)
    (define-key map (kbd "d") 'cg/remove-caller)
    (define-key map (kbd "l") 'cg/select-caller-location)
    (define-key map (kbd "r") 'cg/reset-caller-filter)
    (define-key map (kbd "<RET>") 'cg/goto-file-at-point)
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
  (when cg-display-file
    (add-hook 'widget-move-hook (lambda () (cg/display-file-at-point))))
  (run-mode-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (global-set-key (kbd "C-c g") 'call-graph)


(provide 'call-graph)
;;; call-graph.el ends here
