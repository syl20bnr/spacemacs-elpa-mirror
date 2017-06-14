;;; salt-mode.el --- Major mode for Salt States

;; Copyright (C) 2015  Ben Hayden

;; Author: Ben Hayden <hayden767@gmail.com>
;; Maintainer: Glynn Forrest <me@glynnforrest.com>
;; URL: https://github.com/glynnforrest/salt-mode
;; Package-Version: 20170614.435
;; Keywords: languages
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (yaml-mode "0.0.12") (mmm-mode "0.5.4") (mmm-jinja2 "0.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file incorporates work covered by the following copyright and
;; permission notice:

;;   Licensed under the Apache License, Version 2.0 (the "License"); you may not
;;   use this file except in compliance with the License.  You may obtain a copy
;;   of the License at
;;
;;       http://www.apache.org/licenses/LICENSE-2.0
;;
;;   Unless required by applicable law or agreed to in writing, software
;;   distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;;   WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
;;   License for the specific language governing permissions and limitations
;;   under the License.

;;; Commentary:

;; GNU Emacs major mode for editing Salt States.

;; Provides syntax highlighting, indentation, and jinja templating.

;; Syntax highlighting: Fontification supports YAML & Jinja using mmm-mode

;; Tag complete: Using mmm-mode you can generate insert templates:
;; C-c % {
;;   generates {{ _ }} with cursor where the underscore is
;; C-c % #
;; C-c % %
;;   for {# and {% as well.

;;; Code:

(require 'yaml-mode)
(require 'mmm-auto)
(require 'mmm-jinja2)
(require 'thingatpt)

(defgroup salt-mode nil
  "SaltStack major mode."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "salt-mode-"
  :group 'languages)

(defcustom salt-mode-indent-level 2
  "Indentation of YAML statements."
  :type 'integer
  :group 'salt-mode
  :safe 'integerp)

(defun salt-mode--flyspell-predicate ()
  "Only spellcheck comments and documentation within salt-mode.

Salt strings are usually configuration file data, and not
suitable for spellchecking."
  (memq (get-text-property (- (point) 1) 'face)
        '(font-lock-comment-face font-lock-doc-face)))

(put 'salt-mode 'flyspell-mode-predicate #'salt-mode--flyspell-predicate)

(defun salt-mode--indented-re (lo hi)
  "Return a regexp to match a line with an indent level between LO and HI."
  (format "^%s\\{%d,%d\\}" (make-string salt-mode-indent-level ?\s) lo hi))

(defun salt-mode-bounds-of-state-function-at-point ()
  "Return the bounds of the state function name at the current point."
  (save-excursion
    (skip-chars-backward "a-z0-9_.:")
    (when (and (looking-back (salt-mode--indented-re 1 2))
               (looking-at "[a-z0-9_]+\\.[a-z0-9_]+"))
      (cons (point) (match-end 0)))))

(defun salt-mode-forward-state-function (&optional arg)
  "Move point forward ARG state function definitions.
Move backward if ARG is negative. If ARG is omitted or nil, move
forward one state function definition."
  (interactive "p")
  ;; Use "." as a cheap check for finding potential functions. This
  ;; gets a little hairy because of the optional :, which isn't
  ;; part of the bounds but works best when skipped as if it were.
  (let ((arg (or arg 1)))
    (while (< arg 0)
      (skip-chars-backward "^a-z0-9_.:")
      (let ((bounds (salt-mode-bounds-of-state-function-at-point)))
        (while (and (not bounds) (skip-chars-backward "^."))
          (backward-char)
          (setq bounds (salt-mode-bounds-of-state-function-at-point)))
        (goto-char (car bounds))
        (setq arg (1+ arg))))
    (while (> arg 0)
      (skip-chars-forward "^a-z0-9_.:")
      (let ((bounds (salt-mode-bounds-of-state-function-at-point)))
        (while (and (not bounds) (skip-chars-forward "^."))
          (forward-char)
          (setq bounds (salt-mode-bounds-of-state-function-at-point)))
        (goto-char (cdr bounds))
        (skip-chars-forward ":")
        (setq arg (1- arg))))))

(defun salt-mode-backward-state-function (&optional arg)
  "Move point backward ARG state function definitions.
Move forward if ARG is negative. If ARG is omitted or nil, move
backward one state function definition."
  (interactive "p")
  (forward-thing 'salt-mode-state-function (- (or arg 1))))

(put 'salt-mode-state-function 'bounds-of-thing-at-point
     #'salt-mode-bounds-of-state-function-at-point)

(put 'salt-mode-state-function 'forward-op
     #'salt-mode-forward-state-function)

(defun salt-mode-bounds-of-state-module-at-point ()
  "Return the bounds of the state module name at the current point."
  (save-excursion
    (skip-chars-backward "a-z0-9_.:")
    (when (and (looking-back (salt-mode--indented-re 1 2))
               (looking-at "[a-z0-9_]+"))
      (cons (point) (match-end 0)))))

(put 'salt-mode-state-module 'bounds-of-thing-at-point
     #'salt-mode-bounds-of-state-module-at-point)

(defun salt-mode--state-module-at-point ()
  "Get the state module at point, either pkg or pkg.installed, or return nil."
  ;; TODO return nil if looking at a state id, don't get the last
  ;; function from the previous state id
  (let ((thing (or (thing-at-point 'salt-mode-state-function)
                   (thing-at-point 'salt-mode-state-module))))
    (if (not thing)
        (save-excursion
          ;; try the first word on the current line, e.g.
          ;; | file.managed
          (beginning-of-line)
          (skip-chars-forward " ")
          (if (not (thing-at-point 'salt-mode-state-function))
              ;; no function on this line, try jumping backwards to the last state function
              (ignore-errors (salt-mode-backward-state-function)))
          (setq thing (thing-at-point 'salt-mode-state-function))))
    (when thing
      (set-text-properties 0 (length thing) nil thing))
    thing))

(defun salt-mode--doc-read-arg ()
  "Get the argument for interactively calling `salt-mode-browse-doc'"
  (let* ((default (salt-mode--state-module-at-point))
         (prompt (if default
                     (format "Open salt doc (%s): " default)
                   "Open salt doc, e.g. file.managed: "))
         (word (if (or current-prefix-arg (not default))
                   (completing-read prompt nil nil nil nil nil default)
                 default)))
    (list word)))

(defun salt-mode-browse-doc (module)
  "Browse to the documentation for the state module `MODULE'.

`MODULE' may be the name of a state module (pkg), or the name of a
state module and method (pkg.installed).

When called interactively, use the module at point.
If no module is found or a prefix argument is supplied, prompt for the
module to use.
"
  (interactive (salt-mode--doc-read-arg))
  (let* ((pieces (split-string module "\\." t " +"))
         (module (car pieces))
         (url (format "https://docs.saltstack.com/en/latest/ref/states/all/salt.states.%s.html" module))
         (method (cadr pieces)))
    (browse-url (if method (concat url "#salt.states." module "." method) url))))

(defconst salt-mode-toplevel-keywords
  '("include" "exclude" "extend")
  "Keys with special meaning at the top level of state files.")

(defconst salt-mode-requisite-types
  '("require" "watch" "prereq" "use" "onchanges" "onfail"
    "require_in" "watch_in" "prereq_in" "use_in" "onchanges_in" "onfail_in")
  "Keys that identify requisite relations between states.

More about requisites can be found in the Salt documentation,
https://docs.saltstack.com/en/latest/ref/states/requisites.html")

(defface salt-mode-keyword-face
  '((t (:inherit font-lock-keyword-face)))
  "Face for special Salt highstate keywords (e.g. `include')."
  :group 'salt)

(defface salt-mode-requisite-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for Salt state requisites (e.g. `require', `watch_in')."
  :group 'salt)

(defface salt-mode-state-function-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for Salt state functions (e.g. `file.managed')."
  :group 'salt)

(defface salt-mode-state-id-face
  '((t (:inherit font-lock-constant-face)))
  "Face for unquoted Salt state IDs."
  :group 'salt)

(defconst salt-mode-keywords
  `((,(format "^%s:" (regexp-opt salt-mode-toplevel-keywords t))
     (1 'salt-mode-keyword-face))
    (,(format "^ +- *%s:" (regexp-opt salt-mode-requisite-types t))
     (1 'salt-mode-requisite-face))
    ("^\\([^ \"':#\n][^\"':#\n]*\\):"
     (1 'salt-mode-state-id-face))
    ("^ +\\([a-z][a-z0-9_]*\\.[a-z][a-z0-9_]*\\):?"
     (1 'salt-mode-state-function-face))
    ;; TODO:
    ;; - Match state IDs in extend: forms and requisite lists.
    ;; - Don't match requisites unless they're under functions.
    ;; - Handle top, pillar, and orch files specially.
    )
  "Regexps for YAML keys with special meaning in SLS files.")

(defconst salt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-b") #'salt-mode-backward-state-function)
    (define-key map (kbd "C-M-f") #'salt-mode-forward-state-function)
    ;; (define-key map (kbd "C-M-n") 'salt-mode-forward-state-id)
    ;; (define-key map (kbd "C-M-p") 'salt-mode-backward-state-id)
    map) "Keymap for `salt-mode'.")

;;;###autoload
(define-derived-mode salt-mode yaml-mode "SaltStack"
  "A major mode to edit Salt States."
  (setq tab-width salt-mode-indent-level
        indent-tabs-mode nil
        electric-indent-inhibit t
        mmm-global-mode 'maybe)

  (setq-local yaml-indent-offset salt-mode-indent-level)

  (mmm-add-mode-ext-class 'salt-mode "\\.sls\\'" 'jinja2)
  (font-lock-add-keywords nil salt-mode-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sls\\'" . salt-mode))

(provide 'salt-mode)

;;; salt-mode.el ends here
