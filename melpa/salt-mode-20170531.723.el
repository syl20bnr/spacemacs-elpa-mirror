;;; salt-mode.el --- Major mode for Salt States

;; Copyright (C) 2015  Ben Hayden

;; Author: Ben Hayden <hayden767@gmail.com>
;; Maintainer: Glynn Forrest <me@glynnforrest.com>
;; URL: https://github.com/glynnforrest/salt-mode
;; Package-Version: 20170531.723
;; Keywords: languages
;; Version: 0.1
;; Package-Requires: ((yaml-mode "0.0.12") (mmm-mode "0.5.4") (mmm-jinja2 "0.1"))

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

(defgroup salt nil
  "saltstack editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "salt-"
  :group 'languages)

(defcustom salt-indent-level 2
  "Indentation of YAML statements."
  :type 'integer
  :group 'salt
  :safe 'integerp)

(defun salt--flyspell-predicate ()
  "Only spellcheck comments and documentation within salt-mode.

Salt strings are usually configuration file data, and not
suitable for spellchecking."
  (memq (get-text-property (- (point) 1) 'face)
        '(font-lock-comment-face font-lock-doc-face)))

(put 'salt-mode 'flyspell-mode-predicate #'salt--flyspell-predicate)

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
    ("^\\([^ \"':#][^\"':#\n]*\\):"
     (1 'salt-mode-state-id-face))
    ("^ +\\([a-z][a-z0-9_]*\\.[a-z][a-z0-9_]*\\):?"
     (1 'salt-mode-state-function-face))
    ;; TODO:
    ;; - Match state IDs in extend: forms and requisite lists.
    ;; - Don't match requisites unless they're under functions.
    ;; - Handle top, pillar, and orch files specially.
    )
  "Regexps for YAML keys with special meaning in SLS files.")

;;;###autoload
(define-derived-mode salt-mode yaml-mode "SaltStack"
  "A major mode to edit Salt States."
  (setq tab-width salt-indent-level
        indent-tabs-mode nil
        electric-indent-inhibit t
        mmm-global-mode 'maybe)

  (mmm-add-mode-ext-class 'salt-mode "\\.sls\\'" 'jinja2)
  (font-lock-add-keywords nil salt-mode-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sls\\'" . salt-mode))

(provide 'salt-mode)

;;; salt-mode.el ends here
