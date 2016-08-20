;;; ox-bibtex-chinese.el --- Let ox-bibtex work well for Chinese users

;; * Header
;; Copyright (c) 2016, Feng Shu

;; Author: Feng Shu <tumashu@163.com>
;; URL: https://github.com/tumashu/ox-bibtex-chinese.git
;; Package-Version: 20160510.506
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; * README                                                             :README:
;; ox-bibtex-chinese is an extension of ox-bibtex, which can help chinese user
;; to export bibliography to html.

;; [[./snapshots/ox-bibtex-chinese.gif]]

;; ** Installation
;; ox-bibtex-chinese is now available from the famous emacs package repo
;; [[http://melpa.milkbox.net/][melpa]], so the recommended way is to install it
;; through emacs package management system.

;; ** Usage
;; 1. Install bibtex2html to your system
;; 2. Configure emacs
;;    #+BEGIN_EXAMPLE
;;    (require 'org)
;;    (require 'ox-bibtex)
;;    (require 'ox-bibtex-chinese)
;;    (ox-bibtex-chinese-enable)
;;    #+END_EXAMPLE
;; 3. See the format of "example/thesis.org" and try export it to html file.

;;; Code:
;; * Code                                                                 :code:
;; #+BEGIN_SRC emacs-lisp

(defgroup ox-bibtex-chinese nil
  "Let ox-bibtex work well for Chinese users."
  :group 'ox-bibtex)

(defcustom ox-bibtex-chinese-default-bibtex-style
  (concat (file-name-directory
           (locate-library "ox-bibtex-chinese.el"))
          "bibtex-styles/GBT7714-2005-latex/GBT7714-2005NLang-UTF8.bst")
  "Set bibtex2html default style, when use ox-bibtex.")

(defcustom ox-bibtex-chinese-default-bibtex2html-options
  '("-a" "-noabstract" "-nokeywords" "-i" "-nolinks")
  "Set bibtex2html default options, when use ox-bibtex.")

(defun ox-bibtex-chinese--add-default-style (style)
  "If `org-bibtex-get-style' is not return a valid style, return
`ox-bibtex-chinese-default-bibtex-style'"
  (if (org-not-nil style)
      style
    ox-bibtex-chinese-default-bibtex-style))

(defun ox-bibtex-chinese--add-default-arguments (arguments)
  "Add extra arguments to `org-bibtex-get-arguments, then returned"
  (let ((orig-options (plist-get arguments :options)))
    (plist-put arguments :options
               (delete-dups (append ox-bibtex-chinese-default-bibtex2html-options orig-options)))))

(defun ox-bibtex-chinese-enable ()
  "Enable ox-bibtex-chinese"
  (interactive)
  (if (and (featurep 'org)
           (featurep 'ox-bibtex))
      (progn (advice-add 'org-bibtex-get-style :filter-return #'ox-bibtex-chinese--add-default-style)
             (advice-add 'org-bibtex-get-arguments :filter-return #'ox-bibtex-chinese--add-default-arguments)
             (message "ox-bibtex-chinese is enabled."))
    (message "'org' or 'ox-bibtex' is unavailable.")))
;; #+END_SRC

;; * Footer
;; #+BEGIN_SRC emacs-lisp
(provide 'ox-bibtex-chinese)

;; Local Variables:
;; coding: utf-8-unix
;; End:

;;; ox-bibtex-chinese.el ends here
;; #+END_SRC
