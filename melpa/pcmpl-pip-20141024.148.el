;;; pcmpl-pip.el --- pcomplete for pip

;; Copyright (C) 2014 Wei Zhao
;; Author: Wei Zhao <kaihaosw@gmail.com>
;; Git: https://github.com/kaihaosw/pcmpl-pip.git
;; Version: 0.4
;; Package-Version: 20141024.148
;; Created: 2014-09-10
;; Keywords: pcomplete, pip, python, tools

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Pcomplete for pip.
;; Based on pip 1.5.6 docs.

;;; Code:
(require 'pcomplete)

(defgroup pcmpl-pip nil
  "Pcomplete for pip"
  :group 'pcomplete)

(defcustom pcmpl-pip-cache-file "~/.pip/pip-cache"
  "Location of pip cache file."
  :group 'pcmpl-pip
  :type 'string)

(defconst pcmpl-pip-index-url "https://pypi.python.org/simple/")

;;;###autoload
(defun pcmpl-pip-clean-cache ()
  "Clean the pip cache file."
  (interactive)
  (shell-command-to-string (concat "rm " pcmpl-pip-cache-file)))

;; https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/pip/pip.plugin.zsh
(defun pcmpl-pip-create-index ()
  "Create the pip indexes file."
  (let* ((temp "/tmp/pip-cache")
         (dir (file-name-directory pcmpl-pip-cache-file)))
    (message "caching pip package index...")
    (unless (file-exists-p dir)
      (make-directory dir))
    (shell-command-to-string (concat "curl " pcmpl-pip-index-url
                                     " | sed -n '/<a href/ s/.*>\\([^<]\\{1,\\}\\).*/\\1/p'"
                                     " >> " temp))
    (shell-command-to-string (concat "sort " temp
                                     " | uniq | tr '\n' ' ' > "
                                     pcmpl-pip-cache-file))
    (shell-command-to-string (concat "rm " temp))))

;;;###autoload
(defun pcmpl-pip-update-index ()
  "Update the current pip cache file."
  (interactive)
  (pcmpl-pip-clean-cache)
  (pcmpl-pip-create-index))

;;
(defconst pcmpl-pip-general-options
  '("-h" "--help" "-v" "--verbose" "-V" "--version"
    "-q" "--quiet" "--log-file" "--log" "--proxy"
    "--timeout" "--exists-action" "--cert"))

(defconst pcmpl-pip-options
  '(("install" . (("-e" "--editable"
                   "-r" "--requirement"
                   "-b" "--build"
                   "-t" "--target"
                   "-d" "--download"
                   "--download-cache" "--src"
                   "-U" "--upgrade"
                   "--force-reinstall"
                   "-I" "--ignore-installed"
                   "--no-deps"
                   "--no-install"
                   "--no-download"
                   "--install-option"
                   "--global-option"
                   "--user"
                   "--egg"
                   "--root"
                   "--compile"
                   "--no-compile"
                   "--no-use-wheel"
                   "--pre"
                   "--no-clean"
                   "-i" "--index-url"
                   "--extra-index-url"
                   "--no-index"
                   "-f" "--find-links"
                   "--allow-external"
                   "--allow-all-external"
                   "--allow-unverified"
                   "--process-dependency-links")))
    ("uninstall" . (("-r" "--requirement"
                     "-y" "--yes")))
    ("freeze" . (("-f" "--find-links"
                  "-l" "--local")))
    ("list" . (("-o" "--outdated"
                "-u" "--uptodate"
                "-e" "--editable"
                "-l" "--local"
                "--pre"
                "-i" "--index-url"
                "--extra-index-url"
                "--no-index"
                "-f" "--find-links"
                "--allow-external"
                "--allow-all-external"
                "--allow-unverified"
                "--process-dependency-links")))
    ("show" . (("-f" "--files")))
    ("search" . (("--index")))
    ("wheel" . (("-w" "--wheel-dir"
                 "--no-use-wheel"
                 "--build-option"
                 "-r" "--requirement"
                 "--download-cache"
                 "--no-deps"
                 "-b" "--build"
                 "--global-option"
                 "--pre"
                 "--no-clean"
                 "-i" "--index-url"
                 "--extra-index-url"
                 "--no-index"
                 "-f" "--find-links"
                 "--allow-external"
                 "--allow-all-external"
                 "--allow-unverified"
                 "--process-dependency-links")))))

(defun pcmpl-pip-command-options (command)
  (cadr (assoc command pcmpl-pip-options)))

(defconst pcmpl-pip-commands
  (cons "help" (mapcar 'car pcmpl-pip-options)))

;;
(defconst pcmpl-pip-global-commands
  '("install" "search"))

(defconst pcmpl-pip-local-commands
  '("uninstall" "show"))

(defun pcmpl-pip-all ()
  "All packages."
  (split-string (shell-command-to-string (concat "cat " pcmpl-pip-cache-file))))

(defun pcmpl-pip-installed ()
  "All installed packages."
  (split-string (shell-command-to-string "pip freeze | cut -d '=' -f 1")))


;;;###autoload
(defun pcomplete/pip ()
  (let ((command (nth 1 pcomplete-args))
        (all-packages (pcmpl-pip-all))
        (all-installed (pcmpl-pip-installed)))
    (unless (file-exists-p pcmpl-pip-cache-file)
      (pcmpl-pip-create-index))
    (if (pcomplete-match "^-" 0)
        (pcomplete-here pcmpl-pip-general-options)
      (pcomplete-here* pcmpl-pip-commands))
    (when (member command pcmpl-pip-commands)
      (while
          (cond
           ((or (pcomplete-match "^--requirement" 0)
                (pcomplete-match "^-r" 0))
            (while (pcomplete-here (pcomplete-entries))))
           ((or (pcomplete-match "^-U" 0)
                (pcomplete-match "^--upgrade" 0))
            (while (pcomplete-here all-installed)))
           ((pcomplete-match "^-" 0)
            (pcomplete-here (pcmpl-pip-command-options command)))
           ((string= command "help")
            (pcomplete-here pcmpl-pip-commands))
           ((member command pcmpl-pip-global-commands)
            (pcomplete-here all-packages))
           ((member command pcmpl-pip-local-commands)
            (pcomplete-here all-installed)))))))

(provide 'pcmpl-pip)

;;; pcmpl-pip.el ends here
