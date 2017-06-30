;;; neon-mode.el --- Simple major mode for editing neon files

;; Copyright (C) 2015-2017 Matúš Goljer <matus.goljer@gmail.com>

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 1.1.0
;; Package-Version: 1.2.2
;; Created: 26th March 2015
;; Keywords: conf

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Major mode for editing neon files: http://ne-on.org/

;; Currently only provides syntax highlighting, otherwise derives from
;; `conf-colon-mode'.

;;; Code:

(require 'conf-mode)
(require 'thingatpt)

(defcustom neon-mode-find-project-root-function (if (featurep 'projectile)
                                                    'projectile-project-root
                                                  'neon-mode--project-root-by-composer)
  "Function to detect the project root."
  :type '(choice (const :tag "Use Projectile's project root detection" projectile-project-root)
                 (const :tag "Detect project root by composer.json presence" neon-mode--project-root-by-composer)
                 (function :tag "Use custom function")))

(defun neon-mode--project-root-by-composer ()
  "Find the project root by looking up tree hierarchy for composer.json."
  (locate-dominating-file (or (buffer-file-name) default-directory)
                          "composer.json"))


(defun neon-mode-find-project-root ()
  "Find the project root.

Uses `neon-mode-find-project-root-function' todo the actual lookup."
  (funcall neon-mode-find-project-root-function))

(defun neon-mode-find-class-file (class)
  "Find class at point.

This requires php binary to be present on the system and also
only works for projects using composer autoloading."
  (interactive (list (let ((thing (thing-at-point-looking-at "\\(\\w\\|\\s\\\\)+" 100)))
                       (when thing
                         (let ((beginning (match-beginning 0))
                               (end (match-end 0)))
                           (buffer-substring-no-properties beginning end))))))
  (when class
    (let ((root (neon-mode-find-project-root)))
      (when root
        (let ((path (with-temp-buffer
                      (call-process "php" nil t nil "-r" "
$autoloader = include ($argv[1] . '/vendor/autoload.php');
$path = $autoloader->findFile($argv[2]);
if (is_string($path)) {
    $result = realpath($path);
} else {
    $result = false;
}
echo json_encode($result);" root class)
                      (json-read-from-string (buffer-string)))))
          (when path
            (push-mark)
            (find-file path)))))))

(defvar conf-neon-font-lock-keywords
  `(
    (,(concat "\\_<" (regexp-opt '("true" "True" "TRUE" "yes" "Yes"
                                  "YES" "on" "On" "ON" "false" "False"
                                  "FALSE" "no" "No" "NO" "off" "Off" "OFF"
                                  "enabled" "disabled"))
              "\\_>")
     0 'font-lock-constant-face)
    ("\\<%\\(.*?\\)%\\>" 0 'font-lock-keyword-face)
    ("[^[:alnum:]]\\(@\\_<\\(.*?\\)\\_>\\)" 1 'font-lock-type-face)
    ("::\\(\\sw+?\\)\\>" 1 'font-lock-function-name-face)
    ("\\_<\\$\\(.*?\\)\\_>" 1 'font-lock-variable-name-face)
    ,@conf-colon-font-lock-keywords))

(defvar neon-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-'") 'neon-mode-find-class-file)
    map)
  "Neon mode key map.")

;;;###autoload
(define-derived-mode neon-mode conf-colon-mode "Conf[Neon]"
  "Conf Mode starter for Neon files.
\"Assignments\" are with `:'.
For details see `conf-mode'."
  (conf-mode-initialize "#" 'conf-neon-font-lock-keywords))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.neon\\'" . neon-mode))

(provide 'neon-mode)
;;; neon-mode.el ends here
