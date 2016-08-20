;;; helm-filesets.el --- A helm source for emacs filesets

;; Copyright (C) 2014 Graham Clark

;; Author: Graham Clark <grclark@gmail.com>
;; URL: https://github.com/gcla/helm-filesets
;; Package-Version: 20140929.1135
;; Version: 0.1
;; Keywords: filesets
;; Package-Requires: ((helm "1.6.3") (filesets+ "0"))

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Given an emacs fileset called e.g. "Public HTML", this call will
;; define a variable representing a helm source from that fileset:
;;
;; (defvar my/helm-source-public-html (helm-make-source-filesets "Public HTML"))
;;
;; Then my/helm-source-public-html can be added to helm-for-files-preferred-list, for example, to provide candidate completion with the helm-for-files
;; function.

;;; Code:

(require 'helm)

;;;###autoload
(defun helm-make-source-filesets (fsname)
  `((name . ,(format "%s Fileset" fsname))
    (init
     . (lambda ()
         (require 'filesets+ nil t)))
    (candidates . (lambda ()
                    (with-helm-current-buffer
                      (filesets-get-filelist (filesets-get-fileset-from-name ,fsname))
                      )))
    (keymap . ,helm-generic-files-map)
    (help-message . helm-generic-file-help-message)
    (mode-line . helm-generic-file-mode-line-string)
    (type . file)))

(provide 'helm-filesets)

;;; helm-filesets.el ends here
