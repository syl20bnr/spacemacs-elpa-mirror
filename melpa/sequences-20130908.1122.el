;;; sequences.el --- Ports of some Clojure sequence functions.

;; CC BY Tim Visher

;; Author: Tim Visher <tim.visher@gmail.com>
;; Keywords: convenience
;; Package-Version: 20130908.1122
;; Package-Requires: ((emacs "24"))
;; Version: 0.1.0

;; This file is not part of GNU Emacs.

;; This work is licensed under the Creative Commons Attribution 3.0
;; Unported License. To view a copy of this license, visit
;; <http://creativecommons.org/licenses/by/3.0/> or send a letter to
;; Creative Commons, 444 Castro Street, Suite 900, Mountain View,
;; California, 94041, USA.

;;; Commentary:

;; Ports of Clojure sequence functions that I find useful to have
;; around. Maybe others will too?

;;; Code:

(require 'cl-lib)

(defun sequences-tree-seq (branch? children root)
  (cl-labels ((walk (node)
                    (cons node
                          (if (funcall branch? node)
                              (cl-mapcan #'walk (cddr (funcall children node)))))))
    (walk root)))

(defun sequences-file-seq (root)
  (sequences-tree-seq 'file-directory-p
                      (lambda (directory)
                        (directory-files directory t))
                      (expand-file-name root)))

(provide 'sequences)

;;; Local Variables:
;;; tab-width:2
;;; indent-tabs-mode:nil
;;; lexical-binding:t
;;; End:
;;; sequences.el ends here
