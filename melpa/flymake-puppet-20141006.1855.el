;;; flymake-puppet.el --- An Emacs flymake handler for syntax-checking puppet using puppet-lint

;; Copyright 2013 Ben Prew

;; Author: Ben Prew
;; URL: https://github.com/benprew/flymake-puppet
;; Package-Version: 20141006.1855
;; Version: 1.0.0
;; Package-Requires: ((flymake-easy "0.9"))


(require 'flymake-easy)

(defconst flymake-puppet-err-line-patterns
  '(("\\(.*line \\([0-9]+\\).*\\)" nil 2 nil 1)
    ("\\(.*.rb:[0-9]+.*\\)" nil nil nil 1)))

(defvar flymake-puppet-executable "puppet-lint"
  "The executable to use for puppet-lint")

(defvar flymake-puppet-options nil "Puppet-lint options")

(defun flymake-puppet-command (filename)
  "Construct a command that flymake can use to check puppet source."
  (append (list flymake-puppet-executable) flymake-puppet-options
          (list filename)))

;;;###autoload
(defun flymake-puppet-load ()
  "Configure flymake mode to check the current buffer's puppet syntax."
  (interactive)
  (flymake-easy-load 'flymake-puppet-command
                     flymake-puppet-err-line-patterns
                     'tempdir
                     "pp"))

(provide 'flymake-puppet)

;;; flymake-puppet.el ends here
