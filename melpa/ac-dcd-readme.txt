Auto Completion source for DCD.  This code was modified from ac-dscanner.el
which originally came from auto-complete-clang-async.el.  Originally from
the DCD git repository https://github.com/Hackerpilot/DCD/blob/master/editors/emacs/ac-dcd.el.

Usage:

(require 'ac-dcd)
(add-to-list 'ac-modes 'd-mode)
(add-hook 'd-mode-hook
          (lambda () "set up ac-dcd"
            (auto-complete-mode t)
            (yas-minor-mode-on)
            (ac-dcd-maybe-start-server)
            (ac-dcd-add-imports)
            (add-to-list 'ac-sources 'ac-source-dcd)))
