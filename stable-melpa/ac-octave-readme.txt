Installation:

If you have `melpa' and `emacs24' installed, simply type:

	M-x package-install ac-octave

Add following lines to your init file:

    (require 'ac-octave)
    (defun ac-octave-mode-setup ()
      (setq ac-sources '(ac-source-octave)))
      (add-hook 'octave-mode-hook
        '(lambda () (ac-octave-mode-setup)))
