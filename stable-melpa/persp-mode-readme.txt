Based on the perspective.el by Natalie Weizenbaum
 (http://github.com/nex3/perspective-el) but the perspectives are shared
  among the frames and could be saved/restored from/to a file.

Homepage: https://github.com/Bad-ptr/persp-mode.el

Installation:

From the MELPA: M-x package-install RET persp-mode RET
From a file: M-x package-install-file RET 'path to this file' RET
Or put this file into your load-path.

Configuration:

When installed through the package-install:
(with-eval-after-load "persp-mode-autoloads"
  (setq wg-morph-on nil)
  ;; switch off the animation of restoring window configuration
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))

When installed without generating an autoloads file:
(with-eval-after-load "persp-mode"
  (setq wg-morph-on nil)
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
(require 'persp-mode)

Dependencies:

The ability to save/restore window configurations from/to a file
 depends on the workgroups.el(https://github.com/tlh/workgroups.el)
  for the emacs versions < 24.4

Keys:

n -- switch to next perspective.
p -- switch to previous perspective.
s -- create/switch to perspective in frame.
S -- create/switch to perspective in window.
r -- rename perspective.
c -- copy current perspective.
C -- kill perspective.
  Calling with prefix argument will not kill perspective's buffers
  (however if you try to kill 'none' persp -- it'l kill all opened buffers).
a -- add buffer to perspective.
  Calling with prefix argument reverses the effect of the persp-switch-to-added-buffer.
b -- switch to buffer in perspective.
t -- switch to buffer without adding it to current perspective.
  Calling with prefix argument allows to remove a buffer from perspective without
  killing and switching to another buffer.
i -- import all buffers from another perspective.
I -- import window configuration from another perspective.
k -- remove buffer from perspective.
  Calling with prefix argument reverses the effect of the persp-auto-kill-buffer-on-remove.
K -- kill buffer.
w -- save perspectives to file.
W -- save subset of perspectives to file.
l -- load perspectives from file.
L -- load subset of perspectives from file.
o -- switch off persp-mode.
  (This may be useful when you launch emacs just to edit a single file and don't want to
restore buffers)

These key sequences must follow the `persp-keymap-prefix' which you can customize
 (by default it is 'C-c p' in older releases it was 'C-x x')
  so if you want to invoke the < s - create/switch perspective > command
   you must first type the prefix ('C-c p') and then 's'(full sequence is C-c p s).

If you want to bind a new key for persp-mode, use the `persp-key-map`:
 `(define-key persp-key-map (kbd ...) ...)`.

If you kill a buffer with the 'C-x k' it will be killed only if it belongs to
 a single perspective, otherwise it'l be just removed from the current perspective.
But if you kill a buffer from the 'none'(nil) perspective --
 it will be removed from all perspectives and then killed.


Customization:

M-x: customize-group RET persp-mode RET
