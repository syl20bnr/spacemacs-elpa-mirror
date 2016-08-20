  Setup:

     (require 'uzumaki)
     (uzumaki-minor-mode 1)


  The default cycling is done through "major-mode" buffers.
  But its behavior can be changed:

     (uzumaki-set-cycle-mode 'all-no-hidden)
  or
     (setq uzumaki-cycle-mode 'system)
  or
     (uzumaki-set-cycle-mode 'regex)
     (uzumaki-add-regex "^\\*CEDET.*\\*")
     (uzumaki-add-regex "^\\*Back.*\\*")


  Is possible to add regexes to never show buffers in accordance
  with its criteria.  But remember that these regexes will override
  any mode buffers, so if a never-show criteria is found in whatever mode,
  the respective buffer will not be showed.
  The uzumaki-never-show-list is exclusive.

     (uzumaki-add-never-show-regex "^\\*CEDET.*\\*")
     (uzumaki-add-never-show-regex "^\\*Back.*\\*")


  Without change the configured cycler mode, the user can set other
  key-bindings specifying the desired options:

(defun my:uzumaki-mode-keys ()
  "my keybindings for 'uzumaki-minor-mode'."
  (define-key uzumaki-minor-mode-map (kbd "C-c <left>") 'uzumaki-cycle-to-prev-buffer 'all)
  (define-key uzumaki-minor-mode-map (kbd "C-c <right>") 'uzumaki-cycle-to-next-buffer 'all)
  (define-key uzumaki-minor-mode-map (kbd "C-c <") 'uzumaki-cycle-to-prev-buffer 'all-no-hidden)
  (define-key uzumaki-minor-mode-map (kbd "C-c >") 'uzumaki-cycle-to-next-buffer 'all-no-hidden)

  ;; If there is a conflicting key, unbind the uzumaki-minor-mode respective one.
  (define-key uzumaki-minor-mode-map (kbd "C-,") nil)
  (define-key uzumaki-minor-mode-map (kbd "C-.") nil))

(add-hook 'uzumaki-minor-mode-hook 'my:uzumaki-mode-keys)


  The same can be said about uzumaki code reuse, once the major part of
  functions has an &optional argument.
