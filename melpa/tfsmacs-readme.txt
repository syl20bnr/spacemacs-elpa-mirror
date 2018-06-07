Basic steps to setup:
  1. Obtain the Team Explorer Everywhere CLI tool from https://github.com/Microsoft/team-explorer-everywhere/releases
  2. Place `tfsmacs.el' in your `load-path'.
  3. In your .emacs file:
       (require 'tfsmacs)
       (setq tfsmacs-cmd  "location/of/TEE/tf")
       (setq tfsmacs-login "/login:domain\\userid,password")
  4. Also in your .emacs file,  set local or global key bindings for tfs commands.  Or use the provided keymap.
     Example:
       (global-set-key  "\C-ct" 'tfsmacs-map)
     OR:
       (global-set-key  "\C-ctp" 'tfsmacs-pendingchanges)
       (global-set-key  "\C-cto" 'tfsmacs-checkout)
       (global-set-key  "\C-cti" 'tfsmacs-checkin)
       (global-set-key  "\C-ctr" 'tfsmacs-rename)
       (global-set-key  "\C-ctg" 'tfsmacs-get)
       (global-set-key  "\C-cth" 'tfsmacs-history)
       (global-set-key  "\C-ctc" 'tfsmacs-changeset)
       (global-set-key  "\C-ctu" 'tfsmacs-undo)
       (global-set-key  "\C-ct-" 'tfsmacs-delete)
       (global-set-key  "\C-ct+" 'tfsmacs-add)
For a detailed user manual see:
https://github.com/sebasmonia/tfsmacs/blob/master/README.md
