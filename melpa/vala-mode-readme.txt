   See http://live.gnome.org/Vala for details about Vala language.

   This is a separate mode to implement the Vala constructs and
   font-locking. It is mostly the csharp-mode from
   http://mfgames.com/linux/csharp-mode with vala specific keywords
   and filename suffixes.

   Note: The interface used in this file requires CC Mode 5.30 or
   later.

.emacs (don't put in (require 'vala-mode))
(autoload 'vala-mode "vala-mode" "Major mode for editing Vala code." t)
(setq auto-mode-alist
   (append '(("\\.vala$" . vala-mode)) auto-mode-alist))
