Extend Emacs' bat-mode to support auto-completion, code-navigation and
refactoring capabilities.

Once installed, add the following to your .emacs file to configure Emacs
and bmx-mode using the bmx-mode default-settings:

(require 'bmx-mode)
(bmx-mode-setup-defaults)

This minor-mode is all Elisp, and thus cross-platform and does not depend
on any native platform tools or host OS.
