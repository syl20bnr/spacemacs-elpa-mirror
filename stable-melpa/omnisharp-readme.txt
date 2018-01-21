omnisharp-emacs is a port of the awesome OmniSharp server to the
Emacs text editor. It provides IDE-like features for editing files
in C# solutions in Emacs, provided by an OmniSharp server instance
that works in the background.

See the project home page for more information.


(require 'cl)
(require 'cl-lib)
(require 'csharp-mode)
(require 'json)
(require 'files)
(require 'ido)
(require 'thingatpt)
(require 'dash)
(require 'compile)
(require 'dired)
(require 'popup)
(require 'etags)
(require 'flycheck)
(require 's)
(require 'shut-up)
(require 'f)

(require 'omnisharp-settings)
(require 'omnisharp-server-management)
(require 'omnisharp-utils)
(require 'omnisharp-http-utils)
(require 'omnisharp-server-actions)
(require 'omnisharp-auto-complete-actions)
(require 'omnisharp-current-symbol-actions)
(require 'omnisharp-navigation-actions)
(require 'omnisharp-helm-integration)
(require 'omnisharp-solution-actions)
(require 'omnisharp-format-actions)
(require 'omnisharp-server-installation)
