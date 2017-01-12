   This is a major mode for editing C# code.  It performs automatic
   indentation of C# syntax; font locking; and integration with
   imenu.el.

   csharp-mode requires CC Mode 5.30 or later.  It works with
   cc-mode 5.31.3, which is current at this time.

Features:

  - font-lock and indent of C# syntax including:
      all c# keywords and major syntax
      attributes that decorate methods, classes, fields, properties
      enum types
      #if/#endif  #region/#endregion
      instance initializers
      anonymous functions and methods
      verbatim literal strings (those that begin with @)
      generics

  - automagic code-doc generation when you type three slashes.

  - compatible with electric-pair-mode for intelligent insertion
    of matched braces, quotes, etc.

  - imenu integration - generates an index of namespaces, classes,
    interfaces, methods, and properties for easy navigation within
    the buffer.



Installation instructions
--------------------------------

Put csharp-mode.el somewhere in your load path, optionally byte-compile
it, and add the following to your .emacs file:

  (autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
  (setq auto-mode-alist
     (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))


Optionally, define and register a mode-hook function.  To do so, use
something like this in your .emacs file:

  (defun my-csharp-mode-fn ()
     "function that runs when csharp-mode is initialized for a buffer."
     (turn-on-auto-revert-mode)
     (setq indent-tabs-mode nil)
     ...insert more code here...
     ...including any custom key bindings you might want ...
  )
  (add-hook  'csharp-mode-hook 'my-csharp-mode-fn t)


 General
 ----------------------------

 Mostly C# mode will "just work."  Use `describe-mode' to see the
 default keybindings and the highlights of the mode.


 imenu integration
 -----------------------------

 This should just work.  For those who don't know what imenu is, it
 allows navigation to different points within the file from an
 "Index" menu, in the window's menubar.  csharp-mode computes the
 menu containing the namespaces, classes, methods, and so on, in the
 buffer.  This happens at the time the file is loaded; for large
 files it takes a bit of time to complete the scan.  If you don't
 want this capability, set `csharp-want-imenu' to nil.
