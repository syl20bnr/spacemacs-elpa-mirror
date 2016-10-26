This program provides a couple methods for quickly finding any file
in a given project.  It depends on GNU find.

Usage,
  - `M-x find-file-in-project-by-selected' use the selected region
     as the keyword to search file.  Or you need provide the keyword
     if no region selected.
  - `M-x find-directory-in-project-by-selected' use the select region
     to find directory.  Or you need provide the keyword if no region
     selected.
  - `M-x find-file-in-project' will start search file immediately
  - `M-x ffip-create-project-file' create .dir-locals.el

A project is found by searching up the directory tree until a file
is found that matches `ffip-project-file'.
You can set `ffip-project-root-function' to provide an alternate
function to search for the project root.  By default, it looks only
for files whose names match `ffip-patterns',

If you have so many files that it becomes unwieldy, you can set
`ffip-find-options' to a string which will be passed to the `find'
invocation in order to exclude irrelevant subdirectories/files.
For instance, in a Ruby on Rails project, you are interested in all
.rb files that don't exist in the "vendor" directory.  In that case
you could set `ffip-find-options' to "-not -regex \".*vendor.*\"".

The variable `ffip-filename-rules' create some extra file names for
search when calling `find-file-in-project-by-selected'. For example,
When file basename `helloWorld' provided, `HelloWorld', `hello-world'
are added as the file name search patterns.
`C-h v ffip-filename-rules' to see its default value.

All these variables may be overridden on a per-directory basis in
your .dir-locals.el.  See (info "(Emacs) Directory Variables") for
details.

To find in *current directory*, use `find-file-in-current-directory'
and `find-file-in-current-directory-by-selected'.

`ffip-show-diff' execute the backend from `ffip-diff-backends'.
The selected index is the parameter passed to `ffip-show-diff'
whose default value in one.
The output of execution is expected be in Unified Diff Format.
The output is inserted into *ffip-diff* buffer.
In the buffer, press "o/C-c C-c"/ENTER" or `M-x ffip-diff-find-file'
to open correspong file.

`ffip-diff-find-file-before-hook' is called before `ffip-diff-find-file'.

If you use evil-mode, insert below code into ~/.emacs,
  (defun ffip-diff-mode-hook-setup ()
      (evil-local-set-key 'normal "K" 'diff-hunk-prev)
      (evil-local-set-key 'normal "J" 'diff-hunk-next)
      (evil-local-set-key 'normal "P" 'diff-file-prev)
      (evil-local-set-key 'normal "N" 'diff-file-next)
      (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
      (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
  (add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)

`find-relative-path' find file/directory and copy its relative path
into `kill-ring'. You can customize `ffip-find-relative-path-callback'
to format the relative path,
  (setq ffip-find-relative-path-callback 'ffip-copy-reactjs-import)
  (setq ffip-find-relative-path-callback 'ffip-copy-org-file-link)

`ivy-mode' is used for filter/search UI
In `ivy-mode', SPACE is translated to regex ".*".
For example, the search string "dec fun pro" is transformed into
regular expression "\\(dec\\).*\\(fun\\).*\\(pro\\)"
`C-h i g (ivy)' for more key-binding tips.

`ffip-save-ivy-last' saves the most recent search result.
`ffip-ivy-resume' re-use the save result. Both requires `ivy-mode'
installed.

You can switch to `ido-mode' by `(setq ffip-prefer-ido-mode t)'

GNU Find can be installed,
  - through `Brew' on OS X
  - through `Cygwin' or `MYSYS2' on Windows.
Find executable will be automatically detected. But you can manually
specify the executable location by insert below code into ~/.emacs,

  (if (eq system-type 'windows-nt)
     (setq ffip-find-executable "c:\\\\cygwin64\\\\bin\\\\find"))
This program works on Windows/Cygwin/Linux/Mac Emacs.

Windows setup is as easy as installing Cygwin into default directory on
ANY driver. That's all.

See https://github.com/technomancy/find-file-in-project for advanced tips.

Recommended binding: (global-set-key (kbd "C-x f") 'find-file-in-project)
