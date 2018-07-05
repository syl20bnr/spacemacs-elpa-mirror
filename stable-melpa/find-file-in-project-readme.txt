This program provides a couple methods for quickly finding any file
in a given project.
- Only dependency is GNU/BSD find
- Works on Windows with minimum setup
- Works flawlessly on Tramp Mode (https://www.emacswiki.org/emacs/TrampMode)
- fd (faster alternative of find, see https://github.com/sharkdp/fd) is supported

Usage,
  - You can insert `(setq ffip-use-rust-fd t)' into ".emacs" to use fd (alternative of find)
  - `M-x find-file-in-project-at-point' guess the file name at point and
     find file
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

`ffip-insert-file' insert file content into current buffer.

`find-file-with-similar-name' find file with similar name to current
opened file. The regular expression `ffip-strip-file-name-regex' is
also used by `find-file-with-similar-name'.

all these variables may be overridden on a per-directory basis in
your .dir-locals.el.  See (info "(Emacs) Directory Variables") for
details.

Sample .dir-locals.el,

((nil . ((ffip-project-root . "~/projs/PROJECT_DIR")
         ;; ingore files bigger than 64k and directory "dist/"
         (ffip-find-options . "-not -size +64k -not -iwholename '*/dist/*'")
         ;; only search files with following extensions
         (ffip-patterns . ("*.html" "*.js" "*.css" "*.java" "*.xml" "*.js"))
         (eval . (progn
                   (require 'find-file-in-project)
                   ;; ingore directory ".tox/"
                   (setq ffip-prune-patterns `("*/.tox/*" ,@ffip-prune-patterns))
                   ;; Do NOT ignore directory "bin/"
                   (setq ffip-prune-patterns `(delete "*/bin/*" ,@ffip-prune-patterns))))
         )))

To find in *current directory*, use `find-file-in-current-directory'
and `find-file-in-current-directory-by-selected'.

`ffip-split-window-horizontally' and `ffip-split-window-vertically' find&open file
in split window.

`ffip-show-diff' execute the backend from `ffip-diff-backends'.
The output is in Unified Diff Format and inserted into *ffip-diff* buffer.
Press "o" or "C-c C-c" or "ENTER" or `M-x ffip-diff-find-file' in the
buffer to open corresponding file.

`ffip-diff-find-file-before-hook' is called before `ffip-diff-find-file'.

`ffip-diff-apply-hunk' applies current hunk in `diff-mode' (please note
`ffip-diff-mode' inherits from `diff-mode') to the target.
file. The target file could be located by searching `recentf-list'.
Except this extra feature, `ffip-diff-apply-hunk' is same as `diff-apply-hunk'.
So `diff-apply-hunk' can be replaced by `ffip-diff-apply-hunk'.

If you use `evil-mode', insert below code into ~/.emacs,
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

BSD/GNU Find can be installed through `Cygwin' or `MYSYS2' on Windows.
Executable is automatically detected. But you can manually specify
the executable location by insert below code into ~/.emacs,

  (if (eq system-type 'windows-nt)
     (setq ffip-find-executable "c:\\\\cygwin64\\\\bin\\\\find"))

This program works on Windows/Cygwin/Linux/Mac.

Windows setup is as easy as installing Cygwin into default directory on
ANY driver.

See https://github.com/technomancy/find-file-in-project for advanced tips.

Recommended binding: (global-set-key (kbd "C-x f") 'find-file-in-project)
