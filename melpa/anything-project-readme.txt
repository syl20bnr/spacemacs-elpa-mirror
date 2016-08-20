anything-project.el is pure emacs lisp version of anything-find-project-resources.el.
Many ideas are from
http://trac.codecheck.in/share/browser/lang/elisp/anything-find-project-resources/trunk/anything-find-project-resources.el
and
http://blog.jrock.us/articles/eproject.POD

drop this file into a directory in your `load-path',
and put these lines into your .emacs file.

(require 'anything-project)
(global-set-key (kbd "C-c C-f") 'anything-project)

type C-c C-f to invoke anything with project files.
project root directory is automatically detected by anything-project.el

clear cache, If `anything-project' function is called with prefix arg (C-u M-x anything-project)

you can add new project rule by `ap:add-project' function
keywords :look-for, :include-regexp and :exclude-regexp can be regexp or list of regexp
below are few samples

(ap:add-project
 :name 'perl
 :look-for '("Makefile.PL" "Build.PL") ; or
 :include-regexp '("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$") ;or
 )

(ap:add-project
 :name 'perl
 :look-for '("Makefile.PL" "Build.PL")
 :include-regexp '("\\.pm$" "\\.t$" "\\.pl$" "\\.PL$")
 :exclude-regexp "/tmp" ; can be regexp or list of regexp
 )
