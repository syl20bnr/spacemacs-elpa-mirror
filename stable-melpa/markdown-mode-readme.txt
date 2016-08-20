markdown-mode is a major mode for editing [Markdown][]-formatted
text files in GNU Emacs.  markdown-mode is free software, licensed
under the GNU GPL.

 [Markdown]: http://daringfireball.net/projects/markdown/

The latest stable version is markdown-mode 2.1, released on January 9, 2016:

   * [markdown-mode.el][]
   * [Screenshot][][^theme]
   * [Release notes][]

 [markdown-mode.el]: http://jblevins.org/projects/markdown-mode/markdown-mode.el
 [Screenshot]: http://jblevins.org/projects/markdown-mode/screenshots/20160108-001.png
 [Release notes]: http://jblevins.org/projects/markdown-mode/rev-2-1

[^theme]: The theme used in the screenshot is
    [color-theme-twilight](https://github.com/crafterm/twilight-emacs).

The latest development version can be obtained from the Git
repository at <http://jblevins.org/git/markdown-mode.git> or from
[GitHub][]:

    git clone git://jblevins.org/git/markdown-mode.git
    git clone https://github.com/jrblevin/markdown-mode.git

[![Build Status][status]][travis]

 [devel.el]: http://jblevins.org/git/markdown-mode.git/plain/markdown-mode.el
 [GitHub]: https://github.com/jrblevin/markdown-mode/
 [travis]: https://travis-ci.org/jrblevin/markdown-mode
 [status]: https://travis-ci.org/jrblevin/markdown-mode.svg?branch=master

markdown-mode is also available in several package managers, including:

   * Debian Linux: [elpa-markdown-mode][] and [emacs-goodies-el][]
   * Ubuntu Linux: [elpa-markdown-mode][elpa-ubuntu] and [emacs-goodies-el][emacs-goodies-el-ubuntu]
   * RedHat and Fedora Linux: [emacs-goodies][]
   * NetBSD: [textproc/markdown-mode][]
   * Arch Linux (AUR): [emacs-markdown-mode-git][]
   * MacPorts: [markdown-mode.el][macports-package] ([pending][macports-ticket])
   * FreeBSD: [textproc/markdown-mode.el][freebsd-port]

 [elpa-markdown-mode]: https://packages.debian.org/sid/lisp/elpa-markdown-mode
 [elpa-ubuntu]: http://packages.ubuntu.com/search?keywords=elpa-markdown-mode
 [emacs-goodies-el]: http://packages.debian.org/emacs-goodies-el
 [emacs-goodies-el-ubuntu]: http://packages.ubuntu.com/search?keywords=emacs-goodies-el
 [emacs-goodies]: https://apps.fedoraproject.org/packages/emacs-goodies
 [textproc/markdown-mode]: http://pkgsrc.se/textproc/markdown-mode
 [emacs-markdown-mode-git]: https://aur.archlinux.org/packages/emacs-goodies-el/
 [macports-package]: https://trac.macports.org/browser/trunk/dports/editors/markdown-mode.el/Portfile
 [macports-ticket]: http://trac.macports.org/ticket/35716
 [freebsd-port]: http://svnweb.freebsd.org/ports/head/textproc/markdown-mode.el

Installation:

Make sure to place `markdown-mode.el` somewhere in the load-path and add
the following lines to your `.emacs` file to associate markdown-mode
with `.text`, `.markdown`, and `.md` files:

    (autoload 'markdown-mode "markdown-mode"
       "Major mode for editing Markdown files" t)
    (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
    (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

There is no official Markdown file extension, nor is there even a
_de facto_ standard, so you can easily add, change, or remove any
of the file extensions above as needed.

`markdown-mode' depends on `cl-lib', which has been bundled with
GNU Emacs since 24.3.  Users of GNU Emacs 24.1 and 24.2 can install
`cl-lib' with `package.el'.
