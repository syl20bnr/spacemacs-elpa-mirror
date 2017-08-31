markdown-mode is a major mode for editing [Markdown][]-formatted
text.  The latest stable version is markdown-mode 2.3, released on
August 31, 2017.  See the [release notes][] for details.
markdown-mode is free software, licensed under the GNU GPL,
version 3 or later.

![Markdown Mode Screenshot](https://jblevins.org/projects/markdown-mode/screenshots/20170818-001.png)

[Markdown]: http://daringfireball.net/projects/markdown/
[release notes]: https://jblevins.org/projects/markdown-mode/rev-2-3

Documentation:

<a href="https://leanpub.com/markdown-mode">
<img src="https://jblevins.org/projects/markdown-mode/guide-v2.3.png" align="right" height="350" width="231">
</a>

The primary documentation for Markdown Mode is available below, and
is generated from comments in the source code.  For a more in-depth
treatment, the [_Guide to Markdown Mode for Emacs_][guide] covers
Markdown syntax, advanced movement and editing in Emacs,
extensions, configuration examples, tips and tricks, and a survey
of other packages that work with Markdown Mode.  Finally, Emacs is
also a self-documenting editor.  This means that the source code
itself contains additional documentation: each function has its own
docstring available via `C-h f` (`describe-function'), individual
keybindings can be investigated with `C-h k` (`describe-key'), and
a complete list of keybindings is available using `C-h m`
(`describe-mode').

 [guide]: https://leanpub.com/markdown-mode

Installation:

_Note:_ To use all of the features of `markdown-mode', you'll need
to install the Emacs package itself and also have a local Markdown
processor installed (e.g., Markdown.pl, MultiMarkdown, or Pandoc).
The external processor is not required for editing, but will be
used for rendering HTML for preview and export. After installing
the Emacs package, be sure to configure `markdown-command' to point
to the preferred Markdown executable on your system.  See the
Customization section below for more details.

The recommended way to install `markdown-mode' is to install the package
from [MELPA Stable](https://stable.melpa.org/#/markdown-mode)
using `package.el'. First, configure `package.el' and the MELPA Stable
repository by adding the following to your `.emacs', `init.el',
or equivalent startup file:

``` Lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
```

Then, after restarting Emacs or evaluating the above statements, issue
the following command: `M-x package-install RET markdown-mode RET`.
When installed this way, the major modes `markdown-mode' and `gfm-mode'
will be autoloaded and `markdown-mode' will be used for file names
ending in either `.md` or `.markdown`.

Alternatively, if you manage loading packages with [use-package][]
then you can automatically install and configure `markdown-mode' by
adding a declaration such as this one to your init file (as an
example; adjust settings as desired):

``` Lisp
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
```

[MELPA Stable]: http://stable.melpa.org/
[use-package]: https://github.com/jwiegley/use-package

**Direct Download**

Alternatively you can manually download and install markdown-mode.
First, download the [latest stable version][markdown-mode.el] and
save the file where Emacs can find it (i.e., a directory in your
`load-path'). You can then configure `markdown-mode' and `gfm-mode'
to load automatically by adding the following to your init file:

``` Lisp
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
```

[markdown-mode.el]: https://jblevins.org/projects/markdown-mode/markdown-mode.el

**Development Version**

To follow or contribute to markdown-mode development, you can
browse or clone the Git repository
[on GitHub](https://github.com/jrblevin/markdown-mode):

```
git clone https://github.com/jrblevin/markdown-mode.git
```

If you prefer to install and use the development version, which may
become unstable at some times, you can either clone the Git
repository as above or install markdown-mode from
[MELPA](https://melpa.org/#/markdown-mode).

If you clone the repository directly, then make sure that Emacs can
find it by adding the following line to your startup file:

``` Lisp
(add-to-list 'load-path "/path/to/markdown-mode/repository")
```

**Packaged Installation**

markdown-mode is also available in several package managers. You
may want to confirm that the package you install contains the
latest stable version first (and please notify the package
maintainer if not).

   * Debian Linux: [elpa-markdown-mode][] and [emacs-goodies-el][]
   * Ubuntu Linux: [elpa-markdown-mode][elpa-ubuntu] and [emacs-goodies-el][emacs-goodies-el-ubuntu]
   * RedHat and Fedora Linux: [emacs-goodies][]
   * NetBSD: [textproc/markdown-mode][]
   * MacPorts: [markdown-mode.el][macports-package] ([pending][macports-ticket])
   * FreeBSD: [textproc/markdown-mode.el][freebsd-port]

 [elpa-markdown-mode]: https://packages.debian.org/sid/lisp/elpa-markdown-mode
 [elpa-ubuntu]: http://packages.ubuntu.com/search?keywords=elpa-markdown-mode
 [emacs-goodies-el]: http://packages.debian.org/emacs-goodies-el
 [emacs-goodies-el-ubuntu]: http://packages.ubuntu.com/search?keywords=emacs-goodies-el
 [emacs-goodies]: https://apps.fedoraproject.org/packages/emacs-goodies
 [textproc/markdown-mode]: http://pkgsrc.se/textproc/markdown-mode
 [macports-package]: https://trac.macports.org/browser/trunk/dports/editors/markdown-mode.el/Portfile
 [macports-ticket]: http://trac.macports.org/ticket/35716
 [freebsd-port]: http://svnweb.freebsd.org/ports/head/textproc/markdown-mode.el

**Dependencies**

To enable editing of code blocks in indirect buffers using `C-c '`,
you will need to install the [`edit-indirect'][ei] package.

  [ei]: https://github.com/Fanael/edit-indirect/
