Enable AUCTeX-preview in non-LaTeX modes.
Strategy:
- collect all header contents and latex fragments from the source buffer
- prepare a LaTeX document from this information
- let preview do its stuff in the LaTeX document
- move the preview overlays from the LaTeX document to the source buffer

Install texfrag via Melpa by M-x package-install texfrag.

If texfrag is activated for some buffer the image overlays for LaTeX fragments
such as equations known from AUCTeX preview can be generated
with the commands from the TeX menu (e.g. "Generate previews for document").

The major mode of the source buffer should have a
texfrag setup function registered in `texfrag-setup-alist'.
Thereby, it is sufficient if the major mode is derived
from one already registered in `texfrag-setup-alist'.

The defaults are adapted to doxygen.
For the support of LaTeX fragments in doxygen comments put the following
into your init file:

The default prefix-key sequence for texfrag-mode is the same as for preview-mode, i.e., C-c C-p.
You can change the default prefix-key sequence by customizing texfrag-prefix.
If you want to modify the prefix key sequence just for one major mode use
`texfrag-set-prefix' in the major mode hook before you run texfrag-mode.

You can adapt the LaTeX-header to your needs by buffer-locally setting
the variable `texfrag-header-function' to a function without arguments
that returns the LaTeX header as a string.  Inspect the definition of
`texfrag-header-default' as an example.

The easiest way to adapt the LaTeX fragment syntax of some major mode
is to set `texfrag-frag-alist' in the mode hook of that major mode.
For `org-mode' the function `texfrag-org-mode-hook-function'
can be used as minimal implementation of such a hook function.
Install it via
(add-hook 'org-mode-hook #'texfrag-org-mode-hook-function)
if you like.  Note that this function only handles the most primitive
syntax for LaTeX fragments in org-mode buffers, i.e., $...$ and \[\].

For more complicated cases you can install your own
parsers of LaTeX fragments in the variable
`texfrag-next-frag-function' (see the documentation of that variable).


Requirements:
- depends on Emacs "25" (because of when-let)
- requires AUCTeX with preview.el.
