org-toc helps you to have an up-to-date table of contents in org files
without exporting (useful primarily for readme files on GitHub).

After installation put into your .emacs file something like

(if (require 'org-toc nil t)
    (add-hook 'org-mode-hook 'org-toc-enable)
  (warn "org-toc not found"))

And every time you'll be saving an org file, the first headline with a :TOC:
tag will be updated with the current table of contents.

For details, see https://github.com/snosov1/org-toc

