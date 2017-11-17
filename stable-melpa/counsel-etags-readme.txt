Usage:
  "M-x counsel-etags-find-tag-at-point" to navigate.  This command will also
  run `counsel-etags-scan-code' automatically if tags file is not built yet.

  "M-x counsel-etags-scan-code" to create tags file
  "M-x counsel-etags-grep" to grep
  "M-x counsel-etags-grep-symbol-at-point" to grep the symbol at point
  "M-x counsel-etags-recent-tag" open recent tag

That's all!

Tips:
- Add below code into "~/.emacs" to auto-update scan code:
  (add-hook 'prog-mode-hook
    (lambda ()
      (add-hook 'after-save-hook
                'counsel-etags-virtual-update-tags 'append 'local)))

- You can use ivy's negative pattern to filter candidates.
  For example, input "keyword1 !keyword2 keyword3" means:
  "(keyword1 and (not (keyword2 or keyword3))"

See https://github.com/redguardtoo/counsel-etags/ for more advanced tips.
