Usage:
  "Exuberant Ctags" and "GNU Find" should exist at first.

  "M-x counsel-etags-find-tag-at-point" to navigate.  This command will also
  run `counsel-etags-scan-code' automatically if tags file is not built yet.

  "M-x counsel-etags-scan-code" to create tags file
  "M-x counsel-etags-grep" to grep
  "M-x counsel-etags-grep-symbol-at-point" to grep the symbol at point
  "M-x counsel-etags-recent-tag" to open recent tag
  "M-x counsel-etags-find-tag" to fuzzy searching tag

That's all!

Tips:
- Add below code into "~/.emacs" to auto-update scan code:

  ;; Don't ask before rereading the TAGS files if they have changed
  (setq tags-revert-without-query t)
  ;; Don't warn when TAGS files are large
  (setq large-file-warning-threshold nil)
  (add-hook 'prog-mode-hook
    (lambda ()
      (add-hook 'after-save-hook
                'counsel-etags-virtual-update-tags 'append 'local)))

- You can use ivy's negative pattern to filter candidates.
  For example, input "keyword1 !keyword2 keyword3" means:
  "(keyword1 and (not (keyword2 or keyword3))"

- You can setup `counsel-etags-ignore-directories' and `counsel-etags-ignore-filenames',
  (eval-after-load 'counsel-etags
    '(progn
       ;; counsel-etags-ignore-directories does NOT support wildcast
       (add-to-list 'counsel-etags-ignore-directories "build_clang")
       (add-to-list 'counsel-etags-ignore-directories "build_clang")
       ;; counsel-etags-ignore-filenames supports wildcast
       (add-to-list 'counsel-etags-ignore-filenames "TAGS")
       (add-to-list 'counsel-etags-ignore-filenames "*.json")))

See https://github.com/redguardtoo/counsel-etags/ for more tips.
