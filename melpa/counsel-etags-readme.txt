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

- Use `grep-find-ignored-directories', `grep-find-ignored-files' to ignore directories/files,

  (eval-after-load 'grep
    '(progn
       (dolist (v '("auto"
                    "target"
                    "node_modules"
                    "bower_components"
                    "*dist"
                    ".sass_cache"
                    ".cache"
                    ".npm"
                    "elpa"))
         (add-to-list 'grep-find-ignored-directories v))

       (dolist (v '("*.min.js"
                    "*.map"
                    "*.bundle.js"
                    "*.min.css"
                    "tags"
                    "TAGS"
                    "GTAGS"
                    "GRTAGS"
                    "GPATH"
                    "cscope.files"
                    "*.json"
                    "*.log"))
       (add-to-list 'grep-find-ignored-files v))))

See https://github.com/redguardtoo/counsel-etags/ for more tips.
