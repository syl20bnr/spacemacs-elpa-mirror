Multiple commands are provided to grep files in the project to get
auto complete candidates.
The keyword to grep is text from line beginning to current cursor.
Project is *automatically* detected if Git/Mercurial/Subversion is used.
You can override the project root by setting `eacl-project-root',

List of commands,

`eacl-complete-line' complete line.  You could assign key binding
"C-x C-l" to this command.

`eacl-complete-statement' completes statement which ends with ";".
For example, input "import" and run this command.

`eacl-complete-snippet' completes snippets which ends with "}".
For example, input "if" and run this command.

`eacl-complete-tag' completes HTML tag which ends with ">".
For example, input "<div" and run this command.

Modify `grep-find-ignored-directories' and `grep-find-ignored-files'
to setup directories and files grep should ignore:
  (eval-after-load 'grep
    '(progn
       (dolist (v '("node_modules"
                    "bower_components"
                    ".sass_cache"
                    ".cache"
                    ".npm"))
         (add-to-list 'grep-find-ignored-directories v))
       (dolist (v '("*.min.js"
                    "*.bundle.js"
                    "*.min.css"
                    "*.json"
                    "*.log"))
         (add-to-list 'grep-find-ignored-files v))))

Or you can setup above ignore options in ".dir-locals.el".
The content of ".dir-locals.el":
  ((nil . ((eval . (progn
                     (dolist (v '("node_modules"
                                  "bower_components"
                                  ".sass_cache"
                                  ".cache"
                                  ".npm"))
                       (add-to-list 'grep-find-ignored-directories v))
                     (dolist (v '("*.min.js"
                                  "*.bundle.js"
                                  "*.min.css"
                                  "*.json"
                                  "*.log"))
                       (add-to-list 'grep-find-ignored-files v)))))))

GNU Grep v3.1+, Emacs v24.3 and Ivy (https://github.com/abo-abo/swiper)
are required.

On macOS:
  - Use HomeBrew (https://brew.sh/) to install latest GNU Grep on macOS
  - Insert `(setq eacl-grep-program "ggrep")' into "~/.emacs".
  - Bundled "BSD Grep" is too outdated to use
