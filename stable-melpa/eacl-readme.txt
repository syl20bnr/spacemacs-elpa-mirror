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

GNU Grep, Emacs 24.3 and counsel (https://github.com/abo-abo/swiper)
are required.

Please use HomeBrew (https://brew.sh/) to install GNU Grep on macOS.
Then insert `(setq eacl-grep-program "ggrep")' into "~/.emacs".
The bundled "BSD Grep" on macOS is too outdated to use.
