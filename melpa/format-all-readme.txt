Lets you auto-format source code in many languages using the same
command for all languages, instead of learning a different elisp
package and formatting command for each language.  Just do M-x
`format-all-buffer' and it will try its best to do the right thing.

Supported languages:

- C/C++/Objective-C (clang-format)
- Crystal (crystal tool format)
- CSS/Less/SCSS (prettier)
- D (dfmt)
- Elixir (mix format)
- Elm (elm-format)
- Emacs Lisp (native)
- Go (gofmt)
- GraphQL (prettier)
- Haskell (hindent)
- HTML/XHTML/XML (tidy)
- Java (clang-format)
- JavaScript/JSON/JSX/TypeScript/Vue (prettier)
- Kotlin (ktlint)
- Markdown (prettier)
- OCaml (ocp-indent)
- Perl (perltidy)
- Protocol Buffers (clang-format)
- Python (autopep8)
- Ruby (rufo)
- Rust (rustfmt)
- Shell script (shfmt)
- SQL (sqlformat)
- Swift (swiftformat)
- YAML (yq)

You will need to install external programs to do the formatting.
If `format-all-buffer` can't find the right program, it will try to
tell you how to install it.

A local minor mode called `format-all-mode` is available to format
code on save.  Please see the documentation for that function for
instructions.

There are currently no customize variables, since it's not clear
what approach should be taken.  Please see
https://github.com/lassik/emacs-format-all-the-code/issues for
discussion.

Many of the external formatters support configuration files in the
source code directory to control their formatting.  Please see the
documentation for each formatter.

New external formatters can be added easily if they can read code
from standard input and format it to standard output.  Feel free to
submit a pull request or ask for help in GitHub issues.
