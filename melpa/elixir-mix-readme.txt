  Manual Installation:

   (add-to-list 'load-path "~/path/to/elixir-mix.el/")
   (require 'elixir-mix)
   (global-elixir-mix-mode)

  Interesting variables are:

      `elixir-mix-command`

           Path to the executable <mix> command

      `elixir-mix-buffer-name`

           Name for the buffer used for mix shell output.

  Major commands are:

       M-x elixir-mix-new

           Create a new Elixir application.

       M-x elixir-mix-test

           Run the whole Elixir application test suite.

       M-x elixir-mix-test-this-buffer

           Run the current buffer through <mix test> command.

       M-x elixir-mix-test-file

           Run a file through <mix test> command.

       M-x elixir-mix-test-at-point

           Run the test at point.

       M-x elixir-mix-compile

           Compile the whole Elixir application.

       M-x elixir-mix-run

           Runs the given expression in the Elixir application context.

       M-x elixir-mix-deps-with-prompt

           Prompt for mix deps commands.

       M-x elixir-mix-local-with-prompt

           Prompt for mix local commands.

       M-x elixir-mix-local-install

           Prompt for mix local.install <path> or <url>.

       M-x elixir-mix-local-install-with-path

           Runs local.install and prompt for a <path> as argument.

       M-x elixir-mix-local-install-with-url

           Runs local.install and prompt for a <url> as argument.

       M-x elixir-mix-help

           Show help output for a specific mix command.

       M-x elixir-mix-execute

           Run any command in the context of the application.
           Just run any command as you like, including arguments
           for the specific command.  (example: test --quick)
