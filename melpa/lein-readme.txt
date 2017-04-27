This provides an eshell interface to the Leiningen project
automation tool for Clojure. (http://leiningen.org) It communicates
over nREPL (https://github.com/kingtim/nrepl.el) to avoid starting
a new process for every command. Note that tasks which call
eval-in-project will still start a project JVM; it's only
Leiningen's own startup time which is avoided.

Usage

Currently you need to launch Leiningen once per Emacs instance with
M-x lein-launch. Then start eshell with M-x eshell and use
Leiningen as you would normally.
