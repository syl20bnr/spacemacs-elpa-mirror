Flycheck checker for json-mode using jsonlint from demjson which support lint rules.

# Setup
  0. Uninstall zaach/jsonlint if necessary to avoid conflicts: npm -g uninstall jsonlint
  1. Install demjson: pip install demjson or easy_install demjson
  2. (require 'flycheck-demjsonlint) in your init file

# Lint Rules(.demjsonrc)
  * Built-in support: .eslintrc, package.json
  * Precedence: ~/.demjsonrc over pkg-dir/.demjsonrc
  * Format: one record per line, `filename=jsonlint-cmd-options`, e.g. `.eslintrc=-S`
  * Implementation: demjsonlint is just a wrapper of jsonlint from demjson, jsonlint-cmd-options is passed to jsonlint as is
