This provides flycheck integration for processing-mode. It allows flycheck to
use the processing-java command-line tool to parse results for its display.

Basic usage:

 (require 'flycheck-processing)
 (flycheck-processing-setup)
