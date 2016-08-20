This package provides a minor mode to highlight code coverage in
source code files.

At present it only knows how to parse coverage data in the format
provided by the Simplecov gem (specifically, the RSpec results in
the .resultset.json file it outputs).

Coverage highlighting will be automatically updated whenever the
coverage results change after running specs.  You can individually
toggle Coverage on/off in as many buffers as you like.


(require 'json)
(require 'ov)
(require 'cl-lib)
(require 'timer)
(autoload 'vc-git-root "vc-git")
