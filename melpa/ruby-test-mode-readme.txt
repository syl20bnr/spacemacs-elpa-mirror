This mode provides commands for running ruby tests.  The output is
shown in separate buffer '*Ruby-Test*' in ruby-test
mode.  Backtraces from failures and errors are marked, and can be
clicked to bring up the relevent source file, where point is moved
to the named line.

The tests can be both, either rspec behaviours, or unit
tests.  (File names are assumed to end in _spec.rb or _test.rb to
tell the type.)  When the command for running a test is invoked, it
looks at several places for an actual test to run: first, it looks
if the current buffer is a test (or spec), secondly, if not, it
checks whether one of the visible buffers is, thirdly it looks if
there has been a test run before (during this session), in which
case that test is invoked again.

Using the command `ruby-test-run-test-at-point', you can run test
cases separately from others in the same file.

Keybindings:

C-c C-t n    - Runs the current buffer's file as an unit test or an
C-c C-t C-n    rspec example.

C-c C-t t    - Runs the unit test or rspec example at the current buffer's
C-c C-t C-t    buffer's point.

C-c C-s      - Toggle between implementation and test/example files.


(require 'ruby-mode)
(require 'pcre2el)
