Allow for execution of ruby processes dumping the results into a
compilation buffer.  Useful for executing tests, or rake tasks
where the ability to jump to errors in source code is desirable.

The functions you will probably want to use are

ruby-compilation-run
ruby-compilation-rake
ruby-compilation-this-buffer (C-x t)
ruby-compilation-this-test (C-x T)
