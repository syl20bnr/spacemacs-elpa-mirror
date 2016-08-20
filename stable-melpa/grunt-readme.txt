I got tired of managing shells and one off Async Command buffers to
kick off Grunt tasks. This package provides rudimentary access to
the tasks in a Gruntfile.

When your default-directory is somewhere in a JS project with a
Gruntfile, invoke `grunt-exec' or bind something to it. You can
either execute one of the suggested registered tasks, or input a
custom task of your own. It will create one buffer per project per
task, killing any existing buffers by default.
