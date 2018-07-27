Spark is a straightforward Emacs Lisp port of Takaya OCHIAI's
cl-spark: https://github.com/tkych/cl-spark

Note: The characters used by spark is also dependent on face.

Use `spark' to generate a sparkline string.  Use `spark-v' to
instead create a vertical bar graph.

The `spark-ticks' and `spark-vticks' variables hold the
characters used to draw the graph for `spark' and `spark-v'
respectively.

Usage:

(require 'spark)
(spark '(0 30 55 80 33 150)) => \"▁▂▃▅▂█\"
