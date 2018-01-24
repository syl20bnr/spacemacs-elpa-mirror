This program lets you run an xquery against a file, via saxonb.

If the result contains nodes, it tries to link the results back to
the original file.

To use, customize the `xquery-tool-java-binary' and
`xquery-tool-saxonb-jar' settings (M-x customize-group RET
xquery-tool), and then call `xquery-tool-query' from a buffer
visiting an xml document.

TODOs:
- add different backends (basex?)
- find additional xquery/xpath test cases (maybe from https://dev.w3.org/2011/QT3-test-suite/)
- search "TODO" in this file
