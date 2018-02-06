Library for generic timestamp handling.  It is targeted at bulk
processing, therefore many functions are optimized for speed, but
not necessarily for ease of use.  For example, formatting is done
in two steps: first you need to generate a formatting function for
given pattern, and only using it obtain formatted strings.

Package's main feature currently is timestamp formatting based on
Java pattern.  Arbitrary timezones and locales (i.e. not
necessarily those used by the system) are supported.  See function
`datetime-float-formatter' for details.

Library also supports timestamp matching.  It can generate regular
expressions that match timestamps corresponding to given pattern.
These regular expressions can give false positives, but for most
purposes are good enough to detect timestamps in text files,
e.g. in various application logs.  See `datetime-matching-regexp'.

Finally, library provides functions to select an appropriate
timestamp format for given locale.  For example, function
`datetime-locale-date-pattern' returns a Java pattern suitable for
formatting date only, without time part.  However, it is not
required that formats are generated this way.

Timestamp parsing is currently not implemented, but planned for a
future version.
