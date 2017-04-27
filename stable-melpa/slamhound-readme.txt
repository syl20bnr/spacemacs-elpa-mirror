Destroys the ns form of a clojure-mode buffer and attempts to
rebuild it by searching the classpath. Requires an active
connection to either slime or nrepl.

M-x slamhound

If the namespace cannot be reconstructed for whatever reason, the
file will remain untouched and the reason will be shown.
