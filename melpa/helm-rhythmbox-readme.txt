Control Rhythmbox's play queue via Helm.

Start by calling `helm-rhythmbox' to browse the library retrieved from a
running Rhythmbox instance via D-Bus.  The default action is to play the
selected song.  There is also the option to enqueue the selected song, this
also works for a selection.

The whole library is retrieved on first use and can be reloaded
with `helm-rhythmbox-reload-library'.

Note that candidates formatted with `helm-rhythmbox-candidate-format' are
cached, so to see the effect of changing `helm-rhythmbox-candidate-format'
(after already having loaded the library) requires reloading the library to
reset the cache.
