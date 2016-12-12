mentor is a GNU Emacs frontend for the `rTorrent' bittorrent client.  It uses
XML-RPC to communicate with rTorrent, and needs rTorrent to be configured
accordingly.

This project aims to provide a feature complete and highly customizable
interface, which will feel familiar to Emacs users. Key bindings are chosen
to be as close to the vanilla rTorrent curses interface as possible.

mentor still has some way to go before it can really be considered a complete
interface, so please moderate your expectations. It works fine for many
common tasks though, and in some cases (dare we say?) much better than the
standard ncurses interface.

* QUICK START

Assuming you are installing mentor through MELPA, which is the recommended
way, here are some instructions to get you started. Otherwise, please see the
README.org file included in the repository for additional instructions.

1. Add this to your init.el:

   (require 'mentor)
   (setq mentor-rtorrent-url "scgi://localhost:5000")

   Evaluate these lines or restart Emacs.

2. Add this to your ~/.rtorrent.rc:

   scgi_port = 127.0.0.1:5000
   xmlrpc_dialect = i8
   encoding_list = UTF-8

   Make sure `scgi_port' matches `mentor-rtorrent-url' above. Restart rTorrent.

3. To start mentor, run:

   M-x mentor

** ADDITIONAL CONFIGURATION

You can find Mentor in the `External' customize group.

  M-x customize

** CONFIGURING RTORRENT

For more information on configuring rTorrent, refer to:

http://libtorrent.rakshasa.no/wiki/RTorrentXMLRPCGuide

* KNOWN ISSUES

- The file view needs much love, and is currently not known to be
  working. Sorry.

- There is no view for trackers/peers/extra information.

- There is currently no support (patches welcome) for communicating with
  rtorrent over a local socket using the more secure scgi_local command. This
  means it is currently not advisable to use mentor on anything but single
  user systems.

- mentor currently has some performance issues if you have many torrents
  (several hundreds). Be aware.

* CONTACT

You can find the latest development version of mentor here:

http://www.github.com/skangas/mentor

Bug reports, comments, and suggestions are welcome! Send them to
<skangas@skangas.se> or report them on GitHub.
