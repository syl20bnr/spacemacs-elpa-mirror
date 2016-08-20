`matrix-client' is a chat client and API library for the Matrix.org decentralized RPC
system. `(package-install 'matrix-client)' and then either deploy your own homeserver or register
an account on the public homeserver https://matrix.org/beta/#/login . After you've done that, M-x
matrix-client will set you up with buffers corresponding to your Matrix rooms. You can join new
ones with /join, leave with /leave or /part, and hook in to the custom functions provided by
=matrix-client=.

Implementation-wise `matrix-client' itself provides most of the core plumbing for
an interactive Matrix chat client. It uses the Matrix event stream framework
to dispatch a global event stream to individual rooms. There are a set of
'event handlers' and 'input filters' in `matrix-client-handlers' which are used to
implement the render flow of the various event types and actions a user can
take.
