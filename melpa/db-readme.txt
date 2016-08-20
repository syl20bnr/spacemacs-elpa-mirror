This is a simple database interface and implementation.

It should be possible to specify any kind of key/value database
with this interface.

The supplied implementation is an Emacs hash-table implementation
backed with serializing objects.  It is NOT intended for anything
other than very simple use cases and will not scale very well at
all.

However, other implementations (mongodb, redis or PostgreSQL
hstore) would be easy to implement and fit in here.
