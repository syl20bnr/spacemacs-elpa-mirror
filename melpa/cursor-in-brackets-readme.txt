The `cursor-in-brackets-mode` is a minor mode
that moves a cursor in brackets/quotes
when a right bracket/quote is inserted after a left bracket/quote.
Supported brackets and quotes are listed in the following.

- ()
- {}
- []
- <>
- ""
- ''
- `'
- ``
- $$

The `cursor-in-brackets-mode` also supports region-based bracketing.
When a right bracket is inserted with a `transient-mark-mode` region,
the region becomes bracketed and the cursor is moved before the right bracket.
When a left bracket is inserted instead,
the region becomes bracketed and the cursor is moved after the left bracket.
