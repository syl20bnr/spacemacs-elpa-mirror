cryptsy-public-api provides library functionality for working with the
Cryptsy public API.  The public API can be used to query general market
information for crypto currencies.

There are two main methods:
  - cryptsy-public-api-get-market-data
  - cryptsy-public-api-get-orderbook-data

Leaving off the `market-id' parameter or using `:all' will fetch general
information rather than data for a single market.

This library also supplies macros for creating helper methods to access
markets directly rather than messing around with JSON responses.  The
following macros are available:

  - cryptsy-public-api-def-info-accessors
    This creates functions needed to access a currency's basic info
    including the last trade price, time and volume.


For more information on the API, see https://www.cryptsy.com/pages/publicapi
