Most of this is taken straight from ag.el, and then heavily modified to query a
running hound server instead of running ag to provide the search results.

At a high level, this will send a query to the hound service api, parse the json
response, massage that data and dump it into a buffer, then apply a compilation
mode to the buffer that lets you jump directly to the search result files.
