This package provides an evil operator to replace chars of a text object with a given char.
one use case is fill the insides of a markdown table headline as follows (cursor is on []):

|desc | item|
| []  |     |
-> zxi|-
|desc | item|
|-----|     |
-> wh.
|desc | item|
|-----|-----|
