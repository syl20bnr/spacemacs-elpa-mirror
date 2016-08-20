Shows word count per heading line, summed over sub-headings.
Aims to be fast, so doesn't check carefully what it's counting.  ;-)

Implementation based on:
- Paul Sexton's word count posted on org-mode mailing list 21/2/11.
- clock overlays

v2
29/4/11
Don't modify buffer, and fixed handling of empty sections.

v3
29/4/11
Handle narrowing correctly, so partial word count works on narrowed regions.
