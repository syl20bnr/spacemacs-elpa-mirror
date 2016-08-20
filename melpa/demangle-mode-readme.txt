`demangle-mode' is an Emacs minor mode that automatically demangles
C++ symbols.  For example, in this mode:

- `_ZNSaIcED2Ev' displays as `std::allocator<char>::~allocator()'
- `_ZTISt10ostrstream' displays as `typeinfo for std::ostrstream'
- `_GLOBAL__I_abc' displays as `global constructors keyed to abc'

See <https://github.com/liblit/demangle-mode#readme> for additional
documentation: usage suggestions, background & motivation,
compatibility notes, and known issues & design limitations.

Visit <https://github.com/liblit/demangle-mode/issues> or use
command `demangle-mode-submit-bug-report' to report bugs or offer
suggestions for improvement.
