This library enables the completion of C/C++ header file names using Company.

To initialize it, just add it to `company-backends':

(add-to-list 'company-backends 'company-c-headers)

When you type an #include declaration within a supported major mode (see
`company-c-headers-modes'), company-c-headers will search for header files
within predefined search paths.  company-c-headers can search "system" and
"user" paths, depending on the type of #include declaration you type.

You will probably want to customize the `company-c-headers-path-user' and
`company-c-headers-path-system' variables for your specific needs.
