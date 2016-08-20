* ASN.1/GDMO mode for GNU Emacs

This is a major mode for editing ASN.1/GDMO files.

** Setup

For installation, please add the following lines to your ~/.emacs:

: (add-to-list 'auto-mode-alist '("\\.asn1$" . asn1-mode))
: (add-to-list 'auto-mode-alist '("\\.gdmo$" . asn1-mode))

** Reference

- [[http://www.itu.int/ITU-T/recommendations/rec.aspx?rec=9604]
   [ITU-T X.680 Information technology – Abstract Syntax Notation
   One (ASN.1): Specification of basic notation]]
- [[http://www.itu.int/ITU-T/recommendations/rec.aspx?rec=9605]
   [ITU-T X.681 Information technology – Abstract Syntax Notation
   One (ASN.1): Information object specification]]
- [[http://www.itu.int/ITU-T/recommendations/rec.aspx?rec=3061]
   [ITU-T X.722: INFORMATION TECHNOLOGY – OPEN SYSTEMS
    INTERCONNECTION – STRUCTURE OF MANAGEMENT INFORMATION:
    GUIDELINES FOR THE DEFINITION OF MANAGED OBJECTS]]
