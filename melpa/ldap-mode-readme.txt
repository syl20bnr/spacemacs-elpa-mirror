LDAP schema is defined in RFC 2252.
LDIF is defined in RFC 2849.
Both sorts of file are defined to use UTF-8 strings and so should
presumably use UTF-8 as the file coding system.

The RFC ASN.1 description of schemas isn't actually the syntax you
need to put in OpenLDAP schema files -- each object or attribute
definition is prefixed by an option name.  See
`ldap-convert-asn1-schema' for conversion.

This file isn't called ldap.el, since that's part of EUDC.
