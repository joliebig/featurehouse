module Network.GnuTLS.OID where

#include <gnutls/x509.h>

type OID = String

oidX520CountryName :: OID
oidX520CountryName = #{const_str GNUTLS_OID_X520_COUNTRY_NAME}

oidX520OrganizationName :: OID
oidX520OrganizationName = #{const_str GNUTLS_OID_X520_ORGANIZATION_NAME}

oidX520OrganizationalUnitName :: OID
oidX520OrganizationalUnitName = #{const_str GNUTLS_OID_X520_ORGANIZATIONAL_UNIT_NAME}

oidX520CommonName :: OID
oidX520CommonName = #{const_str GNUTLS_OID_X520_COMMON_NAME}

oidX520LocalityName :: OID
oidX520LocalityName = #{const_str GNUTLS_OID_X520_LOCALITY_NAME}

oidX520StateOrProvinceName :: OID
oidX520StateOrProvinceName = #{const_str GNUTLS_OID_X520_STATE_OR_PROVINCE_NAME }

oidX520Initials :: OID
oidX520Initials = #{const_str GNUTLS_OID_X520_INITIALS}

oidX520GenerationQualifier :: OID
oidX520GenerationQualifier = #{const_str GNUTLS_OID_X520_GENERATION_QUALIFIER}

oidX520Surname :: OID
oidX520Surname = #{const_str GNUTLS_OID_X520_SURNAME}

oidX520GivenName :: OID
oidX520GivenName = #{const_str GNUTLS_OID_X520_GIVEN_NAME}

oidX520Title :: OID
oidX520Title = #{const_str GNUTLS_OID_X520_TITLE}

-- ...

oidPKCS9Email :: OID
oidPKCS9Email = #{const_str GNUTLS_OID_PKCS9_EMAIL}

-- ...

oidX509v3SubjectKeyIdentifier :: OID
oidX509v3SubjectKeyIdentifier = "2.5.29.14"
